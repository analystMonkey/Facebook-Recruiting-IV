#Facebook Recruiting IV: Human or Robot?
#Ver. 0.0.2 #Bag o' words model built

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("bit64")
require("tm")
require('doParallel')
require("glmnet")

#Read Settings file
directories <- fromJSON(file = "SETTINGS.json")

#Set Directories
workingDirectory <- directories$workingDirectory
setwd(workingDirectory)
#Set Data Directory
dataDirectory <- directories$dataDirectory
#EDA Plots location
EDAPlotsLoc <- directories$EDALoc
#h2o location
h2o.jarLoc <- directories$h2o.jarLoc

#Define extra functions
source(file.path(workingDirectory, "bidderId2Boilerplate.R"))

#Detect available cores
numCores <- detectCores()

#Load Raw Data & data transformation----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)
bids <- fread(file.path(dataDirectory, directories$bids), verbose = TRUE)

#Transform training data and bidders data into a boilerplate
boilerplateTrain <- mclapply(train$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                                 bidsDT = bids, trainDt = train)
boilerplateListTest <- mclapply(test$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                                bidsDT = bids, trainDt = test)
boilerplateTrain <- do.call(c, boilerplateTrain)
boilerplateTest <- do.call(c, boilerplateListTest)

rm(bids)

#Text processing / sparse matrix creation-------------
#Use TM Package to create corpus
corpusSparse <- removeSparseTerms(DocumentTermMatrix
                                  (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), sparse = 0.995)

corpusSparse <- sparseMatrix(i=corpusSparse$i,
                              j=corpusSparse$j,
                              x=corpusSparse$v, dims=c(corpusSparse$nrow, corpusSparse$ncol))

inherits(corpusSparse,"sparseMatrix")

#Split train and test boilerplates
corpusSparseTrain <- corpusSparse[1:nrow(train), ]
corpusSparseTest <- corpusSparse[(nrow(train) + 1):nrow(corpusSparse), ]

#Model Comparison
#5 fold CV with glmnet
registerDoParallel(numCores)

GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain, 
                           y = as.factor(train$outcome), 
                           nfolds = 10, parallel = TRUE, family = "binomial")

plot(GLMNETModelCV)
coef(GLMNETModelCV)

#Full Model
GLMNETFullModel <- GLMNETModelCV$glmnet.fit

#Make Predictions
predictedBots <- signif(predict(GLMNETModelCV, newx = corpusSparseTest, type = "response", s ='lambda.min'), digits = 5)

#Make a submission file------------------
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
sampleSubmissionFile$prediction <- predictedBots

#Write File
write.csv(sampleSubmissionFile, file = "GLMNETBagOWordsI.csv", row.names = FALSE)
system('zip GLMNETBagOWordsI.zip GLMNETBagOWordsI.csv')