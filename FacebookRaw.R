#Facebook Recruiting IV: Human or Robot?
#Ver. 0.0.3 #Bag o' words model with weightTfIdf

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("bit64")
require("tm")
require('doParallel')
require("glmnet")
require("Metrics")

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
boilerplateTest <- mclapply(test$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                                bidsDT = bids, trainDt = test)
boilerplateTrain <- do.call(c, boilerplateTrain)
boilerplateTest <- do.call(c, boilerplateTest)

rm(bids)

#Text processing / sparse matrix creation-------------
#Use TM Package to create corpus
#corpusSparse <- removeSparseTerms(DocumentTermMatrix
#                                 (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), sparse = 0.999)

#Use TM Package to create corpus with TfIdf
corpusSparse <- removeSparseTerms(weightTfIdf(DocumentTermMatrix
                                              (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), normalize = TRUE),
                                  sparse = 0.999)

corpusSparse <- sparseMatrix(i=corpusSparse$i,
                              j=corpusSparse$j,
                              x=corpusSparse$v, dims=c(corpusSparse$nrow, corpusSparse$ncol))

inherits(corpusSparse,"sparseMatrix")

#Split train and test boilerplates
corpusSparseTrain <- corpusSparse[1:nrow(train), ]
corpusSparseTest <- corpusSparse[(nrow(train) + 1):nrow(corpusSparse), ]

rm(boilerplateTrain, boilerplateTest)

#Model Comparison------------
registerDoParallel(numCores)

#Do a simple data holdout of 20%
trainTrainLength <- floor(nrow(corpusSparseTrain) * 0.7)   #60% of training data used to train models
trainValidationLength <- floor(nrow(corpusSparseTrain) * 0.3)   #20% of training data to validate models
idxsdiff <- nrow(corpusSparseTrain) - (trainTrainLength + trainValidationLength)

set.seed(1001001)
groupsVector <- sample(c(rep(1, trainTrainLength), rep(2, trainValidationLength + idxsdiff)), 
                       nrow(corpusSparseTrain))

#Shuffle Indices
set.seed(1001002)
dataSplits <- split(seq(1, nrow(corpusSparseTrain)), as.factor(groupsVector))

#10 fold CV with glmnet
GLMNETModelCVMini <- cv.glmnet(x = corpusSparseTrain[dataSplits[[1]], ], 
                               y = as.factor(train$outcome)[dataSplits[[1]]], 
                               nfolds = 10, parallel = TRUE, family = "binomial",
                               standardize = FALSE, alpha = 0, nlambda = 150)

plot(GLMNETModelCVMini)

predictedBotsValidation <- signif(predict(GLMNETModelCVMini, newx =  corpusSparseTrain[dataSplits[[2]], ], 
                                          type = "response", s ='lambda.min'), digits = 5)

holdoutAuc <- auc(train$outcome[dataSplits[[2]]], predictedBotsValidation)
print(paste0("AUC score of the holdout data of: ", holdoutAuc))

rm(GLMNETModelCVMini, predictedBotsValidation)

#Modeling--------------------
#5 fold CV with glmnet
GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain, 
                           y = as.factor(train$outcome), 
                           nfolds = 10, parallel = TRUE, family = "binomial",
                           standardize = FALSE, alpha = 0, nlambda = 150)

plot(GLMNETModelCV)

#Make Predictions
predictedBots <- signif(predict(GLMNETModelCV, newx = corpusSparseTest, 
                                type = "response", s ='lambda.min'), digits = 5)

#Make a submission file------------------
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
sampleSubmissionFile$prediction <- predictedBots

#Write File
write.csv(sampleSubmissionFile, file = "GLMNETRidgeBagOWordsTfIdfI.csv", row.names = FALSE)
system('zip GLMNETRidgeBagOWordsTfIdfI.zip GLMNETRidgeBagOWordsTfIdfI.csv')