#Facebook Recruiting IV: Human or Robot?
#Ver. 0.0.4 #time processing included, it still takes too long

#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("bit64")
require("tm")
require('doParallel')
require("glmnet")
require("penalizedLDA")
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
source(file.path(workingDirectory, "timeNumericFeatures.R"))
source(file.path(workingDirectory, "timeCharacterFeatures.R"))

#Detect available cores
numCores <- detectCores()

#Load Raw Data & data transformation----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)
bids <- fread(file.path(dataDirectory, directories$bids), verbose = TRUE)

bids$country[bids$country == ""] <- "NoCountry"

#Auction Time Medians + Median Average Deviations
auctionTimes <- lapply(unique(bids$auction), function(auctionId, bidsDt){
  auctionTimes <- bidsDt[auction == auctionId, time]  
  return(c(median(auctionTimes), mad(auctionTimes)))
}, bidsDt = bids)
#Assign names
names(auctionTimes) <- unique(bids$auction)

#Time numeric features
timeFeaturesTrain <- mclapply(train$bidder_id, timeNumericFeatures, mc.cores = numCores,
                                     bidsDt = bids, auctionsFeaturesList = auctionTimes)
timeFeaturesTest <- mclapply(test$bidder_id, timeNumericFeatures, mc.cores = numCores,
                                    bidsDt = bids, auctionsFeaturesList = auctionTimes)

numericalTimeFeaturesTrain <- lapply(timeFeaturesTrain, function(timeList){
  return(timeList[[1]])
})
characterTimeFeaturesTrain <- lapply(timeFeaturesTrain, function(timeList){
  return(timeList[[2]])
})
numericalTimeFeaturesTest <- lapply(timeFeaturesTest, function(timeList){
  return(timeList[[1]])
})
characterTimeFeaturesTest <- lapply(timeFeaturesTest, function(timeList){
  return(timeList[[2]])
})

numericalTimeFeaturesTrain <- as.matrix(do.call(rbind, numericalTimeFeaturesTrain))
numericalTimeFeaturesTest <- as.matrix(do.call(rbind, numericalTimeFeaturesTest))
                                     
#Transform training data and bidders data into a boilerplate
boilerplateTrain <- mclapply(train$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                                 bidsDT = bids, trainDt = train)
boilerplateTest <- mclapply(test$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                                bidsDT = bids, trainDt = test)

#Append character time rounded standard scores to boilerplates
for (i in 1:length(boilerplateTrain)){
  boilerplateTrain[[i]] <- c(boilerplateTrain[[i]], characterTimeFeaturesTrain[[i]])
}
for (i in 1:length(timeFeaturesTest)){
  timeFeaturesTest[[i]] <- c(timeFeaturesTest[[i]], characterTimeFeaturesTest[[i]])
}

#Create a data frame with the text data
boilerplateTrain <- do.call(c, boilerplateTrain)
boilerplateTest <- do.call(c, boilerplateTest)

rm(bids)

#Text processing / sparse matrix creation-------------
#Use TM Package to create corpus with TfIdf
corpusSparse <- removeSparseTerms(weightTfIdf(DocumentTermMatrix
                                              (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), normalize = TRUE),
                                  sparse = 0.9995)

corpusSparse <- sparseMatrix(i=corpusSparse$i,
                              j=corpusSparse$j,
                              x=corpusSparse$v, dims=c(corpusSparse$nrow, corpusSparse$ncol))

inherits(corpusSparse,"sparseMatrix")

#Split train and test boilerplates
corpusSparseTrain <- corpusSparse[1:nrow(train), ]
corpusSparseTest <- corpusSparse[(nrow(train) + 1):nrow(corpusSparse), ]

#Add time numerical 

rm(boilerplateTrain, boilerplateTest, corpusSparse)

#Model Comparison------------
#Ridge classification
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
                               standardize = FALSE, alpha = 0)

plot(GLMNETModelCVMini)

predictedBotsValidation <- signif(predict(GLMNETModelCVMini, newx =  corpusSparseTrain[dataSplits[[2]], ], 
                                          type = "response", s ='lambda.min'), digits = 5)

holdoutAuc <- auc(train$outcome[dataSplits[[2]]], predictedBotsValidation)
print(paste0("AUC score of the holdout data of: ", holdoutAuc))

rm(GLMNETModelCVMini, predictedBotsValidation)

#Penalized LDA
#Cross Validation
LDAModelMini <- PenalizedLDA.cv(x = scale(as.matrix(corpusSparseTrain[dataSplits[[1]], ])), 
                                y = train$outcome[dataSplits[[1]]] + 1,
                                nfold = 5)

plot(LDAModelMini)

# Perform penalized LDA 
LDAModelFull <- PenalizedLDA(x = corpusSparseTrain[dataSplits[[1]], ], 
                    y = train$outcome[dataSplits[[1]]],
                    lambda = LDAModelMini$bestlambda,
                    K = LDAModelMini$bestK)

#Modeling--------------------
#10 fold CV with glmnet
GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain, 
                           y = as.factor(train$outcome), 
                           nfolds = 10, parallel = TRUE, family = "binomial",
                           standardize = FALSE, alpha = 0)

plot(GLMNETModelCV)

#Make Predictions
predictedBots <- signif(predict(GLMNETModelCV, newx = corpusSparseTest, 
                                type = "response", s ='lambda.min'), digits = 5)

#Make a submission file------------------
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
sampleSubmissionFile$prediction <- predictedBots

#Write File
write.csv(sampleSubmissionFile, file = "GLMNETRidgeBagOWordsTfIdfIIII.csv", row.names = FALSE)
system('zip GLMNETRidgeBagOWordsTfIdfIIII.zip GLMNETRidgeBagOWordsTfIdfIIII.csv')