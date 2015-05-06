#Facebook Recruiting IV: Human or Robot?
#Ver. 0.0.8 #Ranking corrected + full combined features in bids
#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("bit64")
require("tm")
require('doParallel')
require("glmnet")
require("e1071")
require("SparseM")
require("Metrics")
require("ggplot2")

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
#vw77Dir = workingDirectory
vwDataDirectory <- file.path(directories$dataDirectory, "vw")
hipersearchScriptLoc <- directories$vwSearchLoc
vw77Dir <- directories$vwLoc

#Define extra functions
source(file.path(workingDirectory, "bidderId2Boilerplate.R"))
source(file.path(workingDirectory, "timeNumericFeatures.R"))
source(file.path(workingDirectory, "dispersionTimeFeatures.R"))
source(file.path(workingDirectory, "bidderId2vw.R"))

#Detect available cores
numCores <- detectCores()

#Load Raw Data & data transformation----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)
bids <- fread(file.path(dataDirectory, directories$bids), verbose = TRUE)

bids$country[bids$country == ""] <- "NoCountry"

#Auction Time Medians + Median Average Deviations
auctionsScoresTable <- mclapply(unique(bids$auction), timeNumericFeatures, mc.cores = numCores,
                                bidsDt = bids)

auctionsScoresTable <- do.call(rbind, auctionsScoresTable)

#Merge standard scores with bids data
bids <- merge(bids, auctionsScoresTable, by = "bid_id")
rm(auctionsScoresTable)

#Create combined columns with the most representative coeficient rows
bids$IPUrl <- paste0(bids$ip, bids$url)
bids$URLAuction <- paste0(bids$url, bids$auction)
bids$IPAuction <- paste0(bids$auction, bids$ip)

#Sort by time
bids <- bids[order(rank(-time))]

#save(bids, file = "bidsExtraFeatures.RData")

#Extract training/test time data and create a table with that data
# numericTimeFeaturesTrain <- mclapply(train$bidder_id, dispersionTimeFeatures, mc.cores = numCores,
#                                      bidsDT = bids, trainDt = train)
# numericTimeFeaturesTest <- mclapply(test$bidder_id, dispersionTimeFeatures, mc.cores = numCores,
#                                      bidsDT = bids, trainDt = test)

#Create a sparse matrix with the numeric data
# numericTimeFeaturesTrain <- Matrix(do.call(rbind, numericTimeFeaturesTrain), sparse = TRUE)
# numericTimeFeaturesTest <- Matrix(do.call(rbind, numericTimeFeaturesTest), sparse = TRUE)
# inherits(numericTimeFeaturesTrain,"sparseMatrix")
# inherits(numericTimeFeaturesTest,"sparseMatrix")

#Transform training/test data and bidders data into a boilerplate
boilerplateTrain <- mclapply(train$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                             bidsDT = bids, trainDt = train, ngrams = 1)
boilerplateTest <- mclapply(test$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                            bidsDT = bids, trainDt = test, ngrams = 1)

#Create a data frame with the text data
boilerplateTrain <- do.call(c, boilerplateTrain)
boilerplateTest <- do.call(c, boilerplateTest)

rm(bids)

#Vowpal Wabbit file preparation--------------------------------------
#Vowpal Wabbit Train File
vowpalWabbitTextTrain <- lapply(train$bidder_id, bidderId2vw, bidsDT = bids, trainDt = train)
vowpalWabbitTextTrain <- do.call(rbind, vowpalWabbitTextTrain)
write.table(vowpalWabbitTextTrain, file = file.path(dataDirectory, "vw", "facebookTrain.vw"), 
            eol = "\n", row.names = FALSE, col.names = FALSE, quote = FALSE)

#Vowpal Wabbit Test File
vowpalWabbitTextTest <- lapply(test$bidder_id, bidderId2vw, bidsDT = bids, trainDt = test, train = FALSE)
vowpalWabbitTextTest <- do.call(rbind, vowpalWabbitTextTest)
write.table(vowpalWabbitTextTest, file = file.path(dataDirectory, "vw", "facebookTest.vw"),
            eol = "\n", row.names = FALSE, col.names = FALSE, quote = FALSE)

#Text processing / sparse matrix creation-------------
#Use TM Package to create corpus with TfIdf
corpusSparse <- removeSparseTerms(weightTfIdf(DocumentTermMatrix
                                              (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), normalize = TRUE),
                                  sparse = 0.9995)
modelTerms <- corpusSparse$dimnames$Terms

corpusSparse <- sparseMatrix(i=corpusSparse$i,
                              j=corpusSparse$j,
                              x=corpusSparse$v, dims=c(corpusSparse$nrow, corpusSparse$ncol))

inherits(corpusSparse,"sparseMatrix")

#Split train and test boilerplates
corpusSparseTrain <- corpusSparse[1:nrow(train), ]
corpusSparseTest <- corpusSparse[(nrow(train) + 1):nrow(corpusSparse), ]

#Add time numerical 
#corpusSparseTrain <- cbind(numericTimeFeaturesTrain, corpusSparseTrain)
#corpusSparseTest <- cbind(numericTimeFeaturesTest, corpusSparseTest)

#Remove unnecesary data
rm(boilerplateTrain, boilerplateTest, corpusSparse)
#rm(numericTimeFeaturesTrain, numericTimeFeaturesTest, boilerplateTrain, boilerplateTest, corpusSparse)

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

#Elastic Net alpha values validation
alphaValues2Test <- c(seq(0.10, 0.98, 0.02))

holdoutAucScores <- sapply(alphaValues2Test, function(alphaValue){
  
  #10 fold CV with glmnet
  GLMNETModelCVMini <- cv.glmnet(x = corpusSparseTrain[dataSplits[[1]], ], 
                                 y = as.factor(train$outcome)[dataSplits[[1]]], 
                                 nfolds = 10, parallel = TRUE, family = "binomial",
                                 standardize = FALSE, alpha = alphaValue)
  
  plot(GLMNETModelCVMini)
  
  predictedBotsValidation <- signif(predict(GLMNETModelCVMini, newx =  corpusSparseTrain[dataSplits[[2]], ], 
                                            type = "response", s = "lambda.min"), digits = 6)
  
  holdoutAuc <- auc(train$outcome[dataSplits[[2]]], predictedBotsValidation)
  print(head(predictedBotsValidation))
  print(paste0("AUC score of the holdout data of: ", holdoutAuc, " with an alpha value of: ", alphaValue))
  
  #Clear out memory
  rm(GLMNETModelCVMini, predictedBotsValidation)
  
  return(c(holdoutAuc, alphaValue))
})

#Alpha plot
holdoutAucScores <- as.data.frame(t(holdoutAucScores))
names(holdoutAucScores) <- c("AUC", "Alpha")
ggplot(data = holdoutAucScores, aes(x = Alpha, y = AUC)) + geom_point()

#SVM
#10 fold cross validation parameter selection
set.seed(1001003)
tune.out = tune(svm, x = corpusSparseTrain[dataSplits[[1]], ], 
                y = as.factor(train$outcome)[dataSplits[[1]]], 
                kernel = "radial", scale = FALSE,
                ranges = list(cost = c(0.001 ,0.01 ,0.1 ,1 ,5 ,10 ,100)))

#Modeling
SVMModelCVMini <- svm(x = corpusSparseTrain[dataSplits[[1]], ], 
                      y = as.factor(train$outcome)[dataSplits[[1]]], kernel = "radial",
                      type = "C-classification", C = 10, probability = TRUE, scale = FALSE)

plot(SVMModelCVMini)

predictedBotsValidation <- predict(SVMModelCVMini, newdata =  corpusSparseTrain[dataSplits[[2]], ], probability = TRUE)
predictedBotsValidation <- attr(predictedBotsValidation, "probabilities")[, 2]

holdoutAuc <- auc(train$outcome[dataSplits[[2]]], predictedBotsValidation)
print(paste0("AUC score of the holdout data of: ", holdoutAuc))

##Vowpal Wabbit
#vw hypersearch
# for a logistic loss train-set:
system(paste0(hipersearchScriptLoc, 'vw-hypersearch -L 0.001 10 vw --loss_function logistic --learning_rate % ',
              vwDataDirectory, '/facebookTrain.vw -b 28'))
#Train Data
system(paste0(vw77Dir, 'vw -d ', vwDataDirectory, '/facebookTrain.vw -f ', vwDataDirectory,
              'baselineModelLRate.vw --loss_function logistic -b 28 --learning_rate 0.808174'))
#Test Data
system(paste0(vw77Dir, 'vw -d ', vwDataDirectory, '/facebookTest.vw -t -i ', vwDataDirectory, 
              'baselineModelLRate.vw -p ', vwDataDirectory, 'testBaselineModelLRate.txt'))

#Read output vw .txt - Transform to probabilities
Vw2csv <- function(fileName){
  require("e1071")  
  predictionsvw <- read.table(paste0(vwDataDirectory, fileName))
  print(paste0("file read: ", vwDataDirectory, fileName))  
  predictionsvw <- signif(sigmoid(predictionsvw[, 1]), digits = 4)
  write.csv(predictionsvw, file = paste0(dataDirectory, gsub(".txt", "", fileName), ".csv"), row.names = FALSE)
  print(paste0("file written: ", dataDirectory, gsub(".txt", "", fileName), ".csv"))
  return(TRUE)
}
#Transformatio to probabilities
transformedVw <- sapply(vwPredictions2Transform, Vw2csv)

#Modeling--------------------
#10 fold CV with glmnet
GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain, 
                           y = as.factor(train$outcome), 
                           nfolds = 10, family = "binomial", parallel = TRUE,
                           standardize = FALSE, alpha = 0.91)

plot(GLMNETModelCV)
modelTerms[which(coef(GLMNETModelCV)[, 1] > 0)]

#Make Predictions
predictedBots <- signif(predict(GLMNETModelCV, newx = corpusSparseTest, 
                                type = "response", s = "lambda.min"), digits = 6)

#Make a submission file------------------
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)
sampleSubmissionFile$prediction <- predictedBots

#Write File
write.csv(sampleSubmissionFile, file = "GLMNETRidgeBagOWordsTfIdfX.csv", row.names = FALSE)
system('zip GLMNETRidgeBagOWordsTfIdfX.zip GLMNETRidgeBagOWordsTfIdfX.csv')