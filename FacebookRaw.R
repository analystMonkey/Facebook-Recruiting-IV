#Facebook Recruiting IV: Human or Robot?
#Ver. 0.1.4 #Simultaneous data extraction included
#Libraries, directories, options and extra functions----------------------
require("rjson")
require("parallel")
require("data.table")
require("bit64")
require("leaps")
require("tm")
require('doParallel')
require("glmnet")
require("xgboost")
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
source(file.path(workingDirectory, "featureLengths.R"))
source(file.path(workingDirectory, "dispersionTimeFeatures.R"))
source(file.path(workingDirectory, "simultaneousAuctionTimeFinder.R"))
#source(file.path(workingDirectory, "bidsTimeFrequency.R"))
source(file.path(workingDirectory, "multiplot.R"))


#Detect available cores
numCores <- detectCores()

#Load Raw Data & data transformation----------------------
train <- fread(file.path(dataDirectory, directories$trainFile), verbose = TRUE)
test <- fread(file.path(dataDirectory, directories$testFile), verbose = TRUE)
bids <- fread(file.path(dataDirectory, directories$bids), verbose = TRUE)

#Remove training data that cannot be found in the bids data table
train <- train[train$bidder_id %in% bids$bidder_id]
validTestDocuments <- which(test$bidder_id %in% bids$bidder_id)
test <- test[test$bidder_id %in% bids$bidder_id]

#Sort bids by time
bids <- bids[order(rank(-time))]

#No Country change to "NoCountry" string
bids$country[bids$country == ""] <- "NoCountry"

#Auction Time Medians + Median Average Deviations
auctionsScoresTable <- mclapply(unique(bids$auction), timeNumericFeatures, mc.cores = numCores,
                                bidsDt = bids)

auctionsScoresTable <- do.call(rbind, auctionsScoresTable)

#Merge standard scores with bids data
bids <- merge(bids, auctionsScoresTable, by = "bid_id")
#Orphan auctions / bids transform secuential value to FALSE
bids$sequential[is.na(bids$sequential)] <- FALSE
rm(auctionsScoresTable)

#Create combined columns with the most representative coeficient rows
#bids$IPUrl <- paste(bids$ip, bids$url, sep = "_")
#bids$URLAuction <- paste(bids$url, bids$auction, sep = "_")
#bids$IPAuction <- paste(bids$auction, bids$ip, sep = "_")

#save(bids, file = "bidsExtraFeatures.RData")

# #Extract time frequencies from training/test 
# freqTimeFeaturesTrain <- mclapply(train$bidder_id, bidsTimeFrequency, mc.cores = numCores,
#                                  bidsDT = bids)
# freqTimeFeaturesTest <- mclapply(test$bidder_id, bidsTimeFrequency, mc.cores = numCores,
#                                 bidsDT = bids)
# 
# #Create a sparse matrix with the numeric data
# freqTimeFeaturesTrain <- Matrix(do.call(rbind, freqTimeFeaturesTrain), sparse = TRUE)
# freqTimeFeaturesTest <- Matrix(do.call(rbind, freqTimeFeaturesTest), sparse = TRUE)
# colnames(freqTimeFeaturesTrain) <- c("frequencyMedian", "frequencyMad", "frequencyQuantiles", 
#                                      "frequencyMedian30", "frequencyMad30", "frequencyQuantiles30")
# colnames(freqTimeFeaturesTest) <- c("frequencyMedian", "frequencyMad", "frequencyQuantiles", 
#                                     "frequencyMedian30", "frequencyMad30", "frequencyQuantiles30")
# 
# inherits(freqTimeFeaturesTrain,"sparseMatrix")
# inherits(freqTimeFeaturesTest,"sparseMatrix")

#Extract training/test time data and create a table with that data
numericTimeFeaturesTrain <- mclapply(train$bidder_id, dispersionTimeFeatures, mc.cores = numCores,
                                     bidsDT = bids, trainDt = train)
numericTimeFeaturesTest <- mclapply(test$bidder_id, dispersionTimeFeatures, mc.cores = numCores,
                                    bidsDT = bids, trainDt = test)

#Create a sparse matrix with the numeric data
numericTimeFeaturesTrain <- Matrix(do.call(rbind, numericTimeFeaturesTrain), sparse = TRUE)
numericTimeFeaturesTest <- Matrix(do.call(rbind, numericTimeFeaturesTest), sparse = TRUE)
colnames(numericTimeFeaturesTrain) <- c("minimumMad", "minimumSd", "minimumRank", "minimumMadRank",
                                        "maximumMad", "maximumSd", "maximumRank", "maximumMadRank",
                                        "medianMad", "medianSd", "medianRank", "medianMadRank",
                                        "medianAverageDeviationTimeMad", "medianAverageDeviationTimeSd", "madRank", "madMadRank")
colnames(numericTimeFeaturesTest) <- c("minimumMad", "minimumSd", "minimumRank", "minimumMadRank",
                                       "maximumMad", "maximumSd", "maximumRank", "maximumMadRank",
                                       "medianMad", "medianSd", "medianRank", "medianMadRank",
                                       "medianAverageDeviationTimeMad", "medianAverageDeviationTimeSd", "madRank", "madMadRank")

inherits(numericTimeFeaturesTrain,"sparseMatrix")
inherits(numericTimeFeaturesTest,"sparseMatrix")

#Extract training/test unique data
uniqueFeaturesTrain <- mclapply(train$bidder_id, featureLengths, mc.cores = numCores,
                                bidsDT = bids, ngrams = 1)
uniqueFeaturesTest <- mclapply(test$bidder_id, featureLengths, mc.cores = numCores,
                               bidsDT = bids, ngrams = 1)

#Create a sparse matrix with the numeric data
uniqueFeaturesTrain <- Matrix(do.call(rbind, uniqueFeaturesTrain), sparse = TRUE)
uniqueFeaturesTest <- Matrix(do.call(rbind, uniqueFeaturesTest), sparse = TRUE)
colnames(uniqueFeaturesTrain) <- c("auction", "device", "country", "ip", "url", 
                                   "medianBidsPerAuction", "maxBidsPerAuction", "minBidsPerAuction", "madBidsPerAuction",
                                   "medianBidsPerAuctionNoSeq", "maxBidsPerAuctionNoSeq", "minBidsPerAuctionNoSeq", "madBidsPerAuctionNoSeq",
                                   "sumSequentials", "sumNonSequentials")
colnames(uniqueFeaturesTest) <- c("auction", "device", "country", "ip", "url", 
                                  "medianBidsPerAuction", "maxBidsPerAuction", "minBidsPerAuction", "madBidsPerAuction",
                                  "medianBidsPerAuctionNoSeq", "maxBidsPerAuctionNoSeq", "minBidsPerAuctionNoSeq", "madBidsPerAuctionNoSeq",
                                  "sumSequentials", "sumNonSequentials")

inherits(uniqueFeaturesTrain,"sparseMatrix")
inherits(uniqueFeaturesTest,"sparseMatrix")

#Extract simultaneous bids data
simultaneousFeaturesTrain <- mclapply(train$bidder_id, simultaneousAuctionTimeFinder, mc.cores = numCores,
                                      bidsDt = bidsDtSearch, range = 10000)
simultaneousFeaturesTest <- mclapply(test$bidder_id, simultaneousAuctionTimeFinder, mc.cores = numCores,
                                     bidsDt = bidsDtSearch, range = 10000)

#Create a sparse matrix with the numeric data
simultaneousFeaturesTrain <- Matrix(do.call(rbind, simultaneousFeaturesTrain), sparse = TRUE)
simultaneousFeaturesTest <- Matrix(do.call(rbind, simultaneousFeaturesTest), sparse = TRUE)
colnames(simultaneousFeaturesTrain) <- c("simultaneousBids", "simultaneousBidsNormalized", 
                                         "simultaneousBidsPerAuction", "simultaneousBidsDispersion",
                                         "simultaneousBidsMedian")
colnames(simultaneousFeaturesTest) <- c("simultaneousBids", "simultaneousBidsNormalized", 
                                        "simultaneousBidsPerAuction", "simultaneousBidsDispersion",
                                        "simultaneousBidsMedian")

inherits(simultaneousFeaturesTrain,"sparseMatrix")
inherits(simultaneousFeaturesTest,"sparseMatrix")

#Transform training/test data and bidders data into a boilerplate
boilerplateTrain <- mclapply(train$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                             bidsDT = bids, ngrams = 1)
boilerplateTest <- mclapply(test$bidder_id, bidderId2Boilerplate, mc.cores = numCores,
                            bidsDT = bids, ngrams = 1)

#Create a data frame with the text data
boilerplateTrain <- do.call(c, boilerplateTrain)
boilerplateTest <- do.call(c, boilerplateTest)

rm(bids)

#EDA #1 Best Unique and dispersion features--------------------------
numericData <- as.matrix(scale(cbind(uniqueFeaturesTrain, numericTimeFeaturesTrain, simultaneousFeaturesTrain)))

linearBestModels <- regsubsets(x = numericData, y = as.factor(train$outcome), 
                               method = "forward", nvmax = 20)

#Plot the best number of predictors
bestMods <- summary(linearBestModels)
bestNumberOfPredictors <- which.min(bestMods$cp)
plot(bestMods$cp, xlab="Number of Variables", ylab="CP Error", main ="Best Number of Features")
points(bestNumberOfPredictors, bestMods$cp[bestNumberOfPredictors], pch=20, col="red")

#Name of the most predictive rankings
predictors1 <- as.data.frame(bestMods$which)
bestFeatures <- names(sort(apply(predictors1[, -1], 2, sum), decreasing = TRUE)[1:bestNumberOfPredictors])

bestFeaturesUnique <- colnames(uniqueFeaturesTrain)[colnames(uniqueFeaturesTrain) %in% bestFeatures]
bestFeaturesNumeric <- colnames(numericTimeFeaturesTrain)[colnames(numericTimeFeaturesTrain) %in% bestFeatures]

#EDA #2 Bidder auction vs. time plot--------------------------
#define plotting function 
set.seed(1000005)
randomHumans <- train$bidder_id[sample(which(train$outcome == 0), 6)]
set.seed(1000005)
randomRobots <- train$bidder_id[sample(which(train$outcome == 1), 6)]

#Plotting function
plotAuctions <- function(bidder, bidsDt){  
  bidderAuction <- as.data.frame(bidsDt[bidder_id == bidder, .(auction, time)])
  plotAuctions <- ggplot(data = bidderAuction, aes(x = auction, y = log(time), colour = auction)) + geom_point() 
  
  return(plotAuctions)
}

#Humans
humanPlots <- lapply(randomHumans, plotAuctions, bidsDt = bids)
multiplot(humanPlots[[1]], humanPlots[[2]], humanPlots[[3]], humanPlots[[4]], humanPlots[[5]], humanPlots[[6]], cols = 2)

#Robots
robotPlots <- lapply(randomRobots, plotAuctions, bidsDt = bids)
multiplot(robotPlots[[1]], robotPlots[[2]], robotPlots[[3]], robotPlots[[4]], robotPlots[[5]], robotPlots[[6]], cols = 2)

#EDA #3 Simultaneous bids time frame search--------------------------
#long search, it takes approx 2h run only for EDA)
searchRanges <- c(10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)

rangeCorrelations <- sapply(searchRanges, function(range2Search, trainDt, bidsDtSearch){
  
  simultaneousBids <- mclapply(trainDt$bidder_id, simultaneousAuctionTimeFinder, mc.cores = numCores,
                               bidsDt = bidsDtSearch, range = range2Search)
  
  simultaneousBids <- do.call(rbind, simultaneousBids)
  
  correlations <- cor(simultaneousBids, trainDt$outcome)
  print(correlations)
  print(paste0("Correlations with range ", range2Search))
  
  ggplotData <- as.data.frame(cbind(simultaneousBids, trainDt$outcome))
  names(ggplotData) <- c("simultaneousBids", "simultaneousBidsNormalized", 
                         "simultaneousBidsPerAuction", "simultaneousBidsDispersion",
                         "simultaneousBidsMedian", "BotOrNot")
  
  simBidsPlot <- ggplot(data = ggplotData, aes(x = simultaneousBidsNormalized, y = BotOrNot, colour = BotOrNot)) + geom_point() 
  simBidsNormPlot <- ggplot(data = ggplotData, aes(x = simultaneousBidsDispersion, y = BotOrNot, colour = BotOrNot)) + geom_point()  
  simBidsMedianPlot <- ggplot(data = ggplotData, aes(x = simultaneousBidsMedian, y = BotOrNot, colour = BotOrNot)) + geom_point() 
  simBidsDisPlot <- ggplot(data = ggplotData, aes(x = simultaneousBidsNormalized, y = simultaneousBidsDispersion, colour = BotOrNot)) + geom_point() 
  
  multiplot(simBidsPlot, simBidsNormPlot, simBidsMedianPlot, simBidsDisPlot, cols = 2)
  rm(simultaneousBids)
  
  return(correlations)
  
}, trainDt = train, bidsDtSearch = bids)

#Text processing / sparse matrix creation-------------
#Use TM Package to create corpus with TfIdf
# corpusSparse <- removeSparseTerms(weightTfIdf(DocumentTermMatrix
#                                               (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), normalize = TRUE),
#                                   sparse = 0.9995)
#Use TM Package to create corpus with SMART weighting - tf-prob(idf)-n
# corpusSparse <- removeSparseTerms(weightSMART(DocumentTermMatrix
#                                               (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), spec = "npn"),
#                                   sparse = 0.9995)
#Use TM Package to create corpus with SMART weighting - tf-prob(idf)-c
# corpusSparse <- removeSparseTerms(weightSMART(DocumentTermMatrix
#                                               (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), spec = "npc"),
#                                   sparse = 0.9995)
#Use TM Package to create corpus with binary weighting
corpusSparse <- removeSparseTerms(weightBin(DocumentTermMatrix
                                            (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest))))),
                                  sparse = 0.9995)
#Use TM Package to create corpus with SMART weighting - b-n-cos
# corpusSparse <- removeSparseTerms(weightSMART(DocumentTermMatrix
#                                               (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), spec = "bnc"),
#                                   sparse = 0.9995)
#Use TM Package to create corpus with SMART weighting - b-idf-n
# corpusSparse <- removeSparseTerms(weightSMART(DocumentTermMatrix
#                                               (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), spec = "btn"),
#                                   sparse = 0.9995)
#Use TM Package to create corpus with SMART weighting - b-idf-cos
# corpusSparse <- removeSparseTerms(weightSMART(DocumentTermMatrix
#                                               (Corpus(VectorSource(c(boilerplateTrain, boilerplateTest)))), spec = "btn"),
#                                   sparse = 0.9995)

modelTerms <- corpusSparse$dimnames$Terms

corpusSparse <- sparseMatrix(i=corpusSparse$i,
                             j=corpusSparse$j,
                             x=corpusSparse$v, dims=c(corpusSparse$nrow, corpusSparse$ncol))

inherits(corpusSparse,"sparseMatrix")

#Split train and test boilerplates
corpusSparseTrain <- corpusSparse[1:nrow(train), ]
corpusSparseTestValid <- corpusSparse[(nrow(train) + 1):nrow(corpusSparse), ]
corpusSparseTest <- Matrix(data = 0, nrow = 4700, ncol = ncol(corpusSparseTestValid), sparse = TRUE)
corpusSparseTest[validTestDocuments, ] <- corpusSparseTestValid

#Add time numerical 
combinedCorpusSparseTrain <- cbind(numericTimeFeaturesTrain, uniqueFeaturesTrain, 
                                   simultaneousFeaturesTrain, corpusSparseTrain)
combinedCorpusSparseTest <- Matrix(data = 0, nrow = 4700, 
                                   ncol = ncol(corpusSparseTestValid) + ncol(numericTimeFeaturesTrain) + ncol(uniqueFeaturesTrain),
                                   sparse = TRUE)
combinedCorpusSparseTestValid <- cbind(numericTimeFeaturesTest, uniqueFeaturesTest, 
                                       simultaneousFeaturesTrain, corpusSparseTestValid)
combinedCorpusSparseTest[validTestDocuments, ] <- combinedCorpusSparseTestValid

#Remove unnecesary data
rm(numericTimeFeaturesTrain, numericTimeFeaturesTest, boilerplateTrain, boilerplateTest, corpusSparse,
   corpusSparseTestValid, combinedCorpusSparseTestValid)

#EDA #4 GLMNET alpha vs. random seed----------------------------
randomOrderModels <- 10
alphaValues2Test <- seq(0.4, 0.96, 0.04)

alphaVsAucPlots <- lapply(seq(1, randomOrderModels), function(modelNumber){
  
  holdoutAucScores <- sapply(alphaValues2Test, function(alphaValue){
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes  
    randomIndexOrder <- sample(seq(1, nrow(corpusSparseTrain)), nrow(corpusSparseTrain))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain[randomIndexOrder, ], 
                               y = as.factor(train$outcome)[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "binomial",
                               standardize = FALSE, alpha = alphaValue, type.measure = "auc")
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("AUC score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    return(c(cvError, alphaValue))
  })  
  
  #Data for ggplot
  holdoutAucScores <- as.data.frame(t(holdoutAucScores))
  names(holdoutAucScores) <- c("AUCcv", "Alpha")
  
  return(holdoutAucScores)
})

ggplot() + geom_line(data = alphaVsAucPlots[[1]], aes(x = Alpha, y = AUCcv), colour = 'red') +
  geom_line(data = alphaVsAucPlots[[2]], aes(x = Alpha, y = AUCcv), colour = 'blue') +
  geom_line(data = alphaVsAucPlots[[3]], aes(x = Alpha, y = AUCcv), colour = 'magenta') + 
  geom_line(data = alphaVsAucPlots[[4]], aes(x = Alpha, y = AUCcv), colour = 'green') +
  geom_line(data = alphaVsAucPlots[[5]], aes(x = Alpha, y = AUCcv), colour = 'black') + 
  geom_line(data = alphaVsAucPlots[[6]], aes(x = Alpha, y = AUCcv), colour = 'yellow') +
  geom_line(data = alphaVsAucPlots[[7]], aes(x = Alpha, y = AUCcv), colour = 'cyan') +
  geom_line(data = alphaVsAucPlots[[8]], aes(x = Alpha, y = AUCcv), colour = 'red') +
  geom_line(data = alphaVsAucPlots[[9]], aes(x = Alpha, y = AUCcv), colour = 'blue') +
  geom_line(data = alphaVsAucPlots[[10]], aes(x = Alpha, y = AUCcv), colour = 'magenta') 

#Best alpha / elstic-net parameter
bestAlphas <- lapply(alphaVsAucPlots, function(df){
  df[which.max(df[, 1]), 2]
})

mean(do.call(c, bestAlphas))

#Modeling--------------------
#Elastic-Net classification
#Register cores for parallel processing with glmnet
registerDoParallel(numCores)

#Do a simple data holdout of 20%
# trainTrainLength <- floor(nrow(corpusSparseTrain) * 0.7)   #60% of training data used to train models
# trainValidationLength <- floor(nrow(corpusSparseTrain) * 0.3)   #20% of training data to validate models
# idxsdiff <- nrow(corpusSparseTrain) - (trainTrainLength + trainValidationLength)

# set.seed(1001001)
# groupsVector <- sample(c(rep(1, trainTrainLength), rep(2, trainValidationLength + idxsdiff)), 
#                        nrow(corpusSparseTrain))

# #Shuffle Indices
# set.seed(1001002)
# dataSplits <- split(seq(1, nrow(corpusSparseTrain)), as.factor(groupsVector))

#Elastic Net alpha values validation
alphaValues2Test <- seq(0.1, 0.95, 0.05)
numberOfRepeatedModels <- 5

holdoutAucScores <- sapply(alphaValues2Test, function(alphaValue){
  cvErrors <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){    
    
    #Set seed for sampling
    set.seed(1001000 + modelNumber)
    #Shuffle indexes
    randomIndexOrder <- sample(seq(1, nrow(corpusSparseTrain)), nrow(corpusSparseTrain))
    
    #10 fold CV with glmnet
    GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain[randomIndexOrder, ], 
                               y = as.factor(train$outcome)[randomIndexOrder], 
                               nfolds = 5, parallel = TRUE, family = "binomial",
                               standardize = FALSE, alpha = alphaValue, type.measure = "auc")
    
    plot(GLMNETModelCV)
    cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]
    
    print(paste0("AUC score of : ", cvError, " with an alpha value of: ", alphaValue))
    
    #Clear out memory
    rm(GLMNETModelCV)
    
    return(cvError)
  })
  
  return(c(mean(cvErrors), alphaValue))
})

#Alpha plot
holdoutAucScores <- as.data.frame(t(holdoutAucScores))
names(holdoutAucScores) <- c("AUCcv", "Alpha")
ggplot(data = holdoutAucScores, aes(x = Alpha, y = AUCcv)) + geom_line()

#Best Alpha
bestAlpha <- holdoutAucScores[which.max(holdoutAucScores$AUCcv), 2]

#Repeated models
numberOfRepeatedModels <- 10
predictionsGLMNET <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber){
  
  #Set seed for sampling
  set.seed(1001000 + modelNumber)
  #Shuffle indexes  
  randomIndexOrder <- sample(seq(1, nrow(corpusSparseTrain)), nrow(corpusSparseTrain))
  
  #10 fold CV with glmnet
  GLMNETModelCV <- cv.glmnet(x = corpusSparseTrain[randomIndexOrder, ], 
                             y = as.factor(train$outcome)[randomIndexOrder], 
                             nfolds = 5, parallel = TRUE, family = "binomial",
                             standardize = FALSE, alpha = bestAlpha, type.measure = "auc")
  
  plot(GLMNETModelCV)
  cvError <- GLMNETModelCV$cvm[which(GLMNETModelCV$lambda == GLMNETModelCV$lambda.min)]  
  
  predictedBots <- signif(predict(GLMNETModelCV, newx =  corpusSparseTest, 
                                  type = "response", s = "lambda.min"), digits = 6)
  
  print(paste0("AUC score of : ", cvError, " with an alpha value of: ", bestAlpha))
  
  #Clear out memory
  rm(GLMNETModelCV)
  
  return(predictedBots)
})

#Xgboost
#Contruct xgb.DMatrix object
DMMatrixTrain <- xgb.DMatrix(data = combinedCorpusSparseTrain, label = train$outcome)
DMMatrixTest <- xgb.DMatrix(data = combinedCorpusSparseTest)

#Cross validation
numberOfRepeatedModels <- 5
xgboostAUC <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber, train){
  
  xgboostModelCV <- xgb.cv(data = train, nrounds = 60, nfold = 5, showsd = TRUE, 
                           metrics = "auc", verbose = TRUE, 
                           "objective" = "binary:logistic", "max.depth" = 300, 
                           "nthread" = numCores, "max_delta_step" = 1, "set.seed" = modelNumber)
  
  #Plot the progress of the model
  holdoutAucScores <- as.data.frame(xgboostModelCV)
  holdoutAucScores$test.auc.mean <- as.numeric(holdoutAucScores$test.auc.mean)
  holdoutAucScores$iteration <- seq(1, nrow(holdoutAucScores))
  print(ggplot(data = holdoutAucScores, aes(x = iteration, y = test.auc.mean)) + geom_line())
  
  #Save AUC and the location of the best iteration
  auc <- max(as.numeric(holdoutAucScores$test.auc.mean))
  bestIter <- which.max(as.numeric(holdoutAucScores$test.auc.mean))
  
  print(paste0("model number ", modelNumber, " has an AUC of: ", auc))
  return(c(auc, bestIter, as.numeric(holdoutAucScores$test.auc.mean)))
  
}, train = DMMatrixTrain)

print(paste0("average AUC of ",  mean(xgboostAUC[1, ]), " with ", numberOfRepeatedModels, " models."))
bestIteration <- floor(mean(xgboostAUC[2, ]))

#xgboost cross validation plot with n models
holdoutAucScores <- as.data.frame(xgboostAUC[3:nrow(xgboostAUC), ])
holdoutAucScores$iteration <- seq(1, nrow(holdoutAucScores))
ggplot() + geom_line(data = holdoutAucScores, aes(x = iteration, y = V1), colour = 'red') +
  geom_line(data = holdoutAucScores, aes(x = iteration, y = V2), colour = 'blue') + 
  geom_line(data = holdoutAucScores, aes(x = iteration, y = V3), colour = 'magenta') +
  geom_line(data = holdoutAucScores, aes(x = iteration, y = V4), colour = 'green') +
  geom_line(data = holdoutAucScores, aes(x = iteration, y = V5), colour = 'black')

#Full xgboost model
numberOfRepeatedModels <- 5
xgboostMultiPredictions <- sapply(seq(1, numberOfRepeatedModels), function(modelNumber, iter, train, test){
  
  xgboostModel <- xgboost(data = train, nrounds = iter + 10,
                          showsd = TRUE, metrics = "auc", verbose = TRUE, 
                          "objective" = "binary:logistic", "max.depth" = 300, "nthread" = numCores,
                          "max_delta_step" = 1, "set.seed" = modelNumber)
  
  model <- xgb.dump(xgboostModel, with.stats = T)
  model[1:10]
  
  #Plot feature importance  
  # Compute feature importance matrix
  #importance_matrix <- xgb.importance(modelTerms, model = xgboostModel)  
  #Importance graph
  #xgb.plot.importance(importance_matrix[1:10,])
  
  #Predict
  xgboostPrediction <- predict(xgboostModel, test)
  print(paste0("model number ", modelNumber, " processed"))
  rm(xgboostModel)
  return(xgboostPrediction)
  
}, iter = bestIteration, train = DMMatrixTrain, test = DMMatrixTest)

xgboostPrediction <- apply(xgboostMultiPredictions, 1, mean)

#Make a submission file------------------
#Read the sample file
sampleSubmissionFile <- fread(file.path(dataDirectory, "sampleSubmission.csv"), verbose = TRUE)

#GLMNET
sampleSubmissionFile$prediction <- apply(predictionsGLMNET, 1, mean)

#Write File
write.csv(sampleSubmissionFile, file = "GLMNETElasticNetBagOWordsTfIdfXIV.csv", row.names = FALSE)
system('zip GLMNETElasticNetBagOWordsTfIdfXIV.zip GLMNETElasticNetBagOWordsTfIdfXIV.csv')

#xgboost
sampleSubmissionFile$prediction <- xgboostPrediction

#Write File
write.csv(sampleSubmissionFile, file = "xgboostTfIdfIV.csv", row.names = FALSE)
system('zip xgboostTfIdfIV.zip xgboostTfIdfIV.csv')

#Combined submission
sampleSubmissionFile$prediction <- apply(cbind(apply(predictionsGLMNET, 1, mean), xgboostPrediction), 1, mean)

#Write File
write.csv(sampleSubmissionFile, file = "CombinedTfIdfI.csv", row.names = FALSE)
system('zip CombinedTfIdfI.zip CombinedTfIdfI.csv')