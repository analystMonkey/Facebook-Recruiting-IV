dispersionTimeFeatures <- function(bidderId, bidsDT, rangesList){
  
  #This function extracts the information about the time spread across different auctions
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
  
  #Statistical Features
  bidderDf <- bidsDT[bidder_id == bidderId, .(standardMadScore, standardScore, SSRank, SMSRank)]
  
  if (nrow(bidderDf) != 0){
    #Extract minimum    
    minimum <- apply(bidderDf, 2, min, na.rm = TRUE)
    #Extract maximum    
    maximum <- apply(bidderDf, 2, max, na.rm = TRUE)
    #Extract median
    median <- apply(bidderDf, 2, median, na.rm = TRUE)
    #Extract Median Average Deviation
    medianAverageDeviationTime <- apply(bidderDf, 2, mad, na.rm = TRUE)
    
    numericValues <- c(minimum, maximum, median, medianAverageDeviationTime)
    
  }else{
    numericValues <- rep(c(0, 0, 1000, 1000), 4)
  }  
  
  bidsTimeAuctions <- bidsDT[bidder_id == bidderId, .(time, auction)]
  
  #Time Range Features
  rangesAuctions <- sapply(unique(bidsTimeAuctions$auction), function(auctionBidder, rangeAuctions){
    if(rangeAuctions[[auctionBidder]][3] != 0){
      timesInAuction <- bidsTimeAuctions[auction == auctionBidder, time]
      rangeAuctionInt64 <- max.integer64(timesInAuction) - min.integer64(timesInAuction)
      rangeActionInAuction <- ifelse(rangeAuctionInt64 == 0, 0, log(rangeAuctionInt64))
      rangeActionNormalized <- rangeAuctionInt64 / rangeAuctions[[auctionBidder]][3]
    }else{
      rangeActionInAuction <- 0 
      rangeActionNormalized <- 0
    }
    return(c(rangeActionInAuction, rangeActionNormalized))
  }, rangeAuctions = rangesList)
  
  minRange <- apply(rangesAuctions, 1, min)
  maxRange <- apply(rangesAuctions, 1, max)
  medianRange <- apply(rangesAuctions, 1, median)
  madRange <- apply(rangesAuctions, 1, mad)
  
  rangeFeatures <- c(minRange, maxRange, medianRange, madRange)

  #Range first derivative features
  if (ncol(rangesAuctions) < 2){

    rangeDerivativeFeatures <- rep(0, 8)

  }else{

    range1Derivative <- abs(diff(rangesAuctions[2, ]))
    min1DerRange <- min(range1Derivative)
    max1DerRange <- max(range1Derivative)
    mad1DerRange <- mad(range1Derivative)
    quantile1DerRange <- quantile(range1Derivative, c(0.1, 0.25, 0.5, 0.75, 0.9))

    rangeDerivativeFeatures <- c(min1DerRange, max1DerRange, mad1DerRange, quantile1DerRange)
  }
  
  #Rest Percentages
  restPercentages <- sapply(unique(bidsTimeAuctions$auction), function(auctionBidder, rangeAuctions){
    if(rangeAuctions[[auctionBidder]][3] != 0){
      timesInAction <- bidsTimeAuctions[auction == auctionBidder, time]
      restingBidderTime <- (abs(min.integer64(timesInAction) - rangeAuctions[[auctionBidder]][1])) / rangeAuctions[[auctionBidder]][3]
    }else{
      restingBidderTime <- 0.5
    }  
    return(restingBidderTime)
  }, rangeAuctions = rangesList)
  
  #Final Percentages
  finalPercentages <- sapply(unique(bidsTimeAuctions$auction), function(auctionBidder, rangeAuctions){
    if(rangeAuctions[[auctionBidder]][3] != 0){
      timesInAction <- bidsTimeAuctions[auction == auctionBidder, time]
      restingBidderTime <- (abs(max.integer64(timesInAction) - rangeAuctions[[auctionBidder]][2])) / rangeAuctions[[auctionBidder]][3]
    }else{
      restingBidderTime <- 0.5
    }  
    return(restingBidderTime)
  }, rangeAuctions = rangesList)
  
  #Rest Statistical Features
  minRest <- min(restPercentages)
  maxRest <- max(restPercentages)
  medianRest <- median(restPercentages)
  madRest <- mad(restPercentages)
  quantileRest <- quantile(restPercentages, c(0.05, 0.1, 0.15, 0.85, 0.9, 0.95))
  
  restFeatures <- c(minRest, maxRest, medianRest, madRest, quantileRest)

  #Rest first derivative features
  if (length(restPercentages) < 2){

    restDerivativeFeatures <- rep(0, 8)

  }else{

    rest1Derivative <- abs(diff(restPercentages))
    min1DerRest <- min(rest1Derivative)
    max1DerRest <- max(rest1Derivative)
    mad1DerRest <- mad(rest1Derivative)
    quantile1DerRest <- quantile(rest1Derivative, c(0.1, 0.25, 0.5, 0.75, 0.9))

    restDerivativeFeatures <- c(min1DerRest, max1DerRest, mad1DerRest, quantile1DerRest)
  }
  
  #Final Moments Statistical Features
  minFinal <- min(finalPercentages)
  maxFinal <- max(finalPercentages)
  medianFinal <- median(finalPercentages)
  madFinal <- mad(finalPercentages)
  quantileFinal <- quantile(finalPercentages, c(0.05, 0.1, 0.15, 0.85, 0.9, 0.95))
  
  finalFeatures <- c(minFinal, maxFinal, medianFinal, madFinal, quantileFinal)

  #Final Moments first derivative features
  if (length(finalPercentages) < 2){

    finalDerivativeFeatures <- rep(0, 8)

  }else{

    final1Derivative <- abs(diff(finalPercentages))
    min1DerFinal <- min(final1Derivative)
    max1DerFinal <- max(final1Derivative)
    mad1DerFinal <- mad(final1Derivative)
    quantile1DerFinal <- quantile(final1Derivative, c(0.1, 0.25, 0.5, 0.75, 0.9))

    finalDerivativeFeatures <- c(min1DerFinal, max1DerFinal, mad1DerFinal, quantile1DerFinal)
  }
  
  #Linear model ordered bids features (from older to most recent)
  glmData <- as.data.frame(restPercentages)
  glmData$indexes <- seq(1, nrow(glmData))
  
  logitModel <- glm(restPercentages ~ indexes, data = glmData)
  
  interceptData <- coef(logitModel)[1]
  slopeData <- ifelse(nrow(glmData) > 1, -coef(logitModel)[2] / (coef(logitModel)[1] + 1e-11), 0)
  devianceData <- logitModel$deviance  
  quantResidualsData <- quantile(logitModel$residuals, c(0.01, 0.2, 0.5, 0.8, 0.99))
  
  linearModelFeatures <- c(interceptData, slopeData, devianceData, quantResidualsData)
  
  #Linear model ordered final data
  glmDataFinal <- as.data.frame(finalPercentages)
  glmDataFinal$indexes <- seq(1, nrow(glmDataFinal))
  
  logitModelFin <- glm(finalPercentages ~ indexes, data = glmDataFinal)
  
  interceptDataFinal <- coef(logitModelFin)[1]
  slopeDataFinal <- ifelse(nrow(glmDataFinal) > 1, -coef(logitModelFin)[2] / (coef(logitModelFin)[1] + 1e-11), 0)
  devianceDataFinal <- logitModelFin$deviance  
  quantResidualsDataFinal <- quantile(logitModelFin$residuals, c(0.01, 0.2, 0.5, 0.8, 0.99))
  
  linearModelFeaturesFin <- c(interceptDataFinal, slopeDataFinal, devianceDataFinal, quantResidualsDataFinal)
  
  return(c(numericValues, rangeFeatures, rangeDerivativeFeatures,
           restFeatures, restDerivativeFeatures, finalFeatures, finalDerivativeFeatures,
           linearModelFeatures, linearModelFeaturesFin))
  
}
