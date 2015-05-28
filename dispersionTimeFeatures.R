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
    numericValues <- rep(c(0, 0, 500, 500), 4)
  }
  
  #Resting Time Features
  bidsTimeAuctions <- bidsDT[bidder_id == bidderId, .(time, auction)]
  
  restPercentages <- sapply(unique(bidsTimeAuctions$auction), function(auctionBidder, rangeAuctions){
    if(rangeAuctions[[auctionBidder]][3] != 0){
      timesInAction <- bidsTimeAuctions[auction == auctionBidder, time]
      restingBidderTime <- (abs(min.integer64(timesInAction) - rangeAuctions[[auctionBidder]][1])) / rangeAuctions[[auctionBidder]][3]
    }else{
      restingBidderTime <- 0.5
    }  
    return(restingBidderTime)
  }, rangeAuctions = rangesList)
  
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
  
  #Final Moments Statistical Features
  minFinal <- min(finalPercentages)
  maxFinal <- max(finalPercentages)
  medianFinal <- median(finalPercentages)
  madFinal <- mad(finalPercentages)
  quantileFinal <- quantile(finalPercentages, c(0.05, 0.1, 0.15, 0.85, 0.9, 0.95))
  
  finalFeatures <- c(minFinal, maxFinal, medianFinal, madFinal, quantileFinal)
  
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
  
  return(c(numericValues, restFeatures, finalFeatures, linearModelFeatures, linearModelFeaturesFin))
  
}