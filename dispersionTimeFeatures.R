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
  
  minRest <- min(restPercentages)
  maxRest <- max(restPercentages)
  medianRest <- median(restPercentages)
  madRest <- mad(restPercentages)
  quantileRest <- quantile(restPercentages, c(0.05, 0.1, 0.15, 0.85, 0.9, 0.95))
  
  restFeatures <- c(minRest, maxRest, medianRest, madRest, quantileRest)
  
  #Linear model ordered bids features (from older to most recent)
  glmData <- as.data.frame(restPercentages)
  glmData$indexes <- seq(1, nrow(glmData))
  
  logitModel <- glm(restPercentages ~ indexes, data = glmData)
  
  interceptData <- coef(logitModel)[1]
  slopeData <- -coef(logitModel)[2] / coef(logitModel)[1]
  devianceData <- logitModel$deviance  
  quantResidualsData <- quantile(logitModel$residuals, c(0.01, 0.2, 0.5, 0.8, 0.99))
  aicData <- logitModel$aic
  
  linearModelFeatures <- c(interceptData, slopeData, devianceData, quantResidualsData, aicData)
  
  return(c(numericValues, restFeatures))
  
}