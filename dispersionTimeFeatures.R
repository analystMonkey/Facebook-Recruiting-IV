dispersionTimeFeatures <- function(bidderId, bidsDT, trainDt){
  
  #This function extracts the information about the time spread across different auctions
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
  
  bidderDf <- bidsDT[bidder_id == bidderId, .(standardMadScore, standardScore)]
  
  if (nrow(bidderDf) != 0){
    #Extract minimum    
    minimum <- apply(bidderDf, 2, min, na.rm = TRUE)
    #Extract maximum    
    maximum <- apply(bidderDf, 2, max, na.rm = TRUE)
    #Extract median
    median <- apply(bidderDf, 2, median, na.rm = TRUE)
    #Extract Median Average Deviation
    medianAverageDeviationTime <- apply(bidderDf, 2, mad, na.rm = TRUE)
    #Extract quantiles
    #medianAbsoluteDeviation <- apply(bidderDf[, "standardMadScore", with = FALSE], 2, quantile, 
    #      c(0.01, 0.02, 0.03, 0.05, 0.08, 0.13, 0.21, 0.34, 0.55, 0.89) ,na.rm = TRUE)
    
    #numericValues <- c(minimum, maximum, median, medianAverageDeviationTime, medianAbsoluteDeviation)
    numericValues <- c(minimum, maximum, median, medianAverageDeviationTime)
    
  }else{
    #numericValues <- rep(0, 18)
    numericValues <- rep(0, 8)
  }
  
  return(numericValues) 
  
}