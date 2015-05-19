dispersionTimeFeatures <- function(bidderId, bidsDT, trainDt){
  
  #This function extracts the information about the time spread across different auctions
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
  
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
    numericValues <- rep(c(0, 0, 100, 100), 4)
  }
  
  return(numericValues) 
  
}