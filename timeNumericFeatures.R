timeNumericFeatures <- function(bidderId, bidsDt, auctionsFeaturesList){
  
  require("bit64")
  
  bidderDt <- bidsDt[bidder_id == bidderId]
  
  if (nrow(bidderDt) > 0){
    madTimes <- sapply(bidderDt$bid_id, function(bidId, auctionsList){
      bidMedianFeatures <- auctionsList[[bidderDt[bid_id == bidId, auction]]]
      if (bidMedianFeatures[2] != 0){
        madScore <- signif((bidderDt[bid_id == bidId, time] - bidMedianFeatures[1]) / (bidMedianFeatures[2] * 1.4826), 4)
      }else{
        madScore <- 0
      }  
      return(madScore)
    }, auctionsList = auctionsFeaturesList)
    
    return(list(c(min(madTimes, na.rm = TRUE), max(madTimes, na.rm = TRUE),
                  mad(madTimes, na.rm = TRUE), quantile(madTimes, seq(0.05, 1, 0.05), na.rm = TRUE)), 
                as.character(round(madTimes))))
  }else{
    return(list(rep(0, 23), "0"))
  }
  
}