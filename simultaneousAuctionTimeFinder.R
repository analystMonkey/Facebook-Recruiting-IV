simultaneousAuctionTimeFinder <- function(bidderIds, bidsDt, range){
  
  bidderAuction <- bidsDt[bidder_id == bidderIds, .(auction, time)]
  
  roundedAuctionTimes <- lapply(unique(bidderAuction$auction), function(auctionId, auctionBidderDt){
    
    auctionBidderDt <- bidderAuction[auction == auctionId, .(auction, time)]
    #Remove consecutive bids with same time, keep only one
    uniqueTimesInAuction <- unique(auctionBidderDt$time)
    #Reduce to values within a certain range 
    uniqueTimesInAuction <- round(uniqueTimesInAuction / range)
    return(uniqueTimesInAuction)
    
  }, auctionBidderDt = bidderAuction)
  
  auctionsTimesVector <- do.call(c, roundedAuctionTimes)
  simultaneousTable <- table(auctionsTimesVector)
  simultaneousBids <- sum(simultaneousTable - 1)
  simultaneousBidsNormalized <- simultaneousBids / length(auctionsTimesVector)
  simultaneousBidsPerAuction <- simultaneousBids / length(unique(bidderAuction$auction))  
  simultaneousBidsDispersion <- mad(simultaneousTable)
  simultaneousBidsMedian <- median(simultaneousTable)  
  
  return(c(simultaneousBids, simultaneousBidsNormalized, 
           simultaneousBidsPerAuction, simultaneousBidsDispersion, simultaneousBidsMedian))
}