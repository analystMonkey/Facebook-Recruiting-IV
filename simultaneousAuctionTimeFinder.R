simultaneousAuctionTimeFinder <- function(bidderIds, bidsDt, range){
  
  bidderAuction <- bidsDt[bidder_id == bidderIds, .(auction, time)]
  
#   #Round times inside a range
#   bidderAuction$time <- as.integer64(substr(as.character(bidderAuction$time), 1, 16 - range))
#   timesTable <- table(bidderAuction$time)
#   uniqueSimultaneousTime <- bidderAuction$time[timesTable > 1]
#   
#   #Define a simultaneous event search
#   simultaneousChecker <- function(uniqueTime, Variable2Check){    
#     simultaneousEvent <- bidderAuction[bidderAuction$time == uniqueTime, Variable2Check, with = FALSE]
#     return(length(simultaneousEvent) - 1)    
#   }
#   
#   #Simultaneous Auctions
#   if (sum(table(bidderAuction$time) > 1) == 0){    
#     simultaneousAuctions <- 0    
#   }else{
#     simultaneousAuctionsVector <- sapply(uniqueSimultaneousTime, simultaneousChecker, Variable2Check = "auction")    
#     simultaneousAuctions <- sum(simultaneousAuctionsVector)
#   }  
  
  #Simultaneous Auctions counter
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
  simultaneousBidsMedian <- median(simultaneousTable - 1)  
  
  return(c(simultaneousBids, simultaneousBidsNormalized, 
           simultaneousBidsPerAuction, simultaneousBidsDispersion, simultaneousBidsMedian))
}