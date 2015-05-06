timeNumericFeatures <- function(auctionId, bidsDt){
  
  auctionTimes <- bidsDt[auction == auctionId, .(bid_id, time)]  
  setnames(auctionTimes, "time", "standardMadScore")
  medianAuction <- median(auctionTimes[, standardMadScore])
  meanAuction <- mean(auctionTimes[, standardMadScore])
  madAuction <- mad(auctionTimes[, standardMadScore])
  sdAuction <- sd(auctionTimes[, standardMadScore])
  #Standard scores
  auctionTimes$standardScore <- signif((auctionTimes[, standardMadScore] - meanAuction) / sdAuction, 5)    
  auctionTimes$standardMadScore <- signif((auctionTimes[, standardMadScore] - medianAuction) / (madAuction * 1.4826), 5)  
  #Rank scores
  auctionTimes$SSRank <- round(rank(-auctionTimes$standardScore))
  auctionTimes$SMSRank <- round(rank(-auctionTimes$standardMadScore))
  
  return(auctionTimes)
  
}