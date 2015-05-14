timeNumericFeatures <- function(auctionId, bidsDt){
  
  auctionTimes <- bidsDt[auction == auctionId, .(bid_id, time, bidder_id)]  
  setnames(auctionTimes, "time", "standardMadScore")
  
  logTimes <- log(auctionTimes[, standardMadScore])
  medianAuction <- median(logTimes)
  meanAuction <- mean(logTimes)
  madAuction <- mad(logTimes)
  sdAuction <- sd(logTimes)
  #Standard scores
  auctionTimes$standardScore <- signif((logTimes - meanAuction) / sdAuction, 5)    
  auctionTimes$standardMadScore <- signif((logTimes - medianAuction) / (madAuction * 1.4826), 5)  
  #Rank scores
  auctionTimes$SSRank <- round(rank(-auctionTimes$standardScore))
  auctionTimes$SMSRank <- round(rank(-auctionTimes$standardMadScore))
  
  if (nrow(auctionTimes) > 1){
    
    #Secuential up-bidding    
    seqFirst <- auctionTimes$bidder_id[1] == auctionTimes$bidder_id[2]
    seqLast <- auctionTimes$bidder_id[length(auctionTimes$bidder_id)] == auctionTimes$bidder_id[length(auctionTimes$bidder_id) - 1]
    
    sequencePrevious <- auctionTimes$bidder_id[2:(length(auctionTimes$bidder_id) - 1)] == 
      auctionTimes$bidder_id[1:(length(auctionTimes$bidder_id) - 2)]
    sequenceLater <- auctionTimes$bidder_id[2:(length(auctionTimes$bidder_id) - 1)] ==
      auctionTimes$bidder_id[3:length(auctionTimes$bidder_id)]
    
    is.sequential <- sequencePrevious | sequenceLater
    
    is.sequential <- c(seqFirst, is.sequential, seqLast)
    
  }
  if (nrow(auctionTimes) <= 1){
    is.sequential <- FALSE
  }
  
  auctionTimes <- auctionTimes[, .(bid_id, standardMadScore, standardScore, SSRank, SMSRank)]
  auctionTimes$sequential <- is.sequential
  
  return(auctionTimes)
  
}