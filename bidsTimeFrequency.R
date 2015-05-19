bidsTimeFrequency <- function(bidderId, bidsDT){
  
  #This function extracts the time sequentiality from bidders in each auction
  
  bidderAuctions <- as.data.frame(bidsDT[bidder_id == bidderId, .(auction)])[, 1]
  
  auctionStats <- sapply(unique(bidderAuctions), function(singleAuction, bidderUniqueId){
    
    #Time vector full
    auctionTimes <- bidsDT[auction == singleAuction, .(time, bidder_id = bidderUniqueId)] 
    
    #Time Vector final 30%
    auctionTimes30 <- tail(auctionTimes,  floor(length(auctionTimes$time) * 0.4))
    #time interval for a predetermined number (5) of occurrences
    
    #Full Data
    interval <- seq(1, length(auctionTimes$time), 5)    
    intervalsFrequencies <- sapply(interval, function(intervalInput, timeVector){
      
      if(length(timeVector) - intervalInput == 0){
        return(NA)
      }
      if(intervalInput + 4 > length(auctionTimes$time)){
        return(log((timeVector[length(auctionTimes$time)] - timeVector[intervalInput]) * 2))
      }
      else{
        return(log(timeVector[intervalInput + 4] - timeVector[intervalInput]))
      }      
      
    }, timeVector = auctionTimes$time)
    
    frequencyMedian <- median(intervalsFrequencies, na.rm = TRUE)
    frequencyMad <- mad(intervalsFrequencies, na.rm = TRUE)
    frequencyQuantiles <- quantile(intervalsFrequencies, c(0.01, 0.15, 0.85, 0.99), na.rm = TRUE)
    
    #30%
    interval30 <- seq(1, floor(length(auctionTimes$time) * 0.4), 5)
    
    intervalsFrequencies30 <- sapply(interval30, function(intervalInput, timeVector){
      
      if(length(timeVector) - intervalInput == 0){
        return(NA)
      }
      if(intervalInput + 4 > length(auctionTimes$time)){
        return(log((timeVector[length(auctionTimes$time)] - timeVector[intervalInput]) * 2))
      }
      else{
        return(log(timeVector[intervalInput + 4] - timeVector[intervalInput]))
      }      
      
    }, timeVector = auctionTimes30$time)
    
    frequencyMedian30 <- median(intervalsFrequencies30, na.rm = TRUE)
    frequencyMad30 <- mad(intervalsFrequencies30, na.rm = TRUE)
    frequencyQuantiles30 <- quantile(intervalsFrequencies30, c(0.01, 0.15, 0.85, 0.99), na.rm = TRUE)
        
    #Debugging Print
    print(singleAuction)    
    return(c(frequencyMedian, frequencyMad, frequencyQuantiles, frequencyMedian30, frequencyMad30, frequencyQuantiles30))
        
  }, bidderUniqueId = bidderId)  
  
  auctionStats[auctionStats == -Inf] <- NA
  
  bidderSeqStats <- apply(auctionStats, 1, median, na.rm = TRUE)
  bidderSeqDispertionStats <- apply(auctionStats, 1, mad, na.rm = TRUE)
  
  print(bidderId)
  
  return(c(bidderSeqStats, bidderSeqDispertionStats))
}