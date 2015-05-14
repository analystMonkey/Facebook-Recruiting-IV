featureLengths <- function(bidderId, bidsDT, ngrams = 1){
  
  #This function transforms the bids characteristics into numeric values
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
  
  bidderDf <- as.data.frame(bidsDT[bidder_id == bidderId, .(auction, device, country, ip, url)])
    
  featureNames <- names(bidderDf)
  
  bidderFeatures <- lapply(featureNames, function(colName){
    strings <- as.character(bidderDf[, colName])  
    
    if (colName == "merchandise"){
      strings <- gsub(" ", "", strings)
    }
    
    if (ngrams == 1 | colName == "IPUrl"  | colName == "URLAuction" | colName == "IPAuction" | colName == "SMSRank" | 
          colName == "SSRank" | nrow(bidderDf) == 0 | nrow(bidderDf) == 1){
      
      string2return <- length(unique(strings))
      
    }else{
      
      stringsNgrams <- length(unique(get.ngrams(ngram(paste(strings, collapse = " "), n = ngrams))))
      string2return <- length(unique(strings)) +  stringsNgrams   
    }  
    
    return(string2return)
  })
  
  medianBidsPerAuction <- median(table(bidderDf$auction))
  maxBidsPerAuction <- max(table(bidderDf$auction))
  meanBidsPerAuction <- floor(mean(table(bidderDf$auction)))
  madBidsPerAuction <- round(mad(table(bidderDf$auction)))
  
  bidderSeq <- bidsDT[bidder_id == bidderId, sequential]
  
  #Non Sequential distributions
  if(sum(!bidderSeq, na.rm = TRUE) == 0){
    medianBidsPerAuctionNoSeq <- 0
    maxBidsPerAuctionNoSeq <- 0
    meanBidsPerAuctionNoSeq <- 0
    madBidsPerAuctionNoSeq <- 0
  }else{
    medianBidsPerAuctionNoSeq <- median(table(bidderDf$auction[!bidderSeq]))
    maxBidsPerAuctionNoSeq <- max(table(bidderDf$auction[!bidderSeq]))
    meanBidsPerAuctionNoSeq <- floor(mean(table(bidderDf$auction[!bidderSeq])))
    madBidsPerAuctionNoSeq <- round(mad(table(bidderDf$auction[!bidderSeq])))
    
  }  
  
  sumSequentials <- sum(bidderSeq, na.rm = TRUE)
  sumNonSequentials <- length(bidderSeq) - sumSequentials
    
  return(c(do.call(c, bidderFeatures),
           floor(medianBidsPerAuction), maxBidsPerAuction, meanBidsPerAuction, madBidsPerAuction,
           floor(medianBidsPerAuctionNoSeq), maxBidsPerAuctionNoSeq, meanBidsPerAuctionNoSeq, madBidsPerAuctionNoSeq,
           sumSequentials, sumNonSequentials))
}
