bidderId2Boilerplate <- function(bidderId, bidsDT, ngrams = 1){
  
  #This function transforms the bidders characteristics into boilerplate
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
    
  require("ngram")
  
  bidderDf <- as.data.frame(bidsDT[bidder_id == bidderId, .(auction, merchandise, device, country, 
                                                            ip, url)])
  
  featureNames <- names(bidderDf)
    
  bidderFeatures <- lapply(featureNames, function(colName){
    strings <- as.character(bidderDf[, colName])  
    
    if (colName == "merchandise"){
      strings <- gsub(" ", "", strings)
    }
    
    if (ngrams == 1 | colName == "IPUrl"  | colName == "URLAuction" | colName == "IPAuction" | colName == "SMSRank" | 
          colName == "SSRank" | nrow(bidderDf) == 0 | nrow(bidderDf) == 1){
      
      string2return <- unique(strings)
      
    }else{
      
      stringsNgrams <- unique(get.ngrams(ngram(paste(strings, collapse = " "), n = ngrams)))
      string2return <- c(unique(strings), gsub(" ", "", stringsNgrams))      
    }  
    
    return(string2return)
  })
    
  return(paste(do.call(c, bidderFeatures), collapse=" "))
}
