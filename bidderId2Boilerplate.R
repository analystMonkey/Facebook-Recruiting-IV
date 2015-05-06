bidderId2Boilerplate <- function(bidderId, bidsDT, trainDt, ngrams = 1){
  
  #This function transforms the bidders characteristics into boilerplate
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
    
  require("ngram")
  
  bidderDf <- as.data.frame(bidsDT[bidder_id == bidderId])
  
  featureNames <- names(bidderDf)[c(-1, -6, -10, -11, -13)]
    
  bidderFeatures <- lapply(featureNames, function(colName){
    strings <- as.character(bidderDf[, colName])  
    
    if (colName == "merchandise"){
      strings <- gsub(" ", "", strings)
    }
    
    if (ngrams == 1 | colName == "IPUrl" | colName == "SMSRank" | 
          colName == "SSRank" | nrow(bidderDf) == 0 | nrow(bidderDf) == 1){
      
      string2return <- unique(strings)
    }else{
      stringsNgrams <- unique(get.ngrams(ngram(paste(strings, collapse = " "), n = ngrams)))
      string2return <- c(unique(strings), gsub(" ", "", stringsNgrams))      
    }  
    
    return(string2return)
  })
  #Append payment_account adderss 
  additionalFeatures <- as.character(trainDt[which(trainDt[, "bidder_id", with = FALSE] == bidderId)])[c(2, 3)]
  #transform to character
  bidderFeatures <- c(bidderFeatures, additionalFeatures)
  
  return(paste(do.call(c, bidderFeatures), collapse=" "))
}
