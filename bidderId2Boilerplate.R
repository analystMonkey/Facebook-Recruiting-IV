bidderId2Boilerplate <- function(bidderId, bidsDT, trainDt){
  
  #This function transforms the bidders characteristics into boilerplate
  #bidderId is the unique bidder's identifier and bidsDT is the bidders data frame
  
  bidderDf <- as.data.frame(bidsDT[bidder_id == bidderId])
  bidderFeatures <- lapply(names(bidderDf)[c(-1, -6)], function(colName, DataFrame){
    string2return <- unique(as.character(bidderDf[, colName]))
    #string2return <- as.character(bidderDf[, colName])
    if (colName == "merchandise"){
      string2return <- gsub(" ", "", string2return)
    }
    return(string2return)
  })
  additionalFeatures <- as.character(trainDt[which(trainDt[, "bidder_id", with = FALSE] == bidderId)])[c(2, 3)]
  
  bidderFeatures <- c(bidderFeatures, additionalFeatures)
  
  return(paste(do.call(c, bidderFeatures), collapse=" "))
}
