simultaneousAuctionTimeFinder <- function(bidderIds, bidsDt, range){
  
  bidderInfoDt <- bidsDt[bidder_id == bidderIds, .(auction, device, country, time)]
  bidderInfoDt$time <- as.integer64(substr(as.character(bidderInfoDt$time), 1, 16 - range))
  
  #Bid Frequency in Auctions Counter
  auctionFreq <- sapply(unique(bidderInfoDt$auction), function(auctionId, BidderDt){
    
    auctionDt <- BidderDt[auction == auctionId, .(auction, time)]
    #Remove consecutive bids with same time, keep only one
    quantileFrequencies <- quantile(table.integer64(auctionDt$time), c(0.05, 0.5, 0.95))
    return(quantileFrequencies)
    
  }, BidderDt = bidderInfoDt)
  
  freqMin <- apply(auctionFreq, 1, min)
  freqMax <- apply(auctionFreq, 1, max)
  freqMedian <- apply(auctionFreq, 1, median)
  freqMad <- apply(auctionFreq, 1, mad)
  
  bidsFrequencyStats <- c(freqMin, freqMax, freqMedian, freqMad)
  
  #Simultaneous Auctions counter
  roundedAuctionTimes <- lapply(unique(bidderInfoDt$auction), function(auctionId, BidderDt){
    
    auctionDt <- BidderDt[auction == auctionId, .(auction, time)]
    #Remove consecutive bids with same time, keep only one
    uniqueTimesInAuction <- unique(auctionDt$time)

    return(uniqueTimesInAuction)
    
  }, BidderDt = bidderInfoDt)
  
  auctionsTimesVector <- as.character(do.call(c, roundedAuctionTimes))
  simultaneousTable <- table(auctionsTimesVector)
  simultaneousBids <- sum(simultaneousTable - 1)
  simultaneousBidsNormalized <- simultaneousBids / length(auctionsTimesVector)
  simultaneousBidsPerAuction <- simultaneousBids / length(unique(bidderInfoDt$auction))  
  simultaneousBidsDispersion <- mad(simultaneousTable)
  simultaneousBidsMedian <- median(simultaneousTable - 1) 
  
  simAuctions <- c(simultaneousBids, simultaneousBidsNormalized, simultaneousBidsPerAuction,
                   simultaneousBidsDispersion, simultaneousBidsMedian)
  
  #Simultaneous Devices counter
  roundedDevicesTimes <- lapply(unique(bidderInfoDt$device), function(deviceId, BidderDt){
    
    auctionDt <- BidderDt[device == deviceId, .(device, time)]
    #Remove consecutive bids with same time, keep only one
    uniqueTimesInDevices <- unique(auctionDt$time)
    
    return(uniqueTimesInDevices)
    
  }, BidderDt = bidderInfoDt)
  
  devicesTimesVector <- as.character(do.call(c, roundedDevicesTimes))
  simultaneousTableDevices <- table(devicesTimesVector)
  simultaneousDevices <- sum(simultaneousTableDevices - 1)
  simultaneousDevicesNormalized <- simultaneousDevices / length(devicesTimesVector)
  simultaneousDevicesPerAuction <- simultaneousDevices / length(unique(bidderInfoDt$device))  
  simultaneousDevicesDispersion <- mad(simultaneousTableDevices)
  simultaneousDevicesMedian <- median(simultaneousTableDevices - 1) 
  
  simDevices <- c(simultaneousDevices, simultaneousDevicesNormalized, simultaneousDevicesPerAuction,
                  simultaneousDevicesDispersion, simultaneousDevicesMedian)
  
  #Simultaneous Countries counter
  roundedCountriesTimes <- lapply(unique(bidderInfoDt$country), function(countryId, BidderDt){
    
    auctionDt <- BidderDt[country == countryId, .(country, time)]
    #Remove consecutive bids with same time, keep only one
    uniqueTimesInCountries <- unique(auctionDt$time)
    
    return(uniqueTimesInCountries)
    
  }, BidderDt = bidderInfoDt)
  
  countriesTimesVector <- as.character(do.call(c, roundedCountriesTimes))
  simultaneousTableCountries <- table(countriesTimesVector)
  simultaneousCountries <- sum(simultaneousTableCountries - 1)
  simultaneousCountriesNormalized <- simultaneousCountries / length(simultaneousTableCountries)
  simultaneousCountriesPerAuction <- simultaneousCountries / length(unique(bidderInfoDt$country))  
  simultaneousCountriesDispersion <- mad(simultaneousTableCountries)
  simultaneousCountriesMedian <- median(simultaneousTableCountries - 1) 
  
  simCountries <- c(simultaneousCountries, simultaneousCountriesNormalized, simultaneousCountriesPerAuction,
                    simultaneousCountriesDispersion, simultaneousCountriesMedian)
  
  return(c(bidsFrequencyStats, simAuctions, simDevices, simCountries))
}
