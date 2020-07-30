# what if debris flow information is in the remarks?

debrisFlow <- grep("debris flow", stormData$remarks)
stormData$evtype[debrisFlow]
# debris flows ocurrs in many remarks related to flooding. It's likely that flooding or heavy rain caused debris flows. We shouldn't, however, change that initial classificaion to debris flow. The documen stated that 'landslide' was changed to 'debris flow', so we should only change those.

landslide <- which(stormData$evtype == 'landslide')
stormData$evtype[landslide] <- "debris flow"

flathead <- subset(stormData, stormData$countyname == "FLATHEAD")
unique(flathead$evtype)



length(unique(amatch(stormData$evtype, eventTypes, maxDist = 1)))



small <- stormData[1:5000,]
unique(small[['evtype']])
length(unique(small[['evtype']]))




termCorrection <- function(x, table) {
    pb <- txtProgressBar(min = 1, max = length(table), style = 3)
    uniqueEvents <- length(unique(x[['evtype']]))
    print(paste("Unique events before corrections:", uniqueEvents))
    for(i in 1:length(table)) {
        type <- table[i]
        if(nchar(type) / 2 <= 2) {
            maxDist <- 2
        } else {
            maxDist <- ceiling(nchar(type)/3) - 1 # this sets astronimical high tides to low tides, which doesn't make sense
        }
        matches <- amatch(x[['evtype']], type, maxDist = maxDist)
        matchIdx <- which(!is.na(matches))
        x[['evtype']][matchIdx] <- type
        setTxtProgressBar(pb, i)
    }
    close(pb)
    uniqueEvents <- length(unique(x[['evtype']]))
    print(paste("Unique events after corrections:", uniqueEvents))
    x
}



tstmWindHail <- check(copy, "tstm wind/hail", 4)

length(tstmWindHail)
length(grep("hail", copy$remarks[tstmWindHail]))

# should we make a function that finds all non-matches, then uses a regular expression to see what odd categories could possibly be corrected?
