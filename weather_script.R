library(tidyverse)
library(data.table)
library(lubridate)
library(stringdist)

stormData <- fread("repdata_data_StormData.csv.bz2", header = TRUE,
                  na.strings = c("", " ", NA))

names(stormData)

# there is a huge number of rows and columns. We can get rid of several columns
# such as STATEOFFIC (state office), REFNUM, TIMEZONE, 

round(colSums(is.na(stormData))/nrow(stormData)*100,1)

stormData <- stormData %>% 
    select(-c(TIME_ZONE, STATEOFFIC, REFNUM, BGN_AZI, END_AZI, ZONENAMES,
              COUNTYENDN, LATITUDE_E, LONGITUDE_)) %>% 
    mutate(BGN_DATE = as.Date(mdy_hms(stormData$BGN_DATE), format = "%m/%d/%Y"))

colnames(stormData) <- tolower(colnames(stormData))
dim(stormData)
round(colSums(is.na(stormData))/nrow(stormData)*100,1)


unique(stormData$evtype) %>% 
    length()

# according to the manual, there are only 48 event types. What if some were put as lower case and other as upper case?
stormData$evtype %>% 
    unique() # 985

toupper(stormData$evtype) %>% 
    unique() %>% 
    length # 898. Clearly some were coded as upper and some as lower case

stormData$evtype <- tolower(stormData$evtype)

# we need to eliminate several years of data because NOAA didn't record all of the event types listed until 1996. We will delete all data prior to 1995. We could include data prior to 1996, but that would overrepresent some forms of the data.

stormData <- subset(stormData, stormData$bgn_date >= as.Date("1996-01-01"))

stormData$evtype %>% 
    unique() %>% 
    length()

# according to the storm data preparation report, 'landslide' has been renamed to 'debris flow'. We should first make spelling corrections to landslide, THEN change all of those to Debris Flow.

eventTypes <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Landslide", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme Cold/Wind Chill", "Flash Flood", "Flood", "Frost/Freeze", "Funnel Cloud", "Freezing Fog", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane (Typhoon)", "Ice Storm", "Lake-Effect Snow", "Lakeshore Flood", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Surge/Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")

eventTypes <- tolower(eventTypes)
eventTypes

# do we have matches for all of the 48 even types?

allEventsPresent <- function(x, table) {
    for(i in 1:length(table)) {
        if(i == 1) {
            hits <- numeric()
        }
        if(any(table[i] == x[['evtype']])) {
            hits <- c(hits, table[i])
        } else {
            hits <- c(hits, FALSE)
        }
    }
    noEvents <- which(hits == FALSE) # there are some events without a match
    if(length(table[noEvents] > 0)) {
        table[noEvents]
    } else {
        print("No event types missing from list of 48 official events")
    }
}

allEventsPresent(stormData, eventTypes)

# it looks like nothing was entered in the format of "hurricane (typhoon)". However, many were entered as hust "hurricane" or just "typhoon". We may need to make special corrections for typhoon.

which(stormData$evtype == "hurricane")
which(stormData$evtype == "typhoon")


typhoon <- amatch(stormData$evtype, "typhoon", maxDist = 3)
typhoonIdx <- which(!is.na(typhoon))
stormData$evtype[typhoonIdx] # it looks like there weren't any spelling mistakes about typhoon, so let's change all those to hurricane 
unique(stormData$evtype[typhoonIdx])
stormData$evtype[typhoonIdx] <- "hurricane (typhoon)"
stormData$evtype[typhoonIdx]

# I could also see there being an issue with "hurricane (typhoon)" because the name is so long. 

hurricaneTyphoon <- amatch(stormData$evtype, "hurricane (typhoon", maxDist = 3)
hurricaneTyphoonIdx <- which(!is.na(hurricaneTyphoon))
stormData$evtype[hurricaneTyphoonIdx] # it looks like there weren't any spelling mistakes about typhoon, so let's change all those to hurricane 
unique(stormData$evtype[hurricaneTyphoonIdx])
stormData$evtype[hurricaneTyphoonIdx] <- "hurricane (typhoon)"
stormData$evtype[hurricaneTyphoonIdx]
unique(stormData$evtype[hurricaneTyphoonIdx])

a <- amatch(stormData$evtype, "hurricane (typhoon)", maxDist = 5)
aIdx <- which(!is.na(a))
stormData$evtype[aIdx]
unique(stormData$evtype[aIdx])

# think we should temporarily change "hurricane (typhoon)" to just hurricane because there could be misspellings of "hurricane" that wouldn't get fixed with amatch().

which(eventTypes == "hurricane (typhoon)")
eventTypes[25] <- "hurricane"

allEventsPresent(stormData, eventTypes)

# I think we need to make corrections in iterations


remainingCorrections <- function(x, table) {
    matches <- amatch(x, table, maxDist = 0.1)
    imperfectMatch <- which(is.na(matches))
    perfectMatch <- which(!is.na(matches))
    print(paste("Imperfect matches:", length(imperfectMatch)))
    print(paste("Perfect matches:", length(perfectMatch)))
    print(paste("Percent remaining that need correction:",
                round(length(imperfectMatch)/length(perfectMatch)*100,1)))
}

remainingCorrections(stormData$evtype, eventTypes)

termCorrection <- function(x, table) {
    pb <- txtProgressBar(min = 1, max = length(table), style = 3)
    uniqueEvents <- length(unique(x[['evtype']]))
    print(paste("Unique events before corrections:", uniqueEvents))
    for(i in 1:length(table)) {
        type <- table[i]
        maxDist <- 1
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

copy <- termCorrection(stormData, eventTypes)

allEventsPresent(copy, eventTypes) # we didn't eliminate any events with amatch by mistake
allEventsPresent(stormData, eventTypes)

unique(stormData$evtype[!(stormData$evtype %in% copy$evtype)]) # this shows us which events were 'corrected' with termCorrection. It looks like it was mostly data entry errors of plural word forms and extra spaces.

check <- function(x, table, maxDist) {
    matches <- amatch(x[['evtype']], table, maxDist = maxDist)
    matchesIdx <- which(!is.na(matches))
    uniqueMatches <- unique(x[['evtype']][matchesIdx])
    print("Unique matches")
    print(uniqueMatches)
    matchesIdx
}

hail <- check(stormData, "hail", 1)

remainingCorrections(copy$evtype, eventTypes)

summary(copy$evtype)

correctOneEvent <- function(x, currentValue, correction) {
    matches <- amatch(x[['evtype']], currentValue, maxDist = 0.1)
    matchesIdx <- which(!is.na(matches))
    print(paste("Number of terms to change:", length(matchesIdx)))
    x[['evtype']][matchesIdx] <- correction
    print(paste("Do any of the incorrect values exist?",
                any(x[['evtype']][matchesIdx] == currentValue)))
    x
}

needsCorrectionSummary <- function(x, table) {
    matches <- amatch(x[['evtype']], table, maxDist = 0.1)
    needsCorrection <- which(is.na(matches))
    summary(x[['evtype']][needsCorrection])
}

copy <- correctOneEvent(copy, "tstm wind", "thunderstorm wind")
copy <- correctOneEvent(copy, "marine tstm wind", "marine thunderstorm wind")
copy <- correctOneEvent(copy, "wild/forest fire", "wildfire")
copy <- correctOneEvent(copy, "extreme cold", "extreme cold/wind chill")
copy <- correctOneEvent(copy, "extreme windchill", "extreme cold/wind chill")
copy <- correctOneEvent(copy, "unseasonably cold", "extreme cold/wind chill")
copy <- correctOneEvent(copy, "cold", "extreme cold/wind chill")
copy <- correctOneEvent(copy, "snow", "heavy snow")
copy <- correctOneEvent(copy, "winter weather/mix", "winter weather")
copy <- correctOneEvent(copy, "wintry mix", "winter weather")
copy <- correctOneEvent(copy, "heavy surf/high surf", "high surf")
copy <- correctOneEvent(copy, "heavy surf", "high surf")
copy <- correctOneEvent(copy, "fog", "dense fog")
copy <- correctOneEvent(copy, "coastal flooding", "coastal flood")
copy <- correctOneEvent(copy, "wind", "high wind")
copy <- correctOneEvent(copy, "storm surge", "storm surge/tide")
copy <- correctOneEvent(copy, "hurricane", "hurricane (typhoon)")
copy <- correctOneEvent(copy, "record warmth", "heat")
copy <- correctOneEvent(copy, "record heat", "heat")
copy <- correctOneEvent(copy, "unseasonably warm", "heat")
copy <- correctOneEvent(copy, "frost", "frost/freeze")
copy <- correctOneEvent(copy, "freeze", "frost/freeze")

needsCorrectionSummary(copy, eventTypes)
remainingCorrections(copy$evtype, eventTypes) # I'm going to say that 1.1 percent of matches remaining is good enough.

landslide <- which(copy$evtype == "landslide")
copy$evtype[landslide] <- "debris flow"

which(stormData$evtype == "marine thunderstorm wind")
which(eventTypes == "marine thunderstorm wind")

# we need to change landslide to debris flow and hurricane to hurricane (typhoon)
eventTypes[which(eventTypes == "landslide")] <- "debris flow"
eventTypes[which(eventTypes == "hurricane")] <- "hurricane (tyhoon)"

stormData <- copy

officialMatches <- check(stormData, eventTypes, 1)
stormData <- stormData[officialMatches,]

stormData$evtype <- as.factor(stormData$evtype)
stormData$propdmgexp <- as.factor(stormData$propdmgexp)
summary(stormData$propdmgexp)
stormData$propdmgexp[which(stormData$propdmgexp == 0)] <- NA

# we still have "0" as a factor level, so let's turn propdmgexp back into a character, and then back into a factor to remove 0 as a factor level.
stormData$propdmgexp <- as.character(stormData$propdmgexp)
stormData$propdmgexp <- as.factor(stormData$propdmgexp)
summary(stormData$propdmgexp)


stormData$cropdmgexp <- as.factor(stormData$cropdmgexp)
summary(stormData$cropdmgexp)


# we should see how many values even have property or crop damage
100 - length(which(stormData$propdmg == 0))/nrow(stormData)*100 # 29.0%
100 - length(which(stormData$cropdmg == 0))/nrow(stormData)*100 # 2.8%

100 - length(which(stormData$fatalities == 0))/nrow(stormData)*100 # 0.7%
100 - length(which(stormData$injuries == 0))/nrow(stormData)*100 # 1.4%

# Given that large shares of storms do NOT cause any kind of property damage, crop damage, fatalaties, or injuries, we should only included cases where the values for these categories is greater than 0. Otherwise, our data might look incredibly skewed.

which(stormData$propdmg > 0)
stormPropDmg <- stormData[which(stormData$propdmg > 0),]
stormCropDmg <- stormData[which(stormData$cropdmg > 0),]

stormFatal <- stormData[which(stormData$fatalities > 0),]
stormInj <- stormData[which(stormData$injuries > 0),]

# we need to summarize the data by event type before plotting         
fatalitySummary <-  stormFatal %>% 
    group_by(evtype) %>% 
    summarize(mean = mean(fatalities),
              median = median(fatalities),
              sd = sd(fatalities),
              n = n(),
              total = sum(injuries),
              se = sd(fatalities)/sqrt(n()),
              min = min(fatalities),
              max = max(fatalities))
fatalitySummary

# set values with NA for sd and se to 0
fatalitySummary$sd[which(is.na(fatalitySummary$sd))] <- 0
fatalitySummary$se[which(is.na(fatalitySummary$se))] <- 0

View(fatalitySummary)

fatalGraph <- ggplot(fatalitySummary) +
    geom_col(aes(x = reorder(evtype, -mean), y = mean, fill = evtype)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "none") +
    xlab("Event Type") +
    ylab("Mean Fatalities") +
    ggtitle("Mean Fatalities by Event Type") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_errorbar(aes(x = evtype, ymin = mean - se,
                                ymax = mean + se))
fatalGraph # tsunami throws things off quite a bit since it only has 2 observations


injurySummary <-  stormInj %>% 
    group_by(evtype) %>% 
    summarize(mean = mean(injuries),
              median = median(injuries),
              sd = sd(injuries),
              n = n(),
              total = sum(injuries),
              se = sd(injuries)/sqrt(n()),
              min = min(injuries),
              max = max(injuries))
injurySummary
injurySummary$sd[which(is.na(injurySummary$sd))] <- 0
injurySummary$se[which(is.na(injurySummary$se))] <- 0
View(injurySummary)

injuryGraph <- ggplot(injurySummary) +
    geom_col(aes(x = reorder(evtype, -mean), y = mean, fill = evtype)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position = "none") +
    xlab("Event Type") +
    ylab("Mean Injuries") +
    ggtitle("Mean Injuries by Event Type") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_errorbar(aes(x = evtype, ymin = mean - se,
                      ymax = mean + se))
injuryGraph



stormPropDmg <- stormPropDmg %>% 
    mutate(totalPropDmg = propdmg *case_when(
        stormPropDmg$propdmgexp == "K" ~ 1e3,
        stormPropDmg$propdmgexp == "M" ~ 1e6,
        stormPropDmg$propdmgexp == "B" ~ 1e9))


