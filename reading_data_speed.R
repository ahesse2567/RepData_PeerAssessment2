# reading data speeds

library(data.table)

# if(!file.exists("repdata_data_StormData.csv")) { # this doesn't quite work
# unzip(zipfile = "repdata_data_StormData.csv.bz2")
# }

before <-  Sys.time()
bunzip2("repdata_data_StormData.csv.bz2", remove = FALSE, 
        skip = TRUE)
weather <- read.csv("repdata_data_StormData.csv", header = TRUE,
                    na.strings = c("", "NA"))
after <- Sys.time()
duration <- after-before
duration
# Time difference of 43.51383 secs


before2 <- Sys.time()
weather2 <- read.csv("repdata_data_StormData.csv.bz2", header = TRUE,
                     na.strings = c("", "NA"))
after2 <- Sys.time()
duration2 <- after2-before2
duration2
# Time difference of 43.8109 secs. Techincally this is a little slower than unzipping

before3 <- Sys.time()
weather3 <- fread("repdata_data_StormData.csv.bz2", header = TRUE,
                  na.strings = c("", " ", NA))
after3 <- Sys.time()
duration3 <- after3-before3
duration3
# Time difference of 25.58647 secs. By far the fastest