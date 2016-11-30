################################################################################
# Code to munge historical depths into one data source.
# Code reads xlxs files as strange formatting pads out csv's with 1000's of
# ghost rows.





library(gdata)
library(lubridate)
library(dplyr)
library(tidyr)


wd <- paste0("Z:\\DEC\\SouthWestWetlandsMonitoring_16029E01\\",
             "DATA\\Working\\hist_depth_data")

setwd(wd)



xlist <- list.files(pattern = "*.xlsx")

# a <- read.xls(xlist[2], sheet = 1)
resdf <- data.frame(Code = as.character(), Date = as.character(),
                    Depth..m. = as.numeric(), stringsAsFactors = FALSE)

for(i in 1: length(xlist)){
  a <- read.xls(xlist[i], sheet = 1)
  b <- a[,c(1, 3:4)]
  b$Date <- parse_date_time(b$Date, orders = c("dmY", "Ymd"))
  resdf <- rbind(resdf, b)
  print(paste0("Done ", xlist[i]))
  
}

# NA's represent blank cells which mean "other data recorded that day but not depth"
# These need to be removed

ind <- is.na(resdf$Depth..m.)
resdf2 <- resdf[!ind,]

outdf <- resdf2%>%
  spread(Code, Depth..m.)%>%
  rename(DATE = date)%>%
  arrange(Date)

sum(duplicated(outdf$Date)) # just checking!

write.csv(outdf, file = paste0("dfhist_as_at_", Sys.Date(), ".csv"), 
          row.names = FALSE)

