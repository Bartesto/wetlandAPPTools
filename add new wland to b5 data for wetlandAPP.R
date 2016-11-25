################################################################################
# Script to amalgamate newly extracted data to the band 5 data frame for the 
# wetlandAPP (wetland depth modelling app)
#
# Takes extracted b5 data for new wetlands and adds to existing dfb5 data for
# the APP.
# 
# Filepaths are relative so may need adjusting if you change folder structure or
# location.
#
# By Bart Huntley
# 25/11/2016

library(dplyr)

# Grab new data to incorporate
toDo <- list.files(".\\newData")
toDolist <- vector("list", length(toDo))

for(i in 1:length(toDo)){
  dat <- read.csv(paste0(".\\newData\\", toDo[i]), header = TRUE, 
                  stringsAsFactors = FALSE)
  names <- colnames(dat)
  if('X' %in% names){
    dat <- dat[,-1]
  }
  dat$date <- as.Date(dat$date)
  toDolist[[i]] <- dat
}

# Grab existing dfb5 from the APP repo
existing <- list.files("..\\wetlandAPP\\data", pattern = glob2rx("dfb5*"))

baseData <- read.csv(paste("..\\wetlandAPP\\data", existing, sep = "\\"),
                     header = TRUE, stringsAsFactors = FALSE)
baseData$date <- as.Date(baseData$date, "%d/%m/%Y")

newBase <- baseData

# Full join - may not work when updating wetlands that already exist in APP data
for(j in 1:length(toDolist)){
  newBase <- full_join(newBase, toDolist[[j]], "date")
}

# Muck around to give alph order to column names
newBase <- newBase%>%
  arrange(date)
colSort <- names(newBase)[-1]
colSort <- order(colSort)+1
colsort <- c(1, colSort)
newBase2 <- newBase[,c(1,colSort)]

# Write back updated data to wetlandAPP
write.csv(newBase2, file = paste0("..\\wetlandAPP\\data\\", "dfb5_as_at_", 
                                  Sys.Date(), ".csv"), row.names = FALSE)



