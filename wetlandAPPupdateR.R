################################################################################
# The wetlandAPP band 5 data updating function.
#
# The band 5 data must be in data frames as extracted from the function extractR
# If this function is run from a different location than .../wetlandAPPTools then 
# the function parameter arguments need to be updated with applicable filepaths.
# 
# @param locNEW is the location of newly extracted landsat band 5 data
# @param locOLD is the location of the existing landsat band 5 data within the 
# data folder residing inside the wetlandAPP
#
# See html doco for more details and description of pmin method for merge
# 
# Bart Huntley
# 29/11/2016




wetlandAPPupdateR <- function(locNEW = ".\\newData", locOLD = "..\\wetlandAPP\\data"){
  
  # Function to install and/or load libraries
  is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
  load_or_install<-function(package_names){  
    for(package_name in package_names)  
    {  
      if(!is_installed(package_name))  
      {  
        install.packages(package_name,repos="http://cran.csiro.au/")  
      }  
      library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
    }  
  }
  load_or_install(c("lubridate", "dplyr"))
  
  # Function to merge duplicated columns (uses pmin)
  fixColumn <- function(colm, myDF) { 
    x <- myDF[[paste0(colm, ".x")]]
    y <- myDF[[paste0(colm, ".y")]]
    z <- as.data.frame(pmin(x, y, na.rm = TRUE))
    return(z)
  }
  
  # Grab new data to incorporate
  toDo <- list.files(locNEW)
  toDolist <- vector("list", length(toDo))
  
  for(i in 1:length(toDo)){
    dat <- read.csv(paste(locNEW, toDo[i], sep = "\\"), header = TRUE, 
                    stringsAsFactors = FALSE)
    names <- colnames(dat)
    if('X' %in% names){
      dat <- dat[,-1]
    }
    dat$date <- parse_date_time(dat$date , orders = c("dmY", "Ymd"))
    toDolist[[i]] <- dat
  }
  
  # Grab existing dfb5 from the APP repo
  existing <- list.files(locOLD, pattern = glob2rx("dfb5*"))
  
  baseData <- read.csv(paste(locOLD, existing, sep = "\\"),
                       header = TRUE, stringsAsFactors = FALSE)
  baseData$date <- parse_date_time(baseData$date , orders = c("dmY", "Ymd"))
  
  # Full join - gives .x and .y copies of duplicate column names
  for(j in 1:length(toDolist)){
    baseData <- full_join(baseData, toDolist[[j]], "date")
  }
  
  # Muck around to give alpha order to column names
  newBase <- baseData%>%
    arrange(date)
  colSort <- names(newBase)[-1]
  colSort <- order(colSort)+1
  colsort <- c(1, colSort)
  newBase2 <- newBase[,c(1,colSort)]
  
  # Find duplicated columnss
  dupes <- substr(names(newBase2)[duplicated(substr(names(newBase2), 1, 4))],
                  1, 4)
  
  # Use function (using pmin) to merge duplicate columns and delete .y version
  newBase2[, paste0(dupes, ".x")]  <- sapply(dupes, fixColumn, newBase2)
  for (cc in paste0(dupes, ".y")){
    newBase2[[cc]] <- NULL
  }
  
  # Get rid of .x suffix
  names(newBase2) <- substr(names(newBase2), 1, 4)
  
  # Write back updated data to wetlandAPP
  write.csv(newBase2, file = paste0(locOLD, "\\dfb5_as_at_", 
                                    Sys.Date(), ".csv"), row.names = FALSE)
  
  
}

# locNEW = ".\\newData"
# locOLD = "..\\wetlandAPP\\data"

wetAPPupdate()
