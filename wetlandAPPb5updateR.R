



locNew <- "./newData"
locOld <- "../wetlandAPP/data"

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
load_or_install(c("tidyverse", "lubridate"))


# Get existing b5 data
existing <- list.files(locOld, pattern = "dfb5*")
existdf <- read.csv(paste0(locOld, "/", existing), header = TRUE,
                     stringsAsFactors = FALSE)
existdf$DATE <- parse_date_time(existdf$DATE, orders = c("dmY", "Ymd"))

# Make long and get rid of NA's
existdfl <- existdf%>%
  gather("WLAND", "B5", 2:length(names(existdf)))%>%
  filter(!is.na(B5))


toDo <- list.files(locNew)
newdfl <- data.frame(DATE = as.character(), WLAND = as.character(),
                     B5 = as.numeric(), stringsAsFactors = FALSE)

for(i in 1:length(toDo)){
  dat <- read.csv(paste(locNew, toDo[i], sep = "\\"), header = TRUE, 
                  stringsAsFactors = FALSE)
  names <- names(dat)
  if('X' %in% names){
    dat <- dat[,-1]
  }
  dat$date <- parse_date_time(dat$date , orders = c("dmY", "Ymd"))
  dat2 <- dat%>%
    gather("WLAND", "B5", 2:length(names(dat)))%>%
    filter(!is.na(B5))%>%
    rename(DATE = date)%>%
    arrange(WLAND, DATE)
  newdfl <- rbind(newdfl, dat2)
}

outdf <- rbind(existdfl, newdfl)
outdf2 <- outdf%>%
  arrange(WLAND, DATE)%>%
  spread(WLAND, B5)



write.csv(outdf2, file = paste0(locOld, "/", "dfb5_as_at_", Sys.Date(), ".csv"),
          row.names = FALSE)
