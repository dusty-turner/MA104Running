# Use this code with the GPX file extracted from RunKeeper.com  
# The result of this code is to output .csv files 
choose.dir(getwd(), "Choose a suitable folder") # select subfolder 'scripts', works OK
library(plotKML)
library(dplyr)
library(geosphere)
library(lubridate)
#test
# Use default titles of GPX files.
# Set working directory here:
# setwd("~/R GPS")

# Create a list of the file names that begin with "20....", which is the default for GPX files exported from RunKeeper.com
files <- list.files(pattern = "\\b20")

readGPX(files[1], way=T)

# Loop to iterate through each GPX file.
for (j in 1:length(files)) {
  # Select first file from the list and import data into R object
  wplist <- readGPX(files[j], way=T)
  
  # Extract latitude, longitude, elevation, time, name and comments and apppend to R dataframe
  wpdf<- wplist$tracks[[1]][[1]] 
  
  # Create vector to store calculations for distance between data points (distance given in meters) and set starting distance to 0 
  distBetween <- c()
  distBetween[1] = 0
  
  # Loop to calculate distances between all collected points
  for (i in 1:(nrow(wpdf)-1)) {
    distBetween[i+1] = distm(cbind(wpdf$lon[i],wpdf$lat[i]),cbind(wpdf$lon[i+1],wpdf$lat[i+1]),fun=distHaversine)
  }
  
  # Append the between distances as a column to the data 
  wpdf["distBetween"] <- distBetween
  
  # Convert imported date and time data into a usable format (note: time zone defaults to Z)
  wpdfNew = wpdf %>% 
    mutate(DTG = ymd_hms(time))
  
  # Create vector to store calculations for time between data points (time given in seconds) and set starting time to 0
  timeDiff <- c()
  timeDiff[1] = 0
  
  # Loop to calculate the time (in seconds) between all collected points
  for(i in 1:(nrow(wpdfNew)-1)) {
    timeDiff[i+1] = as.numeric(difftime(wpdfNew$DTG[i+1],wpdfNew$DTG[i],units = "sec"))
  }
  
  # Append the between times as a column to the data
  wpdfNew["timeDiff"] <- timeDiff
  
  # Mutate the data so that we have the cumulative time and distances and then select the data that we want.
  wpdfNew = wpdfNew %>%
    mutate(totDist = cumsum(distBetween))%>%
    mutate(totTime = cumsum(timeDiff)) %>%
    select(DTG, lon, lat, totTime, totDist, ele)
  
  # Write .csv file
  write.csv(wpdfNew, paste0("Workout_",j,".csv"))
  
}



# Other stuff that goes into the app.


library(stringr)

temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

firsthelper = NULL
helper = NULL
for(i in 1:length(myfiles)){
 helper =  as.data.frame(
    matrix(
      unlist(
        strsplit(
          as.character(unlist(myfiles[[i]][[1]])) , ","
        )
      ), ncol = 7, byrow = T
      
    )
  )
 
 n <- nrow(helper)
 v <- rep(i,n)
 helper = cbind(helper,v)
 
 firsthelper = rbind(firsthelper,helper)
}

firsthelper

names(firsthelper) = c("X", "DTG", "lon", "lat", "totTime", "totDist", "ele")

tail(firsthelper)

firsthelper = firsthelper %>%
  mutate(DTG = as.POSIXct(DTG)) %>%
  mutate(lon = as.numeric(as.character(lon))) %>%
  mutate(lat = as.numeric(as.character(lat))) %>%
  mutate(totTime = as.numeric(as.character(totTime))) %>%
  mutate(totDist = as.numeric(as.character(totDist))) %>%
  mutate(ele = as.numeric(as.character(ele))) 

str(firsthelper)


as.data.frame(
 matrix(
 unlist(
  strsplit(
as.character(unlist(myfiles[[1]][[1]])) , ","
 )
), ncol = 7, byrow = T
 
)
)
