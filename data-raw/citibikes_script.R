library(readr)

starttimes <- vector()

for(i in 1:12){
  temp <- tempfile()
  if(i < 10){
  download.file(paste("https://s3.amazonaws.com/tripdata/20140", i, "-citibike-tripdata.zip", sep = ""), temp)
  }
  else{
    download.file(paste("https://s3.amazonaws.com/tripdata/2014", i, "-citibike-tripdata.zip", sep = ""), temp)
  }
  starttimes[i] <- as.character(read_csv(unzip(temp, exdir = "C:/Users/Jonathan/Desktop/R_WorkDir"),
           n_max = 1)$starttime)
}

starttimes