require(readr)
# download file
temp<-tempfile()

download.file('https://s3.amazonaws.com/tripdata/201603-citibike-tripdata.zip', 
              destfile='trip_201603.zip', method='curl')
trip_201603<- unzip("trip_201603.zip")
class(trip_201603)
#the temps?
trip_201603_csv<- read_csv(trip_201603)
#url generate
citibike_url <- function(year, month) {
  base_url <- "https://s3.amazonaws.com/tripdata/"
  sprintf(paste0(base_url, "%04d%02d-citibike-tripdata.zip"), year, month)
}
citibike_basename<- function(year, month){
  basename<- "trip_"
  sprintf(paste0(basename,"%04d%02d.zip"),year, month)
}
citibike_csvname<- function(year, month){
  basename<- "trip_"
  sprintf(paste0(basename,"%04d%02d_csv"),year, month)
}
data_name<- function( year, month){
  sprintf(paste0("data","%04d%02d"),year, month)
}
citibike_download<- function( year, month ){
  fileaddress<- citibike_url (year, month )
  filename<- citibike_basename (year, month )
  filename_csv<- citibike_csvname (year, month) 
  download.file( fileaddress, destfile= filename, method='curl')
  read_csv(unzip( filename ))
}

# citibike_download(2016,01)
data<-citibike_load(2016,01)
head(data)

                