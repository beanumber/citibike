#' etl_transform
#' @rdname etl_extract.etl_citibike
#' @method etl_transform etl_citibike
#' @importFrom utils unzip
#' @importFrom data.table fread fwrite 
#' @inheritParams etl::etl_extract
#' @details This function unzips NYC CitiBike data for years and months specified. 
#' @export
etl_transform.etl_citibike <- function(obj, years = 2013, months = 7, ...) {
  message("Transforming raw data ...")
  dir <- attr(obj, "raw_dir")
  src <- list.files(dir, full.names = TRUE)
  files <- basename(src)
  new_dir <- attr(obj, "load_dir")
  
  year_month <- valid_year_month(years, months) %>%
    mutate(month = ifelse(month< 10, paste0("0",month), month))%>%
    mutate_(year_month = ~paste0(year, month)) %>%
    mutate_(zip_files = ~paste0(year_month, "-citibike-tripdata.zip")) %>%
    mutate_(out_files = ~paste0(year_month,"-citibike-tripdata.csv"))%>%
    filter_(~zip_files %in% files) %>%
    mutate_(path = ~paste0(dir,"/",zip_files)) %>%
    mutate_(out_path = ~paste0(new_dir,"/",out_files))
  # zip file path
  path <- year_month$path
  # valid csv path
  csv_path <- year_month$out_path
  # unzip files interested
  lapply(path, function(x) unzip(x, exdir = new_dir))
  # rename
  load_files <- list.files(new_dir)
  check_name <- function(file_name){
    if(grepl("-citibike-tripdata", x = file_name) == FALSE){
      new_name <- gsub("-", "", x = file_name)
      new_name <- gsub("  Citi Bike trip data", "-citibike-tripdata", x = new_name)
      new_name
    }else{
      file_name
    }
  }
  new_names <- vector(mode="character", length=length(load_files))
  count = 1
  for (i in load_files){
    new_names[count] <- check_name(i)
    count = count +1
  }
  file.rename(from = file.path(new_dir,load_files), to = file.path(new_dir,new_names))
  
  # the following function takes in the file of a csv file, modifies the data and outputs the file under the same name
  modify_file <- function(csv_load_path){
    # read the file into R
    data <- data.table::fread(csv_load_path)
    # rename columns
    colnames(data) <- c("Trip_Duration", "Start_Time","Stop_Time", "Start_Station_ID", "Start_Station_Name", 
                        "Start_Station_Latitude", "Start_Station_Longitude", "End_Station_ID", "End_Station_Name",
                        "End_Station_Latitude", "End_Station_Longitude","Bike_ID", "User_Type", "Birth_Year", "Gender")
    # check for date format
    if(suppressWarnings(all(is.na(lubridate::parse_date_time(head(data$Start_Time), orders= c("mdY HMS","mdy HM")))) == FALSE)){
      # change the format to ymd HMS
      data<- data %>%
        mutate(Start_Time = parse_date_time(Start_Time, orders= c("mdY HMS","mdy HM")),
               Stop_Time = parse_date_time(Stop_Time, orders= c("mdY HMS","mdy HM")))  
    }else{
      # no need to change the format
    }
    # change from ymd HMS to ymd HM
    data$Start_Time <- substring(data$Start_Time,1,16)
    data$Stop_Time <- substring(data$Stop_Time,1,16)
    
    # output the modified csv out
    data.table::fwrite(x = data, csv_load_path, append = FALSE)
    
  }
  
  # apply the function to each file
  lapply(csv_path,function(x) modify_file(x))
  
  invisible(obj)
}
