#' etl_transform
#' @rdname etl_extract.etl_citibike
#' @method etl_transform etl_citibike
#' @importFrom utils unzip 
#' @inheritParams etl::etl_extract
#' @details This function unzips NYC CitiBike data for years and months specified. 
#' @export
etl_transform.etl_citibike <- function(obj, years = 2013, months = 7, ...) {
  message("Transforming raw data ...")
  dir <- attr(obj, "raw_dir")
  src <- list.files(dir, full.names = TRUE)
  files <- basename(src)
  
  year_month <- valid_year_month(years, months) %>%
    mutate(month = ifelse(month< 10, paste0("0",month), month))%>%
    mutate_(year_month = ~paste0(year, month)) %>%
    mutate_(zip_files = ~paste0(year_month, "-citibike-tripdata.zip")) %>%
    filter_(~zip_files %in% files) %>%
    mutate_(path = ~paste0(dir,"/",zip_files))
  path <- year_month$path
  new_dir <- attr(obj, "load_dir")
  for (i in path) {
    utils::unzip(i, exdir = new_dir)
    }
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
  # change the date format
  for (i in load_files){
    # read in file
    data <- fread(input = paste(new_dir,i,sep = "/"))
    # rename columns
    colnames(data) <- c("Trip_Duration", "Start_Time","Stop_Time", "Start_Station_ID", "Start_Station_Name", 
                        "Start_Station_Latitude", "Start_Station_Longitude", "End_Station_ID", "End_Station_Name",
                        "End_Station_Latitude", "End_Station_Longitude","Bike_ID", "User_Type", "Birth_Year", "Gender")
    # recognize date format
    if(all(is.na(parse_date_time(head(data$Start_Time), orders= "mdY HMS"))) == FALSE){
      # the format is month day year
      data <- data %>%
        mutate(Start_Time = parse_date_time(Start_Time, orders= "mdY HMS")) %>%
        mutate(Stop_Time = parse_date_time(Stop_Time, orders= "mdY HMS")) 
      
    }else{
      # write out file directly
      
    }
    fwrite(x = data, paste(new_dir,i,sep = "/"), append = FALSE)
  }
  
  invisible(obj)
}
