#' etl_transform
#' @rdname etl_extract.etl_citibike
#' @method etl_transform etl_citibike
#' @importFrom utils unzip head
#' @importFrom data.table fread fwrite 
#' @inheritParams etl::etl_extract
#' @details This function unzips and reads in the NYC CitiBike data for years 
#' and months specified into R. After cleaning up and reformatting, 
#' R outputs the data and save as files.
#' 
#' @export
etl_transform.etl_citibike <- function(obj, years = 2013, months = 7, ...) {
  # pop up message to start
  message("Transforming raw data ...")
  # link to the raw folder directory
  dir <- attr(obj, "raw_dir")
  # list the files under the raw folder
  src <- list.files(dir, full.names = TRUE)
  # get the base name of the files
  files <- basename(src)
  # connect to the load folder
  new_dir <- attr(obj, "load_dir")
  # check for valid years and months
  year_month <- valid_year_month(years, months) %>%
    mutate_(month = ~ifelse(month < 10, paste0("0",month), month))%>%
    mutate_(year_month = ~paste0(year, month)) %>%
    mutate_(zip_files = ~paste0(year_month, "-citibike-tripdata.zip")) %>%
    mutate_(out_files = ~paste0(year_month, "-citibike-tripdata.csv"))%>%
    filter_(~zip_files %in% files) %>%
    mutate_(path = ~paste0(dir, "/", zip_files)) %>%
    mutate_(out_path = ~paste0(new_dir, "/", out_files)) 
  # zip file path
  path <- year_month$path
  # valid csv path
  csv_path <- year_month$out_path
  # unzip files interested
  lapply(path, function(x) unzip(x, exdir = new_dir))
  # the original names
  load_files <- list.files(new_dir, full.names = TRUE)
  # the old names in the load directory
  oldnames <- list.files(new_dir)
  # check if the name is in the right format
  check_name <- function(file_name){
    if(grepl("-citibike-tripdata", x = file_name) == FALSE){
      new_name <- gsub("-", "", x = file_name)
      new_name <- gsub("  Citi Bike trip data", "-citibike-tripdata", 
                       x = new_name)
      new_name
    } else{
      file_name
    }
  }
  # return the new valid names
  new_names <- do.call(rbind, lapply(load_files, function(x) check_name(x)))
  # rename the files
  file.rename(from = file.path(new_dir, oldnames), 
              to = file.path(new_dir, year_month$out_files))
  
  # the following function takes in the file of a csv file
  # modifies the data and outputs the file under the same name
  modify_file <- function(csv_load_path) {
    # read the file into R
    data <- data.table::fread(csv_load_path)
    # rename columns
    colnames(data) <- c("Trip_Duration", "Start_Time","Stop_Time", 
                        "Start_Station_ID", "Start_Station_Name", 
                        "Start_Station_Latitude", "Start_Station_Longitude",
                        "End_Station_ID", "End_Station_Name",
                        "End_Station_Latitude", "End_Station_Longitude",
                        "Bike_ID", "User_Type", "Birth_Year", "Gender")
    # check for date format
    if(suppressWarnings(all(is.na(
      lubridate::parse_date_time(utils::head(data$Start_Time), 
      orders= c("mdY HMS","mdy HM")))) == FALSE)) {
      # change the format to ymd HMS
      data <- data %>%
        dplyr::mutate_(Start_Time = ~lubridate::parse_date_time(Start_Time, 
                                            orders = c("mdY HMS","mdy HM")),
               Stop_Time = ~lubridate::parse_date_time(Stop_Time, 
                                           orders = c("mdY HMS","mdy HM")))  
    } else{
      # no need to change the format
    }
    # change from ymd HMS to ymd HM
    data$Start_Time <- substring(data$Start_Time, 1, 16)
    data$Stop_Time <- substring(data$Stop_Time, 1, 16)
    
    # output the modified csv out
    data.table::fwrite(x = data, csv_load_path, append = FALSE)
    
  }
  # apply the function to each file
  lapply(csv_path, function(x) modify_file(x))
  invisible(obj)
}
