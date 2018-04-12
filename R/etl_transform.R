utils::globalVariables(c("."))

#' etl_transform
#' @rdname etl_extract.etl_citibike
#' @method etl_transform etl_citibike
#' @importFrom utils unzip head
#' @importFrom lubridate parse_date_time
#' @inheritParams etl::etl_extract
#' @details This function unzips and reads in the NYC CitiBike data for years 
#' and months specified into R. After cleaning up and reformatting, 
#' R outputs the data and save as files.
#' 
#' @export
etl_transform.etl_citibike <- function(obj, years = 2013, months = 7, ...) {
  # pop up message to start
  message("Transforming raw data ...")
  
  # check for valid years and months
  valid <- valid_year_month(years, months) %>%
    mutate_(year_month = ~format(month_begin, "%Y%m"))
  
  # list the files under the raw folder
  src <- data_frame(
    path = list.files(attr(obj, "raw_dir"), pattern = "\\.zip", full.names = TRUE)
  ) %>%
    mutate_(month_begin = ~lubridate::date(lubridate::parse_date_time(path, orders = "%Y%m"))) %>%
    inner_join(valid, by = "month_begin") 
  
  # unzip files interested
  message("Unzipping...")
  load_files <- lapply(src$path, unzip, exdir = attr(obj, "load_dir")) %>%
    unlist()
  
  # the original names
  new_files <- fix_names(load_files)

  # rename the files
  file.rename(from = load_files, to = new_files)
  
  # the following function takes in the file of a csv file
  # modifies the data and outputs the file under the same name
  # apply the function to each file
  message("Modifying CSVs...")
  lapply(new_files, modify_file)
  invisible(obj)
}

#' @importFrom readr read_csv write_csv

modify_file <- function(csv_load_path) {
  message(paste("Transforming", csv_load_path, "..."))
  # read the file into R
  # faster but requires extra effort
  # data <- data.table::fread(csv_load_path)
  # 
  # # check for date format
  # if(suppressWarnings(all(is.na(
  #   lubridate::parse_date_time(utils::head(data$Start_Time), 
  #                              orders= c("mdY HMS","mdy HM")))) == FALSE)) {
  #   # change the format to ymd HMS
  #   data <- data %>%
  #     dplyr::mutate_(Start_Time = ~lubridate::parse_date_time(Start_Time, 
  #                                                             orders = c("mdY HMS","mdy HM")),
  #                    Stop_Time = ~lubridate::parse_date_time(Stop_Time, 
  #                                                            orders = c("mdY HMS","mdy HM")))  
  # } else{
  #   # no need to change the format
  # }
  # # change from ymd HMS to ymd HM
  # data$Start_Time <- substring(data$Start_Time, 1, 16)
  # data$Stop_Time <- substring(data$Stop_Time, 1, 16)
  
  # slower but better with data types
  data <- readr::read_csv(csv_load_path)
  # rename columns
  colnames(data) <- c("duration", "start_time","stop_time", 
                      "start_station_id", "start_station_name", 
                      "start_station_latitude", "start_station_longitude",
                      "end_station_id", "end_station_name",
                      "end_station_latitude", "end_station_longitude",
                      "bike_id", "user_type", "birth_year", "gender")
  
  start_stations <- data %>%
    group_by_(station_id = ~start_station_id) %>%
    summarize_(num_starts = ~n(), 
              earliest = ~min(start_time),
              latest = ~max(stop_time),
              num_names = ~n_distinct(start_station_name),
              names = ~last(start_station_name),
              # take the mean of the locations? 
              lat = ~mean(start_station_latitude),
              lon = ~mean(start_station_longitude))
  
  end_stations <- data %>%
    group_by_(station_id = ~end_station_id) %>%
    summarize_(num_stops = ~n(), 
              earliest = ~min(start_time),
              latest = ~max(stop_time),
              num_names = ~n_distinct(end_station_name),
              names = ~last(end_station_name),
              # take the mean of the locations? 
              lat = ~mean(end_station_latitude),
              lon = ~mean(end_station_longitude))
  stations <- start_stations %>%
    bind_rows(end_stations) %>%
    group_by_(~station_id) %>%
    summarize_(name = ~first(names), 
              earliest = ~min(earliest),
              latest = ~max(latest),
              num_starts = ~sum(num_starts, na.rm = TRUE),
              num_stops = ~sum(num_stops, na.rm = TRUE),
              lat = ~mean(lat),
              lon = ~mean(lon))
  
  readr::write_csv(stations, gsub("tripdata", "stations", csv_load_path))
  
  # output the modified csv out
#  data.table::fwrite(x = data, csv_load_path, append = FALSE)
  data %>%
    select_(~-start_station_name, 
            ~-start_station_latitude,
            ~-start_station_longitude,
            ~-end_station_name,
            ~-end_station_latitude,
            ~-end_station_longitude) %>%
    readr::write_csv(csv_load_path, append = FALSE)
}


fix_names <- function(x, ...) {
  x %>%
    gsub(" ", "", x = .) %>%
    gsub("-", "", x = .) %>%
    gsub("_", "", x = .) %>%
    tolower() %>%
    gsub("citibike[a-z]+", "_citibike_tripdata", x = .) %>%
    gsub("nyc", "", x = .)
}