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
  
  invisible(obj)
}
