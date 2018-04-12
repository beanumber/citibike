#' etl_extract
#' @rdname etl_extract.etl_citibike
#' @method etl_extract etl_citibike
#' @import etl
#' @import dplyr
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom utils download.file
#' @importFrom lubridate year month days parse_date_time
#' @inheritParams etl::etl_extract
#' @param years a numeric vector indicating the years to be 
#' downloaded (default is 2013)
#' @param months a numeric vector indicating the months to be 
#' downloaded (default is 7)
#' @details This function downloads NYC CitiBike data from citibike website 
#' for years and months specified. The downloaded files are saved as zip files 
#' in the raw directory under the folder the user created
#' @export
etl_extract.etl_citibike <- function(obj,  years = 2013, months = 7, ...) {
  # the downloadable files of Citi Bike trip data are stored under 
  # Amazon Simple Storage Service website
  raw_url <-"https://s3.amazonaws.com/tripdata/"
  # read in the html document throught the url connection
  webpage <- xml2::read_html(raw_url)
  # extract pieces out of HTML documents
  keys <- webpage %>%
    rvest::html_nodes("key") %>%
    rvest::html_text()
  # return the list of zip files available
  
  zips <- grep("^[0-9]{6}.+\\.zip$", keys, value = TRUE)
  # ignore the first file that contains data from multiple months
  zips <- zips[-1]
  zips <- grep("jc", zips, invert = TRUE, value = TRUE)
  
  valid_dates <- etl::valid_year_month(years, months)
  
  # filter zips for only those months that correspond 
  # to years & months arguments from the user
  zips_df <- data.frame(
    zips, 
    month_begin = lubridate::date(lubridate::parse_date_time(zips, orders = "%Y%m"))
  )
  # check if the date is valid
  # the date dataframe after filtering
  zips_valid <- zips_df %>%
    inner_join(valid_dates, by = "month_begin")
  
  if (nrow(zips_valid) == 0) {
    # pop up warning message when invalid inputs are entered
    warning("No data available during that time range.")
  } else{
    # create the paths for files to download
    src <- paste0(raw_url, zips_valid$zips)
    # Download only those files that don't already exist
    smart_download(obj, src)
  }
  invisible(obj)
}

