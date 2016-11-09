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
#' @param years a numeric vector indicating the years to be downloaded (default is 2013)
#' @param months a numeric vector indicating the months to be downloaded (default is 7)
#' @details This function downloads NYC CitiBike data from citibike website for years and months specified.
#' @export
etl_extract.etl_citibike <- function(obj,  years = 2013, months = 7, ...) {
  raw_url <-"https://s3.amazonaws.com/tripdata/"
  webpage <- xml2::read_html(raw_url)
  keys <- webpage %>%
    rvest::html_nodes("key") %>%
    rvest::html_text()
  zips <- grep("*.zip", keys, value = TRUE)
  zips <- zips[-1]
  
  # filter zips for only those months that correspond to years & months arguments
  zips_df <- data.frame(zips, date = lubridate::parse_date_time(zips, orders = "%Y%m") + lubridate::days(1)) 
  # check if the date is valid
  # the date dataframe after filtering
  zips_valid<- filter(zips_df, lubridate::year(date) %in% years & lubridate::month(date) %in% months)
  
  if(nrow(zips_valid) == 0){
    print("Not a valid date")
  } else{
    src <- paste0(raw_url, zips_valid$zips)
    
    smart_download(obj, src)
    invisible(obj)
  }
}

