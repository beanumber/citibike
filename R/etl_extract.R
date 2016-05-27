#' etl_extract
#' @rdname etl_extract.etl_citibike
#' @export
#' @import etl
#' @import dplyr
#' @importFrom rvest html_nodes html_text
#' @importFrom xml2 read_html
#' @importFrom utils download.file
#' @inheritParams etl::etl_extract
#' @details The function downloads all the data from citibike website.
#' 
#' @examples 
#' \dontrun{
#' bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
#' bikes %>%
#'   etl_extract()
#' }

etl_extract.etl_citibike <- function(obj, ...) {
  raw_url <-"https://s3.amazonaws.com/tripdata/"
  webpage <- xml2::read_html(raw_url)
  keys <- webpage %>%
    rvest::html_nodes("key") %>%
    rvest::html_text()
  zips <- grep("*.zip", keys, value = TRUE)
  zips <- zips[-1]
  appendName <- function(file) {
    base_url <- "https://s3.amazonaws.com/tripdata/"
    sprintf(paste0("https://s3.amazonaws.com/tripdata/", file))
  }
  
  src <- lapply(zips, appendName)
  src <- unlist(src)
  smart_download(obj, src)
  invisible(obj)
}
