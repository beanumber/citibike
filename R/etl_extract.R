#' etl_extract
#' @rdname etl_extract.etl_citibike
#' @inheritParams etl::etl_extract
#' @param year a year represented as a four-digit integer
#' @param months a vector of integers representing the months
#' @details If a \code{year} and/or \code{month} is specified, then
#' only flight data from matching months is used.
#' @export
#' 
#' @examples 
#' db <- "~/dumps/citibike/db.sqlite3"
#' bikes <- etl("citibike", dir = "~/dumps/citibike/")

etl_extract.etl_citibike <- function(obj, year = 2015, month = 1, ...) {
  base_url <- "https://s3.amazonaws.com/tripdata/"
  src <- sprintf(paste0(base_url, "%04d%02d-citibike-tripdata.zip"), year, month)
#  dir <- attr(obj, "raw_dir")
  dir = "~/dumps/citibike/raw/"
  lcl <- paste0(dir, basename(src))
  
  download.file(src, destfile = lcl, method = 'curl')
}