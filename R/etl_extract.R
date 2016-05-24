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
#' bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
#' bikes %>%
#'   etl_extract()

etl_extract.etl_citibike <- function(obj, ...) {
  raw_url<-"https://s3.amazonaws.com/tripdata/"
  webpage<- xml2::read_html(raw_url)
  keys<- webpage %>%
    rvest::html_nodes("key") %>%
    rvest::html_text()
  zips<- grep("*.zip",keys, value= TRUE)
  zips<- zips[-1]
  checkInput<- function(date) {
    ifelse(sum(ifelse(grepl(date,zips)== TRUE,1,0))==1,
           grep(date,zips, value=TRUE), 
           warning("Error Message: Not a valid date. Please try another one."))
  }
  appendName<- function(file){
    base_url <- "https://s3.amazonaws.com/tripdata/"
    sprintf(paste0("https://s3.amazonaws.com/tripdata/", file))
  }
  
  src <- lapply(zips,appendName)
  src<- unlist(src)
  dir <- attr(obj, "raw_dir")
  for (url in src){
    download.file(url, destfile = paste0(dir, "/", basename(url)), method = 'curl')
  }
}
