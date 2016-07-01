#' etl_transform
#' @rdname etl_extract.etl_citibike
#' @method etl_transform etl_citibike
#' @importFrom utils unzip
#' @inheritParams etl::etl_extract
#' @details This function unzips NYC CitiBike data for years and months specified. 
#' @export
#' @examples 
#' \dontrun{
#' bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
#' bikes %>%
#'   etl_extract() %>%
#'   etl_transform()
#' }
etl_transform.etl_citibike <- function(obj, years = 2015, months = 1:12, ...) {
  
  dir <- attr(obj, "raw_dir")
  src <- list.files(dir, full.names = TRUE)
  files <- basename(src)
  
  year_month <- valid_year_month(years, months) %>%
    mutate(month = ifelse(month<10, paste0("0",month), month))%>%
    mutate(year_month = paste0(year, month)) %>%
    mutate(zip_files = paste0(year_month, "-citibike-tripdata.zip")) %>%
    mutate(zip = ifelse(zip_files %in% files, zip_files, ""))
  zip <- year_month$zip
  new_dir <- attr(obj, "load_dir")
  for (i in zip) {utils::unzip(i, exdir = new_dir)}
  invisible(obj)
}
