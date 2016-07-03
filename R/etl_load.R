#' etl_load
#' @rdname etl_extract.etl_citibike
#' @importFrom DBI dbWriteTable
#' @method etl_load etl_citibike
#' @inheritParams etl::etl_extract
#' @details This function loads NYC CitiBike data into a SQLite database for years and months specified.
#' @export
#' @examples
#' \dontrun{
#' bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
#' bikes %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_load()
#' }


etl_load.etl_citibike <- function(obj, schema = FALSE, years = 2013, months = 7, ...) {
  dir <- attr(obj, "load_dir")
  src <- list.files(dir, full.names = TRUE)
  files <- basename(src)
  
  #valid years and month; create corresponding path
  year_month <- valid_year_month(years, months) %>%
    mutate(month = ifelse(month< 10, paste0("0",month), month))%>%
    mutate_(year_month = ~paste0(year, month)) %>%
    mutate_(zip_files = ~paste0(year_month, "-citibike-tripdata.csv")) %>%
    filter_(~zip_files %in% files) %>%
    mutate_(path = ~paste0(dir,"/",zip_files))
  path <- year_month$path
  
  #Write to Table
  message("Writing flight data to the database...")
  for (i in path){
    DBI::dbWriteTable(obj$con, "trips", i, 
                      append = TRUE, overwrite = FALSE, ...)
    }
  invisible(obj)
}


