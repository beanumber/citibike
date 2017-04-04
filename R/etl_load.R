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


etl_load.etl_citibike <- function(obj, years = 2013, months = 7, ...) {
  # connect to the load folder
  dir <- attr(obj, "load_dir")
  # list the files
  src <- list.files(dir, full.names = TRUE)
  # get the base name of the files
  files <- basename(src)
  
  # valid years and month; create corresponding path
  year_month <- valid_year_month(years, months) %>%
    mutate_(month = ~ifelse(month< 10, paste0("0",month), month))%>%
    mutate_(year_month = ~paste0(year, month)) %>%
    mutate_(zip_files = ~paste0(year_month, "-citibike-tripdata.csv")) %>%
    filter_(~zip_files %in% files) %>%
    mutate_(path = ~paste0(dir, "/", zip_files))
  path <- year_month$path
  
  # write to Table
  message("Writing bike data to the database...")
  lapply(path, function(x) DBI::dbWriteTable(obj$con, "trips", x, append = TRUE, 
                                             overwrite = FALSE, ...))
  
  invisible(obj)
}


