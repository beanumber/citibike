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

  valid <- valid_year_month(years, months) %>%
    mutate_(year_month = ~format(month_begin, "%Y%m"))
  
  # list the files under the load folder
  src <- data_frame(
    path = list.files(attr(obj, "load_dir"), pattern = "\\.csv", full.names = TRUE)
  ) %>%
    mutate_(month_begin = ~lubridate::date(lubridate::parse_date_time(path, orders = "%Y%m"))) %>%
    inner_join(valid, by = "month_begin") %>%
    mutate_(table = ~ifelse(grepl("tripdata", path), "trips", "station_months"))
  
  # write to Table
  message("Writing bike data to the database...")
  mapply(value = src$path, name = src$table, FUN = DBI::dbWriteTable, 
         MoreArgs = list(conn = obj$con, 
                         append = TRUE, overwrite = FALSE, ... = ...))
  
  invisible(obj)
}


