#' @rdname etl_extract.etl_citibike
#' @importFrom DBI dbWriteTable
#' @method etl_load etl_citibike 
#' @export
#' 
#' @examples
#' 
#' bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
#' bikes %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_load()
#' 
#' #' # check the results
#' \dontrun{
#' flights.db <- tbl(db, "flights")
#' flights.db %>%
#'   group_by(year, origin) %>%
#'   summarise(N = n(), min.month = min(month), max.month = max(month)) %>%
#'   arrange(desc(N))
#' }


etl_load.etl_citibike <- function(obj, year = 2015, month = 1, ...) {
  dir <- attr(obj, "load_dir")
  src <- sprintf(paste0(dir, "/%04d%02d-citibike-tripdata.csv"), year, month)
  # write the table to the DB
  message("Writing flight data to the database...")
  DBI::dbWriteTable(obj$con, "trips", src, append = TRUE, ...)
}


