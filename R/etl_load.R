#' @rdname etl_extract.etl_citibike
#' @importFrom DBI dbWriteTable
#' @method etl_load etl_citibike 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' library(dplyr)
#' if (require(RMySQL)) {
#' # must have pre-existing database "airlines"
#' db <- src_mysql(user = "mysql", password = "mysql", dbname = "airlines")
#' }
#' library(etl)
#' etl_citibike <- etl_connect("airlines", db, dir = "~/dumps/airlines")
#' # get one entire year of data
#' etl_citibike <- etl_citibike %>%
#'   etl_extract(year = 2013) %>%
#'   etl_transform(month = 6) %>%
#'   etl_load(year = 2013, month = 6)
#' }
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
  #  dir <- attr(obj, "load_dir")
  dir <- gsub("raw", "load", dir)
  src <- sprintf(paste0(dir, "%04d%02d-citibike-tripdata.csv"), year, month)
  # write the table to the DB
  message("Writing flight data to the database...")
  DBI::dbWriteTable(obj$con, "trips", src, append = TRUE, ...)
}


