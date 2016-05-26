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
#'  library(RSQLite)
#'  my_db<- src_sqlite( path= bikes$path, create= TRUE)
#'  my_tbl<- tbl(my_db,"trips")
#'  head(my_tbl)
#' 
#' #' # check the results
#' \dontrun{
#' flights.db <- tbl(db, "flights")
#' flights.db %>%
#'   group_by(year, origin) %>%
#'   summarise(N = n(), min.month = min(month), max.month = max(month)) %>%
#'   arrange(desc(N))
#' }


etl_load.etl_citibike <- function(obj) {
  dir <- attr(obj, "load_dir")
  src <- list.files(dir, full.names = TRUE)
  # write the table to the DB
  message("Writing flight data to the database...")
  sapply()
  for (i in src){
    DBI::dbWriteTable(obj$con, "trips", i, append = TRUE, overwrite= FALSE, ...)
  }
  invisible(obj)
}


