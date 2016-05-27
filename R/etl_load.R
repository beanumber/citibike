#' @rdname etl_extract.etl_citibike
#' @importFrom DBI dbWriteTable
#' @method etl_load etl_citibike
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' bikes <- etl("citibike", dir = "~/Desktop/citibike_data")
#' bikes %>%
#'   etl_extract() %>%
#'   etl_transform() %>%
#'   etl_load()
#' 
#' trips <- tbl(bikes,"trips")
#' head(trips)
#' }
#' #' # check the results
#' \dontrun{
#' trips %>%
#'   group_by(year, origin) %>%
#'   summarise(N = n(), min.month = min(month), max.month = max(month)) %>%
#'   arrange(desc(N))
#' }


etl_load.etl_citibike <- function(obj, schema = FALSE, years = 2015, months = 1:12, ...) {
  dir <- attr(obj, "load_dir")
  src <- list.files(dir, full.names = TRUE)
  # write the table to the DB
  
  message("Writing flight data to the database...")
  for (i in src){
    DBI::dbWriteTable(obj$con, "trips", i, append = TRUE, overwrite = FALSE, ...)
#                      field.types = c(tripduration = "int", starttime = "text"))
  }
  invisible(obj)
}


