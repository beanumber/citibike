#' @rdname etl_extract.etl_citibike
#' @export
#' #' @examples 
#'
#' bikes %>%
#'   etl_transform()

etl_transform.etl_citibike <- function(obj, year = 2015, month = 1, ...) {
  dir <- attr(obj, "raw_dir")
  src <- sprintf(paste0(dir, "/%04d%02d-citibike-tripdata.zip"), year, month)
  new_dir <- attr(obj, "load_dir")
  unzip(src, exdir = new_dir)
}

