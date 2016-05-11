#' @rdname etl_extract.etl_citibike
#' @export

etl_transform.etl_citibike <- function(obj, year = 2015, month = 1, ...) {
  #  dir <- attr(obj, "raw_dir")
  dir = "~/dumps/citibike/raw/"
  src <- sprintf(paste0(dir, "%04d%02d-citibike-tripdata.zip"), year, month)
  #  new_dir <- attr(obj, "load_dir")
  new_dir <- gsub("raw", "load", dir)
  unzip(src, exdir = new_dir)
}