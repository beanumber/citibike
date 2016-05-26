#' @rdname etl_extract.etl_citibike
#' @method etl_transform etl_citibike
#' @export

etl_transform.etl_citibike <- function(obj, ...) {
  dir <- attr(obj, "raw_dir")
  src <- list.files(dir, full.names = TRUE)
  new_dir <- attr(obj, "load_dir")
  for (i in src){
    unzip(i, exdir = new_dir)}
  invisible(obj)
}
