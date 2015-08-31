#' ETL functionality for the airlines database
#' @description Perform ETL operations on the airlines database.
#' 
#' @inheritParams etl::etl_init
#' 
#' @details This will initialize and populate the database.
#' 
#' @method etl_init etl_citibike
#' @import etl
#' @import dplyr
#' @importFrom DBI dbWriteTable
#' @export
#' @seealso \code{\link[etl]{etl_init}}
#' 
#' @examples
#' 
#' require(dplyr)
#' \dontrun{
#' if (require(RPostgreSQL)) {
#' # must have pre-existing database "airlines"
#' db <- src_postgres(host = "localhost", user = "postgres", dbname = "airlines")
#' }
#' airlines <- etl_connect("airlines", db)
#' etl_init(airlines)
#' 
#' if (require(RMySQL)) {
#' # must have pre-existing database "airlines"
#' db <- src_mysql(user = "mysql", password = "mysql", dbname = "airlines")
#' }
#' airlines <- etl_connect("airlines", db)
#' etl_init(airlines)
#' 
#' etl_update(airlines, year = 2013, month = 6)
#' }

etl_init.etl_citibike <- function(obj, ...) {
  if (class(obj$con) == "MySQLConnection") {
    sql <- system.file("sql", "init.mysql", package = "airlines")
  } else if (class(obj$con) == "PostgreSQLConnection") {
    sql <- system.file("sql", "init.psql", package = "airlines")
  } else {
    sql <- system.file("sql", "init.sql", package = "airlines")
  }
  obj$init <- dbRunScript(obj$con, sql)
  return(obj)
}
