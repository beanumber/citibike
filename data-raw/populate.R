# Load data
# 

library(citibike)

db <- src_mysql_cnf(dbname = "citibike")
bikes <- etl("citibike", db = db, dir = "~/dumps/citibike")

bikes %>%
  etl_init()

bikes %>%
  etl_extract(year = 2013:2018, months = 1:12) %>%
  etl_transform(year = 2017, months = 1:12) %>%
  etl_load(years = 2017, months = 1:12)

