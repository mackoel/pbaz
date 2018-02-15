library('RPostgreSQL')
drv_generic <- dbDriver("PostgreSQL")
conn <- dbConnect(drv=drv_generic, "localhost", port = 5432, user = "pbaz", password = "pbaz", dbname = "pbaz")
