library(DBI)
library(RPostgreSQL)
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"morten","hobidor","localhost","beitedata")
