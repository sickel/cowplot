library(DBI)
library(RPostgreSQL)
source('gpslib.R')
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"postgres","postgres","localhost","beitedata")


cowid=286
date='2008-08-11'

data=db2plot(cowid,date,5)
