library(DBI)
library(RPostgreSQL)
source('gpslib.R')
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"postgres","postgres","localhost","beitedata")

palette(rainbow(30))
cowid=286
date='2008-08-11'
tstep=15

data=db2plot(cowid,date,tstep)
