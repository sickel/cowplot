library(DBI)
library(RPostgreSQL)
source('gpslib.R')
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"postgres","postgres","localhost","beitedata")


rgb(81/255,107/255,152/255)->col1
col2="#4eadce"
col3="#a7b808"
col4="#4f3d25"
col5="#9a192b"
bioforskpalette=c(col1,col2,col3,col4,col5)


# palette(rainbow(30))
cowid=286
date='2008-08-11'
tstep=15

data=db2plot(cowid,date,tstep)
