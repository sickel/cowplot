library(rgdal)
library(DBI)
library(RPostgreSQL)
source('gpslib.R')
options("width"=160)

connect2db=function()
{
  drv=dbDriver("PostgreSQL")
  con=dbConnect(drv,"postgres","postgres","localhost","beitedata")
  return(con)
}
con=connect2db()

bioforskpalette=c( "#516B98","#4f3d25","#9a192b","#4eadce","#a7b808");
par(ps=15,lwd=1.5)
if(!(exists('cowid'))){
  cowid=286
}
if(!(exists('date'))){
  date='2008-08-11'
}
if(!(exists('tstep'))){
  tstep=15
}

if(!(exists('data'))){
  data=db2plot(cowid,date,tstep)
}
