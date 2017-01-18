library(rgdal)
library(DBI)
library(RPostgreSQL)
source('gpslib.R')
options("width"=160)

source('userpassword.R')

connect2db=function(user,password)
{
  drv=dbDriver("PostgreSQL")
  con=dbConnect(drv,user,password,"localhost","beiteadferd2016")
  return(con)
}
con=connect2db(user,password)
rm(user)
rm(password)

bioforskpalette=c( "#516B98","#4f3d25","#9a192b","#4eadce","#a7b808");

par(ps=15,lwd=1.5)
if(!(exists('cowid'))){
  cowid=286
}
if(!(exists('fielddate'))){
  fielddate='2008-08-11'
}
if(!(exists('tstep'))){
  tstep=15
}

if(!(exists('data'))){
  data=db2plot(cowid,date,tstep)
}

disttype=c('trav'='movement','dists'='displacement')
options(width=200)
