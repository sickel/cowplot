
library(DBI)
library(RPostgreSQL)
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"postgres","postgres","localhost","beitedata")


cowid=286
date='2008-08-11'

data=fetchdata(cowid,date)
# plot(data$datetime,data$meters_obs,type='l')
plot(data$x,data$y)

delta=12*5
data=distplot(data,delta,date,cowid)

