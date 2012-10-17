library(DBI)
library(RPostgreSQL)
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"postgres","postgres","localhost","beitedata")


cowid=286
date='2008-08-11'

sql=paste('select * from movement where cowid=',cowid," and datetime::date='",date,"'",sep='')
rs=dbSendQuery(con,statement=sql)
data=fetch(rs,n=-1)


 plot(data$datetime,data$meters_obs,type='l')
