library(DBI)
library(RPostgreSQL)
drv=dbDriver("PostgreSQL")
con=dbConnect(drv,"postgres","postgres","localhost","beitedata")


cowid=286
date='2008-08-11'

sql=paste('select * from gps_coord where cowid=',cowid," and date='",date,"'",sep='')
rs=dbSendQuery(con,statement=sql)
data=fetch(rs,n=-1)


# plot(data$datetime,data$meters_obs,type='l')
plot(data$x,data$y)

dists=c(NA)
for (i in (2:length(data$x))){
    dist[i]=sqrt((data$x[i]-data$x[i-1])^2+(data$y[i]-data$y[i-1])^2)
}
    