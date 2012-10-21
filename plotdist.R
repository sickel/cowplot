#
# Calculates the distance from the location <delta> logsteps ago
# Input: data frame with raw metric data (e.g. utm coordinates) in x and y
#

distance=function(data,delta){
  dists=c()
  l=length(data$x)
  for (i in (delta+1:l)){
    dists[i]=sqrt((data$x[i]-data$x[i-delta])^2+(data$y[i]-data$y[i-delta])^2)
  }
  dists=dists[1:l] # Throws away some datapoints that are added to the end
  return(dists)
}


#
# Calculates the distance travelled the last <delta> logsteps 
# Input: length travelled for each logstep.
#
travel=function(dists,delta){
  l=length(dists)
  trav=c()  
  for (i in (delta+1:l)){
      trav[i]=sum(dists[(i-delta):i])
  }
  trav=trav[1:l] # Throws away some datapoints that are added to the end
  return(trav)
}


fetchprecip=function(cowid,date){
#  sql=paste("select distinct lokalitet from gpspoint where cowid=",cowid," and datetime::date='",date,"'")

 sql=paste("select distinct metobs.datetime::date as date, metobs.datetime::time as time,metstationid,\"RR_24\",\"RR_12\"
  from metobs,metstation,gpspoint
  where cowid=",cowid,"and gpspoint.datetime::date='",date,"' and
  gpspoint.lokalitet=metstation.lokalitet and metstation.id=metstationid
  and (metobs.datetime::date='",date,"' or metobs.datetime::date-1='",date,"') and  not (\"RR_24\" is null and \"RR_12\" is null) order by metstationid,date,time")

 rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
 }

  
  #select "RR_24","RR_12"
  #from metobs,metstation,gpspoint
  #where cowid=286 and gpspoint.datetime::date='2008-08-11' and gpspoint.lokalitet=metstation.lokalitet and metstation.id=metstationid and metobs.datetime::date='2008-08-11' 
#}

distplot=function(data,delta,date,cowid){
  dists5s=distance(data,1)
  dists=distance(data,delta)
  main=paste(date,"- cow",cowid)
  plot(data$datetime,dists,col="1",type='l',xlab='',ylab="meters",main=main)
  trav=travel(dists5s,delta)
  lines(data$datetime,trav,col="2")
  data[,paste("dists",delta/12,"min",sep='')]=dists
  data[,paste("trav",delta/12,"min",sep='')]=trav
  invisible(data)
}

fetchdata=function(cowid,date){
  sql=paste('select * from gps_coord where cowid=',cowid," and date='",date,"'",sep='')
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}
