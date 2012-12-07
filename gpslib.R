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


#
# Fetches precipitation data for a given day and area (through the cowid)
#

fetchprecip=function(cowid,date){
 sql=paste("select distinct metobs.datetime::date as date, metobs.datetime::time as time,metstationid,\"RR_24\",\"RR_12\"
  from metobs,metstation,gpspoint
  where cowid=",cowid,"and gpspoint.datetime::date='",date,"' and
  gpspoint.lokalitet=metstation.lokalitet and metstation.id=metstationid
  and (metobs.datetime::date='",date,"' or metobs.datetime::date-1='",date,"') and  not (\"RR_24\" is null and \"RR_12\" is null) order by metstationid,date,time")

 rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
 }


precip=function(precip,date){
  pre=precip[precip$date==date & precip$time=='22:00:00',5 ]
  if(length(pre)>0)
  return(mean(pre))
  # Trigger an error here. Use some of the info from 24h precipitation to estimate
}

#
# Fetches observations for one animal for one day
#

fetchobs=function(cowid,date){
  sql=paste("select timestamp+'-2:00' as timestamp, obstype,0 as n from observation
where timestamp::date='",date,"'and cowid=",cowid)
  # Quick and dirty time zone correction - all other data points are in UTC
  # The n column (always 0) is just added to simplify plotting
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  data$obstype=as.factor(data$obstype)
  return(data)	
}

#
# Calculates travelled distance and movement over the last <delta> timesteps.
# 
#

calcdist=function(data,delta,date,cowid){
  # Reuses dists5s if already defined, calculates if needed
  if(!(exists("dists5s",data))){
    data$dists5s=distance(data,1)
  }
  dists=distance(data,delta)
  trav=travel(data$dists5s,delta)
  dcol=paste("dists",delta/12,"min",sep='')
  tcol=paste("trav",delta/12,"min",sep='')
  data[,dcol]=dists
  data[,tcol]=trav
  rcol=paste("rel",delta/12,"min",sep='')
  # ratio between travel distance and displacement 
  rcol=paste("ratio",delta/12,"min",sep='')
  data[,rcol]=data[,tcol]/data[,dcol]
  return(data)
}

#
# Plots the ratios for a given timestep
#


#
# Plots movement and displacement for a given data set.
#
distplot=function(set,delta,obs=c()){
  coltime=paste(delta/12,'min',sep='')
  dcol=paste('dists',coltime,sep='')
  tcol=paste('trav',coltime,sep='')
# TODO: Check if columns exist. Error message or recalc if not.
  ymax=max(set[tcol],na.rm=TRUE)
  prec=precip(fetchprecip(cowid,date),date)
  main=paste(max(set$date),"- cow",max(set$cowid),'-',prec,"mm -",delta/12,'min') 
  plot(set$datetime,set[,dcol],col="1",type='l',xlab='',ylab="meters",main=main,ylim=c(0,ymax))
  lines(set$datetime,set[,tcol],col="2")
  if(length(obs)>0){
    ot=levels(obs$obstype)
    points(obs$timestamp,obs$n,col=obs$obstype)
    legend(min(set$datetime),y=ymax,c('movement','displacement',ot),
           lty=c('solid','solid',NA,NA,NA),col=c(2,1,1:length(ot)),
           pch=c(NA,NA,rep(1,length(ot))))
  }else{
    legend(min(data$datetime),y=ymax,c('movement','displacement'),lty=c('solid','solid'),col=c(2,1))
  }
}


#
# Fetches a dataset for a given cow and date
#
fetchdata=function(cowid,date){
  sql=paste('select * from gps_coord where cowid=',cowid," and date='",date,"'",sep='')
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}


db2plot=function(cowid,date,deltamin,data=c()){
  delta=deltamin*12
  if(length(data)==0)
    data=fetchdata(cowid,date)
  plot(data$x,data$y)
  data=calcdist(data,delta,date,cowid)
  obs=fetchobs(cowid,date)
  distplot(data,delta,obs)
  invisible(data)
}

logdays=function(){
  sql="select * from cowid_date_location where not cowid is null order by date,lokalitet";
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}  
