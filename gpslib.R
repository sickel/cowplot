# To fetch data and run model:
# data=fetchgpsobs(cowid,date) # or fetchdata if observations are not needed
# data=calcdist(data,5*12) # (5 min interval)
# data=adjustobservations(data,5) - trenges ikke mer, justerer i calcdist




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

# Collects distances for all days
alldists=function(nth=12){
  sets=logdays()
  dist=c()
  n=length(sets[,1])
  for(i in c(1:n)){
    cowid=sets[i,1]
    day=sets[i,2]
    cat("\b\b\b\b\b\b\b\b\b\b\b\b",i,":",n)
    dist=c(dist,distprday(cowid,day,nth))
  }
  sets$dist=dist
  cat("\n")
  return(sets)
}


#
# Plots histogram of distances
# data=
disthist=function(data=alldists(),location="All areas",year='all years'){
  filename=paste("Dists-",location,year,'.pdf',sep='')
  #pdf(filename)
  if(location != "All areas"){
    data=data[data$lokalitet==location,]
  }
  if(year != 'all years'){
    fromdate=paste(year,'-01-01',sep='')
    todate=paste(year+1,'-01-01',sep='')
    data=data[data$date>fromdate & data$date<todate,]
  }
  main=paste(location,year,sep=" -")
  dist=data$dist
  hist(dist,breaks=20,main=main,xlab="Meters pr day",xlim=c(2000,10000))
  #dev.off()
}

#
# calculate the distance travelled for one day at a (default) one minute
# resolution
#

distprday=function(cowid,date,nth=12){
  data=fetchdata(cowid,date)
  return(distforday(data,nth))
}

distforday=function(data,nth=12){
    data=data[c(TRUE,rep(FALSE,nth-1)),] # Once pr minute;
    dist=sum(distance(data,1),na.rm=TRUE)
    return(dist)
  }



#
# Utility function to see how the calculated distance varies depending on
# resolution
#

analysedistprday=function(cowid,date,maxn){
  dists=c()
  for(i in (1:maxn)){
    cat("\b\b\b\b\b",i)
    dists=c(dists,distprday(cowid,date,i))
  }
  cat("\n")
  return(dists)
}
  


# Fetches the background map

fetchmap=function(sted){
  if (sted=="Geilo")
    layer="geilo_kategorisert"
    # layer = "geilo_classified"
  else
    # layer="valdresclassified"
    layer ="valdres_classified"
  map=readOGR("PG:dbname=beitedata user=postgres password=postgres",layer=layer)
  return(map)
}

#
# To collect all observations and their observed distances
#

observationdistances=function(deltamin){
  delta=deltamin*12 # number of 5 sec steps
  sql="select distinct cowid, timestamp::date from observation where cowid > 0"
  # finds all cow / date cmbinations in observations
 rs=dbSendQuery(con,statement=sql)
  sets=fetch(rs,n=-1)
  for(i in c(1:length(sets[,1]))){
  # for(i in c(1:2)){
    cat(i,"\n")
    cowid=sets[i,1]
    date=sets[i,2]
    data=fetchdata(cowid,date)
    if(length(data)>0){
      data=fetchgpsobs(cowid,date)
      data=calcdist(data,delta,date,cowid)
      data=data[!(is.na(data$obstype)),]
      if(!exists('obsspeed')){
        obsspeed=data
      }else{
        obsspeed=rbind(obsspeed,data)
      }
    }
  }
  # print("OK so far")
  obsspeed$obstype=as.factor(obsspeed$obstype)
  obsspeed$lokalitet=as.factor(obsspeed$lokalitet)
  return(obsspeed)
}




# To be run on the output of the former

analyseobsspeed=function(od,deltamin,locality="All",type="Displacement",maxvalue=1000){
  modes=levels(od$obstype)
  binsize=25
  xlim=c(0,maxvalue)
  if(type=="Displacement")fieldname=paste("dists",deltamin,"min",sep="")
  if(type=="Ratio"){
    fieldname=paste("ratio",deltamin,"min",sep="")
    max=1
    binsize=0.05
    xlim=c(0,1)
  }
  if(locality!="All"){
    od=od[od$lokalitet==locality,]
  }
  if(type=="Movement")fieldname=paste("trav",deltamin,"min",sep="")
  print(type)
  max=max(od[,fieldname],na.rm=TRUE)
  # max=max(od[,fieldname],na.rm=TRUE)
  dev.new()
  nf=layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE), respect=TRUE)
  for(mod in modes){  
    print(mod)    
    hist(od[,fieldname][od$obstype==mod],breaks=c(0:ceiling(max/binsize))*binsize,main=paste(type,":",locality,mod),xlim=xlim,xlab=paste(type,deltamin,'minutes'))
    filename=paste("hist",locality,sub('/','-',type),deltamin,sep='_')
  }
  dev.copy2pdf(file=(paste(filename,"pdf",sep='.')))
  dev.copy(png,paste(filename,"png",sep='.'))
  dev.off()
}
         
#
# Makes and saves all histogram from an observation data set-
#


plotallhist=function(od,min,maxvalue){
  for (lok in c("All","Valdres","Geilo")){
    for(type in c("Displacement","Movement","Ratio")){
      analyseobsspeed(od,maxvalue=maxvalue,type=type,locality=lok,deltamin=min)
      dev.off()
    }
  }
}



# Makes and saves all distance-plots

alldistplots=function(){
  days=logdays()
  for(i in c(1:length(days[,1]))){
    cowid=days[i,1]
    date=days[i,2]
    prefix=days[i,3]
    data=fetchdata(cowid,date)
    if(length(data)>0){
      prefix=paste("Dist",prefix,date,cowid,sep='_')
      data=calcdist(data,delta,date,cowid)
      obs=fetchobs(cowid,date)
      pdf(file=paste(prefix,'pdf',sep='.'))
      distplot(data,delta,obs)
      dev.off()
    }
  }
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
  sql=paste("select distinct metobs.datetime::date as date,
     metobs.datetime::time as time,metstationid,\"RR_24\",\"RR_12\"
  from metobs,metstation,gpspoint
  where cowid=",cowid,"and gpspoint.datetime::date='",date,"' and
  gpspoint.lokalitet=metstation.lokalitet and metstation.id=metstationid
  and (metobs.datetime::date='",date,"' or metobs.datetime::date-1='",date,"')
    and  not (\"RR_24\" is null and \"RR_12\" is null)
  order by metstationid,date,time")
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
# Fetches a dataset for a given cow and date
#
fetchdata=function(cowid,date){
  sql=paste('select id,datetime,date,trim(lokalitet) as lokalitet, cowid, x,y, case when extract(second from datetime)<5 and extract(minute from datetime) = 0 then extract(hour from datetime) end as marker from gps_coord where cowid=',cowid," and date='",date,"' order by datetime",sep='')
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}


#
# Fetches gpsdata and observation - for model creation and testing
#

fetchgpsobs=function(cowid,date){
  sql=paste('select * from gps_observation where cowid=',cowid," and date='",
    date,"'",sep='')
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  data$observation=as.factor(data$observation)
  data$obstype=as.factor(data$obstype)
  return(data)	
}

  


#
# Fetches observations for one animal for one day
#

fetchobs=function(cowid=NULL,date=NULL,lok=NULL){
  sql="select timestamp+'-2:00' as timestamp,cowid,obstype,0 as n,x,y from observationxy "
  # Quick and dirty time zone correction - all other data points are in UTC
  # The n column (always 0) is just added to simplify plotting
  fields=c()
  if(!(is.null(cowid))) fields=c(fields,paste("cowid",cowid,sep='='))
  if(!(is.null(date))) fields=c(fields,
                         paste("timestamp::date='",date,"'",sep=''))
  if(!(is.null(lok))) fields=c(fields,paste("lokalitet='",lok,"'",sep=''))
  if(length(fields)>0){
    where=paste(fields,collapse=" and ")
    sql=paste(sql,where,sep=" where ")
  }
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  data$obstype=as.factor(data$obstype)
  return(data)	
}

#
# Adjust timing
#

adjusttiming=function(dataset,delta){
  lgt=length(dataset)
  dataset=dataset[(delta/2):lgt]
  length(dataset)=lgt
  return(dataset)
}

#
# Calculates travelled distance and movement over the last <delta> timesteps.
# 
#

calcdist=function(data,delta,date='',cowid=''){
  # Reuses dists5s if already defined, calculates if needed
  if(!(exists("dists5s",data))){
    data$dists5s=distance(data,1)
  }
  dists=distance(data,delta)
  dists=adjusttiming(dists,delta)
  trav=travel(data$dists5s,delta)
  trav=adjusttiming(trav,delta)
  # Returns values in meters per minute.
  dcol=paste("dists",delta/12,"min",sep='')
  tcol=paste("trav",delta/12,"min",sep='')
  data[,dcol]=dists
  data[,tcol]=trav
  # ratio between travel distance and displacement 
  rcol=paste("ratio",delta/12,"min",sep='')
  data[,rcol]=data[,dcol]/data[,tcol]
  # NaN values are 1: (no movement at all)
  data[is.nan(data[,rcol]),rcol]=1
  return(data)
}

#
# Setting classes on all points
#

setclass=function(data){
  restspeed=25
  walkspeed=300
  class=c()
  for (i in (1:length(data$x))){
    if(!(is.na(data$dists15min[i]))){
      if(data$dists15min[i]<restspeed){
        class[i]='resting'
      }else{
        if(data$dists15min[i]>walkspeed){
          class[i]='walking'
        }else{
          class[i]='grazing'
        }
      }
    }
  }
  return(as.factor(class))
}

#
# Plots the ratios for a given timestep
#

plotr=function(data,min,f=5){
  rcol=paste("ratio",min,"min",sep='')
  lines(data$datetime,data[,rcol]*f,col=8,lty=2)
}

#
# Plots movement and displacement for a given data set.
#
distplot=function(set,delta,obs=c()){
  legendoffset=3.5
  coltime=paste(delta/12,'min',sep='')
  dcol=paste('dists',coltime,sep='')
  tcol=paste('trav',coltime,sep='')
  # TODO: Check if columns exist. Error message or recalc if not.
  ymax=max(set[tcol],na.rm=TRUE)
  prec=precip(fetchprecip(cowid,date),date)
  main=paste(max(set$date),"- cow",max(set$cowid),'-',prec,"mm -",delta/12,
    'min') 
  plot(set$datetime,set[,dcol],col="1",type='l',xlab='UTC-time',ylab="meters",
       main=main,ylim=c(0,ymax))
  lines(set$datetime,set[,tcol],col="2")
  # Calculated ratio - normalized to scale on graph
  lines(set$datetime,set[,dcol]/set[,tcol]*ymax,col="3")
  if(length(obs)>0){
    ot=levels(obs$obstype)
    points(obs$timestamp,obs$n-(ymax/50),col=as.integer(obs$obstype)+2,pch=7)
    legend(max(set$datetime)-(3600*legendoffset),y=ymax,
           c('movement','displacement',ot),
           lty=c('solid','solid',NA,NA,NA,NA),col=c(2,1,3:(length(ot)+2)), pch=c(NA,NA,rep(7,length(ot))))
  }else{
    legend(max(data$datetime)-3600*legendoffset,y=ymax,
           c('movement','displacement'),lty=c('solid','solid'),col=c(2,1))
  }
}

storevalidation=function(xt,cowid,date,modelid){
  sql=paste("select id from modelvalidation where cowid=",cowid,"and date=",sqlquote(date),"and modelid=",modelid,sep='')
  rs=dbSendQuery(con,statement=sql)
  ret=fetch(rs,n=-1)
  if(length(ret)>0){
    cat(paste('rerun',cowid,date,modelid))
    return('rerun')
  }
  g="grazing"
  r="resting"
  w="walking"
  xt[is.na(xt)]<-0
  sql=paste("insert into modelvalidation (modelid,cowid,date,g2g,r2r,w2w,g2r,g2w,r2g,r2w,w2g,w2r) values(",paste(modelid,cowid,sqlquote(date),xt[g,g],xt[r,r],xt[w,w],xt[g,r],xt[g,w],xt[r,g],xt[r,w],xt[w,g],xt[w,r],sep=','),")",sep='')
  rs=dbSendQuery(con,statement=sql)
  return('ok')
}


storeresult=function(data,modelid){
  sql="insert into modelresult (modelrunid,gpspointid,result) values "
  insert=''
  n=length(data$id)
  for (i in c(1:n)){
    if (!is.na(data$model[i])){
      row=paste('(',modelid,',',data$id[i],',',sqlquote(as.character(data$model[i])),')')
      if(insert==''){
        insert=row
      }else{
        insert=paste(insert,row,sep=',')
      }
    }
  }
  sql=paste(sql,insert)
  rs=dbSendQuery(con,statement=sql)
}
        
#
# Runs a model and plots for all main animals
#
mainmodel=function(lok='',rtrav=2, wrat=0.6,wtrav=10,mins=5,rlength=310,wlength=50,rrat=1,mtyp=c('d','d'),onlymain=TRUE,doplot=FALSE){
  obsset=listobsdays(main=onlymain,lok=lok)
 # modeloutput=data.frame()  
  modelid=storemodel(rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp)
  cat(modelid,"\n========")
  cowids=c()
  dates=c()
  
  for(i in 1:length(obsset$date)){
    cowid=obsset[i,2]
    date=obsset[i,1]
    loka=obsset[i,3]
    filename=paste(loka,date,cowid,'png',sep='.')
    data=fetchgpsobs(cowid,date)
    date=format(as.Date(date,origin="1970-01-01"))
    if(length(data)>2){
      cat("\n",filename," ")
      if(doplot) png(filename)
      # cat("OK")
      data=runandplotmodel(data,rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp,doplot)
      # cat("OK")
      if(doplot) dev.off()
      xt=analysesinglemodel(data,calcratio=FALSE)      
      # storeresult(data,modelid)
      rerun=storevalidation(xt,cowid,date,modelid)
      if(rerun=='rerun' && norerun) return()
      tothit=sum(xt[1,1],xt[2,2],xt[3,3],na.rm=TRUE)
      totobs=sum(xt,na.rm=TRUE)
      totmiss=totobs-tothit
      # temp=list(loka,date,cowid,rrat,rtrav,wrat,wtrav,mins,rlength,wlength,totobs,tothit,totmiss,xt))
      temp=c(rrat,rtrav,wrat,wtrav,mins,rlength,wlength,totobs,tothit,totmiss,xt)
      
#      print(temp)
      cowids=c(cowids,cowid)
      dates=c(dates,date)
      if(!exists('modeloutput')){
        modeloutput=temp
        #  modeloutput=as.data.frame(temp)
         #
      }else{
         modeloutput=rbind(modeloutput,temp)
      }
    }
  }
  cat("\n")
  rownames(modeloutput)=c(1:length(modeloutput[,1]))
  modeloutput=as.data.frame(modeloutput)
# "lok","date","cowid",
  colnames=c("rrat","rtrav","wrat","wtrav","mins","rlength","wlength","totobs","tothit","totmiss",
                        "g2g","r2g","w2g",
                        "g2r","r2r","w2r",
                        "g2w","r2w","w2w")
  colnames(modeloutput)=colnames
  modeloutput$cowid=cowids
  modeloutput$date=dates
  modeloutput$lok=loka
  cat("Modelid :",modelid,"\n")
  invisible(modeloutput)
}

sqlquote=function(str){
  return(paste("'",str,"'",sep=''))
}

storemodel=function(rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp){
   sql=paste("select id from modelrun where restspeed=",rtrav," and walkspeed=",wtrav,"and walkratio=",wrat,"and restratio=",rrat," and walkspan=",wlength,"and restspan=",rlength," and minutes=",mins," and walktyp=",sqlquote(mtyp[2]),"and resttyp=",sqlquote(mtyp[1]),sep=' ')
   rs=dbSendQuery(con,statement=sql)
   ret=fetch(rs,n=-1)
   if(length(ret)>0){
     return(ret[1,1])
   }else{
     sql=paste("insert into modelrun (restspeed,restratio,walkspeed,walkratio,walkspan,restspan,minutes,resttyp,walktyp) values(",paste(rtrav,rrat,wtrav,wrat,wlength,rlength,mins,sqlquote(mtyp[1]),sqlquote(mtyp[2]),sep=','),')',sep='');
     rs=dbSendQuery(con,statement=sql)
     sql="select currval('modelrun_id_seq')"
     rs=dbSendQuery(con,statement=sql)
     ret=fetch(rs,n=-1)
     return(ret$currval)
   }
 }
  


runandsavemodel=function(data,rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp=c('d','d')){
   data=calcdist(data,mins*12)
   modelid=storemodel(rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp)
   data=model2(data,  rtrav,wrat,wtrav,mins,rlength,wlength,mtyp,rrat)
   storeresult(data,modelid)
   invisible(data)
 }


runandsaveall=function(lok,rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp=c('d','d')){
  modelid=storemodel(rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp)
  cat(modelid,"\n==========\n")
  days=logdays(lok=lok)
  n=length(days$date)
  for(i in (1:n)){
    if(i<100)cat(' ')
    if(i<10)cat(' ')
    cat(i,':',n,' ',days$cowid[i],' ',format(as.Date(days$date[i],origin="1970-01-01")),"\n")
    data=fetchdata(days$cowid[i],format(as.Date(days$date[i],origin="1970-01-01")))
    if(length(data$id)>0){
      data=calcdist(data,mins*12)
      data=model2(data,  rtrav,wrat,wtrav,mins,rlength,wlength,mtyp,rrat)
      storeresult(data,modelid)
    }
  }
}


runandplotmodel=function(data,rtrav,wrat,wtrav,mins,rlength,wlength,rrat,mtyp=c('d','d'),doplot=TRUE){
   data=calcdist(data,mins*12)
   data=model2(data,rtrav,wrat,wtrav,mins,rlength,wlength,mtyp,rrat)
   if(doplot){
     plotobsmod(data,mins)
   }
   invisible(data)
 }

#
# Utility-function - fetches data and plots it
#
db2plot=function(cowid,date,deltamin,data=c()){
  delta=deltamin*12
  if(length(data)==0)
    data=fetchdata(cowid,date)
  plot(data$x,data$y)
  if(length(dev.list())>1){
    dev.next()
  }else{
    dev.new()
  }
  data=calcdist(data,delta,date,cowid)
  obs=fetchobs(cowid,date)
  distplot(data,delta,obs)
  invisible(data)
}

#
# Utility: Lists available data
#

logdays=function(cowid='',date='',lok=''){
  select=''
  if(cowid>''){
    select=paste(" and cowid=",cowid,sep='')
  }
  if(date>''){
    select=paste(select," and date='",date,"'",sep='')
  }
  if(lok>''){
    select=paste(select," and lokalitet='",lok,"'",sep='')
  }
  sql=paste("select * from cowid_date_location where not cowid is null",
    select," order by date,lokalitet",sep='');
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}  


#
# Makes distance plots for all animals in the herd for one day
#

herddist=function(date,dm){
  logs=logdays(date=date)
  herd=logs$cowid
  dev.new()
  def.par <- par(no.readonly = TRUE)
  nf=layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE), respect=TRUE)
  
  for(cow in herd){
    data=fetchdata(cow,date)
    data=calcdist(data,15*12,date,cow)
    obs=fetchobs(cow,date)
    distplot(data,15*12,obs)
    plotr(data,15)
  }
  par(def.par)
}

# makes an xymap of movement speeds
# Todo: Make this useable with the standard plotmap()

herdxy=function(date,dm,slow=10,fast=200,set='trav'){
  logs=logdays(date=date)
  herd=logs$cowid
  dev.new()
  def.par <- par(no.readonly = TRUE)
  nf=layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE), respect=TRUE)
  for(cow in herd){
    data=fetchdata(cow,date)
    data=calcdist(data,15*12,date,cow)
    obs=fetchobs(cow,date)
    plot(data$x,data$y)
    plotxyfilter(data,dm,lim=fast,set=set,col='wheat')
    plotxyfilter(data,dm,lim=slow,set=set,col='steelblue',gt=FALSE)
  }
  par(def.par)
}

#
# Utility function: Lists days for a location
#
locationdates=function(loc){
  dates=logdays()
  return(unique(dates[dates$lokalitet==lok,2]))
}


fetchmodanalyse=function(){
  o=observationdistances(5)
  o=testmodel(o)
  return(o)
}

fetchparams=function(modelrunid){
  sql=paste("select  id,restspeed,restratio,walkspeed,walkratio,walkspan,restspan,minutes,walktyp,resttyp,addtime from modelrun where id=",modelrunid)
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  rtrav<<-data[1,2]
  rrat<<-data[1,3]
  wtrav<<-data[1,4]
  wrat<<-data[1,5]
  wlength<<-data[1,6]
  rlength<<-data[1,7]
  mins<<-data[1,8]
  wtyp<<-data[1,9]
  rtyp<<-data[1,10]
  mtyp<<-c(rtyp,wtyp)
  return(data)
}

showparams=function(id=0){
  if(id>0) return(fetchparams(id))
  else   return(as.data.frame(list(id='',restspeed=rtrav,restratio=rrat,walkspeed=wtrav,walkratio=wrat,walkspan=wlength,restspan=rlength,minutes=mins,walktyp=mtyp[1],resttyp=mtyp[2])))
}
#
# Geilo: 5 min movement: > 25 m/5min : grazing
#                        < 25 m/5min : rest
# ratio5 >0.8 && movement5 > 75 m/5min
#
# Valdres: 
# ratio5 >0.8 && movement5 > 75 m/5min
# oldvaldres=function(o,rtrav=10,wrat=0.8,wtrav=80){
# geilomodel=function(o,rtrav=25,wrat=0.8,wtrav=100){
# valdresmodel=function(o,rtrav=10,wrat=0.7,wtrav=80,length=500){

#
# model2 is the model used finally
# 

model2=function(o,rtrav=1,wrat=0,wtrav=2,mins=5,rlength=180,wlength=50,dtyp=c('d','d'),rrat=1){
  # o : data frame holding the 
  # rtrav: speed discriminator for resting / grazing
  # wrat: min displacement / distance ratio for walking / grazing
  # wtrav: speed discriminator for grazing / walking
  # mins : minutes averaging time
  # rlength : minimum numbers of points for a valid resting period
  # wlength : minimum numbers of points for a valid walking period
  # dtype : which type of distance measurement to use for resting and walking speed (d: displacement, t: walked distance)
  # rrat: max displacement / distance ratio for resting / grazing
  tp=c('d'='dists','t'='trav')
  typ=tp[dtyp[1]]
  rdf=paste(typ,mins,"min",sep="") # resting distance field
  rf=paste("ratio",mins,"min",sep="") # ratiofield
  typ=tp[dtyp[2]]
  wdf=paste(typ,mins,"min",sep="") # walking distance field
  wtrav=wtrav*mins
  rtrav=rtrav*mins
  # model is a vector holding the model results
  # Sets model to resting if the speed is less than limit and the displacement/distance ratio is less than or equal to limit, else to grazing
  model=ifelse(((is.na(o[rf]) | o[rf]<=rrat) & (o[rdf]<rtrav) ),'resting','grazing')
  # Sets model to walking if speed is more than limit and the displacement/distance ratio is more than limit, else to grazing. 
  model=ifelse(( o[rf]> wrat & (o[wdf]>wtrav)) ,'walking',model)
  model=as.factor(model)
  o$model=model
  # Throw away too short concecutive walking or resting
  o=removeshort(o,rlength,wlength) 
  return(o)
}

removeshort=function(o,glength=500,wlength=50){
  # rle- runlengthencoding, gives out concecutive factors
  rle=rle(as.vector(o$model))
  rl=data.frame(val=rle$values,len=rle$lengths)
  rl$newval=rl$val
  rl$newval[rl$len<glength & rl$val=='resting']='grazing'
  rl$newval[rl$len<wlength & rl$val=='walking']='grazing'
  rle$values=rl$newval
  # Inverse.rle returns the original - or in this case manipulated, vector
  o$model=as.factor(inverse.rle(rle))
  return(o)
}



rleframe=function(data,natoblank=TRUE){
  if(natoblank){
    data=as.character(data)
    data[is.na(data)]='';
    data=as.factor(data)
  }
  data=as.vector(data)
  rl=rle(data)
  return(data.frame(rl$values,rl$lengths))
}


analysesinglemodel=function(o,lok="all",calcratio=TRUE){
# Easier to handle afterwards if the return matrix always is the same 
  if(lok!='all'){
    o=o[o$lokalitet==lok,]
  }
  xtreal=xtabs(~obstype+model,data=o)
  # adds in to make sure that all combinations exists
  acts=c('walking','grazing','resting')
  for(i in acts){
    for(j in acts){
      rw=o[1,]
      rw$datetime=NA
      rw$obstype=i
      rw$model=j
      o=rbind(o,rw)
    }
  }
  xt=xtabs(~obstype+model,data=o)
  xt=xt-1
  if(calcratio){
    xt=round(xt/rowSums(xt),3)*100
  }
  # Sets "syntetic" observations to NA
  for (a in setdiff(dimnames(xt)$obstype,dimnames(xtreal)$obstype))
    {
    for (m in dimnames(xt)$model) 
      { 
        xt[a,m]=NA
      }
  }
  return(xt)
}

modelhits=function(o,lok="all"){
  o=analysesinglemodel(o,lok)
  print(o[1,1]+o[3,2]+o[4,3])
  return(o)
}

analysemodelset=function(o,lok="all",rtravs,wrats,wtravs,
  testmodel=testmodeltrav,mins=5){
  for (rtrav in rtravs){
    for(wrat in wrats){
      for(wtrav in wtravs){
        o=testmodel(o,rtrav,wrat,wtrav)
        xt=analysesinglemodel(o,lok)
        tothit=xt[1,1]+xt[3,2]+xt[4,3]
        data=c(rtrav,wrat,wtrav,tothit,xt)
       
        if(!exists('output')){
          output=data
         # output=data.frame(data,row.names=c)
         #
        }else{
          output=rbind(output,data)
      }        
      }
    }
  }
  colnames(output)=c("rtrav","wrat","wtrav","tothit",
                        "g2g","gw2g","r2g","w2g",
                        "g2r","gw2r","r2r","w2r",
                        "g2w","gw2w","r2w","w2w")
  rownames(output)=c(1:length(output[,1]))
  return(as.data.frame(output))
}

# Valdres: 10 m 
#
#  To run model analyses:
#

if(FALSE){
  rtravs=c(1:10)*10
  wrats=c(1:9)/10
  wtravs=rtravs+40
  vmov5=analysemodelset(o,lok='Valdres',rtravs,wrats,wtravs,
    testmodel=testmodeltrav)
  vdisp5=analysemodelset(o,lok='Valdres',rtravs,wrats,wtravs,
    testmodel=testmodeldist)
  gmov5=analysemodelset(o,lok='Geilo',rtravs,wrats,wtravs,
    testmodel=testmodeltrav)
  gdisp5=analysemodelset(o,lok='Geilo',rtravs,wrats,wtravs,
    testmodel=testmodeldist)
}





fetchtrackwithvegcat=function(lok,date,cowid){
  table=ifelse(lok=="Valdres","valdres_classifiedgps","geilo_classified")
  sql=paste("select * from ",table," where date='",date,"' and cowid=",cowid," order by datetime",sep="");
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  data$category=as.factor(data$category)
  return(data)
}


runallmodels=function(lok,deltamin=5,days=NA){
  if(is.na(days)){
    days=logdays()
  }
  for(day in unique(days[days$lokalitet==lok,2])){
    logs=logdays(date=date)
    # Pick out from dates in stead?
    if(length(logs)>0){
      herd=logs$cowid
      for(cow in herd){
        data=fetchtrackwithvegcat(cow,day)
        data=calcdist(data,deltamin*12)
        data=valdresmodel(data)
      }
    }
  }
}

#
# List the animals observed
# main=TRUE: Lists only the main animals
# lok=... Lists only observation for the indicated location

listobsdays=function(main=FALSE,lok=''){
  sql="select distinct timestamp::date as date, cowid,lokalitet from observation"
  if(main | lok>''){
    sql=paste(sql,'where',sep=' ')
    if(main){
      sql=paste(sql,'main=true',sep=' ')
      if(lok>'')
        sql=paste(sql,'and',sep=' ')
    }
    if(lok>''){
      sql=paste(sql,' lokalitet=\'',lok,'\'',sep='')
    }
  }
  sql=paste(sql,'order by timestamp::date')
  rs=dbSendQuery(con,statement=sql)
  sets=fetch(rs,n=-1)
  return(sets)
}

#
# Fetches all observation days, runs model on each day and reports hits
#


runmodelspace=function(deltamin,models,lok='',rtravs,wrats,wtravs,rtimes){
  delta=deltamin*12 # number of 5 sec steps
  and=ifelse(lok=='','',paste("and lokalitet='",lok,"'",sep=''))
  limit=0
  limit=ifelse(limit>0,paste('limit ',limit),'')
  sql=paste("select distinct cowid, timestamp::date from observation where cowid > 0",and,limit)
  # finds all cow / date combinations in observations
  rs=dbSendQuery(con,statement=sql)
  sets=fetch(rs,n=-1)
#  print(sets)
  nloop=length(sets[,1])
  # nloop=2 # testing
  for(i in c(1:nloop)){
    cat(i,cowid,format(as.Date(date,origin="1970-01-01")),":\n")
    cowid=sets[i,1]
    date=sets[i,2]
    data=fetchgpsobs(cowid,date)
    if(length(data[,1])>0){
      # data=adjustobservations(data,delta)
      data=calcdist(data,delta)
      j=0
      for(model in models){
                                        # Runs all the models in the
                                        # entire parameter space
        for(rtrav in rtravs){
          for(wrat in wrats){
            for(wtrav in wtravs){
              for(rtime in rtimes){
              j=j+1
              cat("\b\b\b\b\b")
              cat(j)
              data=model(data,rtrav,wrat,wtrav,deltamin,rtime)
              data=data[!(is.na(data$obstype)),]
              xt=analysesinglemodel(data,lok)
              tothit=0
              for(d in intersect(dimnames(xt)$obstype,dimnames(xt)$model)){
                tothit=tothit+xt[d,d]
              }
              out=c(rtrav,wrat,wtrav,rtime,tothit,deltamin,xt)
              if(!exists('output')){
                output=out
              }else{
                output=rbind(output,out)
              }
              }
            }
          }
        }
      }
      cat("\n")
    }                                   #}
  }
#                                      print("O so far")
#  obsspeed$obstype=as.factor(obsspeed$obstype)
#  obsspeed$lokalitet=as.factor(obsspeed$lokalitet)
  rownames(output)=c(1:length(output[,1]))
  colnames(output)=c("rtrav","wrat","wtrav","rtime","tothit","deltamin",
                         "g2g","r2g","w2g",
                        "g2r","r2r","w2r",
                        "g2w","r2w","w2w")
  output=data.frame(output)
  output=output[!(is.na(output$tothit)),]
  return(output)
}




#
# Overview of observations and model
#

plotobsmod=function(d,deltamin,type='dists',timeunit='min'){
  lok=d[1,'lokalitet']
  date=d[1,'date']
  cowid=d[1,'cowid']
  palette(bioforskpalette)
  clmn=paste(type,deltamin,timeunit,sep='')
  par(xpd=TRUE)
  top=100*deltamin
  plot(d$datetime,d[,clmn],col=d$model,main=paste(lok,cowid,date,deltamin,"min"),pch=20,
       xlab='',ylab='',ylim=c(0,top))
  points(d$datetime,rep(top/-50,length(d[,1])),col=d$obstype,pch=7)
  legend(max(d$datetime)-3500,top*1.15,pch=16,col=c(1:3),
         legend=c("grazing","resting","walking"),bg="white")
}


#
# TODO: Beregne derivert - få mer av gå-toppene som gange.
#
# Derivative:
#

if(FALSE){
spl <- smooth.spline(x, y=ycs)
pred <- predict(spl)

plot (x, ycs, log="xy")
lines(pred, col=2)

ycs.prime <- diff(ycs)/diff(x)
pred.prime <- predict(spl, deriv=1)

plot(ycs.prime)
lines(pred.prime$y, col=2)
}


