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

# Collects distances for all days
alldists=function(){
  sets=logdays()
  dist=c()
  for(i in c(1:length(sets[,1])))
    dist=c(dist,distprday(sets[i,1],sets[i,2]))
  sets$dist=dist
  return(sets)
}


# Fetches the background map

fetchmap=function(sted){
  if (sted=="Geilo")
    # layer="geilo_kategorisert"
    layer = "geilo_classified"
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
    print(i)
    cowid=sets[i,1]
    date=sets[i,2]
    data=fetchdata(cowid,date)
    if(length(data)>0){
      data=fetchgpsobs(cowid,date)
      data=adjustobservations(data,delta)
      data=calcdist(data,delta,date,cowid)
      data=data[!(is.na(data$adjobs)),]
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
  modes=levels(od$adjobs)
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
    hist(od[,fieldname][od$adjobs==mod],breaks=c(0:ceiling(max/binsize))*binsize,main=paste(type,":",locality,mod),xlim=xlim,xlab=paste(type,deltamin,'minutes'))
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
#  calculate the distance travelled for one day at a one minute resolution
#

distprday=function(cowid,date){
    data=fetchdata(cowid,date)
    data=data[c(TRUE,rep(11,FALSE)),] # Once pr minute;
    # use rep()!
    dist=sum(distance(data,1),na.rm=TRUE)
    return(dist)
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
# Calculates travelled distance and movement over the last <delta> timesteps.
# 
#

calcdist=function(data,delta,date='',cowid=''){
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
  # ratio between travel distance and displacement 
  rcol=paste("ratio",delta/12,"min",sep='')
  data[,rcol]=data[,dcol]/data[,tcol]
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
           lty=c('solid','solid',NA,NA,NA,NA),col=c(2,1,3:(length(ot)+2)),
           pch=c(NA,NA,rep(7,length(ot))))
  }else{
    legend(max(data$datetime)-3600*legendoffset,y=ymax,
           c('movement','displacement'),lty=c('solid','solid'),col=c(2,1))
  }
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

logdays=function(cowid='',date=''){
  select=''
  if(cowid>''){
    select=paste(" and cowid=",cowid,sep='')
  }
  if(date>''){
    select=paste(select," and date='",date,"'",sep='')
  }
  sql=paste("select * from cowid_date_location where not cowid is null",
    select," order by date,lokalitet",sep='');
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}  


#
# Plots a filtered xy-plot.
#

plotxyfilter=function(data,min,lim,set='ratio',col=2,gt=TRUE){
  rcol=paste(set,min,"min",sep='')
  if(gt){
    points(data$x[data[,rcol]>lim],data$y[data[,rcol]>lim],col=col)
  }else{
    points(data$x[data[,rcol]<lim],data$y[data[,rcol]<lim],col=col)
  }
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
# Untility function: Lists days for a location
#
locationdates=function(loc){
  dates=logdays()
  return(unique(dates[dates$lokalitet==lok,2]))
}


#
# Geilo: 5 min movement: > 25 m/5min : grazing
#                        < 25 m/5min : rest
# ratio5 >0.8 && movement5 > 75 m/5min
#
# Valdres: 
# ratio5 >0.8 && movement5 > 75 m/5min

fetchmodanalyse=function(){
  o=observationdistances(5)
  o=testmodel(o)
  return(o)
}

modeltt=function(o,rtrav=25,wrat=0.8,wtrav=100,mins=5,length=500){
  tf=paste("trav",mins,"min",sep="")
  rf=paste("ratio",mins,"min",sep="")
  df=paste("dists",mins,"min",sep="")
  o$model=ifelse((o[tf]<rtrav),'resting','grazing')
  o$model=ifelse((o[rf]> wrat & o[tf]>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  o=removeshort(o,length)
  return(o)
}

modeltd=function(o,rtrav=25,wrat=0.8,wtrav=100,mins=5,length=500){
  tf=paste("trav",mins,"min",sep="")
  rf=paste("ratio",mins,"min",sep="")
  df=paste("dists",mins,"min",sep="")
  o$model=ifelse((o[tf]<rtrav),'resting','grazing')
  o$model=ifelse((o[rf]> wrat & o[df]>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  o=removeshort(o,length)
  return(o)
}
modeldt=function(o,rtrav=25,wrat=0.8,wtrav=100,mins=5,length=500){
  tf=paste("trav",mins,"min",sep="")
  rf=paste("ratio",mins,"min",sep="")
  df=paste("dists",mins,"min",sep="")
  o$model=ifelse((o[df]<rtrav),'resting','grazing')
  o$model=ifelse((o[rf]> wrat & o[tf]>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  o=removeshort(o,length)
  return(o)
}


modeldd=function(o,rtrav=25,wrat=0.8,wtrav=100,mins=5,length=500){
  tf=paste("trav",mins,"min",sep="")
  rf=paste("ratio",mins,"min",sep="")
  df=paste("dists",mins,"min",sep="")
  o$model=ifelse((o[df]<rtrav),'resting','grazing')
  o$model=ifelse((o[rf]> wrat & o[df]>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  o=removeshort(o,length)
  return(o)
}

oldvaldres=function(o,rtrav=10,wrat=0.8,wtrav=80){
  o$model=ifelse((o$dists5min<rtrav),'resting','grazing')
  o$model=ifelse((o$ratio5min> wrat & o$dists5min>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  return(o)
}


geilomodel=function(o,rtrav=25,wrat=0.8,wtrav=100){
  o$model=ifelse((o$trav5min<rtrav),'resting','grazing')
  o$model=ifelse((o$ratio5min> wrat & o$dists5min>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  o=removeshort(o)
  return(o)
}

valdresmodel=function(o,rtrav=10,wrat=0.7,wtrav=80,length=500){
  o$model=ifelse((o$dists5min<rtrav),'resting','grazing')
  o$model=ifelse((o$ratio5min> wrat & o$dists5min>wtrav) ,'walking',o$model)
  o$model=as.factor(o$model)
  o=removeshort(o,length)
  return(o)
}

removeshort=function(o,length=500){
  rle=rle(as.vector(o$model))
  rl=data.frame(val=rle$values,len=rle$lengths)
  rl$newval=rl$val
  rl$newval[rl$len<length & rl$val=='resting']='grazing'
  rle$values=rl$newval
  o$model=as.factor(inverse.rle(rle))
  return(o)
}




analysesinglemodel=function(o,lok="all"){
  if(lok!='all'){
    o=o[o$lokalitet==lok,]
  }
  # adds in to make sure that all combinations exists
  acts=c('walking','grazing','resting')
  for(i in acts){
    for(j in acts){
      rw=o[1,]
      rw$datetime=NA
      rw$adjobs=i
      rw$model=j
      o=rbind(o,rw)
    }
  }
  xt=xtabs(~adjobs+model,data=o)
  xt=xt-1
  xt=round(xt/rowSums(xt),3)*100
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
    cat(i,cowid,date,":\n")
    cowid=sets[i,1]
    date=sets[i,2]
    data=fetchgpsobs(cowid,date)
    if(length(data[,1])>0){
      data=adjustobservations(data,delta)
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
              data=data[!(is.na(data$adjobs)),]
              xt=analysesinglemodel(data,lok)
              tothit=0
              for(d in intersect(dimnames(xt)$adjobs,dimnames(xt)$model)){
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
#                                      print("OK so far")
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
# Moves observation delta/2 timestamps later to fit it with
# behaviour around observation time (then the adjusted observation is fit
# with the speed calculated around the real observation point)
# 

adjustobservations=function(data,deltamin){
  delta=deltamin*12
  adjobs=as.character(data$obstype)
  l=length(adjobs)
  rep=round(delta/2)
  adjobs=c(rep(NA,rep),adjobs)
  length(adjobs)=l
#  data$adjobs=as.factor(adjobs)
  data$adjobs=adjobs
  return(data)
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
  plot(d$datetime,d[,clmn],col=d$model,main=paste(lok,cowid,date),pch=20,
       xlab='',ylab='')
  top=max(d[,clmn],na.rm=TRUE)
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


#
#
# Run a complete model set
#


