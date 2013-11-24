cumuldata=function(lok,type='grazing'){
  sqlbase=paste("select count(*) from ",lok,"_adjusted_model where result='",type,"' and newperc <=",sep='')
  ps=c(0,10,20,30,40,50,60,65,70,75,80,85,90,95,96,97,98,99,100)
  sets=c()
  for(p in ps){
    sql=paste(sqlbase,p)
    rs=dbSendQuery(con,statement=sql)
    ret=fetch(rs,n=-1)
    sets=c(sets,ret$count[1])
  }
  ref=tail(sets,n=1)
  df=data.frame(cbind(ps,sets))
  df$perc=df$sets/ref*100
  return(df)
}
 #" select 100,count(*) from geilo_adjusted_model where result='grazing' and newperc=100" 



probplot=function(type='grazing'){
  size=1.5  
  cdat_valdres=cumuldata('Valdres',type)
  cdat_geilo=cumuldata('Geilo',type)
  
  plot(cdat_geilo[,1],cdat_geilo[,2]/5359.44,ylab='Percentage of grazing points',xlab='Minimum probability of correct classification',col=5,pch=15,, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
  points(cdat_valdres[,1],cdat_valdres[,2]/6777.8,col=2,pch=15)
  legend(0,70,c('Valdres','Hallingdal'),c(2,5),cex=size)
 
}


totalcumuls=function(lok){
  df=cumuldata(lok,'grazing')
  df$grazing=df$perc
  dt=cumuldata(lok,'walking')
  df$walking=dt$perc
  dt=cumuldata(lok,'resting')
  df$resting=dt$perc
  return(df)
}


if(FALSE){
pchv=1
pchg=2
colg=1
colr=2
colw=3

plot(cdf_valdres$ps,cdf_valdres$grazing,ylim=c(0,100),ylab='Percentage of grazing points',xlab='Cumultative probability of correct classification',col=colg,pch=pchv, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
points(cdf_valdres$ps,cdf_valdres$resting,pch=pchv,col=colr)
points(cdf_valdres$ps,cdf_valdres$walking,pch=pchv,col=colw)

points(cdf_geilo$ps,cdf_geilo$grazing,pch=pchg,col=colg)
points(cdf_geilo$ps,cdf_geilo$resting,pch=pchg,col=colr)
points(cdf_geilo$ps,cdf_geilo$walking,pch=pchg,col=colw)


legend(0,100,c('Valdres - grazing',' - walking',' - resting','Hallingdal - grazing',' - walking',' - resting'),
col=c(colg,colw,colr,colg,colw,colr),
pch=c(pchv,pchv,pchv,pchg,pchg,pchg))




plot(cdf_valdres$ps,cdf_valdres$grazing,ylim=c(0,100),ylab='Percentage of grazing points',xlab='Minimum probability of correct classification',col=colg,pch=pchv, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
points(cdf_valdres$ps,cdf_valdres$resting,pch=pchv,col=colr)
points(cdf_valdres$ps,cdf_valdres$walking,pch=pchv,col=colw)
|

pchv=1
pchg=2
colg=1
colr=2
colw=3
plot(cdf_valdres$ps,cdf_valdres$grazing,ylim=c(0,100),ylab='Percentage of grazing points',xlab='Cumultative probability of correct classification',col=colg,pch=pchv, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
points(cdf_valdres$ps,cdf_valdres$resting,pch=pchv,col=colr)
points(cdf_valdres$ps,cdf_valdres$walking,pch=pchv,col=colw)

points(cdf_geilo$ps,cdf_geilo$grazing,pch=pchg,col=colg)
points(cdf_geilo$ps,cdf_geilo$resting,pch=pchg,col=colr)
points(cdf_geilo$ps,cdf_geilo$walking,pch=pchg,col=colw)


legend(0,50,c('Valdres - grazing',' - walking',' - resting','Hallingdal - grazing',' - walking',' - resting'),
col=c(colg,colw,colr,colg,colw,colr),
pch=c(pchv,pchv,pchv,pchg,pchg,pchg))



cdf_valdres=cumuldata('valdres','grazing')
cdf_valdres$grazing=cdf_valdres$perc
}