#"source('gpslib.R')
#source('distancedriver.R')
#lok=Sys.getenv('RHISTLOK')
if(lok=='') lok='Valdres' 
sql=paste("select * from defaultmodelrun where lokalitet='",lok,"'",sep='')
rs=dbSendQuery(con,statement=sql)
modelparams=fetch(rs,n=-1)
rspeed=modelparams$restspeed/60
wspeed=modelparams$walkspeed/60
rratio=modelparams$restratio
wratio=modelparams$walkratio
rtyp=modelparams$resttyp
wtyp=modelparams$walktyp
#inttime=as.numeric(Sys.getenv('RINTTIME'))
if(is.na(inttime)) inttime=5
cat("============\n")
cat(lok,"\n")
cat(inttime,"\n")
cat("============\n")
par=par()
size=1.4
nbreak=30
par(cex.lab=size)
par(cex.axis=size)
par(cex.main=size)
par(cex.sub=size)
par(mai=c(1.02,2.0,0.82,0.42))
if(!(exists('obsdist'))) obsdist=observationdistances(inttime)
lokobs=obsdist[obsdist$lokalitet==lok,]
lokobs$distms=lokobs[,paste('dists',inttime,'min',sep='')]/(inttime*60)
lokobs$travms=lokobs[,paste('trav',inttime,'min',sep='')]/(inttime*60)
lokobs$ratio=lokobs[,paste('ratio',inttime,'min',sep='')]
#quantile
quants=c()
for (b in c('walking','grazing','resting')){

file=paste(lok,b,inttime,'distance.png',sep='_')
png(file)
assign(paste("hist_distance",inttime,b,sep="_"),hist(lokobs$travms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- movement",inttime,"mins"),xlim=c(0,1.5),breaks=c(0:nbreak)/nbreak*1.5,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5)))
# die()
dev.off()
print(file)
print(quantile(lokobs$travms[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))




file=paste(lok,b,inttime,'displacement.png',sep='_')
png(file)
assign(paste("hist_displacement",inttime,b,sep="_"),hist(lokobs$distms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- displacement",inttime,"mins"),xlim=c(0,1.5),breaks=c(0:nbreak)/nbreak*1.5,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5)))
dev.off()
print(file)
print(quantile(lokobs$distms[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))

file=paste(lok,b,inttime,'ratio.png',sep='_')
png(file)
assign(paste("hist_ratio",inttime,b,sep="_"),hist(lokobs$ratio[lokobs$obstype==b],xlab='',main=paste(lok,b,"- ratio",inttime,"mins"),xlim=c(0,1),breaks=c(0:nbreak)/nbreak,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5)))
dev.off()
print(file)
print(quantile(lokobs$ratio[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))


}

w=500
h=350

for(var in c('displacement','distance','ratio')){
  unit='m/s'
  div=
  
  if(var=='ratio'){
	unit=''
	div=30
}
  file=paste(lok,inttime,var,'.png',sep='_')
  png(file,width=w,height=h)
  
  grz=c(get(paste('hist',var,inttime,'grazing',sep='_'))$counts,0)
  rst=c(get(paste('hist',var,inttime,'resting',sep='_'))$counts,0)
  wlk=c(get(paste('hist',var,inttime,'walking',sep='_'))$counts,0)
  brks=get(paste('hist',var,inttime,'walking',sep='_'))$breaks
  ymax=max(c(grz,rst))
  plot(brks,grz,type='s',ylab='n',xlab=unit,main=paste(lok,var),ylim=c(0,ymax))
  lines(c(0,0),c(0,grz[1]))
  lines(brks,rst,type="s",col=2)
  lines(c(0,0),c(0,rst[1]),col=2)
  lines(brks,wlk,type="s",col=3)
  lines(c(0,0),c(0,wlk[1]),col=3)
  yext=ymax*1.1
  if((var=='displacement' && rtyp=='d') || (var=='distance' && rtyp=='t'))
    lines(c(rspeed,rspeed),c(-10,yext),col=2)
  if((var=='displacement' && wtyp=='d') || (var=='distance' && wtyp=='t'))
    lines(c(wspeed,wspeed),c(-10,yext),col=3)
  if(var=='ratio'){
    if(wratio<1) lines(c(wratio,wratio),c(-10,yext),col=3)
    if(rratio>0) lines(c(rratio,rratio),c(-10,yext),col=2)
    }   

  yleg=max(c(grz,rst))
  xleg=max(brks)*0.8
  legend(x=xleg,y=yleg,legend=c('grazing','resting','walking'),lty=1,col=c(1,2,3))

  dev.off()
}

par(par)
