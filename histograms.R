if(lok=='Valdres'){
  inttime=10
}else{
  inttime=15
}
par=par()
size=1.4
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

file=paste(lok,b,'distance.png',sep='_')
png(file)
hist(lokobs$travms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- movement"),xlim=c(0,1.5),breaks=c(0:15)*0.1,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5))
# die()
dev.off()
print(file)
print(quantile(lokobs$travms[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))


file=paste(lok,b,'displacement.png',sep='_')
png(file)
hist(lokobs$distms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- displacement"),xlim=c(0,1.5),breaks=c(0:15)*0.1,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5))
dev.off()
print(file)
print(quantile(lokobs$distms[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))

file=paste(lok,b,'ratio.png',sep='_')
png(file)
hist(lokobs$ratio[lokobs$obstype==b],xlab='',main=paste(lok,b,"- ratio"),xlim=c(0,1),breaks=c(0:10)*0.1,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5))
dev.off()
print(file)
print(quantile(lokobs$ratio[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))


      
}

par(par)
