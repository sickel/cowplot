source('gpslib.R')
source('distancedriver.R')
lok=Sys.getenv('RHISTLOK')
if(lok=='') lok='Valdres'
inttime=as.numeric(Sys.getenv('RINTTIME'))
if(is.na(inttime)) inttime=10
cat("============\n")
cat(lok,"\n")
cat(inttime," mins\n")
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
lokobs$avgdist=lokobs[,paste('avgd',inttime,'min',sep='')]
                                        #quantile
quants=c()
for (b in c('walking','grazing','resting')){

file=paste(lok,b,inttime,'distance.png',sep='_')
png(file)
hist(lokobs$travms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- movement",inttime,"mins"),xlim=c(0,1.5),breaks=c(0:nbreak)/nbreak*1.5,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5))
# die()
dev.off()
print(file)
print(quantile(lokobs$travms[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))


file=paste(lok,b,inttime,'displacement.png',sep='_')
png(file)
hist(lokobs$distms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- displacement",inttime,"mins"),xlim=c(0,1.5),breaks=c(0:nbreak)/nbreak*1.5,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5))
dev.off()
print(file)
print(quantile(lokobs$distms[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))

file=paste(lok,b,inttime,'ratio.png',sep='_')
png(file)
hist(lokobs$ratio[lokobs$obstype==b],xlab='',main=paste(lok,b,"- ratio",inttime,"mins"),xlim=c(0,1),breaks=c(0:nbreak)/nbreak,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5))
dev.off()
print(file)

print(quantile(lokobs$ratio[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))
file=paste(lok,b,inttime,'avgd.png',sep='_')
png(file)
hist(lokobs$avgdist[lokobs$obstype==b],main=paste(lok,b,"- average distance",inttime,"mins"),breaks=nbreak,cex.lab=size,cex.axis=size,cex.main=size,cex.sub=size,lwd=size,col='gray',mai=c(1,2,1,0.5),xlab="meters")
dev.off()
print(file)
print(quantile(lokobs$ratio[lokobs$obstype==b],c(0.05,0.95),na.rm=TRUE))


      
}

par(par)
