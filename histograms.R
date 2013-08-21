lok='Valdres'
inttime=10

lok='Geilo'
inttime=15

# obsdist=observationdistances(inttime)
#lokobs=obsdist[obsdist$lokalitet==lok,]
#lokobs$distms<-lokobs[paste('dists',inttime,'min',sep='')]/inttime*60
#lokobs$travms<-lokobs[paste('trav',inttime,'min',sep='')]/inttime*60

for (b in c('walking','grazing','resting')){

file=paste(lok,b,'distance.png',sep='_')
png(file)
hist(lokobs$travms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- distance"),xlim=c(0,1.5),breaks=c(0:15)*0.1)
dev.off()

file=paste(lok,b,'displacement.png',sep='_')
png(file)

hist(lokobs$distms[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- displacement"),xlim=c(0,1.5),breaks=c(0:15)*0.1)
dev.off()

file=paste(lok,b,'ratio.png',sep='_')
png(file)

hist(lokobs$ratio15min[lokobs$obstype==b],xlab='m/s',main=paste(lok,b,"- ratio"),xlim=c(0,1),breaks=c(0:10)*0.1)
dev.off()

}
