#
#
# Run a complete model set
#
  rtravs=c(1:3)*10
  wtravs=c(2:5)*20
  wrats=c(1:7)/10
  rtimes=c(2:5)*50
modtimes=c(5,10,15,20)
                                        # Valdres: rt  wr wt rtim min mod rtrav wrat wtrav rtime   tothit deltamin      
#      1   10 0.1 40   40   5  dd    10  0.1    40    40 
#
# Valdres
rtravs=c(4,6,8,10,12,14)
wtravs=c(36,38,40,42,44)
wrats=c(0.05,0.07,1,1.2,1.5)
rtims=c(6:10)*5
modtimes=c(3,4,5,6,7)
  # rtravs=c(1:10)*10
  # wtravs=rtravs+40
  # wrats=c(1:9)/10
  rtravs=c(1:2)*5
  wtravs=c(8:10)*10
  wrats=c(1:3)/10
  rtimes=c(7:10)*50
modtimes=c(5,10)
tempsumm<-NULL
  for(lok in c('Valdres','Geilo')){
    cat("========================================\n")
    loksummary=paste(lok,'summ',sep='.')
    if(exists("tempsumm")){tempsumm<-NULL}
    for(time in modtimes){
      for(mtyp in c('td','dt','tt','dd')){
        outvar=paste(lok,time,mtyp,sep='.')
        mod=paste('model',mtyp,sep='')
        cat('-------------------------->',lok,' ',mod,' ',time," min\n")
        modlist=c(get(mod))
        out=runmodelspace(time,modlist,lok,rtravs,wrats,wtravs,rtimes)
        out$modtype=rep(mtyp,length(out[,1]))
        if(is.null(tempsumm)){
          tempsumm=out
        }else{
          tempsumm=rbind(tempsumm,out)
        }
      }
    }
    tempsumm$modtype=as.factor(tempsumm$modtype)
    assign(paste(lok,'summary',sep='.'),tempsumm)
    attach(tempsumm)
    tmpagg=aggregate(tempsumm,list(rtrav,wrat,wtrav,rtime,deltamin,modtype),FUN='mean')
    detach(tempsumm)
    names(tmpagg)[1:6]=c('rt','wr','wt','rtim','min','mod')
    assign(paste(lok,'aggregate',sep='.'),tmpagg)
    rm(tmpagg)
  }
