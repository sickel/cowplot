  # rtravs=c(1:10)*10
  # wtravs=rtravs+40
  # wrats=c(1:9)/10
  rtravs=c(1:3)*10
  wtravs=c(2:5)*20
  wrats=c(1:7)/10
  rtimes=c(2:5)*20
 # rtravs=c(1:2)*5
 # wtravs=c(8:10)*10
 # wrats=c(1:3)/10
 # rtimes=c(5:7)*50
  tempsumm<-NULL
  for(lok in c('Valdres','Geilo')){
    cat("========================================\n")
    loksummary=paste(lok,'summ',sep='.')
    if(exists("tempsumm")){tempsumm<-NULL}
    for(time in c(5,10,15,20)){
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
  }
