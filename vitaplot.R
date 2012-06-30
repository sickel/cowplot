vitaplot<-function(data,error,ylab,xlab,type="win",filename='output'){
  main=''
  if(exists('debug') && dev.cur()[[1]]>1) dev.off()
  width=916
  height=629
  if(type=="win"){
    dev.new(width=width/300,height=height/300)
  }else{
    
    png(paste(filename,'.png',sep=''),width=916,height=629)
  }
 
  palette(c('gray75','grey25','white'))
  #par(xpd=T, mar=par()$mar+c(0,0,0,8))
  #par(xpd=T, mar=c(0,0,0,8))
  if(exists('mar')){
    par(mar=mar)
  }
  par(oma=c(0,0,0,0),xpd=T)
  par(ps=8,mgp=c(1.6,1,0))
  mx=max(data+error)
  oom=log(mx,10)
  maxy=round(mx,digits=-floor(oom))

  bp<-barplot(data,beside=TRUE,ylab=ylab,xlab=xlab,main=main,ylim=c(0,maxy),col=c(1,2))
  segments(bp,data-error,bp,data+error)

  ew=(bp[2,1]-bp[1,1])/2
  ew=ew/2
  segments(bp-ew,data-error,bp+ew,data-error)
  segments(bp-ew,data+error,bp+ew,data+error)

  legend(max(bp),maxy,rownames(data),fill=c(1,2),bty='n')
  par(mar=c(5, 4, 4, 2) + 0.1)
  if(type!="win"){
    dev.off()	
  }
}