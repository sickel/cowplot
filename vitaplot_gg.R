ggvitaplot<-function(data,xlab,ylab,type="win",filename='output'){
  main=''
  if(exists('debug') && dev.cur()[[1]]>1) dev.off()
  width=916
  height=629
  res=150
  if(type=="win"){
    dev.new(width=width/res,height=height/res)
  }else{
    
    png(paste(filename,'.png',sep=''),width=916,height=629)
  }
 
  #palette(c('gray75','grey25','white'))
  #par(xpd=T, mar=par()$mar+c(0,0,0,8))
  #par(xpd=T, mar=c(0,0,0,8))
  #if(exists('mar')){
  #  par(mar=mar)
  #}
  #par(oma=c(0,0,0,0),xpd=T)
  #par(ps=8,mgp=c(1.6,1,0))
  mx=max(df$data+df$error)
  oom=log(mx,10)
  maxy=round(mx,digits=-floor(oom))
  dataplot=ggplot(data=df, aes(x=species,y=dataset,fill=dim))
  dataplot=dataplot+geom_bar(position=position_dodge())
  dataplot=dataplot+scale_fill_manual(values=c("#BBBBBB", "#444444"))
  dataplot=dataplot+ylab(ylab)+xlab(xlab)+opts(legend.title=theme_blank())

  limits <- aes(ymax = df$dataset+df$error, ymin=df$dataset-df$error)
  dodge=position_dodge(width=0.9)
  dataplot=dataplot+  geom_errorbar(limits, position=dodge, width=0.5) 
  dataplot=dataplot+ opts(panel.background = theme_rect(fill='white', colour='white'))
  dataplot=dataplot+ opts(panel.grid.major = theme_line(colour = "gray"))
  # dataplot=dataplot+ coord_cartesian(ylim=c(3, 400))
  dataplot=dataplot+ coord_cartesian(ylim=c(0, 400))
  # dataplot=dataplot+facet_grid(. ~type)
  print(dataplot)
  if(type!="win"){
    dev.off()	
  }
}