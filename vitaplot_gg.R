ggvitaplot<-function(data,xlab,ylab,type="win",filename='output'){
  if(exists('debug') && dev.cur()[[1]]>1) dev.off()
  width=1374
  height=971	
  res=200
  if(type=="win"){
    dev.new(width=width/res,height=height/res)
  }else{
    png(paste(filename,'.png',sep=''),width=916,height=629,res=res)
  }
  max=max(df$dataset+df$error)*1.05
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
  dataplot=dataplot+ coord_cartesian(ylim=c(0, max))
  dataplot=dataplot + opts(axis.text.x = theme_text(colour = "black",angle=90),axis.text.y = theme_text(colour = "black"))
  dataplot=dataplot + opts(legend.key.size = unit(0.3, "cm"))
  print(dataplot)
  if(type!="win"){
    dev.off()	
  }
}