ggvitaplot<-function(data,xlab,ylab,type="win",filename='output',barwidth=0.9,submark=''){
  
  if(exists('debug') && dev.cur()[[1]]>1) dev.off()
  width=1374
  height=971	
  res=300
  if(type=="win"){
    dev.new(width=width/res,height=height/res)
  }else{
    png(paste(filename,'.png',sep=''),width=975,height=689,res=200)
  }
  max=max(df$dataset+df$error)*1.05
  dodgewidth=barwidth*1.1
  dodge=position_dodge(width=dodgewidth)  
  cat(filename,max,"->")
  if(max > 350) max=530
  if(max < 200) max=200
  cat(max,"\n")
  dataplot=ggplot(data=df, aes(x=species,y=dataset,fill=dim,ylim=c(0,max),width=1))
  dataplot=dataplot+geom_bar(width=barwidth, position=dodge)
  #dataplot=dataplot+geom_bar(width=barwidth, position=position_dodge(width=dodgewidth))
  dataplot=dataplot+scale_fill_manual(values=c("#BBBBBB", "#444444"))
  dataplot=dataplot+ylab(ylab)+xlab(xlab)+opts(legend.title=theme_blank())
  limits <- aes(ymax = df$dataset+df$error, ymin=df$dataset-df$error)
  
  dataplot=dataplot+  geom_errorbar(limits, position=dodge, width=barwidth*0.75) 
  dataplot=dataplot+ opts(panel.background = theme_rect(fill='white', colour='white'))
  dataplot=dataplot+ opts(panel.grid.major = theme_line(colour = "gray"))
  dataplot=dataplot+ scale_y_continuous(limits=c(0, max),breaks=seq(0, 500, 50))
  dataplot=dataplot + opts(axis.text.x = theme_text(colour = "black",angle=0),axis.text.y = theme_text(colour = "black"))
  dataplot=dataplot + opts(legend.key.size = unit(0.3, "cm"))
  
  if('sig' %in% colnames(df)){
    anno=data.frame(x=c(1:4),y=(df$dataset+df$error)*1.05,label=df$sig)
    print(df$dataset+df$error)  
  # print(anno)
    dataplot=dataplot+geom_text(aes(x=anno$x,y=anno$y,label=anno$label,size=4))
    dataplot=dataplot+ opts(legend.position = "none") 
  }
  print(dataplot)
  if(submark>''){
    makeLabel(submark)
    #subaes=aes(x=0,y=max,label=submark,size=4)
    #dataplot=dataplot + grid.text(label=submark)
  }
  if(type!="win"){
    dev.off()	
  }
  return(dataplot)
}