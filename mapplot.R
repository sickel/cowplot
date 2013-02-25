#
# Plots a base map with legend
#
lgncex=0.3 # defines size of the legend

plotmap=function(){
  if(!(exists('lok'))) lok='Valdres'
  if(!(exists('map'))) map=fetchmap(lok)
  if(lok=='Valdres'){
    legy=bbox(map)[2,1]+1500
    legx=bbox(map)[1,1]-500
    xlim=NULL
    palette(c("firebrick","SpringGreen","LawnGreen","LightPink","RosyBrown","SandyBrown","LightGrey","LightPink","LightGrey","DarkKhaki","DarkSeaGreen","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen","MediumSeaGreen","SpringGreen","wheat","peru","PaleGreen3","Yellow3","AntiqueWhite","LemonChiffon","thistle","ForestGreen","OliveDrab","DarkOliveGreen","Green"))
  }else{
    legy=bbox(map)[2,2]+500
    legx=bbox(map)[1,1]-450
    xlim=c(bbox(map)[1,1]-300,bbox(map)[1,2]+50) # Need some extra space for legend
    palette(c("LightGrey","Black","LawnGreen","cornflower blue","RosyBrown","SandyBrown","DarkKhaki","DarkSeaGreen","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen","MediumSeaGreen","SpringGreen","wheat","peru","PaleGreen3","Yellow3","AntiqueWhite","LemonChiffon","thistle","ForestGreen","LightPink","LightPink","OliveDrab","DarkOliveGreen","Green","Gold","SaddleBrown","LightGray","coral","DarkOrange3","DarkOrchid4","IndianRed","firebrick","LemonChiffon","khaki","LightGray","Black","SpringGreen","Green Yellow"))
  }
  plot(map,col=map$categorycode,xlim=xlim,lwd=0.2)
  types=unique(map$category)
  nums=unique(map$categorycode)
  legend(legx,legy,types,fill=nums,border=nums,cex=lgncex,bty="n")
}

obspointplot=function(od){
    points(od$x,od$y,col='white',pch=19,cex=0.6)
    palette(c("red","blue","green","yellow","white"))
    points(od$x,od$y,col=as.integer(od$obstype),pch=19,cex=0.45)
    if(lok=="Valdres"){
      legy=bbox(map)[2,1]+500
      legx=bbox(map)[1,1]-500
    }else{
      legy=bbox(map)[2,2]+500
      legx=bbox(map)[1,1]+750
    }
    legend(legx,legy,levels(od$obstype),col=palette(),cex=lgncex,pch=19,bty="n")
}


fetchodday=function(date){
  od=fetchobs(NULL,date)
  od=od[order(od$obstype),]
  return(od)
}

dayplot=function(date){
  lwd=1
  logs=logdays(date=date)
  herd=logs$cowid
  palette(c("cyan","magenta","orange","yellow","purple"))
  for(i in c(1:length(herd))){
    cow=herd[i]
    data=fetchdata(cow,date)
    if(length(data)>0){
      lines(data$x,data$y,col=i,lwd=lwd)
      text(data$x,data$y,labels=data$marker-1,cex=0.3)
    }
  }
  if(lok=="Valdres"){
    legy=bbox(map)[2,1]+500
    legx=bbox(map)[1,1]-300
  }else{
    legy=bbox(map)[2,2]+500
    legx=bbox(map)[1,1]+950
  }
  legend(legx,legy,herd,col=c(1:length(herd)),cex=lgncex,lwd=lwd,bty="n")
}

  
if(FALSE){
   if(!(exists('od'))) {
    od=fetchgpsobslok(lok)
    od$obstype=as.factor(od$obstype)
    od=od[order(od$obstype),]
  }
}


plotdatetrack=function(date){
  if(pdfplot) pdf(paste(lok,"_track_",date,".pdf",sep=""),paper="a4",width=0,height=0)
  else
  plotmap()
  od=fetchodday(date)
  if(length(od)>0) obspointplot(od)
  dayplot(date)
  title(main=date)
  if(pdfplot) dev.off()
}


plotalltracs=function(lok){
map=fetchmap(lok)
for(day in unique(days[days$lokalitet==lok,2])){
  day=format(as.Date(day,origin="1970-01-01"))
  print(day)
 plotdatetrack(day)
}
}
