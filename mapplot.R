if(!(exists('lok'))) lok='Valdres'
lgncex=0.22 # defines size of the legend
if(!(exists('map'))) map=fetchmap(lok)
if(!(exists('od'))) {
  od=fetchgpsobslok(lok)
  od$obstype=as.factor(od$obstype)
  od=od[order(od$obstype),]
}
if('lok'=='Valdres'){
  legy=bbox(map)[2,2]-1500
  legx=bbox(map)[1,1]-500
  xlim=NULL
  palette(c("firebrick","SpringGreen","LawnGreen","LightPink","RosyBrown","SandyBrown","LightGrey","LightPink","LightGrey","DarkKhaki","DarkSeaGreen","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen","MediumSeaGreen","SpringGreen","wheat","peru","PaleGreen3","Yellow3","AntiqueWhite","LemonChiffon","thistle","ForestGreen","OliveDrab","DarkOliveGreen","Green"))
}else{
  legy=bbox(map)[2,2]+500
  legx=bbox(map)[1,1]-450
  xlim=c(bbox(map)[1,1]-300,bbox(map)[1,2]+50)
  palette(c("LightGrey","Black","LawnGreen","cornflower blue","RosyBrown","SandyBrown","DarkKhaki","DarkSeaGreen","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen","MediumSeaGreen","SpringGreen","wheat","peru","PaleGreen3","Yellow3","AntiqueWhite","LemonChiffon","thistle","ForestGreen","LightPink","LightPink","OliveDrab","DarkOliveGreen","Green","Gold","SaddleBrown","LightGray","coral","DarkOrange3","DarkOrchid4","IndianRed","firebrick","LemonChiffon","khaki","LightGray","Black","SpringGreen","Green Yellow"))
}


lwd=par("lwd")
par(lwd=0.2)
plot(map,col=map$categorycode,xlim=xlim)
types=unique(map$category)
nums=unique(map$categorycode)
legend(legx,legy,types,fill=nums,border=nums,cex=lgncex,bty="n")
points(od$x,od$y,col='white',pch=19,cex=0.6)
palette(c("red","blue","green","yellow","white"))
points(od$x,od$y,col=as.integer(od$obstype),pch=19,cex=0.45)
if(lok=="Valdres"){
  legy=legy-1000
}else{
  legx=bbox(map)[1,1]+750
  # legy=bbox(map)[2,2]+500
}
legend(legx,legy,levels(od$obstype),col=palette(),cex=lgncex,pch=19,bty="n")
par(lwd=lwd)
