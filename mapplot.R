lok='Valdres'
lgncex=0.22 # defines size of the legend
if(!(exists('map'))) map=fetchmap(lok)
if(!(exists('od'))) od=fetchgpsobslok(lok)
# defpal=palette(rainbow.colors(20))
palette(c("black","SpringGreen","LawnGreen","LightPink","RosyBrown","SandyBrown","LightGrey","LightPink","LightGrey","DarkKhaki","DarkSeaGreen","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen","MediumSeaGreen","SpringGreen","wheat","peru","PaleGreen3","Yellow3","AntiqueWhite","LemonChiffon","thistle","ForestGreen","OliveDrab","DarkOliveGreen","Green"))
plot(map,col=map$categorycode) 
legy=bbox(map)[2,2]-1500
legx=bbox(map)[1,1]-500
types=unique(map$category)
nums=unique(map$categorycode)
legend(legx,legy,types,fill=nums,border=nums,cex=lgncex,bty="n")
points(od$x,od$y,col='white',pch=19,cex=0.6)
palette(c("red","blue","green","yellow","white"))
od=od[order(od$obstype),]
points(od$x,od$y,col=as.integer(od$obstype),pch=19,cex=0.45)
legend(legx,legy-1000,levels(od$obstype),col=palette(),cex=lgncex,pch=19,bty="n")
