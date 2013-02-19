lok='Valdres'
if(!(exists('map'))) map=fetchmap(lok)
# defpal=palette(rainbow.colors(20))
palette(c("black","SpringGreen","LawnGreen","LightPink","RosyBrown","SandyBrown","LightGrey","LightPink","LightGrey","DarkKhaki","DarkSeaGreen","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen","MediumSeaGreen","SpringGreen","wheat","peru","PaleGreen3","Yellow3","AntiqueWhite","LemonChiffon","thistle","ForestGreen","OliveDrab","DarkOliveGreen","Green"))
plot(map,col=map$categorycode) 
legy=bbox(map)[2,2]-1500
legx=bbox(map)[1,1]-500
types=levels(unique(map$category))
nums=unique(map$categorycode)
legend(legx,legy,types,fill=nums,cex=0.3)
points(od$x,od$y,col='white',pch=19,cex=0.6)
palette(c("red","blue","green"))
points(od$x,od$y,col=as.integer(od$obstype),pch=19,cex=0.45)
