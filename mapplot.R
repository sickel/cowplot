#
# Plots a base map with legend
#
#
#
lgncex=0.4 # defines size of the legend

plotmap=function(lok="Valdres"){
  par(xpd=NA)
  par(mar=c(0,0,0,0))
  par(mai=c(0,0,0,0))
  
  if(!(exists('map'))) map=fetchmap(lok)
  if(!(exists('rast'))) rast=fetchrast(lok)
  if(lok=='Valdres'){
    # Positions for legend:
    legy=bbox(map)[2,1]+1300
    legx=bbox(map)[1,1]-1300
    xlim=NULL
    pal=c("firebrick","peru","Yellow","LightPink","RosyBrown",
      "SandyBrown","LightGrey","LightPink","LightGrey","DarkKhaki",
      "DeepSkyBlue2","PaleGreen","LightSeaGreen","DarkGoldenrod","YellowGreen",
      "MediumSeaGreen","SpringGreen","wheat","Green Yellow","violet red",
      "Yellow3","AntiqueWhite","PaleGreen3","thistle","DeepPink",
      "OliveDrab","DarkOliveGreen","Green")
    # "PaleGreen3" SpringGreen LawnGreen DarkSeaGreen "ForestGreen"
    # Positions for scale bar:
    sbx=legx+10
    sby=legy+100
    # sby=bbox(map)[2,2]+50
  }else{
    # legy=bbox(map)[2,1]+800
    legy=bbox(map)[2,2]+50
    legx=bbox(map)[1,1]-910
    xlim=c(bbox(map)[1,1]-300,bbox(map)[1,2]+50) # Need some extra space for legend
    pal=c("DimGrey","Red","LightGrey","cornflower blue","DarkOliveGreen",
      "ForestGreen","LightSeaGreen","IndianRed","DarkGoldenrod","Green Yellow",
      "Red","Red","DarkKhaki","YellowGreen","Red",
      "Red","coral","peru","Red","Yellow3",
      "yellow","Red","LightPink","LightPink","DeepSkyBlue2",
      "Red","OliveDrab", "LemonChiffon","MediumSeaGreen","dark orchid",
      "SaddleBrown","violet red","burlywood","DeepPink","Red",
      "PaleGreen","Red","LightGray")
    # ,"LightGray","Black","thistle","firebrick",)  LawnGreen Gol thistle coral
    # SandyBrown "MediumSeaGreen","SpringGreen" "PaleGreen3" ForestGreen SpringGreen
    # "DarkOrchid4",","khaki" Green "RosyBrown", "thistle" DarkSeaGreen
    # DarkOrange3
    sbx=legx+10
    #sby=bbox(map)[2,2]-50
    sby=bbox(map)[2,1]-5000
  
  }
  image(rast, red="band1", green="band2", blue="band3")
  palette(pal)
  plot(map,col=map$categorycode,xlim=xlim,lwd=0.2,add=TRUE)
  types=unique(map$category)
  nums=unique(map$categorycode)
  if(!(exists("nolegend"))||!(nolegend)){
    legend(legx,legy,types,fill=nums,border=nums,cex=lgncex,bg="white")
  }
  drawscale(sbx,sby,1000,"1 km",lgncex*2)
}
# Draws a line of desired lengt at a specified locations
drawscale=function(sbx,sby,length,text="1 km",cex=1){
  lines(c(sbx,sbx+length),c(sby,sby))
  vert=c(sby-50,sby+50)
  lines(c(sbx,sbx),vert)
  lines(c(sbx+length,sbx+length),vert)
  text(c(sbx+length),c(sby),text,cex=cex,pos=4)
}

# Plots all observations
obspointplot=function(od){
    points(od$x,od$y,col='white',pch=19,cex=0.6)
    palette(c("red","blue","green","yellow","white"))
    points(od$x,od$y,col=as.integer(od$obstype),pch=19,cex=0.45)
    if(lok=="Valdres"){
      # Position of legend
      legy=bbox(map)[2,1]+500
      legx=bbox(map)[1,1]+250
    }else{
      legy=bbox(map)[2,2]-1400
      legx=bbox(map)[1,1]-10
    }
    legend(legx,legy,levels(od$obstype),col=palette(),cex=lgncex,pch=19,bty="n")
}

# Fetches observations for one day and orders it for plotting (by incident,
# order by obstype gives the most frequent observationtype first, thereby
# is it plottet "at the bottom" and will not obscure other more infrequent
# observation types

fetchodday=function(date){
  od=fetchobs(NULL,date)
  od=od[order(od$obstype),]
  return(od)
}

# Plots all iformation for one day
# mapplot(lok) must be run first to plot the background map and set up
# the plotting environment
# (or use plotdaytrack() (below)
dayplot=function(date){
  lwd=1
  logs=logdays(date=date)
  if(length(logs)>0){
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
      legx=bbox(map)[1,1]-10
    }else{
      legy=bbox(map)[2,2]-1200
      legx=bbox(map)[1,1]-450
    }
    legend(legx,legy,herd,col=c(1:length(herd)),cex=lgncex,lwd=lwd,bg="white")
  }
}

#
# Plot classified points
# 
classplot=function(data){
  palette(c("red","blue","green"))
  points(data$x,data$y,col=data$class,cex=0.1)
}

# Plots a map with all tracks and observations for one given day and locations
plotdatetrack=function(date,lok){
  if(lok=="Valdres") paper="a4" else paper="a4r"
  if(pdfplot) pdf(paste(lok,"_track_",date,".pdf",sep=""),paper=paper,width=0,height=0,title=paste(lok,date,sep=" "))
  # Widht and height set to 0 gives default margins.
  plotmap(lok)
  od=fetchodday(date)
  if(length(od)>0) obspointplot(od)
  dayplot(date)
  title(main=date)
  if(pdfplot) dev.off()
}

# Run to plot maps for all days

plotalltracks=function(lok){
  # map=fetchmap(lok) # map is a global variable ... :-/
  # must be set before this is run
  for(day in unique(days[days$lokalitet==lok,2])){
    day=format(as.Date(day,origin="1970-01-01"))
    print(day)
    plotdatetrack(day,lok)
  }
}


fetchrast=function(lok){
  if(lok=='Valdres') return(readGDAL('valdres_googlemaps.png'))
  else return(readGDAL('geilo_osm.png'))
}
