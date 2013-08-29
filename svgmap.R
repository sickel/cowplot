library(gridSVG)
# library(pxR)
library(sp)
library(lattice)
library(latticeExtra)
library(maptools)
library(classInt)
library(colorspace)

# 
if(exists('createnewmap')){
  grid.newpage()
  gidcol=19
  if(lok=='Geilo') gidcol=23
  set_Polypath(FALSE) 
  panel.str <- deparse(panel.polygonsplot, width=500)
  panel.str <- sub("grid.polygon\\((.*)\\)","grid.polygon(\\1, name=paste('ID', slot(map, 'data'\\)\\$gid\\[i\\], sep=':'))", panel.str)
  panel.polygonNames <- eval(parse(text=panel.str),envir=environment(panel.polygonsplot))
  p <- spplot(map["category"], panel=panel.polygonNames)
  eval(p)
  set_Polypath(TRUE) 

}

## grobs in the graphical output
grobs <- grid.ls()
## only interested in those with "ID:" in the name
nms <- grobs$name[grobs$type == "grobListing"]
idxNames <- grep('ID:', nms)
IDs <- nms[idxNames]

grobDescent<-function(x=0,y=0,z=0){
 return(unit(1,"npc"))
}
grobAscent<-function(x=0,y=0,z=0){
 return(unit(1,"npc"))
}


# print('sofar')
ididx=0
for (id in unique(IDs)){
  ididx=ididx+1
  ## print(id)
  ## extract information from the data
  ## according to the ID value
  x <- unlist(strsplit(id, 'ID:'))
  i=as.numeric(x[2])
    
  n=which(map$gid==i)
  info=paste(id,"->",map$category[n],'(',map$shape_area[n],'m2 )')
  # info=map$category[n]
  cat(info,"\n")
  g <- grid.get(id)
  ## attach SVG attributes
  grid.garnish(id,
               onmouseover=paste("showTooltip(evt, '",info,"')"),
               onmouseout="hideTooltip()",
               onclick="mark(evt)",
               class=paste('cc',map$categorycode[n],sep=''))
}
 
grid.script(filename="tooltip.js")

svgname=paste(lok,'annomap.svg',sep='_')
gridToSVG(svgname)
