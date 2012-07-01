

rn=c('La','Tr','Cn','Ac','Af')
	
va=c(408,16,66,5,4)
ha=c(136,50,84,1,0)
					
					
seva=c(79,5.1,6.8,1.9,1.6)
seha=c(13.8,5.1,17.9,0.5,0)


dim=c(lapply(c('Site Valdres'),rep,length(rn))[[1]],lapply(c('Site Hallingdal'),rep,length(rn))[[1]])
filename='vita2c'

ylab=expression(paste(alpha,'-tocoferol, µg/g DW'))
xlab='Species'
df=data.frame(species=factor(c(rn,rn),levels=rn),dim=factor(dim),dataset=c(va,ha),error=c(seva,seha))

if(!exists('plottype'))plottype='win'

ggvitaplot(df,xlab,ylab,plottype,filename,0.6)


