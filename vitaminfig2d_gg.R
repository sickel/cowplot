rn=c('Bv','La','Pap','Tr')
vt=c(98.9,101.8,81.2,45.9)
rt=c(43.4,157.9,136,9)
sevt=c(21.1,13,4.5,6.8)
sert=c(13.9,16,7,1.4)

dim=c(lapply(c('Vegetative tissue'),rep,length(rn))[[1]],lapply(c('Reproductive tissue'),rep,length(rn))[[1]])
filename='vita2d'

ylab=expression(paste(alpha,'-tocoferol, µg/g DW'))
xlab='Species'
df=data.frame(species=factor(c(rn,rn),levels=rn),dim=factor(dim),dataset=c(vt,rt),error=c(sevt,sert))

if(!exists('plottype'))plottype='win'

ggvitaplot(df,xlab,ylab,plottype,filename,0.5)

