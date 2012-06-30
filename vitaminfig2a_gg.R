rn=c('Aa','Bv','La','Pap','Sg','Vm','Ac','Ao','Af','Fr')
dim=c(lapply(c('Light'),rep,length(rn))[[1]],lapply(c('Shade'),rep,length(rn))[[1]])
type=c('Herbs','Herbs','Herbs','Herbs','Woody','Woody','Grasses','Grasses','Grasses','Grasses')
li=c(99.9,116.1,291.8,98.5,33.7,142.9,5.4,13.8,3.5,74.9)
sh=c(26.1,254.9,252,77.6,62.7,147.4,0,17.6,0.4,54.8)
seli=c(9.1,13.3,73.4,10.5,5.9,40.2,1.8,5,1.7,8.7)
sesh=c(7.7,84.6,54.2,4.4,11.3,17.4,0,13.7,0.2,3.8)


ylab=expression(paste(alpha,'-tocoferol, µg/g DW'))
xlab='Species'
df=data.frame(species=factor(c(rn,rn),levels=rn),dim=factor(dim),dataset=c(li,sh),error=c(seli,sesh),type=factor(type))

if(!exists('plottype'))plottype='win'

ggvitaplot(df,xlab,ylab,plottype,'vita2a')