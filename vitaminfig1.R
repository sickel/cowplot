
#	Mean, tot toc	"Vanlig" SE
# Herbs	224	96
# Sedges	192	78
# Woody species	189	52
# Grasses	29	12

rn=c('Herbs','Sedges','Woody species','Grasses')
data=c(224,192,189,29)
se=c(96,78,52,12)
sig=c('b','b','b','a')
df=data.frame(species=factor(rn,levels=rn),dataset=data,error=se,sig=sig)
ylab='Total tocopherol\n µg/g DW'
xlab=''
filename='vita1'

if(!exists('plottype'))plottype='win'
rm(dim)
anno=data.frame(x=c(1:4),y=(df$dataset+df$error)+15,label=df$sig)
dataplot=ggvitaplot(df,xlab,ylab,plottype,filename,0.5)
