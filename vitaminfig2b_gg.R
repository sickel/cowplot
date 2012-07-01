rn=c('Aa','Bv','La','Pap','Tr','Poa')
dim=c(lapply(c('July'),rep,length(rn))[[1]],lapply(c('August'),rep,length(rn))[[1]])
ju=c(55.9,317.3,424.2,68.2,40.7,7.3)
au=c(70,53.7,119.6,107.9,25.8,2.8)
seju=c(14.7,65.2,75.9,3.9,5,2.7)
seau=c(14.8,16.9,13.2,7.1,8.5,0.7)
filename='vita2b'

ylab=expression(paste(alpha,'-tocopherol, µg/g DW'))
xlab='Species'
df=data.frame(species=factor(c(rn,rn),levels=rn),dim=factor(dim,levels=c("July","August")),dataset=c(ju,au),error=c(seju,seau))

if(!exists('plottype'))plottype='win'
submark="b)"
max=500
ggvitaplot(df,xlab,ylab,plottype,filename,0.6,submark)



