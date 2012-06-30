rn=c('Aa','Bv','La','Pap','Tr','Poa')
ju=c(55.9,317.3,424.2,68.2,40.7,7.3)
au=c(70,53.7,119.6,107.9,25.8,2.8)
seju=c(14.7,65.2,75.9,3.9,5,2.7)
seau=c(14.8,16.9,13.2,7.1,8.5,0.7)


dimnames=list(c('July','August'),rn)
data=matrix(c(ju,au),nrow=2,byrow=TRUE,dimnames=dimnames)
error=matrix(c(seju,seau),nrow=2,byrow=TRUE,dimnames=dimnames)
ylab=expression(paste(alpha,'-tocoferol, µg/g DW'))
main='Species'

vitaplot(data,error,ylab,main)
