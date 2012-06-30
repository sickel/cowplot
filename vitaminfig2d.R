vt=c(98.9,101.8,81.2,45.9)
rt=c(43.4,157.9,136,9)
sevt=c(21.1,13,4.5,6.8)
sert=c(13.9,16,7,1.4)
dimnames=list(c('Vegetative tissue','Reproductive tissue'),c('Bv','La','Pap','Tr'))
data=matrix(c(vt,rt),nrow=2,byrow=TRUE,dimnames=dimnames)
error=matrix(c(sevt,sert),nrow=2,byrow=TRUE,dimnames=dimnames)
ylab=expression(paste(alpha,'-tocoferol, µg/g DW'))
main='Species'

vitaplot(data,error,ylab,main)