rn=c('Aa','Bv','La','Pap','Sg','Vm','Ac','Ao','Af','Fr')
li=c(99.9,116.1,291.8,98.5,33.7,142.9,5.4,13.8,3.5,74.9)
sh=c(26.1,254.9,252,77.6,62.7,147.4,0,17.6,0.4,54.8)
seli=c(9.1,13.3,73.4,10.5,5.9,40.2,1.8,5,1.7,8.7)
sesh=c(7.7,84.6,54.2,4.4,11.3,17.4,0,13.7,0.2,3.8)


dimnames=list(c('Light','Shade'),rn)
data=matrix(c(li,sh),nrow=2,byrow=TRUE,dimnames=dimnames)
error=matrix(c(seli,sesh),nrow=2,byrow=TRUE,dimnames=dimnames)
ylab=expression(paste(alpha,'-tocoferol, µg/g DW'))
main='Species'
if(!(exists('type')))type='win'
vitaplot(data,error,ylab,main,type,'vitaplot2a')

