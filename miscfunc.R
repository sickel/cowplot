cumuldata=function(lok,type='grazing'){
  sqlbase=paste("select count(*) from ",lok,"_adjusted_model where result='",type,"' and newperc <=",sep='')
  ps=c(0,10,20,30,40,50,60,65,70,75,80,85,90,95,96,97,98,99,100)
  sets=c()
  for(p in ps){
    sql=paste(sqlbase,p)
    rs=dbSendQuery(con,statement=sql)
    ret=fetch(rs,n=-1)
    sets=c(sets,ret$count[1])
  }
  ref=tail(sets,n=1)
  df=data.frame(cbind(ps,sets))
  df$perc=df$sets/ref*100
  return(df)
}
 #" select 100,count(*) from geilo_adjusted_model where result='grazing' and newperc=100" 



probplot=function(type='grazing'){
  size=1.5  
  cdat_valdres=cumuldata('Valdres',type)
  cdat_geilo=cumuldata('Geilo',type)
  
  plot(cdat_geilo[,1],cdat_geilo[,2]/5359.44,ylab='Percentage of grazing points',xlab='Minimum probability of correct classification',col=5,pch=15,, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
  points(cdat_valdres[,1],cdat_valdres[,2]/6777.8,col=2,pch=15)
  legend(0,70,c('Valdres','Hallingdal'),c(2,5),cex=size)
 
}


totalcumuls=function(lok){
  df=cumuldata(lok,'grazing')
  df$grazing=df$perc
  dt=cumuldata(lok,'walking')
  df$walking=dt$perc
  dt=cumuldata(lok,'resting')
  df$resting=dt$perc
  return(df)
}


createcdf=function(){
  cdf_geilo=totalcumuls('Geilo')
  cdf_valdres=totalcumuls('Valdres')
  cdf_valdres$location='Valdres'
  cdf_geilo$location='Hallingdal'
  cdf=rbind(cdf_valdres,cdf_geilo)
  return(melt(data=cdf,id.var=c('location','ps'),measure.var=c('grazing','resting','walking'),variable_name="behaviour"))
}

      
    


if(FALSE){
pchv=1
pchg=2
colg=1
colr=2
colw=3

plot(cdf_valdres$ps,cdf_valdres$grazing,ylim=c(0,100),ylab='Percentage of grazing points',xlab='Cumultative probability of correct classification',col=colg,pch=pchv, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
points(cdf_valdres$ps,cdf_valdres$resting,pch=pchv,col=colr)
points(cdf_valdres$ps,cdf_valdres$walking,pch=pchv,col=colw)

points(cdf_geilo$ps,cdf_geilo$grazing,pch=pchg,col=colg)
points(cdf_geilo$ps,cdf_geilo$resting,pch=pchg,col=colr)
points(cdf_geilo$ps,cdf_geilo$walking,pch=pchg,col=colw)


legend(0,100,c('Valdres - grazing',' - walking',' - resting','Hallingdal - grazing',' - walking',' - resting'),
col=c(colg,colw,colr,colg,colw,colr),
pch=c(pchv,pchv,pchv,pchg,pchg,pchg))




plot(cdf_valdres$ps,cdf_valdres$grazing,ylim=c(0,100),ylab='Percentage of grazing points',xlab='Minimum probability of correct classification',col=colg,pch=pchv, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
points(cdf_valdres$ps,cdf_valdres$resting,pch=pchv,col=colr)
points(cdf_valdres$ps,cdf_valdres$walking,pch=pchv,col=colw)

pchv=1
pchg=2
colg=1
colr=2
colw=3
plot(cdf_valdres$ps,cdf_valdres$grazing,ylim=c(0,100),ylab='Percentage of grazing points',xlab='Cumultative probability of correct classification',col=colg,pch=pchv, cex.lab=size, cex.axis=size, cex.main=size, cex.sub=size)
points(cdf_valdres$ps,cdf_valdres$resting,pch=pchv,col=colr)
points(cdf_valdres$ps,cdf_valdres$walking,pch=pchv,col=colw)

points(cdf_geilo$ps,cdf_geilo$grazing,pch=pchg,col=colg)
points(cdf_geilo$ps,cdf_geilo$resting,pch=pchg,col=colr)
points(cdf_geilo$ps,cdf_geilo$walking,pch=pchg,col=colw)


legend(0,50,c('Valdres - grazing',' - walking',' - resting','Hallingdal - grazing',' - walking',' - resting'),
col=c(colg,colw,colr,colg,colw,colr),
pch=c(pchv,pchv,pchv,pchg,pchg,pchg))



cdf_valdres=cumuldata('valdres','grazing')
cdf_valdres$grazing=cdf_valdres$perc
}




plotexample=function(x){
# x=runif(11,0,1)*c(1:11)
  plot(x,axes=FALSE,xlab='',ylab='')
  lines(c(1,11),c(x[1],x[11]),col='blue',lty='dotdash')
  lines(x)
  text(c(1:11)+0.2,x,c(1:11))
  legend(7.5,2,c('movement','displacement'),lty=c('solid','dotdash'),col=c('black','blue'))
}

qplotexample=function(x){
# x=runif(11,0,1)*c(1:11)
  df=data.frame(x=c(1:11),y=x,type='movement')
  df2=rbind(df,data.frame(x=c(1,11),y=c(df$y[1],df$y[11]),type='displacement'))
  plt=ggplot(df2,aes(x=x,y=y,linetype=type))+ geom_line() 
  plt0=plt+ theme_bw()
  plt=plt+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.border = element_blank(),
    panel.background = element_blank(),axis.line=element_blank(),axis.text=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position=c(0.1,0.9)) 
  plt=plt+geom_point()
  plt=plt+geom_text(data=df,aes(x=x,y=y+0.3,label=c(1:11)))
  # plt=plt+scale_colour_manual(breaks=c("Displacement","Movement"),values=c("red", "blue"))
  plt=plt+ theme(legend.key = element_blank())
  plt=plt+ theme(legend.text=element_text(size=12))
  plt=plt+theme(legend.title=element_blank())
 return(plt)
}


ggplotcumul=function(){
  if(!(exists('cdfm'))){
    cdfm<<-createcdf()
  }
  plt=ggplot(cdfm,aes(y=ps/100,x=value,colour=location,linetype=behaviour))+geom_line()
#  plt=plt+theme_bw()
  plt=plt+scale_colour_manual(values=c('black','#999999'))
  plt=plt+ylab("Cumultative probability of correct classification")+xlab("Percentage of points")
  return(plt)
}


arealist=function(lok){
  sql=paste("select shape_area from ",lok,"classified where shape_area>1", sep="")
  rs=dbSendQuery(con,statement=sql)
  ret=fetch(rs,n=-1)
  return(ret$shape_area)
}


plotareapercentille =function(v,g,col2="darkgray"){
  plot(quantile(v,ps),log="y",type='l',ylim=c(10,1000010),ylab="Area m2",xlab="Percentille",axes=FALSE)
  lines(quantile(g,ps),col=col2,lty="dotdash")
  legend(700,100,c("Valdres","Hallingdal"),lty=c('solid','dotdash'),col=c('black',col2))
  axis(1,at=c(0:10)*100,labels=c(0:10)*10)
  axis(2,at=c(10,100,1000,10000,100000,1000000),labels=c(10,100,1000,10000,100000,1000000))
}

ggplotarperc =function(col2="darkgray",nclass=1000){
  ps=c(1:nclass)/nclass
  al=arealist('Valdres')
  quant=data.frame(q=quantile(al,ps),n=ps*100,lok="Valdres")
  al=arealist('Geilo')
  quant=rbind(quant,data.frame(q=quantile(al,ps),n=ps*100,lok="Hallingdal"))
  #return(quant)
  plt=ggplot(quant,aes(x=n,y=q,linetype=lok,type=""))+geom_hline(y=c(50,500,5000,50000,500000),colour="white",size=0.2)+geom_line()
  plt=plt+ theme(legend.key = element_blank())
  plt=plt+ theme(legend.text=element_text(size=12))
  plt=plt+theme(legend.title=element_blank())
  plt=plt+scale_y_continuous(trans = 'log10', breaks=c(100,1000,10000,100000),labels=comma,minor_breaks=c(50,500,5000,50000,500000))
  plt=plt+xlab('Percentille')+ylab(expression(paste("Area (",m^2,")",sep="")))
  plt=plt+theme(legend.position=c(0.85,0.1))
  plt=plt+theme(axis.text=element_text(size=12,colour="black"))
  #plot(quantile(v,ps),log="y",type='l',ylim=c(10,1000010),ylab="Area m2",xlab="Percentille",axes=FALSE)
  #lines(quantile(g,ps),col=col2,lty="dotdash")
  #legend(700,100,c("Valdres","Hallingdal"),lty=c('solid','dotdash'),col=c('black',col2))
  #axis(1,at=c(0:10)*100,labels=c(0:10)*10)
  #axis(2,at=c(10,100,1000,10000,100000,1000000),labels=c(10,100,1000,10000,100000,1000000))
  return(plt)
}



  