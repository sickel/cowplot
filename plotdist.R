

distance=function(data,delta){
  dists=c()
  l=length(data$x)
  for (i in (delta+1:l)){
    dists[i]=sqrt((data$x[i]-data$x[i-delta])^2+(data$y[i]-data$y[i-delta])^2)
  }
  dists=dists[1:l]
  return(dists)
}

travel=function(dists,delta){
  l=length(dists)
  trav=c()  
  for (i in (delta+1:l)){
      trav[i]=sum(dists[(i-delta):i])
  }
  trav=trav[1:l]
  return(trav)
}


distplot=function(data,delta){
  dists5s=distance(data,1)
  # plot(data$datetime,dists5s,type='l',ylim=c(0,100))
  dists=distance(data,delta)
  plot(data$datetime,dists,col="1",type='l')
  trav=travel(dists5s,delta)
  lines(data$datetime,trav,col="2")
  /*
	Burde satt navn som dists<delta> og trav<delta> - dermed er det mulig å
	kjøre flere ganger med samme datasett og forskjellige delta-verdier
  */
  data$dists=dists
  data$trav=trav
  invisible(data)
}

fectchdata=function(cowid,date){
  sql=paste('select * from gps_coord where cowid=',cowid," and date='",date,"'",sep='')
  rs=dbSendQuery(con,statement=sql)
  data=fetch(rs,n=-1)
  return(data)	
}




