#####################
#find minimas
#function
get_min<-function(trace){
  #initilize slope and minima
  if(na.omit(trace)[1+1]<na.omit(trace)[1]) {slope<- -1} else {slope<-1}
  
  minima.group<-1
  minima<-rep(1, length(trace))
  
  for(i in 2:length(trace)){
    if(!is.na(trace[i]) & !is.na(trace[i-1])){
      if(trace[i]<trace[i-1] & slope==1){
        slope<- -1
        minima[i]<-minima.group
      } else if(trace[i]>=trace[i-1] & slope==-1){
        slope<-1
        minima.group<-minima.group+1
        minima[i]<-minima.group
      } else {minima[i]<-minima.group}
    }else {minima[i]<-minima.group}
    
  }
  return(minima)
}