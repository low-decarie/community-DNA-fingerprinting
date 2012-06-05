#####################
#find peak
#function
get_peak<-function(trace, rm.min=TRUE, min.peak=500, round=TRUE, accuracy=5){
  
  #initilize slope and peak
  if(na.omit(trace)[1+1]<na.omit(trace)[1]) {slope<- -1} else {slope<-1}
  peak<-rep(FALSE, length(trace))
  
  #start at second datum
  for(i in 2:length(trace)){
    #bypass NA values
    if(!is.na(trace[i]) & !is.na(trace[i-1])){
      
      #detect downward change in slope (ie. peak)
      if(trace[i]<trace[i-1] & slope==1){
        slope<- -1
        peak[i]<-TRUE
        
      #detect upward change in slope
      } else if(trace[i]>=trace[i-1] & slope==-1){
        slope<-1
        peak[i]<-FALSE
      } else {peak[i]<-FALSE}
    }else {peak[i]<-FALSE}
    
  }
  if(rm.min){
    peak[trace<min.peak]<-FALSE
  }
  
  if(round){
  peak.temp<-rep(FALSE,length(peak))
  peak.temp[floor((1:length(peak))[peak]/accuracy)*accuracy]<-TRUE
  peak<-peak.temp
  }
  
  
  return(peak)
}