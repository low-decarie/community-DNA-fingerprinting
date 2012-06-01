#This function extracts traces from .ab1 files
#performs smoothing
#find minima

get_trace<-function(file,type="Savitsky-Golay", width=10){

if(!require(seqinr)){
  install.packages("seqinr")}

#Load raw data
raw.trace<-read.abif(file, verbose=F)

#Extract trace data
clean.trace<-data.frame(datum=1:length(raw.trace$Data[["DATA.1"]]),
                        A=raw.trace$Data[["DATA.1"]],
                        T=raw.trace$Data[["DATA.2"]],
                        C=raw.trace$Data[["DATA.3"]],
                        G=raw.trace$Data[["DATA.4"]])


#Smooth the data
trace.smooth<-function(trace, type="Savitsky-Golay", width=10){
  
  if(type=="lowess"){
    smooth.trace<-with(clean.trace, lowess(x=1:length(trace),
                                   y=trace,
                                   f=width/length(trace),
                                   delta=width/2))$y
  }
  
  if(type=="moving-average"){
        moving_average<-function(width=10){
        moving.average<-rep(1,width)/width
        return(moving.average)
      }
      
      moving.average<-moving_average(width)
    
      smooth.trace<-filter(trace, moving.average)
        
  }
  
  if(type=="Savitsky-Golay"){
      # Savitsky-Golay smoothing function 
    savistsky_golay<-function(width=10){
      x<-1:width-width/2
      y<-max(x^2)-x^2
      sg<-y/sum(y)
      return(sg)
    }
      
    sg<-savistsky_golay(width)
    
      smooth.trace<-filter(trace, sg)
  }
  
  return(smooth.trace)
}


clean.trace<-data.frame(clean.trace,
                            A.smooth=trace.smooth(clean.trace$A,type, width),
                             T.smooth=trace.smooth(clean.trace$T,type, width),
                             C.smooth=trace.smooth(clean.trace$C,type, width),
                             G.smooth=trace.smooth(clean.trace$G,type, width))
                   
                 
#####################
#find minimas
#function
find.min<-function(trace){
  #initilize slope and minimas
  if(trace[1+1]<trace[1]) {slope<- -1} else {slope<-1}
  
  minima.group<-1
  minima<-rep(NA, length(trace))
  
  for(i in 2:length(trace)){
    if(trace[i]<trace[i-1] & slope==1){
      slope<- -1
      minima[i]<-minima.group
    } else if(trace[i]>=trace[i-1] & slope==-1){
      slope<-1
      minima.group<-minima.group+1
      minima[i]<-minima.group
      trace$minima[i]<-1
    } else {minima[i]<-minima.group}
    
  }
  return(trace)
}

##################


#Extract peak position
peak.position<-raw.trace$Data[["PLOC.2"]]


# peaks<-peakabif(abifdata=raw.trace,
#                 chanel=1,
#                 npeak=length(peak.position),
#                 fig=F)
                

#Add peak position to clean trace data
clean.trace$peak<-FALSE
clean.trace$peak[clean.trace$datum %in% peak.position]<-TRUE

return(clean.trace)
}

