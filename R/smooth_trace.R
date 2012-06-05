#Smoothing function
#Smoothers can be applied sequentially
#type=c("lowess", "moving-average", "Savitsky-Golay")
#

smooth_trace<<-function(trace, type="Savitsky-Golay", width=10){
  
  smooth.trace<-trace
  if(length(width)!=length(type)){width<-rep(width[1], length(type))}
  
  for(type.temp in type){
  #lowess smoothing
  if(type.temp=="lowess"){
    smooth.trace<-lowess(x=1:length(trace),
                         y=trace,
                         f=width[match(type.temp, type)]/length(trace),
                         delta=width[match(type.temp, type)]/2)$y
  }
  
  
  #moving-average smoothing
  if(type.temp=="moving-average"){
    moving_average<-function(width=10){
      moving.average<-rep(1,width)/width
      return(moving.average)
    }
    
    moving.average<-moving_average(width[match(type.temp, type)])
    
    smooth.trace<-filter(trace, moving.average)
    
  }
  
  
  #Savitsky-Golay smoothing
  #http://en.wikipedia.org/wiki/Savitzky–Golay_smoothing_filter
  
  if(type.temp=="Savitsky-Golay"){
    # Savitsky-Golay smoothing function 
    savistsky_golay<-function(width=10){
      x<-1:width-width/2
      y<-max(x^2)-x^2
      sg<-y/sum(y)
      return(sg)
    }
    
    sg<-savistsky_golay(width[match(type.temp, type)])
    
    smooth.trace<-filter(trace, sg)
  }
  }
  return(smooth.trace)
}