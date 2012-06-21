#Function to pad 0 with assigned shifts
pad.shift<-function(shift, trace_1, trace_2, trace_3){
  
  blank.shift<-data.frame(A=rep(0, abs(shift)),
                          T=rep(0, abs(shift)),
                          C=rep(0, abs(shift)),
                          G=rep(0, abs(shift)))
  
  
  if(shift>0){
    trace_1<-rbind(trace_1, blank.shift)
    trace_2<-rbind(blank.shift, trace_2)
    trace_3<-rbind(blank.shift, trace_3)
  }
  if(shift<0){
    trace_1<-rbind(blank.shift,trace_1)
    trace_2<-rbind(trace_2, blank.shift)
    trace_3<-rbind(trace_3, blank.shift)
  }
  return(list(trace_1, trace_2, trace_3))
}