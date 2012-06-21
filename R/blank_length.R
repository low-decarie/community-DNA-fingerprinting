#Function to pad to equal length
blank_length<-function(trace_1, trace_2, trace_3){
  
  max.length<-max(length(trace_1[,1]), length(trace_2[,1]),length(trace_3[,1]))
  
  blank_pad<-function(trace, max.length){
    trace<-rbind(trace, data.frame(A=rep(0, max.length-length(trace[,1])),
                                   T=rep(0, max.length-length(trace[,1])),
                                   C=rep(0, max.length-length(trace[,1])),
                                   G=rep(0, max.length-length(trace[,1]))))
  }
  
  trace_1<-blank_pad(trace=trace_1, max.length=max.length)
  trace_2<-blank_pad(trace=trace_2, max.length=max.length)
  trace_3<-blank_pad(trace=trace_3, max.length=max.length)
  
  return(list(trace_1,trace_2,trace_3))
}