#Align 3 traces simultaneously
#Align using all base traces (A and B and C...)

align_trace<-function(trace_1, trace_2, trace_3, silent=F){
  
  
  shift_optim<-function(par, trace_1, trace_2, trace_3){
    
    shift_1<-par[1]
    shift_2<-par[2]
    
  #Extract base trace information
    trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
    trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
    trace_3<-trace_3[,names(trace_3) %in% c("A","T", "C","G")]
    

    
    
#Pad so that all traces have the same length
    max.length<-max(length(trace_1[,1]), length(trace_2[,1]),length(trace_3[,1]))
    
    blank_pad<-function(trace, max.length){
      
    trace<-rbind(trace, data.frame(A=rep(0, max.length-length(trace[,1])),
                          T=rep(0, max.length-length(trace[,1])),
                          C=rep(0, max.length-length(trace[,1])),
                          G=rep(0, max.length-length(trace[,1]))))}
    
    trace_1<-blank_pad(trace=trace_1, max.length=max.length)
    trace_2<-blank_pad(trace=trace_2, max.length=max.length)
    trace_3<-blank_pad(trace=trace_3, max.length=max.length)
    
#############################

#Pad with assigned shifts
    blank.shift.1<-data.frame(A=rep(0, abs(shift_1)),
                              T=rep(0, abs(shift_1)),
                              C=rep(0, abs(shift_1)),
                              G=rep(0, abs(shift_1)))
    blank.shift.2<-data.frame(A=rep(0, abs(shift_2)),
                              T=rep(0, abs(shift_2)),
                              C=rep(0, abs(shift_2)),
                              G=rep(0, abs(shift_2)))

    if(shift_1>0){
      trace_1<-rbind(trace_1, blank.shift.1)
      trace_2<-rbind(blank.shift.1, trace_2)
      trace_3<-rbind(blank.shift.1, trace_3)
    }
    if(shift_1<0){
      trace_1<-rbind(blank.shift.1,trace_1)
      trace_2<-rbind(trace_2, blank.shift.1)
      trace_3<-rbind(trace_3, blank.shift.1)
    }
    
    if(shift_2>0){
      trace_1<-rbind(trace_2, blank.shift.2 )
      trace_2<-rbind(blank.shift.2, trace_1)
      trace_3<-rbind(blank.shift.2, trace_3)
    }
    if(shift_2<0){
      trace_1<-rbind(blank.shift.2,trace_2)
      trace_2<-rbind(trace_1, blank.shift.2)
      trace_3<-rbind(trace_3, blank.shift.2)
    }
    

    
    
    
    
    deviation<-sum((trace_1-trace_2)^2, (trace_1-trace_3)^2, (trace_2-trace_3)^2)
    
    return(deviation)
  }
  
  fit<-optim(par=c(0,0),
              fn=shift_optim,
                trace_1=trace_1,
                trace_2=trace_2,
              trace_3=trace_3)
  
  return(fit)
  
  if(!silent){print(fit)}
  
}


#Test with adam data


shift<-align_trace(trace_1=A7_F, trace_2=B7_F, trace_3=AB7_F)