#Align using all base traces (A and B and C...)

align_trace<-function(trace_1, trace_2, silent=F){
  
  
  shift_optim<-function(shift, trace_1, trace_2){
    
    trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
    trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
    
    blank.shift<-data.frame(A=rep(0, abs(shift)),
                      T=rep(0, abs(shift)),
                      C=rep(0, abs(shift)),
                      G=rep(0, abs(shift)))
    
    blank.pad<-data.frame(A=rep(0, abs(length(trace_1[,1])-length(trace_2[,1]))),
                            T=rep(0, abs(length(trace_1[,1])-length(trace_2[,1]))),
                            C=rep(0, abs(length(trace_1[,1])-length(trace_2[,1]))),
                            G=rep(0, abs(length(trace_1[,1])-length(trace_2[,1]))))
    
    
    if(length(trace_1[,1])<length(trace_2[,1])){
      trace_1<-rbind(trace_1, blank.pad)
    }
    if(length(trace_1[,1])>length(trace_2[,1])){
      trace_2<-rbind(trace_2, blank.pad)
    }
    
    if(shift==0){
      trace_1<-trace_1
      trace_2<-trace_2
    }
    if(shift>0){
      trace_1<-rbind(trace_1, blank.shift )
      trace_2<-rbind(blank.shift, trace_2)
    }
    if(shift<0){
      trace_1<-rbind(blank.shift,trace_1)
      trace_2<-rbind(trace_2, blank.shift)
    }


    
    
    
    deviation<-sum((trace_1-trace_2)^2)
    
    return(deviation)
  }
  
  fit<-optimize(f=shift_optim,
                lower=-1000, 
                upper=1000, 
                trace_1=trace_1,
                trace_2=trace_2)
  
  return(fit$minimum)
  
  if(!silent){print(fit)}
  
}


#Test with adam data


  shift<-align_trace(trace_1=A7_F, trace_2=B7_F)
  shift
  shift<-align_trace(trace_1=A7_F, trace_2=AB7_F)
  shift
  shift<-align_trace(trace_1=B7_F, trace_2=AB7_F)
  shift