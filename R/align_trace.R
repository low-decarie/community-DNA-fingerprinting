#Align using only one base trace (A or T or...)

align_trace<-function(trace_1, trace_2, silent=F){
  
  
  shift_optim<-function(shift, trace_1, trace_2){
    
    if(length(trace_1)>length(trace_2)){
      trace_2<-c(trace_2, rep(0, length(trace_1)-length(trace_2)))
    }
    if(length(trace_1)<length(trace_2)){
      trace_1<-c(trace_1, rep(0, length(trace_2)-length(trace_1)))
    }
    
    if(shift==0){
      trace_1<-trace_1
      trace_2<-trace_2
    }
    if(shift>0){
      trace_1<-c(trace_1, rep(0, abs(shift)))
      trace_2<-c(rep(0, abs(shift)), trace_2)
    }
    if(shift<0){
      trace_1<-c(rep(0, abs(shift)),trace_1)
      trace_2<-c(trace_2, rep(0, abs(shift)))
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

for(i in c("A","T", "C","G")){
  shift<-align_trace(trace_1=B7_F[,i], trace_2=AB7_F[,i])
  print(shift)
}
  