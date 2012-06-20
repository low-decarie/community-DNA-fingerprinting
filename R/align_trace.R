#Align two traces using only one base trace (A or T or...)

#Note that the optimal shift for each base appears to be different

align_trace<-function(trace_1, trace_2, silent=F){
  
  
  shift_optim<-function(shift, trace_1, trace_2){
    
    #Rounding may help focus on integers, but flattens the function between integers
    shift<-round(shift)
    
    
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

  #Searching over continuous space is not good!
  fit<-optimize(f=shift_optim,
                lower=-1000, 
                upper=1000, 
                trace_1=trace_1,
                trace_2=trace_2,
                tol=3)  #As we do not need steps below integer, we can use large tollerance
  
  neighbour<-function(x,...){
    x + sample(seq(-10,10), length(x), replace=TRUE)}
  
  fit<-LSopt(OF=shift_optim,
             trace_1=trace_1,
             trace_2=trace_2,
             algo =list(x0=c(0),
                        neighbour=neighbour,
                        nS=10000,
                        printBar=T))
  

  return(fit$minimum)
  
  if(!silent){print(fit)}
                
}


#Test with adam data

for(i in c("A","T", "C","G")){
  shift<-align_trace(trace_1=A7_F[,i], trace_2=B7_F[,i])
  print(shift)
}

for(i in c("A","T", "C","G")){
  shift<-align_trace(trace_1=A7_F[,i], trace_2=AB7_F[,i])
  print(shift)
}

for(i in c("A","T", "C","G")){
  shift<-align_trace(trace_1=B7_F[,i], trace_2=AB7_F[,i])
  print(shift)
}

  