#Align 3 traces simultaneously
#Align using all base traces (A and B and C...)

source("./R/pad_shift.R")
source("./R/blank_length.R")


align_trace<-function(trace_1, trace_2, trace_3, silent=F, max.shift=10){
  
  if(!require(NMOF)){install.packages("NMOF")}
  
  #Extract base trace information
  trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
  trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
  trace_3<-trace_3[,names(trace_3) %in% c("A","T", "C","G")]
  
  #Pad so that all traces have the same length
  lengthened<-blank_length(trace_1, trace_2, trace_3)
  trace_1<-lengthened[[1]]
  trace_2<-lengthened[[2]]
  trace_3<-lengthened[[3]]
  
  
  shift_optim<-function(par, trace_1, trace_2, trace_3){
    
    shift_1<-par[1]
    shift_2<-par[2]
    
      
    #Pad with assigned shifts
    #Shift trace 1 as function of 2 and 3
    padded_1<-pad.shift(shift_1, trace_1, trace_2, trace_3)
    trace_1<-padded_1[[1]]
    trace_2<-padded_1[[2]]
    trace_3<-padded_1[[3]]
    #Shift trace 2 as a function of 1 and 3
    padded_2<-pad.shift(shift_2, trace_2, trace_1, trace_3)
    trace_1<-padded_2[[1]]
    trace_2<-padded_2[[2]]
    trace_3<-padded_2[[3]]
      
    deviation<-sum((trace_1-trace_2)^2, (trace_1-trace_3)^2, (trace_2-trace_3)^2)
      
    return(deviation)
  }
  
  fit<-gridSearch(fun=shift_optim,
                  lower=-max.shift,
                  upper=max.shift,
                  npar=2,
                  n=length(-max.shift:max.shift),
                  trace_1=trace_1,
                  trace_2=trace_2,
                  trace_3=trace_3,
                  method="multicore",
                  mc.control = list(mc.cores=2))
  
  
  return(fit$minlevels)
  
  
}


#Test with adam data


#shift<-align_trace(trace_1, trace_2, trace_3)