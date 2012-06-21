#Align two traces using all base traces (A and B and C...)

#Currently uses exhaustive search of the parameter space
#lpSolve and Rglpk might be trails for non exhaustive searches

#Currently set up for the use of two cores

source("./R/pad_shift.R")
source("./R/blank_length.R")


align_trace<-function(trace_1, trace_2, silent=F,...){
  
  if(!require(NMOF)){install.packages("NMOF")}

  #Extract base information
  trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
  trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
  
  #Pad so that all traces have the same length
  lengthened<-blank_length(trace_1, trace_2, trace_3=trace_2)
  trace_1<-lengthened[[1]]
  trace_2<-lengthened[[2]]
  
  
  shift_optim<-function(shift, trace_1, trace_2){
    
    
    
    #Rounding may help focus on integers, but flattens the function between integers
#     shift<-round(shift)
    

    #Pad with assigned shifts
    #Shift trace 1 as function of 2 and 3
    padded<-pad.shift(shift, trace_1=trace_1, trace_2=trace_2, trace_3=trace_2)
    trace_1<-padded[[1]]
    trace_2<-padded[[2]]


    
    
    
    deviation<-sum((trace_1-trace_2)^2)
    
    return(deviation)
  }

  
  fit<-gridSearch(fun=shift_optim,
                  lower=-250,
                  upper=250,
                  npar=1,
                  n=501,
                  trace_1=trace_1,
                  trace_2=trace_2,
                  method="multicore",
                  mc.control = list(mc.cores=2))
  
  
  return(fit$minlevels)
  
  if(!silent){print(fit)}
  
}


#Test with adam data


  shift<-align_trace(trace_1, trace_2)
  shift
   shift<-align_trace(trace_1, trace_3)
   shift
   shift<-align_trace(trace_2, trace_3)
   shift

#As a sanity check:
shift<-align_trace(trace_2, trace_1)
shift