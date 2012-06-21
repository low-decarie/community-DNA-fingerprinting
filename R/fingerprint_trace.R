#Align and scale two traces so they add up to a third
#Align using all base traces (A and B and C...)

source("./R/pad_shift.R")
source("./R/blank_length.R")

align_trace<-function(trace_1, trace_2, trace_3, trace_values=F){
  
  if(!require(NMOF))install.packages("NMOF")
  if(!require(parallel))install.packages("parallel")
  
  temp_1<-trace_1  
  temp_2<-trace_2
  temp_3<-trace_3
  
  #Extract base trace information
  trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
  trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
  trace_3<-trace_3[,names(trace_3) %in% c("A","T", "C","G")]
  
  #Pad so that all traces have the same length
  lengthened<-blank_length(trace_1, trace_2, trace_3)
  trace_1<-lengthened[[1]]
  trace_2<-lengthened[[2]]
  trace_3<-lengthened[[3]]
  
  
  #Define a scale optimization function
  scale_optim<-function(par, trace_1, trace_2, trace_3){
    scale_1<-par[1]
    scale_2<-par[2]
    
    ssq<-sum(((scale_1*trace_1+scale_2*trace_2)-trace_3)^2)
    
    return(ssq)
  }
  
  
#############################
  #Define a shift optimziation function that calls the scale optim internaly
  shift_optim<-function(par, trace_1, trace_2, trace_3){
    
    shift_1<-par[1]
    shift_2<-par[2]
    
    #############################
    
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
    
    #Fit the scale to the optimal alignement
    scale.fit<-optim(par=c(0.5, 0.5),
                     fn=scale_optim,
                     trace_1=trace_1,
                     trace_2=trace_2,
                     trace_3=trace_3)
                     
    
    return(scale.fit$value[1])
  }
  
  fit<-gridSearch(fun=shift_optim,
                  lower=-200,
                  upper=200,
                  npar=2,
                  n=11,
                  trace_1=trace_1,
                  trace_2=trace_2,
                  trace_3=trace_3,
                  method="multicore",
                  mc.control = list(mc.cores=1))
  
  shifts<-fit$minlevels
  shift_1<-shifts[1]
  shift_2<-shifts[2]
  
  #Use optimal values to create new trace vectore
  
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
  
  #Obtain the scale parameter on the best aligned sequences
  scale.fit<-optim(par=c(0.5, 0.5),
                   fn=scale_optim,
                   trace_1=trace_1,
                   trace_2=trace_2,
                   trace_3=trace_3)
  scale_1<-scale.fit$par[1]
  scale_2<-scale.fit$par[2]
  
  shift_n_scale<-data.frame(shift_1=shift_1,
                           shift_2=shift_2,
                           scale_1=scale_1,
                           scale_2=scale_2)
  
  #Scale
  trace_1<-scale_1*trace_1
  trace_2<-scale_2*trace_2
  
  #Add initial data
  trace_1<-merge(trace_1, temp_1,
                 all=T,
                 sort=F)
  trace_2<-merge(trace_2, temp_2,
                 all=T,
                 sort=F)
  trace_3<-merge(trace_3, temp_3,
                 all=T,
                 sort=F)
  
  traces<-list(trace_1,
               trace_2,
               trace_3)
  
  if(trace_values){
  print(shift_n_scale)
  return(traces)
  }else{
    return(shift_n_scale)
  }
                      
                      
  
  
}


#Test

shift<-align_trace(trace_1, trace_2, trace_3)