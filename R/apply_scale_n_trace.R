source("./R/pad_shift.R")
source("./R/blank_length.R")

apply_shift_n_scale<-function(shift_1,shift_2, scale_1, scale_2,
                    trace_1, trace_2, trace_3){
  
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
  
  return(traces)
}



traces<-apply_shift_n_scale(70, 90, 0.4479, 0.501264,
                            trace_1, trace_2, trace_3)

trace_1<-traces[[1]]
trace_2<-traces[[2]]
trace_3<-traces[[3]]
  