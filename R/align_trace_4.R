#Align using a full exploration of the parameter space (not a stochastic/Newton process)
#Align 3 traces simultaneously
#Align using all base traces (A and B and C...)

align_trace<-function(trace_1,
                      trace_2, 
                      trace_3, 
                      shift.space=data.frame(shift_1=rep(-500:500, 1000), shift_2=rep(-500:500, each=1000)),
                      silent=F,
                      return.deviation=F,
                      ...){
  
  if(!require(plyr)){install.packages("plyr")}
  
  
  shift_optim<-function(shift_1,shift_2, trace_1, trace_2, trace_3){
    
    check.integer <- function(N){
      !length(grep("[^[:digit:]]", as.character(N)))
    }
    
    if(all(check.integer(abs(shift_1)), check.integer(abs(shift_2)))){
      
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
      
      #shift_1 moves trace_1 compared to both 2 and 3
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
      
      
      #shift_2 moves trace_2 compared to trace 1 and 3
      if(shift_2>0){
        trace_2<-rbind(trace_2, blank.shift.2 )
        trace_1<-rbind(blank.shift.2, trace_1)
        trace_3<-rbind(blank.shift.2, trace_3)
      }
      if(shift_2<0){
        trace_2<-rbind(blank.shift.2,trace_2)
        trace_1<-rbind(trace_1, blank.shift.2)
        trace_3<-rbind(trace_3, blank.shift.2)
      }
      
      
      
      deviation<-sum((trace_1-trace_2)^2, (trace_1-trace_3)^2, (trace_2-trace_3)^2)
      
      
    }else{
      deviation<-NA
    } 
    return(deviation)
  }

  
  shift.space$index<-1:length(shift.space[,1])
  shift.space<-idata.frame(shift.space)
  shift.deviations<-ddply(.data=shift.space,
                          .variables="index",
                          deviation=shift_optim(shift_1,
                                      shift_2,
                                      trace_1,
                                      trace_2,
                                      trace_3),
                          transform,
                          ...)
  
  
  selected.shift<-shift.deviations[shift.deviations$deviation<=min(shift.deviations$deviation),
                                   c("shift_1","shift_2")]
  
  
  
  
  
  if(return.deviation){return(shift.deviations)}else{
  return(selected.shift)}
  
  if(!silent){print(selected.shift)}
  
}


#Test with adam data


parallel<-T


#

if(parallel){
  if(!require(doMC)){install.packages("doMC")}
  registerDoMC(2)
  shift<-align_trace(trace_1=A7_F, trace_2=B7_F, trace_3=AB7_F, .parallel=T)
}else{
  shift<-align_trace(trace_1=A7_F, trace_2=B7_F, trace_3=AB7_F, .progress="text")
}


qplot(data=shift.deviations,
      x=shift_1,
      y=shift_2,
      fill=deviation,
      geom="tile")