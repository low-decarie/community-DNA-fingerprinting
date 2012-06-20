#Align two traces using all base traces (A and B and C...)

align_trace<-function(trace_1, trace_2, silent=F,...){
  
  if(!require(DEoptim)){install.packages("DEoptim")}
  if(!require(NMOF)){install.packages("NMOF")}
  if(!require(pso)){install.packages("pso")}

  
  
  shift_optim<-function(shift, trace_1, trace_2){
    
    
    #Rounding may help focus on integers, but flattens the function between integers
    shift<-round(shift)
    
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
  
#Searching continuous space is not very good
#   fit<-optimize(f=shift_optim,
#                 lower=-1000, 
#                 upper=1000, 
#                 trace_1=trace_1,
#                 trace_2=trace_2,
#                 tol=3)
  
  
#   neighbour<-function(x,...){
#        x + sample(seq(-20,20), length(x), replace=TRUE)}
#   
#    fit<-optim(par=0,
#               f=shift_optim,
#               gr=neighbour,
#               method="SANN",
#                   trace_1=trace_1,
#                   trace_2=trace_2,
#               control=list(...))
  
  fit<-psoptim(par=0,
             fn=shift_optim,
             trace_1=trace_1,
             trace_2=trace_2,
             control=list(...))
  
  
  #It appears that local searches rapidly start turning in circles around non optimal values
#   neighbour<-function(x,...){
#     x + sample(seq(-100,100), length(x), replace=TRUE)}
#   
#   fit<-TAopt(OF=shift_optim,
#               trace_1=trace_1,
#               trace_2=trace_2,
#               algo =list(x0=c(0),
#                    neighbour=neighbour,
#                    nS=10000,
#                          ...))
  
  
  
  return(fit$par)
  
  if(!silent){print(fit)}
  
}


#Test with adam data


  shift<-align_trace(trace_1=A7_F, trace_2=B7_F,trace=2, maxit=200)
  shift
  shift<-align_trace(trace_1=A7_F, trace_2=AB7_F)
  shift
  shift<-align_trace(trace_1=B7_F, trace_2=AB7_F)
  shift