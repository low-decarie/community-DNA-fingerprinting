#Scale two visualy aligned traces


#Save a temp of traces
temp_1<-trace_1  
temp_2<-trace_2
temp_3<-trace_3

#Get visually aligned region
# trace_1<-trace_1[5000:5500,]
# trace_2<-trace_2[4955:5455,]
# trace_3<-trace_3[4910:5410,]

# trace_1<-trace_1[6000:6500,]
# trace_2<-trace_2[5955:6455,]
# trace_3<-trace_3[5890:6390,]

trace_1<-trace_1[6000:6150,]
trace_2<-trace_2[5955:6105,]
trace_3<-trace_3[5890:6040,]



scale_fit<-function(trace_1,trace_2,trace_3){

#Remove minimum values
rm.low<-colwise(function(x){
  return(ifelse(x < 500, 0, x))})

rm.low.df<-function(trace){
  trace[,names(trace) %in% c("A","T","C","G")]<-rm.low(trace[,names(trace) %in% c("A","T","C","G")])
  return(trace)
}

trace_1<-rm.low.df(trace_1)
trace_2<-rm.low.df(trace_2)
trace_3<-rm.low.df(trace_3)




####
# Get ratios
###


get_ratio<-function(trace){
  summed<-apply(trace[,names(trace) %in% c("A","T","C","G")], MARGIN=1, FUN=sum)
  trace[,names(trace) %in% c("A","T","C","G")]<-trace[,names(trace) %in% c("A","T","C","G")]/summed
  trace$A[summed==0]<-0
  trace$T[summed==0]<-0
  trace$C[summed==0]<-0
  trace$G[summed==0]<-0
  return(trace)
}

trace_1<-get_ratio(trace_1)
trace_2<-get_ratio(trace_2)
trace_3<-get_ratio(trace_3)



scale_optim<-function(par, trace_1, trace_2, trace_3){
  #if(all(par[1]>0, par[1]<1, par[2]>0, par[2]<1)){
  scale_1<-par[1]
  scale_2<-par[2]
  
  ssq<-sum(((scale_1*trace_1+scale_2*trace_2)-trace_3)^2)
  
  return(ssq)#}else{
  #return(NA)
  # }
}


  # #Extract base trace information
  trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
  trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
  trace_3<-trace_3[,names(trace_3) %in% c("A","T", "C","G")]
  
  scale.fit<-optim(par=c(0.5, 0.5),
                   fn=scale_optim,
                   trace_1=trace_1,
                   trace_2=trace_2,
                   trace_3=trace_3,
                   method="L-BFGS-B",
                   lower=c(0,0),
                   upper=c(1,1),
                   control=list(maxit=50))
  
  return(scale.fit)}




scale.fit<-scale_fit(trace_1,trace_2,trace_3)

fit.com<-scale.fit$par[1]*trace_1[,names(trace_1) %in% c("A","T", "C","G")]+scale.fit$par[2]*trace_2[,names(trace_1) %in% c("A","T", "C","G")]

fit.com$datum<-1:nrow(fit.com)
trace_1$datum<-1:nrow(trace_1)
trace_2$datum<-1:nrow(trace_2)
trace_3$datum<-1:nrow(trace_3)

plot_trace(trace_3,
           #region=c(5000,10000),
           print.plot=F,
           main="Community trace",
           split=F,
           base.call=F)+opts(legend.position="none")

plot_trace(fit.com,
           #region=c(5000,10000),
           print.plot=F,
           main="Fit trace",
           split=F,
           base.call=F)+opts(legend.position="none")


trace_1<-temp_1
trace_2<-temp_2
trace_3<-temp_3