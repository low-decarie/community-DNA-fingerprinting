#Scale two visualy aligned traces

#Save a temp of traces
temp_1<-trace_1  
temp_2<-trace_2
temp_3<-trace_3

#Get visually aligned region
# trace_1<-trace_1[5008:5508,]
# trace_2<-trace_2[4961:5461,]
# trace_3<-trace_3[4910:5410,]

# trace_1<-trace_1[6004:6504,]
# trace_2<-trace_2[5951:6451,]
# trace_3<-trace_3[5890:6390,]

trace_1<-trace_1[6001:6151,]
trace_2<-trace_2[5951:6101,]
trace_3<-trace_3[5890:6040,]

trace_1<-trace_1[(start+shift[1]):(end+shift[1]),]
trace_2<-trace_2[(start-50+shift[2]):(end-50+shift[2]),]
trace_3<-trace_3[(start-111):(end-111),]






scale_fit<-function(trace_1,trace_2,trace_3){

# source("./R/rm_low.R")

# trace_1<-rm.low(trace_1)
# trace_2<-rm.low(trace_2)
# trace_3<-rm.low(trace_3)




source("./R/get_ratio.R")

trace_1<-get_ratio(trace_1)
trace_2<-get_ratio(trace_2)
trace_3<-get_ratio(trace_3)



source("./R/align_trace_3.R")

shift<-align_trace(trace_1, trace_2,trace_3)
shift



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

pdf("./Plots/scale trace segment ratio.pdf")
source("./R/plot_trace.R")
plot.trace_1<-plot_trace(trace_1,
           #region=c(5000,10000),
           print.plot=F,
           main="Trace 1",
           split=F,
                         ylab="Ratio",
           base.call=F)+opts(legend.position="none")

plot.trace_2<-plot_trace(trace_2,
           #region=c(5000,10000),
           print.plot=F,
           main="Trace 2",
           split=F,
                         ylab="Ratio",
           base.call=F)+opts(legend.position="none")

plot.trace_3<-plot_trace(trace_3,
           #region=c(5000,10000),
           print.plot=F,
           main="Community trace",
           split=F,
                         ylab="Ratio",
           base.call=F)+opts(legend.position="none")

fit.plot<-plot_trace(fit.com,
                     #region=c(5000,10000),
                     print.plot=F,
                     main="Fit (scaled sum of trace 1 and trace 2)",
                     split=F,
                     ylab="Ratio",
                     base.call=F)+opts(legend.position="none")

grid.arrange(plot.trace_1,
             plot.trace_2,
             plot.trace_3,
             fit.plot)



dev.off()
trace_1<-temp_1
trace_2<-temp_2
trace_3<-temp_3

# 
# fit.com<-fit.com[,names(fit.com) %in% c("A","T", "C","G")]
# 
# melt.fit.com<-melt(data=fit.com,
#                  measure.vars=c("A",
#                                 "T",
#                                 "C",
#                                 "G"))
# 
# melt.trace_3<-melt(data=trace_3,
#                    measure.vars=c("A",
#                                   "T",
#                                   "C",
#                                   "G"))
# 
# melt.fit.com$original<-melt.trace_3$value
# 
# qplot(data=melt.fit.com,
#       y=original,
#       x=value,
#       colour=variable,
#       xlab="Community",
#       ylab="Fit")+geom_smooth(method="lm", se=F)+
#         opts(legend.position="none")+
#         geom_abline(intercept = 0, slope = 1) 
# 
# 
# fit<-lm(data=melt.fit.com, value~original)
# summary(fit)
