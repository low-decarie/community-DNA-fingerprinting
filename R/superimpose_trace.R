plot_trace<-function(trace_1,
                     trace_2,
                     trace_3,
                     region=c(1, length(trace[,1])),
                     print.plot=T,
                     split=F,
                     maxed=F,
                     base.call=T,
                     ...){
  
  
  
  region<-c(10000,11000)
  
  if(!require(ggplot2)){
    install.packages("ggplot2")}
  
  if(!require(reshape2)){
    install.packages("reshape2")}
  
  melt_trace<-function(trace){
  melt.trace<-melt(data=trace,
                   measure.vars=c("A",
                                  "T",
                                  "C",
                                  "G"))}
  
  melt.1<-melt_trace(trace_1)
  melt.2<-melt_trace(trace_2)
  melt.3<-melt_trace(trace_3)
  
  
  trace.plot<-qplot(data=melt.1,
                    x=datum,
                    y=value,
                    colour=variable,
                    geom="line",
                    xlim=region,
                    alpha=I(0.5))
  
  
  trace.plot<-trace.plot+geom_line(data=melt.2,
                                   aes(x=datum-185.7946,
                                       y=value),
                                   alpha=(0.5),
                                   linetype=I(2))
  
  trace.plot<-trace.plot+geom_line(data=melt.3,
                                   aes(x=datum+65.69755,
                                       y=value),
                                   alpha=(0.5),
                                   linetype=I(3))
  
  if(split){
    trace.plot<-trace.plot+facet_grid(.~variable)
  }
  
  if(maxed){trace.plot<-trace.plot+geom_line(aes(y=smooth.max,
                                                 colour="maxed values"))}
  
  if(base.call){trace.plot<-trace.plot+geom_text(aes(y=rep(0,length(value)),
                                                     label=base.call,
                                                     colour=base.call,
                                                     alpha=peak))+
                                                       opts(legend.position = "none")+
                                                       scale_alpha_manual(values=c(0,1),guide="none")}
  
  if(print.plot){
    print(trace.plot)
  }
  
  return(trace.plot)
}