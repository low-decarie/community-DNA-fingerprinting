plot_trace_split<-function(trace,
                     region=c(1, length(trace[,1])),
                     peak.loc=F,
                     print.plot=T,
                     ...){
  
  if(!require(ggplot2)){
    install.packages("ggplot2")}
  
  if(!require(reshape2)){
    install.packages("reshape2")}
  
  
  melt_trace<<-function(trace){
    melt(data=trace,
         id.vars=c("datum", "peak"))}
  
  melt.trace<-melt_trace(trace)
  
  trace.plot<-qplot(data=melt.trace,
                    x=datum,
                    y=value,
                    facets=~variable,
                    geom="line",
                    xlim=region,
                    ...)
  
  if(peak.loc){
    trace.plot<-trace.plot+geom_vline(xintercept=unique(melt.trace$datum[melt.trace$peak]),
                                      alpha=I(0.1))
  }
  
  if(print.plot){
    print(trace.plot)
  }
  
  return(trace.plot)
}