plot_trace<-function(trace,
                     region=c(1, length(trace[,1])),
                     peak.loc=F,
                     print.plot=T,
                     split=F,
                     smooth=F,
                     min=F,
                     ...){

  if(!require(ggplot2)){
    install.packages("ggplot2")}

  if(!require(reshape)){
    install.packages("reshape")}
  
  if(!require(reshape2)){
    install.packages("reshape2")}


melt.trace<-melt(data=trace,
                   id.vars=c("datum"))
  
  cast_trace<-function(melt.trace){
    melt.trace$type[melt.trace$variable %in% c("A","T","C","G")]<-"value"
    melt.trace$type[melt.trace$variable %in% c("A.smooth","T.smooth","C.smooth","G.smooth")]<-"smooth"
    melt.trace$variable<-substr(melt.trace$variable,1,1)
    
    cast.trace<-cast(melt.trace,
                     formula=...~type)
    
    
    
    return(cast.trace)
  }

cast.trace<-cast_trace(melt.trace)
  
  
plot.value<-"value"
if(smooth){plot.value<-"smooth"}

trace.plot<-qplot(data=cast.trace,
                    x=datum,
                    y=get(plot.value),
                    colour=variable,
                    geom="line",
                  xlim=region,
                  ylab=plot.value,
                  ...)

  
if(split){
  trace.plot<-trace.plot+facet_grid(.~variable)
}
  
if(peak.loc){
#   trace.plot<-trace.plot+geom_vline(xintercept=unique(melt.trace$datum[melt.trace$peak]),
#                                     alpha=I(0.1))
}
  
if(print.plot){
  print(trace.plot)
}

return(trace.plot)
}