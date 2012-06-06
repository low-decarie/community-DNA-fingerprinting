plot_trace<-function(trace,
                     region=c(1, length(trace[,1])),
                     peak.loc=T,
                     print.plot=T,
                     split=F,
                     smooth=F,
                     ...){

  if(!require(ggplot2)){
    install.packages("ggplot2")}
  
  if(!require(reshape2)){
    install.packages("reshape2")}


melt.trace<-melt(data=trace,
                   id.vars=c("datum" ,"peak", "peak.group"))
  
  cast_trace<-function(melt.trace){
    melt.trace$type[melt.trace$variable %in% c("A","T","C","G")]<-"value"
    melt.trace$type[melt.trace$variable %in% c("A.smooth","T.smooth","C.smooth","G.smooth")]<-"smooth"
    melt.trace$type[melt.trace$variable %in% c("A.peak","T.peak","C.peak","G.peak")]<-"base.peak"
    melt.trace$variable<-substr(melt.trace$variable,1,1)
    
    cast.trace<-dcast(melt.trace,
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


if(peak.loc){
    trace.plot<-trace.plot+geom_vline(aes(xintercept=datum,
                                          alpha=peak))
  }  
  
if(split){
  trace.plot<-trace.plot+facet_grid(.~variable)
}
  

  
if(print.plot){
  print(trace.plot)
}

return(trace.plot)
}