plot_trace<-function(trace,
                     region=c(min(trace$datum), max(trace$datum)),
                     print.plot=T,
                     split=F,
                     maxed=F,
                     base.call=T,
                     ...){

  #region<-c(5000,5500)
  
  if(!require(ggplot2)){
    install.packages("ggplot2")}
  
  if(!require(reshape2)){
    install.packages("reshape2")}
  
melt.trace<-melt(data=trace,
                 measure.vars=c("A",
                                "T",
                                "C",
                                "G"))


trace.plot<-qplot(data=melt.trace,
                    x=datum,
                    y=value,
                    colour=variable,
                    geom="line",
                  xlim=region,
                  ...)+
                    scale_colour_manual(values=c("A"="#F8766D",
                                        "T"="#C77CFF",
                                        "C"="#7CAE00",
                                        "G"="#00BFC4",
                                        "N"="black"))
  
if(split){
  trace.plot<-trace.plot+facet_grid(.~variable)
}

if(maxed){trace.plot<-trace.plot+geom_line(aes(y=smooth.max,
                                               colour="maxed values"))}

if(base.call){trace.plot<-trace.plot+geom_text(aes(y=rep(2000,length(value)),
                                                   label=base.call,
                                                   colour=base.call,
                                                   alpha=peak),
                                               size=I(8))+
                                                     opts(legend.position = "none")+
                                                     scale_alpha_manual(values=c(0,1),guide="none")}
  
if(print.plot){
  print(trace.plot)
}

return(trace.plot)
}