region<-c(10500,11000)

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

summed.trace<-trace_1+trace_2

trace_1$datum<-1:nrow(trace_1)
trace_2$datum<-1:nrow(trace_2)
trace_3$datum<-1:nrow(trace_3)

summed.trace$datum<-1:nrow(summed.trace)



melt.1<-melt_trace(trace_1)
melt.2<-melt_trace(trace_2)
melt.3<-melt_trace(trace_3)
summed.trace<-melt_trace(summed.trace)



trace.plot<-qplot(data=melt.3,
                  x=datum,
                  y=value,
                  colour=variable,
                  geom="line",
                  xlim=region,
                  alpha=I(0.5))


trace.plot<-trace.plot+geom_line(data=summed.trace,
                                 aes(x=datum,
                                     y=value),
                                 alpha=(0.5),
                                 linetype=I(2))

trace.plot


