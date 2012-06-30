if(!require(gridExtra)){
  install.packages("gridExtra")}


pdf("./Plots/Adam_trace_plots.pdf")

base.call<-T
split<-F
for(split in c(F,T)){
plot.trace_1<-plot_trace(trace_1,
                      region=c(6000,6150),
                      print.plot=F,
                      main="trace_1",
                      split=split,
                      base.call=base.call,
                         ylim=c(0,2050))
plot.trace_2<-plot_trace(trace_2,
                      region=c(5950,6100),
                      print.plot=F,
                      main="trace_2",
                      split=split,
                      base.call=base.call,
                         ylim=c(0,2050))
plot.trace_3<-plot_trace(trace_3,
                       region=c(5000,10000),
                       print.plot=F,
                       main="trace_3",
                       split=split,
                       base.call=F,
                         ylim=c(0,2050))

grid.arrange(plot.trace_1,
             plot.trace_2,
             plot.trace_3)
}

bases_trace_1<-trace_1[trace_1$peak,]
bases_trace_2<-trace_2[trace_2$peak,]
bases_trace_3<-trace_3[trace_3$peak,]

plot.bases<-function(bases){
qplot(data=bases,
      y=base.call.quality,
      x=datum,
      colour=base.call)
}

plot.bases(bases_trace_1)
plot.bases(bases_trace_2)
plot.bases(bases_trace_3)
dev.off()