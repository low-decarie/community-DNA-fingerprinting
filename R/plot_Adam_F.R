setwd("~/Documents/PhD/Experiments/community-DNA-fingerprinting")
source("./R/plot_trace.R")
source("./R/get_trace.R")

require(gridExtra)

A7_F<-get_trace("./Data/Adam data/7A_F.ab1")
B7_F<-get_trace("./Data/Adam data/7B_F.ab1")
AB7_F<-get_trace("./Data/Adam data/7A+B_F.ab1")

plot.A7_F<-plot_trace(A7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="A7_F",
                      peak.loc=T)
plot.B7_F<-plot_trace(B7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="B7_F",
                      peak.loc=T)
plot.AB7_F<-plot_trace(AB7_F,
                       region=c(5000,5500),
                       print.plot=F,
                       main="AB7_F",
                       peak.loc=T)

grid.arrange(plot.A7_F,
             plot.B7_F,
             plot.AB7_F)

plot.A7_F<-plot_trace_split(A7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="A7_F",
                      peak.loc=T)
plot.B7_F<-plot_trace_split(B7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="B7_F",
                      peak.loc=T)
plot.AB7_F<-plot_trace_split(AB7_F,
                       region=c(5000,5500),
                       print.plot=F,
                       main="AB7_F",
                       peak.loc=T)

grid.arrange(plot.A7_F,
             plot.B7_F,
             plot.AB7_F)