#Using Adam's data to plot out an example and test functions

rm(list=ls())
setwd("~/Documents/PhD/Experiments/community-DNA-fingerprinting")
source("./R/plot_trace.R")
source("./R/get_trace.R")
source("./R/smooth_trace.R")
source("./R/get_peak.R")


if(!require(gridExtra)){
  install.packages("gridExtra")}

A7_F<-get_trace("./Data/Adam data/7A_F.ab1", width=c(5), type=c("Savitsky-Golay"))
B7_F<-get_trace("./Data/Adam data/7B_F.ab1", width=c(5), type=c("Savitsky-Golay"))
AB7_F<-get_trace("./Data/Adam data/7A+B_F.ab1", width=c(5), type=c("Savitsky-Golay"))
  
pdf("./Plots/Adam_F_trace_plots.pdf")

base.call<-T
for(split in c(F,T)){
plot.A7_F<-plot_trace(A7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="A7_F",
                      split=split,
                      base.call=base.call)
plot.B7_F<-plot_trace(B7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="B7_F",
                      split=split,
                      base.call=base.call)
plot.AB7_F<-plot_trace(AB7_F,
                       region=c(5000,5500),
                       print.plot=F,
                       main="B7_F",
                       split=split,
                       base.call=base.call)

grid.arrange(plot.A7_F,
             plot.B7_F,
             plot.AB7_F)
}

bases_A7_F<-A7_F[A7_F$peak,]
bases_B7_F<-B7_F[B7_F$peak,]
bases_AB7_F<-AB7_F[AB7_F$peak,]

plot.bases<-function(bases){
qplot(data=bases,
      y=base.call.quality,
      x=datum,
      colour=base.call)
}

plot.bases(bases_A7_F)
plot.bases(bases_B7_F)
plot.bases(bases_AB7_F)
dev.off()