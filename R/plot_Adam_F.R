setwd("~/Documents/PhD/Experiments/community-DNA-fingerprinting")
source("./R/plot_trace.R")
source("./R/get_trace.R")

if(!require(gridExtra)){
  install.packages("gridExtra")}

A7_F<-get_trace("./Data/Adam data/7A_F.ab1", width=15, type="Savitsky-Golay")
B7_F<-get_trace("./Data/Adam data/7B_F.ab1", width=15, type="Savitsky-Golay")
AB7_F<-get_trace("./Data/Adam data/7A+B_F.ab1", width=15, type="Savitsky-Golay")


pdf("./Plots/Adam_F_trace_plots.pdf")

for(smooth in c(F,T)){
for(split in c(F,T)){
plot.A7_F<-plot_trace(A7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="A7_F",
                      peak.loc=F,
                      split=split,
                      smooth=smooth)
plot.B7_F<-plot_trace(B7_F,
                      region=c(5000,5500),
                      print.plot=F,
                      main="B7_F",
                      peak.loc=F,
                      split=split,
                      smooth=smooth)
plot.AB7_F<-plot_trace(AB7_F,
                       region=c(5000,5500),
                       print.plot=F,
                       main="AB7_F",
                       peak.loc=F,
                       split=split,
                       smooth=smooth)

grid.arrange(plot.A7_F,
             plot.B7_F,
             plot.AB7_F)
}
}

dev.off()