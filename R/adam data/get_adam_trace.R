rm(list=ls())
setwd("~/Documents/PhD/Experiments/community-DNA-fingerprinting")
source("./R/plot_trace.R")
source("./R/get_trace.R")
source("./R/smooth_trace.R")
source("./R/get_peak.R")



#R
A7_R<-get_trace("./Data/Adam data/7A_R.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)
B7_R<-get_trace("./Data/Adam data/7B_R.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)
AB7_R<-get_trace("./Data/Adam data/7A+B_R.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)
#F
A7_F<-get_trace("./Data/Adam data/7A_F.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)
B7_F<-get_trace("./Data/Adam data/7B_F.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)
AB7_F<-get_trace("./Data/Adam data/7A+B_F.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)


Yeast<-get_trace("./Data/Yeast Data/mix_3051-3054-3058.ab1", width=c(5), type=c("Savitsky-Golay"),min.peak=500)



adam_test_data<-list(A7_R,
                     B7_R,
                     AB7_R,
                     A7_F,
                     B7_F,
                     AB7_F)



save("adam_test_data", file="./Outputs/adam_test_data.RData")
