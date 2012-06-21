# adam_test_data<-list(A7_F,
#                      B7_F,
#                      AB7_F)
# 
# save("adam_test_data", file="./Outputs/adam_test_data.RData")

rm(list=ls())
setwd("~/Documents/PhD/Experiments/community-DNA-fingerprinting")
#Test with adam data
load("./Outputs/adam_test_data.RData")
trace_1<-adam_test_data[[1]]
trace_2<-adam_test_data[[2]]
trace_3<-adam_test_data[[3]]