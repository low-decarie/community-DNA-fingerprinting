####
# Get ratios
###
get_ratio<-function(trace){
  trace[,names(trace) %in% c("A","T","C","G")]<-trace[,names(trace) %in% c("A","T","C","G")]/trace$sum.value
  trace$A[trace$sum.value==0]<-0
  trace$T[trace$sum.value==0]<-0
  trace$C[trace$sum.value==0]<-0
  trace$G[trace$sum.value==0]<-0
  return(trace)
}