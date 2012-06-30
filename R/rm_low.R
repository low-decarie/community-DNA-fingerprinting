#Remove minimum values
rm.low<-function(trace, progress="text"){
  trace<-ddply(.data=trace,
               .variables="datum",
               function(x){
                 if(x$sum.value<500){
                   x[,names(x) %in% c("A","T","C","G")]<-c(0,0,0,0)}
                 return(x)},
               .progress=progress)}




# #This currently removes all values below 500 even when the sum is above 500
# rm.low<-colwise(function(x){
#   return(ifelse(x < 500, 0, x))})
# 
# rm.low.df<-function(trace){
#   trace[,names(trace) %in% c("A","T","C","G")]<-rm.low(trace[,names(trace) %in% c("A","T","C","G")])
#   return(trace)
# }