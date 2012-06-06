get_bases<-function(trace, min.peak=500, progress="text", accuracy=10){
  
  if(!require(plyr)){
    install.packages("plyr")}
  if(!require(reshape2)){
    install.packages("reshape2")}
  
  trace$peak.group<-rep(1:(length(trace[,1])/accuracy), each=accuracy)
  
  melt.trace<-melt(data=trace,
                   id.vars=c("datum" ,"peak", "peak.group"))
  
  cast_trace<-function(melt.trace){
    melt.trace$type[melt.trace$variable %in% c("A","T","C","G")]<-"value"
    melt.trace$type[melt.trace$variable %in% c("A.smooth","T.smooth","C.smooth","G.smooth")]<-"smooth"
    melt.trace$type[melt.trace$variable %in% c("A.peak","T.peak","C.peak","G.peak")]<-"base.peak"
    melt.trace$variable<-substr(melt.trace$variable,1,1)
    
    cast.trace<-dcast(melt.trace,
                     formula=...~type)
    
    
    cast.trace$base.peak<-as.logical(cast.trace$base.peak)
    cast.trace$peak.group<-as.factor(cast.trace$peak.group)
    
    return(cast.trace)
  }
  
  cast.trace<-cast_trace(melt.trace)
  
  
  
  bases<-ddply(.data=cast.trace,
               .variables=c("variable", "peak.group"),
               peak.value=max(smooth),
               datum=median(datum),
               has.peak=any(peak),
               summarise,
               .progress="text")
  
   
  return(bases)
}