get_bases<-function(trace, min.peak=500, progress="text", round=TRUE, accuracy=5){
  
  if(!require(plyr)){
    install.packages("plyr")}
  
  
  ##################
  
#   #Create minima groups for each base
#   trace<-data.frame(trace,
#                     A.min=get_min(trace$A.smooth),
#                     T.min=get_min(trace$T.smooth),
#                     C.min=get_min(trace$C.smooth),
#                     G.min=get_min(trace$G.smooth))
  
  trace<-B7_F
  
  #Peak detection
  trace<-data.frame(trace,
                    A.peak=get_peak(trace$A.smooth, round, accuracy),
                    T.peak=get_peak(trace$T.smooth, round, accuracy),
                    C.peak=get_peak(trace$C.smooth, round, accuracy),
                    G.peak=get_peak(trace$G.smooth, round, accuracy))
  
  
  trace$peak<-with(trace, A.peak|T.peak|C.peak|G.peak)
  
  #tester plot
  melt.trace<-melt(data=trace,
                   id.vars=c("datum","peak"))
  
  cast_trace<-function(melt.trace){
    melt.trace$type[melt.trace$variable %in% c("A","T","C","G")]<-"value"
    melt.trace$type[melt.trace$variable %in% c("A.smooth","T.smooth","C.smooth","G.smooth")]<-"smooth"
    #melt.trace$type[melt.trace$variable %in% c("A.min","T.min","C.min","G.min")]<-"min"
    melt.trace$type[melt.trace$variable %in% c("A.peak","T.peak","C.peak","G.peak")]<-"base.peak"
    melt.trace$variable<-substr(melt.trace$variable,1,1)
    
    cast.trace<-cast(melt.trace,
                     formula=...~type)
    
    cast.trace$base.peak<-as.logical(cast.trace$base.peak)
    
    cast.trace$group<-rep(1:length(cast.trace[,1])/
    
    return(cast.trace)
  }
  
  
  cast.trace<-cast_trace(melt.trace)
  qplot(data=cast.trace, x=datum, y=smooth, colour=variable, geom="line", xlim=c(5000, 5100))+geom_vline(aes(xintercept=datum, alpha=peak))
  
  
#   #Define minima groups taking into account values for each base
#   trace$sum.min<-with(trace, A.min+T.min+C.min+G.min)
#   
#   melt.trace<-melt(data=trace,
#                    id.vars=c("datum","sum.min"))
#   cast.trace<-cast_trace(melt.trace)
#   
#   #Determine which minima groups define a peak
#   cast.trace<-ddply(.data=cast.trace,
#                     .variables=c("sum.min", "variable"),
#                     is.peak=max(smooth)>min.peak,
#                     transform,
#                     .progress=progress)
#   
#   #Remove minima groups that do not contain a peak
#   cast.trace<-cast.trace[cast.trace$is.peak,]
#   
#                             
#   #Extract peak area
#   #Note: other function may be better than area (eg. mean elevation)
#   bases<-ddply(.data=cast.trace,
#               .variables=c("sum.min", "variable"),
#               peak.area=sum(smooth),
#               transform,
#               .progress=progress)
#   
  #tester plot
  #qplot(data=bases[5000:5300,], x=datum, y=peak.area, colour=variable, geom="line")+geom_line(aes(y=smooth))+facet_grid(.~variable)
  
  return(bases)
}