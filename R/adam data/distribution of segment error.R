align_n_scale<-function(trace_1, trace_2, trace_3, start, segment.size=300){

  end<-start+segment.size
  range_1<-start:end
  range_2<-(start-50):(end-50)
  range_3<-(start-110):(end-110)
  
#Save a temp of traces
  temp_1<-trace_1  
  temp_2<-trace_2
  temp_3<-trace_3
  
#Get visually aligned region
  trace_1<-temp_1[range_1,]
  trace_2<-temp_1[range_2,]
  trace_3<-temp_3[range_3,]

  #Get ratio
  source("./R/get_ratio.R")
    trace_1<-get_ratio(trace_1)
    trace_2<-get_ratio(trace_2)
    trace_3<-get_ratio(trace_3)
    
  
  #Get alignement
  source("./R/align_trace_3.R")
  shift<-align_trace(trace_1, trace_2,trace_3, max.shift=10)
#   print("Shifts")
#   print(shift)
  
  range_1<-range_1+shift[1]
  range_2<-range_2+shift[2]
  
  trace_1<-temp_1[range_1,]
  trace_2<-temp_2[range_2,]
  trace_3<-temp_3[range_3,]
  

  
  #Get ratio
  source("./R/get_ratio.R")
  trace_1<-get_ratio(trace_1)
  trace_2<-get_ratio(trace_2)
  trace_3<-get_ratio(trace_3)
  
  
  scale_optim<-function(par, trace_1, trace_2, trace_3){

    scale_1<-par[1]
    scale_2<-par[2]
    
    ssq_err<-sum(((scale_1*trace_1+scale_2*trace_2)-trace_3)^2)
    ssq_tot<-sum((trace_3-mean(sapply(trace_3, mean))^2))
                 
    r_sq <- 1-ssq_err/ssq_tot
    
    return(ssq_err)

  }
  
  
  # #Extract base trace information
  trace_1<-trace_1[,names(trace_1) %in% c("A","T", "C","G")]
  trace_2<-trace_2[,names(trace_2) %in% c("A","T", "C","G")]
  trace_3<-trace_3[,names(trace_3) %in% c("A","T", "C","G")]
  
  scale.fit<-optim(par=c(0.5, 0.5),
                   fn=scale_optim,
                   trace_1=trace_1,
                   trace_2=trace_2,
                   trace_3=trace_3,
                   method="L-BFGS-B",
                   lower=c(0,0),
                   upper=c(1,1),
                   control=list(maxit=50))
  scales<-scale.fit$par
  
  return(data.frame(scale_1=scales[1], scale_2=scales[2]))
}


pdf("./Plots/dist and sensitivity to seg size.pdf")

#scale.fit<-align_n_scale(trace_1, trace_2, trace_3, start=6001, segment.size=150)


start.vector<-sample(x=3000:(nrow(trace_1)-7000), size=1000)

segment.size.vector<-c(100,200,400,800,1600,3200)


require(plyr)
require(reshape)
require(doMC)
registerDoMC(2)
getDoParWorkers()


scales<-ldply(.data=start.vector,
              function(x)align_n_scale(trace_1, trace_2, trace_3, start=x, segment.size=300),
              .parallel=T)



scales<-melt(scales)

scales<-ddply(.data=scales,
                .variables="variable",
                       median=median(value),
                transform)

p<-qplot(data=scales,
      x=value,
      fill=variable,
      colour=variable,
      geom="density",
      alpha=I(0.5))+
        geom_vline(aes(xintercept =median, colour=variable))+
        geom_text(aes(x=median, y=2.5,label=round(median, digits=3)))

print(p)


space<-data.frame(start=rep(start.vector, length(segment.size)), size=rep(segment.size, each=length(start.vector)))
space$ID<-1:nrow(space)



scales_size_range<-ddply(.data=space,
              .variables="ID",
              function(x)align_n_scale(trace_1, trace_2, trace_3, start=x$start, segment.size=x$size),
              .parallel=T)

scales_size_range<-join(space, scales_size_range)

scales_size_range<-melt(scales_size_range, measure.vars=c("scale_1", "scale_2"))


p<-qplot(data=scales_size_range,
         x=size,
         y=sd(value),
         colour=variable)

print(p)

p<-qplot(data=scales_size_range,
         x=value,
         colour=variable,
         facets=~size,
         geom=density)

print(p)
         

dev.off()
