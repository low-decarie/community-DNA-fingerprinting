sequence_sim_trace<-function(sequence,...){
  
  trace_value<-function(base){
    traces<-data.frame(A=as.numeric(base=="A"),
                       T=as.numeric(base=="T"),
                       C=as.numeric(base=="C"),
                       G=as.numeric(base=="G"))
    return(traces)
    }
  
  if(!require(stringr))install.packages("stringr")
  
  sequence<-str_split(sequence, pattern="")
  
  traces<-data.frame(lapply(X=sequence,
                FUN=trace_value,
                ...)[[1]])
  
  
  #spline interpolation will be added using spline so that the peak width in datum points matches a trace file
  
  spaced.trace<-data.frame(A=0,
                           T=0,
                           C=0,
                           G=0)
  
  zero.pad<-5

  for(i in 1:nrow(traces)){
    temp<-rbind(traces[i,],
                data.frame(A=rep(0,zero.pad) ,
                           T=rep(0,zero.pad),
                           C=rep(0,zero.pad),
                           G=rep(0,zero.pad)))
    spaced.trace<-rbind(spaced.trace, temp)
    
  }
  
  if(!require(plyr)){install.packages("plyr")}
  smooth.cols<-colwise(smooth_trace)
  
  
  spaced.trace<-smooth.cols(spaced.trace, type="Savitsky-Golay")
  
  spaced.trace$datum<-1:nrow(spaced.trace)
  
  return(spaced.trace)
}

#Example sequenceuence
sequence.1<-"ATCG"
sequence.2<-"AGCG"
sequence.3<-"TGCG"
sim.trace.1<-sequence_sim_trace(sequence=sequence.1)
sim.trace.2<-sequence_sim_trace(sequence=sequence.2)
sim.trace.3<-sequence_sim_trace(sequence=sequence.3)

plot.quant<-function(){
for(q.1 in seq(0.1,1,0.01)){
  #for(q.2 in 1:10){
    #for(q.3 in 1:10){

  q.2=1-q.1
  
#heterozygote<-(sim.trace.1+sim.trace.2)/2
heterozygote<-(q.1*sim.trace.1+q.2*sim.trace.2)
heterozygote$datum<-1:nrow(heterozygote)

if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(reshape2)){install.packages("reshape2")}



plot.sim.trace<-function(sim.trace,...){
  

  
sim.trace<-na.omit(sim.trace)

sim.trace$T<-sim.trace$T+0.008
sim.trace$C<-sim.trace$C+0.016
sim.trace$G<-sim.trace$G+0.024

melt.trace<-melt(data=sim.trace,
                 measure.vars=c("A",
                                "T",
                                "C",
                                "G"))

p<-qplot(data=melt.trace,
         x=datum,
         y=value,
         colour=variable,
         geom="line",
         size=I(3),
         alpha=I(1),
         ...)

p<-p+opts(legend.position="none")



#print(p)

return(p)

}


# setwd("~/Documents/PhD/Experiments/community-DNA-fingerprinting")
# pdf(file="./Plots/community three sim trace.pdf")

if(!require(gridExtra)){install.packages("gridExtra")}

# a<-plot.sim.trace(sim.trace.1, main="Species A")
# b<-plot.sim.trace(sim.trace.2, main="Species B")
# c<-plot.sim.trace(sim.trace.3, main="Species C")
d<-plot.sim.trace(heterozygote, main="Community A+B")
d<-d+geom_text(x=12,y=0.075, label=round(q.1, 1), size=I(10), colour=I("#7CAE00"))
d<-d+geom_text(x=12,y=0.125, label=round(q.2, 1), size=I(10), colour=I("#C77CFF"))

# tmp <- ggplot_gtable(ggplot_build(c))
# leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
# legend <- tmp$grobs[[leg]]

#grid.arrange(a,b,c,d, ncol=1)

print(d)

# dev.off()
}#}}
}

if(!require(animation)){install.packages(animation)}
#saveMovie(plot.quant(), interval=0.1, outdir="/Users/LowDecarie/Documents/PhD/Experiments/community-DNA-fingerprinting/Movies", clean=F)

saveVideo(plot.quant(),
          interval=0.1, 
          outdir="/Users/LowDecarie/Documents/PhD/Experiments/community-DNA-fingerprinting/Movies",
          #video.name="two_mixing.mpg",
          clean=T)
