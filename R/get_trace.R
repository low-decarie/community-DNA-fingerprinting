#This function extracts traces from .ab1 files
#and performs smoothing of values

#function dependencies
# -smooth_trace

get_trace<-function(file,type="Savitsky-Golay", width=10,rm.min=TRUE, min.peak=1000){

  
#Load and/or install required packages
if(!require(seqinr)){
  install.packages("seqinr")}

#Load raw data
raw.trace<-read.abif(file, verbose=F)

#Extract trace data
trace<-data.frame(datum=1:length(raw.trace$Data[["DATA.1"]]),
                        A=raw.trace$Data[["DATA.1"]],
                        T=raw.trace$Data[["DATA.2"]],
                        C=raw.trace$Data[["DATA.3"]],
                        G=raw.trace$Data[["DATA.4"]])



#Call bases
trace<-ddply(.data=trace,
                  .variable="datum",
                  function(x){
                    base<-"N"
                    base.call.quality<-0
                    sum.value=sum(x$A,x$T,x$C,x$G)
                    max.value=max(x$A,x$T,x$C,x$G)
                    if(x$A>max(x$T,x$C,x$G)){
                      base<-"A"
                      base.call.quality<-x$A/sum.value
                    }
                    if(x$T>max(x$A,x$C,x$G)){
                      base<-"T"
                      base.call.quality<-x$T/sum.value
                    }
                    if(x$C>max(x$T,x$A,x$G)){
                      base<-"C"
                      base.call.quality<-x$G/sum.value
                    }
                    if(x$G>max(x$T,x$C,x$A)){
                      base<-"G"
                      base.call.quality<-x$A/sum.value
                    }
                    trace<-data.frame(x,base.call=base,
                                      max.value=max.value,
                                      sum.value=sum.value,
                                      base.call.quality=base.call.quality)
                    return(trace)},
                  .progress="text")

#Smooth max value
trace$smooth.max<-as.numeric(with(trace, smooth_trace(max.value, type, width)))
trace$smooth.max[is.na(trace$smooth.max)]<-0

#Get peaks
trace$peak<-with(trace, get_peak(smooth.max, rm.min, min.peak))

return(trace)
}

