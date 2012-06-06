#This function extracts traces from .ab1 files
#and performs smoothing of values

#function dependencies
# -smooth_trace

get_trace<-function(file,type="Savitsky-Golay", width=10, round=F, accuracy=5){

  
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


#Smooth the data
trace<-data.frame(trace,
                            A.smooth=smooth_trace(trace$A,type, width),
                             T.smooth=smooth_trace(trace$T,type, width),
                             C.smooth=smooth_trace(trace$C,type, width),
                             G.smooth=smooth_trace(trace$G,type, width))

#Pad with 0 rather to replace NA
trace$A.smooth[is.na(trace$A.smooth)]<-0
trace$T.smooth[is.na(trace$T.smooth)]<-0
trace$C.smooth[is.na(trace$C.smooth)]<-0
trace$G.smooth[is.na(trace$G.smooth)]<-0
                                  

#Get peaks
trace<-data.frame(trace,
                  A.peak=get_peak(trace$A.smooth, round, accuracy),
                  T.peak=get_peak(trace$T.smooth, round, accuracy),
                  C.peak=get_peak(trace$C.smooth, round, accuracy),
                  G.peak=get_peak(trace$G.smooth, round, accuracy))


trace$peak<-with(trace, A.peak|T.peak|C.peak|G.peak)

# 
return(trace)
}

