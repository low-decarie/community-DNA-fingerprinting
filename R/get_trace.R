#This function extracts traces from .ab1 files
#and performs smoothing of values

#function dependencies
# -smooth_trace

get_trace<-function(file,type="Savitsky-Golay", width=10){

  
#Load and/or install required packages
if(!require(seqinr)){
  install.packages("seqinr")}

#Load raw data
raw.trace<-read.abif(file, verbose=F)

#Extract trace data
clean.trace<-data.frame(datum=1:length(raw.trace$Data[["DATA.1"]]),
                        A=raw.trace$Data[["DATA.1"]],
                        T=raw.trace$Data[["DATA.2"]],
                        C=raw.trace$Data[["DATA.3"]],
                        G=raw.trace$Data[["DATA.4"]])


#Smooth the data
clean.trace<-data.frame(clean.trace,
                            A.smooth=smooth_trace(clean.trace$A,type, width),
                             T.smooth=smooth_trace(clean.trace$T,type, width),
                             C.smooth=smooth_trace(clean.trace$C,type, width),
                             G.smooth=smooth_trace(clean.trace$G,type, width))

#Pad with 0 rather to replace NA
clean.trace$A.smooth[is.na(clean.trace$A.smooth)]<-0
clean.trace$T.smooth[is.na(clean.trace$T.smooth)]<-0
clean.trace$C.smooth[is.na(clean.trace$C.smooth)]<-0
clean.trace$G.smooth[is.na(clean.trace$G.smooth)]<-0
                                  

# #Extract peak position (peak position as determined by the sequencer)
# peak.position<-raw.trace$Data[["PLOC.2"]]


# peaks<-peakabif(abifdata=raw.trace,
#                 chanel=1,
#                 npeak=length(peak.position),
#                 fig=F)
                

#Add peak position to clean trace data
# clean.trace$peak<-FALSE
# clean.trace$peak[clean.trace$datum %in% peak.position]<-TRUE
# 
return(clean.trace)
}

