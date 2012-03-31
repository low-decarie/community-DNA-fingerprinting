rm(list=ls())
library(seqinr)

# abif functions in seqinr
  #baselineabif
  #peakabif
  #plotabif
  #plotladder
  #read.abif
  #stutterabif
  
  
# main information in abif files (.ab1)
  #all contained within $Data
      #DATA.1-DATA.4 trace values for 4 dyes
      #PBAS.1-PBAS.2 Array of sequence characters as called by user (1) and Basecaller (2)
      #PLOC.1-PLOC.2 Array of peak locations as called by user (1) and Basecaller (2)
      

#Load raw data
raw.trace<-read.abif(file.choose(), verbose=F)

#Extract trace data
clean.trace<-data.frame(datum=1:length(raw.trace$Data[["DATA.1"]]), A=raw.trace$Data[["DATA.1"]],T=raw.trace$Data[["DATA.2"]],C=raw.trace$Data[["DATA.3"]],G=raw.trace$Data[["DATA.4"]])

#Extract peak position
peak.position<-raw.trace$Data[["PLOC.2"]]

#Extract trace values at peak
trace.at.peak<-clean.trace[peak.position,]

#Save file
save(trace.at.peak, file="mixed data.R")
                
                