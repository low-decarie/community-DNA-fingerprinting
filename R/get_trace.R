get_trace<-function(file){

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

#Extract peak position
peak.position<-raw.trace$Data[["PLOC.2"]]

#Add peak position to clean trace data
clean.trace$peak<-FALSE
clean.trace$peak[clean.trace$datum %in% peak.position]<-TRUE

return(clean.trace)}

