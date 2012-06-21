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
  
  return(traces)
}

#Example sequenceuence
sequence<-"ACAAGATGCCATTGTCCCCCGGCCTCCTGCTGCTGCTGCTCTCCGGGGCCACGGCCACCGCTGCCCTGCCCCTGGAGGGTGGCCCCACCGGCCGAGACAGCGAGCATATGCAGGAAGCGGCAGGAATAAGGAAAAGCAGCCTCCTGACTTTCCTCGCTTGGTGGTTTGAGTGGACCTCCCAGGCCAGTGCCGGGCCCCTCATAGGAGAGGAAGCTCGGGAGGTGGCCAGGCGGCAGGAAGGCGCACCCCCCCAGCAATCCGCGCGCCGGGACAGAATGCCCTGCAGGAACTTCTTCTGGAAGACCTTCTCCTCCTGCAAATAAAACCTCACCCATGAATGCTCACGCAAGTTTAATTACAGACCTGAA"
sim.trace<-sequence_sim_trace(sequence=sequence)