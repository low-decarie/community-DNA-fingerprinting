#Alignement of base pair calls using the Biostrings package from bioconductor

align_seq<-function(species_trace_1, community_trace){
  
  if(!require(Biostrings)){
    source("http://bioconductor.org/biocLite.R")
    biocLite("Biostrings")
  }
  
  #Test with Adam data
  species_trace_1<-A7_F
  species_trace_2<-B7_F
  community_trace<-AB7_F
  
  
  species_base_1<-species_trace_1[species_trace_1$peak, "base.call"]
  species_base_2<-species_trace_2[species_trace_2$peak, "base.call"]

  community_base<-community_trace[community_trace$peak, "base.call"]
  
  species_base_1<-paste(species_base_1, sep="", collapse="")
  species_base_2<-paste(species_base_2, sep="", collapse="")
  species_base<-DNAStringSet(c(species_base_1,species_base_2))
  
  community_base<-DNAString(paste(community_base, sep="", collapse=""))

  
  alignement<-pairwiseAlignment(pattern=species_base,
                                subject=community_base,
                                type="global-local")
                                