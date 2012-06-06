align_seq<-function(species_trace, community_trace){
  
  if(!require(Biostrings)){
    source("http://bioconductor.org/biocLite.R")
    biocLite("Biostrings")
  }
  
  species_base<-species_trace[species_trace$peak, "base.call"]
  community_base<-community_base[community_base$peak, "base.call"]
  
  species_base<-DNAString(paste(species, sep="", collapse=""))
  community_base<-DNAString(paste(community, sep="", collapse=""))
  
  alignement<-pairwiseAlignment(pattern=species,
                                subject=community,
                                type="global")
                                