###
#Using adams data
species.traces<-A7_F
community.trace<-AB7_F

align_trace<-function(species_trace, community_trace){
  diffs <- outer(species.traces$A, community.trace$A, '-')
  