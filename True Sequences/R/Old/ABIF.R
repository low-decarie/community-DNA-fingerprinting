library(seqinr)
raw.sequence<-read.abif("/Users/LowDecarie/Dropbox/Environmental sample sequencing/raw sequence data/7A_F.ab1", verbose=F)


plotabif(raw.sequence, chanel=1)


peaks.sequence<-peakabif(raw.sequence, chanel=1, thres=0.1, npeak=250)

#plotladder