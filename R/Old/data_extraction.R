setwd("/Users/LowDecarie/Dropbox/Environmental sample sequencing")

#####################
#find minimas
	#function
	find.min<-function(sequence){
				sequence$minima.group<-1
				smooth.sum<-sequence$smooth.sum
				#initilize slope and minimas
				if(smooth.sum[1+1]<smooth.sum[1]) {slope<- -1} else {slope<-1}
				
				minima.group<-1

				for(i in 2:length(smooth.sum)){
						if(smooth.sum[i]<smooth.sum[i-1] & slope==1){
							slope<- -1
							sequence$minima.group[i]<-minima.group
							 } else if(smooth.sum[i]>=smooth.sum[i-1] & slope==-1){
						slope<-1
						minima.group<-minima.group+1
						sequence$minima.group[i]<-minima.group
						sequence$minima[i]<-1
						} else {sequence$minima.group[i]<-minima.group}
						
					}
	return(sequence)
	}

##################





extract.ab1<-function(file.name){

#load updated read.abif() function
source("/Users/LowDecarie/Dropbox/Environmental sample sequencing/r for raw data/new_read_abif.R")

#load .ab1 files
sequence.ab1<-read.abif(filename=file.name)$Data

#extract data from .ab1 files
sequence<-data.frame(A= sequence.ab1$"DATA.1", T=sequence.ab1$"DATA.2", C= sequence.ab1$"DATA.3", G=sequence.ab1$"DATA.4")
sequence[sequence<0]<-0

#summing across all bases
sequence$sum<-apply(X= sequence, MARGIN=1, FUN=sum)
sequence<-sequence[sequence$sum>0,]
	
#smoothing the sums

#5 point moving average
ma5<-rep(1,5)/5

#20 point moving average
ma20<-rep(1,20)/20

# Savitsky-Golay smoothing function 
sg5 = c(-3, 12, 17, 12, -3)/35

smooth.sum<-filter(sequence$sum, ma20)
sequence$smooth.sum<-smooth.sum
sequence<-na.omit(sequence)
sequence$minima<-0
	

	
sequence<-find.min(sequence)


return(sequence)
}





















####
#trial
####

sequence.data<-extract.ab1("raw sequence data/heterozygote.ab1")





sequence.sum<-aggregate(sequence.data[,1:4], by=list(loci=sequence.data$minima.group), FUN=sum)
sequence.sum$sum<-apply(sequence.sum[2:5],MARGIN=1,FUN=sum)
sequence.sum[,2]<-sequence.sum[,2]/sequence.sum[,6]
sequence.sum[,3]<-sequence.sum[,3]/sequence.sum[,6]
sequence.sum[,4]<-sequence.sum[,4]/sequence.sum[,6]
sequence.sum[,5]<-sequence.sum[,5]/sequence.sum[,6]
sequence.sum$sum<-0

sequence.sum$sum<-apply(sequence.sum[2:5],MARGIN=1,FUN=sum)











sample.seq<-sequence.data[7500:8000,]
plot(sample.seq$sum)
colors<-rainbow(length(unique(sequence.data$minima.group)))
lines(sample.seq$smooth.sum, col=colors[sample.seq$minima.group])
index<-1:length(sample.seq$sum)
abline(v=index[sample.seq$minima==1],col=colors[sample.seq$minima.group[sample.seq$minima==1]])









################################################################################
################################################################################
################################################################################
################################################################################
#
#
#####
## 5'-TCAAAGCTC-3' is the primer
#####
#
#
###
##Housekeeping
#rm(list = ls())
#library(seqinr)
#setwd("/Users/LowDecarie/Dropbox/Environmental sample sequencing")
#
#
#max.to.seq<-function(sample.sequence){
#sample.sequence$seq<-names(sample.sequence)[sample.sequence==max(sample.sequence)]
#	}
#
#sample.sequence$seq<-apply(X=sample.sequence, MARGIN=1, FUN= max.to.seq)
#
#
#sample.sequence<-sample.sequence[sample.sequence$A>0 & sample.sequence$T>0 & sample.sequence$C>0 & sample.sequence$G>0,]
#
#
#sample.sequence<-sample.sequence[1000:1100,]
#
#pdf("r for raw data/example heterozygote.pdf")
##pdf("r for raw data/example homozygote1.pdf")
#plot(1:length(sample.sequence$A), sample.sequence$A, type="l", col="green", ylim=c(0,8000))
#lines(1:length(sample.sequence$A), sample.sequence$T, col="red", ylim=c(0,8000))
#lines(1:length(sample.sequence$A), sample.sequence$C, col="black", ylim=c(0,8000))
#lines(1:length(sample.sequence$A), sample.sequence$G, col="blue", ylim=c(0,8000))
##text(1:length(sample.sequence$A),rep(8000, length(sample.sequence$seq)), sample.sequence$seq)
#dev.off()
#
#
#
