##############################################################
##initial setup
###############################

rm(list=ls())
library(lattice)
library(scatterplot3d)

setwd("/Users/LowDecarie/Documents/PhD/Environmental sample sequencing")
load("sensitivity_results_full.RData")
##############################################################



##############################################################
##Calculate metrics of success
##############################################################

measurements.f<-function(f.p.results){
	
		#extract data	
		attach(f.p.results)
		
		#calculate metrics
			#percent of perfect guesses
			percent.good<-sum(optim.f.p==true.f.p)/length(optim.f.p)
			#summed of squared difference between calculated and true value
			sum.sq<-sum((optim.f.p-true.f.p)^2)
			mean.error<-mean(abs(optim.f.p-true.f.p))
			max.error<-max(abs(optim.f.p-true.f.p))
			percent.false.posit<-length(which(optim.f.p>0 & true.f.p==0)==T)/n.sp
			percent.false.neg<-length(which(optim.f.p==0 & true.f.p>0)==T)/n.sp
			true.simpson.diversity<-sum(true.f.p^2)
			optim.simpson.diversity<-sum(optim.f.p^2)
			sum.true.f.p<-sum(true.f.p)

		
		#bind metrics into data frame
		measurements<-data.frame(unique(f.p.results[,1:6]), percent.good= percent.good, sum.sq= sum.sq,mean.error=mean.error, max.error= max.error, true.simpson.diversity= true.simpson.diversity, optim.simpson.diversity= optim.simpson.diversity, percent.false.posit= percent.false.posit, percent.false.neg= percent.false.neg, sum.true.f.p= sum.true.f.p)
		
		detach(f.p.results)
		
		return(measurements)
		}
###############################




###############################
#applying the metrics to all data
###############################
	measurements<-by(data=f.p.results, INDICES= f.p.results$index, FUN=measurements.f)
	measurements<-do.call(rbind, measurements)
	
	#ratio between n.loci and n.sp, when this ratio is greater than 1, and sample is clean, optimization is usually perfect (no error)
	measurements$loci.sp.ratio<-measurements$n.loci/measurements$n.sp
	#the mean squared error by species
	measurements$mean.sq<-measurements$sum.sq/measurements$n.sp
###############################








save(measurements,file="sensitivity_measurements_full.RData")



