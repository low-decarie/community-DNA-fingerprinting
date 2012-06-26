##################################################################
#
#Initial set up
#
##################################################################
#clear r memory
rm(list=ls())

#set working directory
setwd("/Users/LowDecarie/Documents/PhD/Environmental sample sequencing")

#load libraries
library(gtools)	#needed for the rdirichlet function for the sampling of a sum to 1 space





##################################################################
#
#Species Simulation Functions
#
##################################################################

#create data frame of simulated species sequences
create.sp<-function(n.sp, n.loci)
{		
		n.code<-4
		code<-c("G","T","C","A")
		sp<-seq(1:n.sp)
		loci<-seq(1:n.loci)
		sp.seq<-data.frame(sp=sort(rep(rep(sp, n.loci),n.code)),loci=rep(sort(rep(loci, n.code)), n.sp), code=rep(code, n.sp*n.loci), f=0)
		
		seq<-sample(code,n.sp*n.loci, replace=T)
		rep.4<-function(x){rep.int(x,4)}
		seq<-as.vector(sapply(seq,rep.4))
		sp.seq$f[sp.seq$code==seq]<-1
		sp.seq
}			
			
			
# #create a list of frequencies parameters for each species in sp.seq
# 
# create.randomf<-function(sp.seq)
# {
# 	#extract values from sp.seq
# 	n.sp<-length(unique(sp.seq$sp))
# 	#create random parameters summing to 1
#   #f.p<-NULL
# 	f.p<-round(t(rdirichlet(1, rep(runif(1,0,2),n.sp))),3)
# 	f.p
#   #hist(f.p)
# }

create.randomf<-function(sp.seq)
{
  #extract values from sp.seq
  n.sp<-length(unique(sp.seq$sp))
  f.p<-NULL
  for(i in 1:n.sp){
    f.p<-c(f.p, runif(1, 0, 1-sum(f.p)))
  }
  return(f.p)
}






##################################################################
#
#Sample Simulation Functions
#
##################################################################

#create a dataframe of simulated environmental sample from simulated species data frame
#requires the sp.seq data frame and a parameter list
create.sample<-function(sp.seq, f.p, noise)
{
	#extract values from sp.seq
	n.sp<-length(unique(sp.seq$sp))
	n.loci<-length(unique(sp.seq$loci))
	n.code<-length(unique(sp.seq$code))

	#creat a species data frame with these frequencies
	sp.p<-data.frame(sp=1:length(f.p), p=f.p)
	sp.p<-data.frame(sp=rep(sp.p$sp, n.loci*n.code), p=rep(sp.p$p, n.loci*n.code))
	sp.p<-sp.p[order(sp.p$sp),]
	sp.f<-sp.seq
	sp.f$f<-sp.f$f*sp.p$p

	#create simulated environmental sample
	sample.seq<-aggregate(list(f=sp.f$f), by=list(loci=sp.f$loci, code=sp.f$code), FUN=sum)
	sample.seq<-sample.seq[order(sample.seq$loci),]

	#add noise
	
	sample.seq$f<-sample.seq$f+noise*runif(length(sample.seq$f),-1,1)
	sample.seq$f[sample.seq$f<0]<-0
	sample.seq$f[sample.seq$f>1]<-1
	sample.seq$f<-round(sample.seq$f,3)

	sample.seq
}






##################################################################
#
#Least Square Optimization Functions
#
##################################################################

#Least square optimization of species frequency
#f.p is a vector of length n.sp (number of species) for the frequency of each species
#it is thus a vector of the parameters to be optimized
#create initial parameter values

initiate<-function(sp.seq)
{
	f.p.initial<-rep(1/(length(unique(sp.seq$sp))),length(unique(sp.seq$sp)))
	f.p.initial
}


sum.sq<-function(f.p,sp.seq, sample.seq)
{

	#extract values from sp.seq
	n.sp<-length(unique(sp.seq$sp))
	n.loci<-length(unique(sp.seq$loci))
	n.code<-length(unique(sp.seq$code))
	
	#create a sequence to match to the sample from parameters in f.p
	sp.p<-data.frame(sp=1:length(f.p), p=f.p)
	sp.p<-data.frame(sp=rep(sp.p$sp, n.loci*n.code), p=rep(sp.p$p, n.loci*n.code))
	sp.p<-sp.p[order(sp.p$sp),]
	
	sp.f<-sp.seq
	sp.f$f<-sp.f$f*sp.p$p
	
	#calculate the summed frequency
	sum.f<-aggregate(list(f=sp.f$f), by=list(loci=sp.f$loci, code=sp.f$code), FUN=sum)
	
	#calculate the squared difference to the sample
	sq.dif<-(sum.f$f[order(sum.f$loci)]-sample.seq$f[order(sample.seq$loci)])^2

	#take the sum
	sum.sq<-sum(sq.dif)
	sum.sq
	
}

#optimization
#optimization<-optim(par=f.p.initial, fn=sum.sq, method="Nelder-Mead")






##################################################################
#
#Sensitivity Analysis
#
##################################################################


###############################
#sensitivity analysis settings
steps<-3  	#amount of smoothing
max.n<-100	#max number of species and max.n/3 for loci
#values give to steps is to the fifth power for runtime
###############################



###############################
#sequence of parameter values
n.sp<-round(seq(2, max.n, length.out=steps))
rep.steps<-function(x){rep.int(x,steps^4)}
n.sp<-as.vector(sapply(n.sp, rep.steps))

n.loci<-round(seq(2, max.n, length.out=steps))
rep.steps<-function(x){rep.int(x,steps^3)}
n.loci<-as.vector(sapply(n.loci, rep.steps))
n.loci<-rep.int(n.loci, steps)

prop.sp.sequenced<-seq(0.05,1,length.out=steps)
rep.steps<-function(x){rep.int(x,steps^2)}
prop.sp.sequenced<-as.vector(sapply(prop.sp.sequenced, rep.steps))
prop.sp.sequenced<-rep.int(prop.sp.sequenced, steps^2)

noise<-seq(0,1, length.out=steps)
rep.steps<-function(x){rep.int(x,steps)}
noise<-as.vector(sapply(noise, rep.steps))
noise<-rep.int(noise, steps^3)

i<-1:steps
i<-rep.int(i, steps^4)

index<-1:length(i)

sensitivity.parameters.list<-data.frame(n.sp= n.sp, n.loci= n.loci, prop.sp.sequenced= prop.sp.sequenced, noise=noise, i=i, index= index)
###############################



###############################
#sensitivity analysis function
sensitivity<-function(sensitivity.parameters){

				#extract information from sensitivity.parameters
				###############################
				n.sp<-sensitivity.parameters$n.sp
				n.loci<-sensitivity.parameters$n.loci
				prop.sp.sequenced<-sensitivity.parameters$prop.sp.sequenced
				noise<-sensitivity.parameters$noise
				i<-sensitivity.parameters$i
				index<-sensitivity.parameters$index
	
	
				#create species
				###############################
				t.sp<-floor(n.sp/prop.sp.sequenced) ##the total number of species in the actual sample
				sp.seq<-create.sp(t.sp,n.loci)

				#create sample
				###############################
				f.p<-create.randomf(sp.seq)
				sample.seq<-create.sample(sp.seq, f.p, noise) 
				sp.seq_known<-sp.seq[sp.seq$sp<=n.sp,]
				
				##take only n.sp species in the sample
				###############################
				true.f.p<-f.p[1:n.sp]
				f.p.initial<-initiate(sp.seq_known)
				optimization<-nlminb(start=f.p.initial, objective=sum.sq,sp.seq=sp.seq_known, sample.seq=sample.seq, lower=rep(0, length(f.p.initial)), upper=rep(1, length(f.p.initial)))
				
											
				optim.f.p<-round(optimization$par,3)
				
				
				
				f.p.results<-data.frame(index=index, n.sp=n.sp, n.loci=n.loci, prop.sp.sequenced=prop.sp.sequenced,noise=noise, run=i, f.p= f.p, true.f.p =c(true.f.p, rep(NA, length(f.p)-length(true.f.p))), optim.f.p= c(optim.f.p, rep(NA, length(f.p)-length(optim.f.p))))
				
				

	
				
				return(f.p.results)

#printing needs to be silenced for multicore
#print(paste(n.sp,n.loci, prop.sp.sequenced, noise, i))
	
	}

###############################

n.items<-1

#use indexing of this line to select workable subsets for trouble shooting	
sensitivity.parameters<-sensitivity.parameters.list[sensitivity.parameters.list$noise==0 & sensitivity.parameters.list$prop.sp.sequenced==1,]

#indexing of the above for n random items
#n.items<-1
##[sample(1:length(sensitivity.parameters.list$index), n.items),]


###############################
#The sensitivity analysis can be run in 
#single or
#multicore (parallel) modes
###############################


################Parallel########################
#load needed library
library(multicore)
#set system options for number of cores (keep one processor free for system i.e parallel does not play nice)
options(cores=1)  ## this does not seem to be called by parallel

#function to be run in parallel
sensitivity.by<-function(sensitivity.parameters){f.p.results <-by(data= sensitivity.parameters, INDICES=1:length(sensitivity.parameters$n.sp), FUN=sensitivity, simplify=T)
f.p.results<-do.call(rbind, f.p.results)
f.p.results}
#seeding function
sensitivity.parallel<-function(sensitivity.parameters){parallel(sensitivity.by(sensitivity.parameters))}

#naive parallelization using by (data.frame equivalent of apply)
max.n.threads<-2
jobs<-by(data= sensitivity.parameters, INDICES=sample(1:max.n.threads, replace=T, size=length(sensitivity.parameters$n.sp)), FUN=sensitivity.parallel, simplify=T)
f.p.results<-collect(jobs)

#clean up the "by" class format do a more user friendly data.frame
f.p.results<-do.call(rbind, f.p.results)

#save to R file
save(f.p.results, file="sensitivity_results.RData")
###############################################
################################################


#################Single core####################
#f.p.results<-by(data=sensitivity.parameters, INDICES=1:length(sensitivity.parameters$n.sp), FUN=sensitivity, simplify=F)
#f.p.results<-do.call(rbind, f.p.results)
#save(f.p.results, file="sensitivity_results.RData")
#################################################






