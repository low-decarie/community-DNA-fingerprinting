## Simulates Species assemblages to test likelihood model on sequence data ##
## Corey Chivers & Etienne Low-Decarie, 2010 ##

rm(list=ls())


auto_corr<-function(x,degree=1,len,n_order) ## Draws autocorrelated samples where A is <degree> times as likely to follow A than independence. New samples are independent every <n_order> sample (ie to allow bases to be autocorrelated at a locus across <n_order> species to control the level of polymorphism ).
{
     ret_vec<-array(dim=len)
     ret_vec[1]<-sample(x,1)
     length.x<-length(x)
     prob<-array(1/length.x,dim=length.x)
     for(i in 2:len)
     {
          prob[]<-(1/length.x)
          prob[which(x==ret_vec[i-1])]=(1/length.x)*degree
          prob<-prob/sum(prob) # normalize
          if(i%%(n_order+1)!=0) ## only reset independence every <n_order> draws
          {
               ret_vec[i]<-sample(x,1,prob=prob)
          }else
          {
               ret_vec[i]<-sample(x,1)     
          }
     }
     return(ret_vec)
}



###### Simulate The Assemblage ############
base<-c('A','C','T','G')

num_sp<-20
seq_len<-100

sp<-array(auto_corr(base,degree=num_sp*10,len=num_sp*seq_len,n_order=num_sp),dim=c(num_sp,seq_len))  ## <degree> allows for control of polymorphism. If species A is 'T' at a loci, the next species has a 10x higher than random chance of being a 'T', etc.

sp<-t(sp)  # Species Sequences

sp_freq<-runif(num_sp,0,100)  # Species Frequencies (TRUE)
sp_freq<-sp_freq/sum(sp_freq)
## Because the signal from the sequencer is normalized, we need to simulate our data accordingly. ##

par(mfrow=c(1,2))
plot(sp_freq,sp_freq_noise)
abline(0,1)
hist(sp_freq-sp_freq_noise)
###########################################


#####  Calculate # of polymorphic sites ####
poly<-NULL
for(i in 1:seq_len)
{
	if(length(unique(sp[i,]))==1)
	{
		poly<-c(poly,0)
	}else
	{
		poly<-c(poly,1)
	}
}
print(sum(poly))
############################################




##### Read Sequencer Output #################
read<-array(dim=c(seq_len,4)) ## Sequencer read (ie the data)

for(i in 1:seq_len)
{
     for(b in base)
     {
          read[i,base==b]<-sum(sp_freq[sp[i,]==b])
          read[i,base==b]<-read[i,base==b]+runif(1,0,mean(sp_freq)) ## OBSERVATION ERROR
     }
     read[i,]<-read[i,]/sum(read[i,])
}

par(mfrow=c(2,1))
plot(read[,1],type='l',ylim=c(0,1),xlab='loci',ylab='Intensity',main=paste('Assemblage Sequence\n',sum(poly)/seq_len))
for(i in 2:4)
 lines(read[,i],col=i)
legend('topright',legend=base,col=1:4,lty=1)
##############################################






##### The Likelihood Model ###################
likelihood_theta<-function(pars,n_sp)
{
     ll=0
     for(i in 1:seq_len)
     { 
          for(b in base)
          {
               alpha<-pars[n_sp+1]*  sum(pars[c(sp[i,]==b,FALSE)] )  +1
               beta<-pars[n_sp+1]*(1-sum(pars[c(sp[i,]==b,FALSE)] )) +1
               ll<-ll+dbeta(read[i,base==b],alpha,beta,log=TRUE)
	       curve(dbeta(x,alpha,beta),col=i,add=TRUE)
	       #print(paste(alpha,beta))
          }
     }
     return(-ll)
}
##############################################

par(mfrow=c(1,2))
l<-NULL
for(error in 1:200)
{
par<-c(sp_freq,error)
l<-c(l,likelihood_theta(par,num_sp))
}
plot(l)
which.min(l)
error<-43 #which.min(l)
par<-c(sp_freq,error)
plot(0,0,xlim=c(0,1),ylim=c(0,20))
likelihood_theta(par,num_sp)



#### Run it ##################################
Errors<-c(10,43,50,80,100)
par(mfrow=c(length(Errors),1))
#Errors<-1
for(error in Errors)
{
#par<-c(rep(1/(num_sp+1),num_sp),error)
par<-c(sp_freq,error)
o<-optim(par=par,likelihood_theta,n_sp=num_sp)
r_squared<-cor(sp_freq,o$par[1:num_sp])^2
plot(sp_freq,o$par[1:num_sp],xlim=c(0,3*1/num_sp),ylim=c(0,3*1/num_sp),xlab='Actual',ylab='predicted',main=paste('R^2',round(r_squared,3)) )
abline(0,1)
print(o$par)
print(error)
print(o$value)
}
##############################################


## likelihood profile ##
#l<-NULL
#for(p in seq(0.001,0.5,0.001))
#{
#par<-c(sp_freq[1:(num_sp-1)],p,error)
#l<-c(l,-likelihood_theta(par,num_sp))
#}
#par(mfrow=c(1,1))
#plot(seq(0.001,0.5,0.001),l)
##########################


## Error process at two stages: amplification, and sequence reads ##
## We need a way to model these two main(?) sources of error. ##
## As of now, I have implimented read error (in read sequencer output) ##
## Could model species loss (non-amp) at the amplificaiton level easily ##

## If a species predicted frequency falls below the average background signal level, reject that species from the assemblage. ##
