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

o<-optim(par=par,likelihood_theta,n_sp=num_sp)