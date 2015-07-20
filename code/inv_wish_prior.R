#Trying to specify priors of inverse wishart to recover information from
#multilevel model in Stata

options(scipen=999)

#stata results 

tau.alpha = 0.657
tau.beta = 0.005
cov.tau = 0.005

#implied covaraince matrix
sigma = matrix(c(tau.alpha,cov.tau,cov.tau,tau.beta),2,2)
print(sigma)
invsigma = solve(sigma)
print(solve(invsigma))

#inverse of covariance matrix for wishart prior
invsigma = solve(sigma)
print(invsigma)

#play with inverse wishart
library(MCMCpack)

sim = rep(0,6)
names(sim) = c('meanvar','maxvar','minvar','meancor','maxcor','mincor')
varsum = 0
corsum = 0

for(i in 1:10000){

  #scaled inverse wishart (gelman)
  #zi = c(runif(1,10.4,10.7),runif(1,.07,.09))
  zi = c(runif(1,.5,2.5),runif(1,150,250))
  zi= c(.608,.05)
  
  #generate random draw
  #identity matrix doesn't need solve, but informative would
  sigma_eta = rwish(2,solve(diag(2))) 
  sigma_beta = sigma_eta # gives a sense of scale for iwish 
  #sigma_beta = diag(zi,2,2)%*%sigma_eta%*%diag(zi,2,2)
  
  varsum = varsum + sigma_beta[1,1] + sigma_beta[2,2]
  sim['meanvar'] = varsum/(i*2)
  sim['maxvar'] = max(sim['maxvar'],sigma_beta[1,1], sigma_beta[2,2])
  sim['minvar'] = min(sim['minvar'],sigma_beta[1,1], sigma_beta[2,2])
  
  cor = sigma_beta[1,2]/(sqrt(sigma_beta[1,1])*sqrt(sigma_beta[2,2]))
  corsum = corsum + cor
  sim['meancor'] = corsum/i
  sim['maxcor'] = max(sim['maxcor'],cor)
  sim['mincor'] = min(sim['mincor'], cor)
  
  #test positivedefiniteness
  chol(sigma_beta)
  
  if(i%%20==0){
    print(round(c(i,zi,cor,sigma_beta[1,1], sigma_beta[2,2]), digits=3))
  }
  
  
}


print(round(sim, digits=3))

#test adding algorithm to transfer to openbugs
scale = diag(runif(2,0,0.05),2,2)
w = rwish(2,diag(2))

pre = matrix(0,2,2)
  pre[1,1] = scale[1,1]*w[1,1] + scale[1,2]*w[2,1]
  pre[1,2] = scale[1,1]*w[1,2] + scale[1,2]*w[2,2]
  pre[2,1] = scale[2,1]*w[1,1] + scale[2,2]*w[2,1]
  pre[2,2] = scale[2,1]*w[1,2] + scale[2,2]*w[2,2]

test = matrix(0,2,2)
  test[1,1] = pre[1,1]*scale[1,1] + pre[1,2]*scale[2,1]
  test[1,2] = pre[1,1]*scale[1,2] + pre[1,2]*scale[2,2]
  test[2,1] = pre[2,1]*scale[1,1] + pre[2,2]*scale[2,1]
  test[2,2] = pre[2,1]*scale[1,2] + pre[2,2]*scale[2,2]

print(test)
print(scale%*%w%*%scale)

