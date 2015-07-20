#long format cleaned in R;
#data built like mixture example in /Examples/Schools.odc
#long data is more efficient than wide data
#no need for wide format if not explicitly dealing with missing data
#inits from MLE (stata)

		model
		{

			#level 1 with time and id
			for(N in 1:nobs) {
			
				y[N] ~ dnorm(mu[N], s2inv)
				mu[N] <- alpha[obsid[N]] + beta[obsid[N]]*age[N] + gamma[1]*x[1,N] + gamma[2]*x[2,N] + gamma[3]*x[3,N] + gamma[4]*x[5,N] + gamma[5]*x[6,N]
				
			}


			#@@@@
			#priors
			#@@@@

			a0 ~dnorm(0,1.0E-6)
			b0 ~ dnorm(0,1.0E-6)

			for(j in 1:3){
				a[j] ~ dnorm(0,1.0E-6)
				b[j] ~ dnorm(0,1.0E-6)
			}
			
			for(k in 1:5){
				gamma[k] ~ dnorm(0,1.0E-6)
			}


			#hyperprior and level 2
			for(i in 1:ni){
				
				#Individual expected means
				ev[i,1] <- a0 + z[1,id[i]]*a[1] + z[2,id[i]]*a[2] + z[3,id[i]]*a[3]
				ev[i,2] <- b0 + z[1,id[i]]*b[1] + z[2,id[i]]*b[2] + z[3,id[i]]*b[3]
			
				alpha[i] <- mvm[i,1]
				beta[i] <- mvm[i,2]

				#MVN draw for individual means
				mvm[i,1:2] ~ dmnorm(ev[i,1:2], invsigma[1:2,1:2])
				
			}
			
			#variances
			s2inv ~ dgamma(0.001, 0.001)
			s2 <- 1/s2inv
			
			#Scaled Wishart for inverse cov matrix of random effects (hyperpriors)
			#From Gelman, p. 390 
			#Decomposing the solves the sensitivity to the choice of prior and weakens 
			#strnegth of the prior making it less informative and less important to model
			#the sigma for the mvn is defined as diag(zi)%*%IWish_n(Identity)%*%diag(zi)
			#current criticism is large var leads to large cor as discussed: 
			#http://www.themattsimpson.com/2012/08/20/prior-distributions-for-covariance-matrices-the-scaled-inverse-wishart-prior/
			
			#r1 ~ dunif(1.3,1.7)
			#r2 ~ dunif(3.5,6)
			
			r1 ~ dunif(0,2)
			r2 ~ dunif(0,2)

			zi[1,1] <- r1
			zi[2,2] <- r2
			zi[1,2] <- 0
			zi[2,1] <- 0
			
			sig.eta[1:2,1:2] ~ dwish(iden[1:2,1:2],2)	
				iden[1,1] <- 1
				iden[2,2] <- 1
				iden[1,2] <- 0
				iden[2,1] <- 0
			
			#openbugs has no native matrix multiplication; create sigma by hand
			
				#1 premultiplay sig.eta with zi
				sig.pre[1,1] <- zi[1,1]*sig.eta[1,1] + zi[1,2]*sig.eta[2,1]
				sig.pre[1,2] <- zi[1,1]*sig.eta[1,2] + zi[1,2]*sig.eta[2,2]
				sig.pre[2,1] <- zi[2,1]*sig.eta[1,1] + zi[2,2]*sig.eta[2,1]
				sig.pre[2,2] <- zi[2,1]*sig.eta[1,2] + zi[2,2]*sig.eta[2,2]
						
				#2 postmultiply result with zi	
				invsigma[1,1] <- sig.pre[1,1]*zi[1,1] + sig.pre[1,2]*zi[2,1]
				invsigma[1,2] <- sig.pre[1,1]*zi[1,2] + sig.pre[1,2]*zi[2,2]
				invsigma[2,1] <- sig.pre[2,1]*zi[1,1] + sig.pre[2,2]*zi[2,1]
				invsigma[2,2] <- sig.pre[2,1]*zi[1,2] + sig.pre[2,2]*zi[2,2]
			
			#tau[1:2,1:2] <- inverse(sigma[1:2,1:2])
			
			tau.alpha <- sigma[1,1]
			tau.beta <- sigma[2,2]
			rho <- sigma[1,2]/(sqrt(tau.alpha)*sqrt(tau.beta))
			 			
			sigma[1:2,1:2]<-inverse(invsigma[1:2,1:2])

		}
