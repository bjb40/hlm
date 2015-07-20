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
				mu[N] <- alpha[obsid[N]] + beta*age[N] + gamma[1]*x[1,N] + gamma[2]*x[2,N] + gamma[3]*x[3,N] + gamma[4]*x[5,N] + gamma[5]*x[6,N]
				
			}


			#@@@@
			#priors
			#@@@@

			a0 ~dnorm(0,1.0E-6)
			beta ~ dnorm(0,1.0E-6)
			
			for(k in 1:5){
				gamma[k] ~ dnorm(0,1.0E-6)
			}


			#hyperprior and level 2
			for(i in 1:ni){
				
		
				#MVN draw for individual means
				alpha[i] ~ dnorm(a0, tau2inv)
				
			}
			
			#variances
			s2inv ~ dgamma(0.001, 0.001)
			s2 <- 1/s2inv
			
			tau2inv ~ dgamma(0.001,0.001)
			tau2 <- 1/tau2inv
			
		}
