#long format cleaned in R;
#long data is more efficient than wide data
#no need for wide format if not explicitly dealing with missing data
#inits and from MLE (stata)

		model
		{

			#level 2 with time invariant controls
			for(i in 1:ni){

				#Uniform draw for individual intercept
				alpha[i] ~ dnorm(0,1.0E-6) #iid diffuse draws ; would work with unifrom as well
				
			}

			#level 1 with time and id
			for(N in 1:nobs) {
			
			y[N] ~ dnorm(mu[N], s2inv)
			
				mu[N] <- alpha[obsid[N]] + b0*age[N] + gamma[1]*x[1,N] + gamma[2]*x[2,N] + gamma[3]*x[3,N] + gamma[4]*x[5,N] + gamma[5]*x[6,N]

			}


			#@@@@
			#priors
			#@@@@
			
			for(k in 1:5){
				gamma[k] ~ dnorm(0,1.0E-6)
			}

			#alpha(n)
			a0 <- mean(alpha[1:ni])
			b0 ~ dnorm(0,1.0E-6)
			
			#variances
			s2inv ~ dgamma(0.001, 0.001)
			s2 <- 1/s2inv
			
		}
