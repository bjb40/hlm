#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Dev R 3.0.2 "Frisbee Sailing"; x86_64-w64-mingw32/x64
#This code loads posterior estimates from winbugs
#Creates PPD, and Analyzes
#Bryce Bartlett
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#load universals configuration file
if(exists('outdir')==F){
  source("H:/projects/hlm_final/code/config.R",
       echo =T, print.eval = T, keep.source=T)
}

#@@@@@@@
#Confirm data is loaded
#@@@@@@@

if(exists('bugsdat')==F){
  library(foreign)
  bugsdat = read.dta(paste(outdir,'basic_hlm.dta',sep=''))
}

#@@@@@@@
#functions to load bugs dat
#@@@@@@@

#simple load
s_load=function(parm,d){
  #parm is a list of names corresponding to text files; d is a directory
  for(i in parm){
    print(paste('loading',i))
    if(exists('post')==F){
      post=data.frame( read.table(paste(d,i,'.txt',sep=''))[,2])
      names(post) = i
      }
    else{
      post[,i] = read.table(paste(d,i,'.txt',sep=''))[,2]
    }
  }
  
  return(post)
}


#complex load
c_load = function(parm,d){
  for(i in parm){
    
    #load values
    v = read.table(paste(d,i,'.txt',sep=''))
    
    #load index
    idx = read.table(paste(d,i,'id.txt',sep=''))
    idx[,1]=gsub('\\[|\\]','',idx[,1]) #remove goofy brackets
      
    for(j in idx[,1]){
      print(paste('loading',j))
      r = idx[idx[,1]==j,]
      
      if(exists('post')==F){
        post=data.frame(v[as.numeric(r[2]):as.numeric(r[3]),2])
        names(post) = j
      }

      post[,j] = v[as.numeric(r[2]):as.numeric(r[3]),2]
    }
    
    rm(idx,v,r,i,j)
    
  }
  return(post)
}

#@@@@@@@
#Load Fixed Effects Posterior Draw
#@@@@@@@

fdir= paste(outdir,'bugs/f_eff_post/',sep='')

sp = c('b0',#time slope
         's2' #variance of y
)

cp = c('alpha', #individual dummies
        'gamma' #x effects
)

fpost = cbind(s_load(parm=sp,d=fdir),c_load(parm=cp,d=fdir))  

#@@@@@@@
#Draw Fixed Effects PPD
#@@@@@@@

fppd = bugsdat
#create alpha dummy matrix
idum = matrix(0,nrow(fppd),length(unique(fppd$id)))
for(ob in 1:nrow(idum)){idum[ob,fppd$id[ob]]=1}

#creat column for log-likehood given paramater draw (to calculate DIC)
fpost[,'ll'] = NA

for(draw in 1:nrow(fpost)){
  fppd[,ncol(fppd)+1]= NA
  
  alphai = t(as.matrix(fpost[draw,3:302]))
  gamma = t(as.matrix(fpost[draw,303:307]))
    theta=c(alphai,gamma)
  x = as.matrix(fppd[,c('x1','x2','x3','x5','x6')])
    dat=cbind(idum,x)
  s2 = fpost[draw,'s2']
  y = as.matrix(fppd[,'y'])
  
  yhat = idum%*%alphai + x%*%gamma + fpost[draw,'b0']*fppd[,'age']
  fppd[,ncol(fppd)] = rnorm(length(yhat),mean=yhat,sd=sqrt(fpost[draw,'s2']))
  
  #fpost[draw,'ll'] = (-nrow(fppd)/2)*log(2*pi*s2)+(--0.5/s2)*(t(y-dat%*%theta)%*%(y-dat%*%theta))
  
  fpost[draw,'ll'] = sum(dnorm(y,mean=yhat,sd=sqrt(s2),log=T))
  
  if(draw%%100==0){
    print(c(draw,mean(yhat),mean(fppd[,ncol(fppd)]),sum(yhat),fpost[draw,'ll']))
  }
  
}

rm(gamma,x,yhat)

#export to csv (too big)
#write.csv(fppd, file=paste(fdir,'fppd.csv~',sep=''))

#@@@@@@
#Load Random alpha posterior draw 
#@@@@@@
rintdir=paste(outdir,'bugs/r_int_post/',sep='')

rintpost = cbind(
          s_load(parm=c('a0','s2','tau2'),d=rintdir),
          c_load(parm='gamma', d=rintdir)
          )

#@@@@@@@
#Load Random Coefficients Posterior Draw
#@@@@@@@

rdir = paste(outdir,'bugs/r_eff_post/',sep='')

rpost = cbind(
        s_load(parm=c('s2','a0','b0'),d=rdir),
        c_load(parm=c('a','b','alpha','beta','sigma','gamma'),d=rdir)
    )

yhat = c_load(parm='mu',d=rdir)

#@@@@@@@
#Draw Random Effects PPD
#@@@@@@@

  rppd = bugsdat
  
  #creat column for log-likehood given paramater draw (to calculate DIC)
  rpost[,'ll'] = NA
  
  for(draw in 1:nrow(yhat)){
    rppd[,ncol(rppd)+1]= NA
    
    gamma = t(as.matrix(rpost[draw,614:618]))
    theta= unlist(c(rpost[draw,2:9],gamma))
    z = rppd[,c('z1','z2','z3')]
    zt = rppd[,c('z1','z2','z3')]*rppd$age
    x = as.matrix(rppd[,c('x1','x2','x3','x5','x6')])
    dat= as.matrix(cbind(rep(1,nrow(rppd)),rppd$age,z,zt,x))
    s2 = rpost[draw,'s2']
    y = as.matrix(rppd[,'y'])
    
    #ppd
    rppd[,ncol(rppd)] = rnorm(length(yhat[draw,]),mean=unlist(yhat[draw,]),sd=sqrt(rpost[draw,'s2']))
    
    #point log likelihood
    #rpost[draw,'ll'] = (-nrow(rppd)/2)*log(2*pi*s2)+(-.5/s2)*(t(y-dat%*%theta)%*%(y-dat%*%theta))
    rpost[draw,'ll'] = sum(dnorm(y,mean=unlist(yhat[draw,]),sd=sqrt(s2),log=T))
    
    if(draw%%100==0){
      print(c(draw,mean(unlist(yhat[draw,])),mean(rppd[,ncol(rppd)]),rpost[draw,'ll']))
    }  
  }

#@@@@@@@@@
#calculat deviance information criterion (DIC) for each
#Gelman p. 172 has good discussion
#@@@@@@@@@

#calculate logp(y|mean params)
ftheta = apply(fpost,2,mean)
  alphai = t(as.matrix(ftheta[3:302]))
  gamma = t(as.matrix(ftheta[303:307]))
  theta=c(alphai,gamma)
  x = as.matrix(fppd[,c('x1','x2','x3','x5','x6')])
  dat=cbind(idum,x)
  s2 = ftheta['s2']
  y = as.matrix(fppd[,'y'])
    
  #llmpost = (-nrow(fppd)/2)*log(2*pi*s2)+(-.5/s2)+(t(y-dat%*%theta)%*%(y-dat%*%theta))
  llmpost.fixed = sum(dnorm(y,mean=dat%*%theta,sd=sqrt(s2),log=T)) 

  #close to what I got from a second set of sampling from openbugs
  pdic.fixed = 2*(llmpost.fixed - mean(fpost[,'ll'])) #effective number of parameters
  dic.fixed = -2*llmpost.fixed+2*pdic.fixed

#need to leave out z and zt and a,b, because the alphai and betai are composed of these...
rtheta = apply(rpost,2,mean)
  alphai = as.matrix(rtheta[10:309])
  betai = as.matrix(rtheta[310:609])
  gamma = as.matrix(rtheta[614:618])
  theta=c(alphai,betai,gamma)
  #z = rppd[,c('z1','z2','z3')]
  #zt = rppd[,c('z1','z2','z3')]*rppd$age
  x = as.matrix(rppd[,c('x1','x2','x3','x5','x6')])
  dat= as.matrix(cbind(idum,idum*rppd$age,x))
  s2 = rtheta['s2']
  y = as.matrix(rppd[,'y'])

#rm(yhat,z,zt)

  #llmpost = (-nrow(rppd)/2)*log(2*pi*s2)+(-.5/s2)*(t(y-dat%*%theta)%*%(y-dat%*%theta))
  llmpost.random = sum(dnorm(y,mean=dat%*%theta,sd=sqrt(s2),log=T))
  pdic.random = 2*(llmpost.random - mean(rpost[,'ll'])) #effective number of parameters
  dic.random = -2*llmpost.random+2*pdic.random

