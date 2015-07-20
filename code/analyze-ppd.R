



#@@@@@@@@@@@@
#calculate mean beta
#@@@@@@@@@@@@

rbeta = apply(rpost[,310:609],2,mean)
rbetal = apply(rpost[,310:609],2,quantile,prob=c(.025,.975))

print(c(mean(rbeta),mean(rbetal[1,]),mean(rbetal[2,])))

#bayesian p-val>.0176
sum(apply(rpost[,310:609],1,mean)>0.024)/1000

#@@@@@@@@@@@
#dist of intercepts
#@@@@@@@@@@@@

fmean = apply(fpost[,3:302],2,mean)
flim = apply(fpost[,3:302],2,quantile, prob=c(0.025,0.975))

rmean = apply(rpost[,10:309],2,mean)
rlim = apply(rpost[,10:309],2,quantile, prob=c(0.025,0.975))

order = names(sort(fmean))

par(mfrow=c(1,2))
  plot(fmean[order], ylim=c(-0.25,6))
    segments(1:300,flim[1,order],1:300,flim[2,order],lty=3)

#order = names(sort(rmean))

  plot(rmean[order], ylim=c(-0.25,6), type="p")
    segments(1:300,rlim[1,order],1:300,rlim[2,order],lty=3)


#@@@@@@@@@@@
#dist2--distribution of mean "intercpets"
#These are't strictly comparable because of the random slope parameter
#@@@@@@@@@@@@

par(mfrow=c(1,1))
yl=c(0,.55)
xl=c(0.5,5.5)
x=seq(xl,by=.1)

hist(fmean,freq=F,ylim=yl,xlim=xl,xlab='',ylab='',yaxt='n', main='',
     breaks=15, 
     col=rgb(0,0,0,alpha=.1),
     border=0)
  lines(x,
        dnorm(x, 
              mean=mean(rintpost[,'a0']), 
              sd=sqrt(mean(rintpost[,'tau2']))),
        type='l', lty=4)
legend('topright',legend=c('Individual Fixed','Hierarchical Intercpet'),
       bty='n',
       lty=c(NA,4),
       pch=c(15,NA),
       col=c(rgb(0,0,0,alpha=.1),'black'),
       cex=.75)

mean(fmean)
mean(rintpost[,'a0'])
sd(fmean)

mean(rintpost[,'s2'])
mean(fpost[,'s2'])

#@@@@@@@@@@@
#dist3--distribution of mean "intercpets"
#distribution "implied" by a0 and variance
#select .975, mean, and .025 distribution for sigma1,1 (variance of alpha0)
#and plot
#or... plot the sample
#@@@@@@@@@@@@

color = rgb(0,0,0,alpha=.01)
x = seq(0.5,5.5, by=.1)
yl=c(0,0.6)

#should cut off 2.5% from each side so you get 95% cred intervals

par(mfrow=c(1,1))
plot(x,dnorm(x,mean=rpost[1,'a0'],sd=sqrt(rpost[1,'sigma1,1'])), 
     type='l', col=color, ylim=yl,
     xlab='',ylab='',yaxt='n')
for(draw in 2:1000){
  lines(x,dnorm(x,mean=rpost[draw,'a0'],sd=sqrt(rpost[draw,'sigma1,1'])), type='l', col=color)
  
}

  lines(x,dnorm(x,mean=2.924,sd=sqrt(0.651)), type="l", ylim=yl) #ml
  lines(x,dnorm(x,mean=2.920,sd=sqrt(0.665)), lty=2) #reml-better for var/covar (R p. 52)
  legend('topleft',
         legend=c('Bayesian Poserior Draws','ML Implied','REML Implied'),
         lty=c(1,1,2),
         col=c(rgb(0,0,0,alpha=0.333),'black','black'),
         bty='n',
         cex=.75)

#similar plot for beta
par(mfrow=c(1,2))
x=seq(-1,1,by=.001)
yl=c(0,4.5)
plot(x,dnorm(x,mean=rpost[1,'b0'],sd=sqrt(rpost[1,'sigma2,2'])),typ='l', col=color, ylim=yl)
for(draw in 2:1000){
  lines(x,dnorm(x,mean=rpost[draw,'b0'],sd=sqrt(rpost[draw,'sigma2,2'])), type='l', col=color)
}

plot(x,dnorm(x,mean=0.006,sd=sqrt(.005)), type='l')



#Plot for distribution of variance components
par(mfrow=c(1,3))
plot(density(rpost[,'sigma1,1']))
  lines(seq(0.4,1,by=0.05),dnorm(seq(0.4,1,by=0.05),mean=.651,sd=0.066), lty=2)
plot(seq(0,0.02,by=0.005),dnorm(seq(0,0.02,by=0.005),mean=.005,sd=.001),type='l',lty=2)
  lines(density(rpost[,'sigma2,2']))  
plot(density(rpost[,'sigma1,2']))
  lines(seq(-.015,0.035,by=0.001),dnorm(seq(-.015,0.035,by=0.001),mean=0.005,sd=0.007), lty=2)

#@@@@@@@@@@@
#size of intercept variance
#@@@@@@@@@@@

par(mfrow=c(1,1))
plot(density(rpost[,'sigma1,1']))
  abline(v=0.651, lty=2)


#@@@@@@@@@
#time-varying effect differences
#@@@@@@@@@

hist(rpost[,'b2'], ylim=c(0,80),breaks=20, freq=F, col=rgb(0,0,0,alpha=0.1),border=0)
#plot(density(rpost[,'gamma2']), ylim=c(0,80))
  lines(density(rpost[,'b2']))
  lines(seq(-0.04,0.1,by=.0001),dnorm(seq(-0.04,0.1,by=.0001),mean=0.030,sd=0.015),lty=2)

#@@@@@@@@@@@
#ppd1
#@@@@@@@@@@@@

yf = fppd[,(ncol(bugsdat)+1):ncol(fppd)]
yr = rppd[,(ncol(bugsdat)+1):ncol(rppd)]

par(mfrow=c(1,1))
hist(bugsdat$y,freq=F, xlim=c(-1,6))
  lines(density(unlist(yf)), lty=2)
  lines(density(unlist(yr)), lty=3)


#plot 6 random people over time and compare
#yl=quantile(unlist(fppd[,20:1019]),prob=c(0.025,0.975))
yl= c(0.5,5.5) #acceptable range is 1 to 5
xl=range(fppd$age)+65

p = sample(unique(fppd$id[!(fppd$cx6 %in% c(0,1))]),6) #all have unemployment spell
write.csv(bugsdat[bugsdat$id %in% p,],paste(outdir,'random-6.csv',sep=''))
fps = fppd[fppd$id %in% p,]
rps = rppd[rppd$id %in% p,]
rps$age = rps$age+65
fps$age = fps$age+65

par(mfrow=c(2,3),mar=c(1,1,0,0),oma=c(3,3,1,1))
  for(i in 1:length(p)){
    plot(fps[fps$id==p[i],'age'],fps[fps$id==p[i],'y'], type='p',pch=1,
         xlim=xl,ylim=yl,xaxt='n',xlab='',yaxt='n',ylab='')
    lines(fps[fps$id==p[i],'age'], apply(fps[fps$id==p[i],20:1019],1,mean), type='l', lty=1)
      segments(fps[fps$id==p[i],'age']-.15,apply(fps[fps$id==p[i],20:1019],1,quantile,prob=.025),
               fps[fps$id==p[i],'age']-.15,apply(fps[fps$id==p[i],20:1019],1,quantile,prob=.975)
               ,lty=1)
    
    lines(rps[rps$id==p[i],'age'], apply(rps[rps$id==p[i],20:1019],1,mean), type='l', lty=4)
    segments(rps[rps$id==p[i],'age']+.15,apply(rps[rps$id==p[i],20:1019],1,quantile,prob=.025),
             rps[rps$id==p[i],'age']+.15,apply(rps[rps$id==p[i],20:1019],1,quantile,prob=.975)
             ,lty=4)
    
    
    if(i>3){axis(1,at=seq(xl[1],xl[2],by=2))}
    if(i %in% c(1,4)){axis(2,at=seq(round(yl[1]),round(yl[2]),by=1))}
    if(i ==1){legend('topleft',
              c('Observed','Fixed PPD','Random PPD'),
              lty=c(0,1,4),
              pch=c(1,NA,NA),
              bty='n')
              }  
    
    #abline(h=1, lty=1, col=rgb(0,0,0,alpha=0.25))
    #abline(h=5, lty=1, col=rgb(0,0,0,alpha=0.25))
  
  }


#@@@@@@@@@@@
#full sample
#@@@@@@@@@@

#prepare Bayesian p-values for extreme observations across dummy variables 
#and mean split continuous variables

f.yrep = as.matrix(fppd[,20:1019]) #creaty_rep (y_{rep})
r.yrep = as.matrix(rppd[,20:1019])

#general
sum(apply(f.yrep,2,mean) > mean(fppd[,'y']))/1000
sum(apply(r.yrep,2,mean) > mean(rppd[,'y']))/1000

#split education by median (which is 12 years if education)
z1s = median(fppd$z1)

#low ed
print(c('p low ed',sum(fppd$z1<z1s)/length(fppd$z1)))
sum(apply(f.yrep[fppd$z1<z1s,],2,mean) > mean(fppd[fppd$z1<z1s,'y']))/1000
sum(apply(r.yrep[rppd$z1<z1s,],2,mean) > mean(rppd[rppd$z1<z1s,'y']))/1000
#print('extremes')
#sum(apply(f.yrep[fppd$z1<z1s,],2,max) >  max(fppd[fppd$z1<z1s,'y']))/1000
#sum(apply(r.yrep[rppd$z1<z1s,],2,max) > max(rppd[rppd$z1<z1s,'y']))/1000
#sum(apply(f.yrep[fppd$z1<z1s,],2,min) <  min(fppd[fppd$z1<z1s,'y']))/1000
#sum(apply(r.yrep[rppd$z1<z1s,],2,min) < min(rppd[rppd$z1<z1s,'y']))/1000


#hs
print(c('hs',sum(fppd$z1==z1s)/length(fppd$z1)))
sum(apply(f.yrep[fppd$z1==z1s,],2,mean) > mean(fppd[fppd$z1==z1s,'y']))/1000
sum(apply(r.yrep[rppd$z1==z1s,],2,mean) > mean(rppd[rppd$z1==z1s,'y']))/1000

#high ed
print(c('p high ed',sum(fppd$z1>z1s)/length(fppd$z1)))
sum(apply(f.yrep[fppd$z1>z1s,],2,mean) > mean(fppd[fppd$z1>z1s,'y']))/1000
sum(apply(r.yrep[rppd$z1>z1s,],2,mean) > mean(fppd[rppd$z1>z1s,'y']))/1000

#gender
print(c('p male',sum(fppd$z2==1)/length(fppd$z2)))
sum(apply(f.yrep[fppd$z2==1,],2,mean) > mean(fppd[fppd$z2==1,'y']))/1000
sum(apply(r.yrep[rppd$z2==1,],2,mean) > mean(fppd[rppd$z2==1,'y']))/1000

print(c('p female',sum(fppd$z2==0)/length(fppd$z2)))
sum(apply(f.yrep[fppd$z2==0,],2,mean) > mean(fppd[fppd$z2==0,'y']))/1000
sum(apply(r.yrep[rppd$z2==0,],2,mean) > mean(fppd[rppd$z2==0,'y']))/1000

#gender interval
m.fixed = unlist(f.yrep[fppd$z2==1,])
m.random = unlist(r.yrep[rppd$z2==1,])
m.q = quantile(fppd[fppd$z2==1,'y'],prob=c(.25,.75))

(sum(m.fixed < m.q[1]) + sum(m.fixed>m.q[2]))/length(m.fixed)
(sum(m.random < m.q[1]) + sum(m.random>m.q[2]))/length(m.random)

f.fixed = unlist(f.yrep[fppd$z2==0,])
f.random = unlist(r.yrep[rppd$z2==0,])
f.q = quantile(fppd[fppd$z2==0,'y'],prob=c(.25,.75))

(sum(f.fixed < f.q[1]) + sum(f.fixed>f.q[2]))/length(f.fixed)
(sum(f.random < f.q[1]) + sum(f.random>f.q[2]))/length(f.random)



#wealth 0 or under, under 50,000, over 50,000
x2s = log(50000)
print(c('neg wealth',sum(fppd$x2<0)/length(fppd$x2)))
sum(apply(f.yrep[fppd$x2<0,],2,mean) > mean(fppd[fppd$x2<0,'y']))/1000
sum(apply(r.yrep[rppd$x2<0,],2,mean) > mean(rppd[rppd$x2<0,'y']))/1000

print(c('under 50k wealth',sum(fppd$x2<x2s)/length(fppd$x2)))
sum(apply(f.yrep[fppd$x2<x2s,],2,mean) > mean(fppd[fppd$x2<x2s,'y']))/1000
sum(apply(r.yrep[rppd$x2<x2s,],2,mean) > mean(rppd[rppd$x2<x2s,'y']))/1000

print(c('under 50k wealth',sum(fppd$x2>x2s)/length(fppd$x2)))
sum(apply(f.yrep[fppd$x2>x2s,],2,mean) > mean(fppd[fppd$x2>x2s,'y']))/1000
sum(apply(r.yrep[rppd$x2>x2s,],2,mean) > mean(rppd[rppd$x2>x2s,'y']))/1000

#retirement
print(c('labor force',sum(fppd$x5==1)/length(fppd$x2)))
sum(apply(f.yrep[fppd$x5==1,],2,mean) > mean(fppd[fppd$x5==1,'y']))/1000
sum(apply(r.yrep[rppd$x5==1,],2,mean) > mean(rppd[rppd$x5==1,'y']))/1000

print(c('unemployed',sum(fppd$x6==1)/length(fppd$x6)))
sum(apply(f.yrep[fppd$x6==1,],2,mean) > mean(fppd[fppd$x6==1,'y']))/1000
sum(apply(r.yrep[rppd$x6==1,],2,mean) > mean(rppd[rppd$x6==1,'y']))/1000

print(c('retired',sum(fppd$x4==1)/length(fppd$x4)))
sum(apply(f.yrep[fppd$x4==1,],2,mean) > mean(fppd[fppd$x4==1,'y']))/1000
sum(apply(r.yrep[rppd$x4==1,],2,mean) > mean(rppd[rppd$x4==1,'y']))/1000

#@@@@@@
#some plots for whole sample
#@@@@@@

#maximum
par(mfrow=c(2,2))
  hist(apply(r.yrep[rppd$x2<x2s,],2,min), xlim=c(-2,1),freq=F)
  abline(v=1)
  hist(apply(r.yrep[rppd$x2<x2s,],2,max), xlim=c(5,8.6),freq=F)
  abline(v=5)
  hist(apply(f.yrep[fppd$x2<x2s,],2,min), xlim=c(-2,1), freq=F)
  abline(v=1)
  hist(apply(f.yrep[fppd$x2<x2s,],2,max), xlim=c(5,8.6), freq=F)
  abline(v=5)

#continuous prediction for education 
#(by group--would be interesting to add time like employment/unemployment below)
par(mfrow=c(1,1))
yl = c(2,5)
xn = unique(fppd$z1)[order(unique(fppd$z1))]
plot(aggregate(fppd[,'y'],by=list(fppd$z1), mean), ylim=yl)
f.lm=apply(aggregate(f.yrep,by=list(fppd$z1),mean),1,quantile, prob=c(.08,0.92,0.5))
r.lm=apply(aggregate(r.yrep,by=list(fppd$z1),mean),1,quantile, prob=c(.08,0.92,0.5))
segments(xn-0.25,f.lm[1,],
         xn-0.25,f.lm[2,], lty=1)
segments(xn+0.25,r.lm[1,],
          xn+0.25,r.lm[2,], lty=4)
lines(xn-0.25,f.lm[3,],type='p',pch=20)
lines(xn+0.25,r.lm[3,],type='p',pch=20)

#male over time 
#par(mfrow=c(2,1),mar=c(1,2,0,0),oma=c(3,3,1,1))
par(mfrow=c(1,1))
yl=c(2.25,2.95)
male=fppd$z2==1 & fppd$age <1
female=fppd$z2==0 & fppd$age <1 
xn=seq(-6,0,by=2)
pr = c(0.08,.92,.5)

#male
plot(aggregate(fppd$y[male],by=list(fppd$age[male]),mean), type='p',pch=1, ylim=yl,xaxt='n',xlab='',xlim=c(-7,1))
f.lm=apply(aggregate(f.yrep[male,],by=list(fppd$age[male]),mean),1,quantile,prob=pr)
r.lm=apply(aggregate(r.yrep[male,],by=list(rppd$age[male]),mean),1,quantile,prob=pr)
segments(xn-.1, f.lm[1,],
         xn-.1, f.lm[2,], lty=1)
segments(xn+.1, r.lm[1,],
         xn+.1, r.lm[2,], lty=4)
lines(xn-.1,f.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
lines(xn+.1,r.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
legend('topleft',
       c('Male (obs)','Female (obs)','Fixed PPD','Random PPD'),
       lty=c(0,0,1,4),
       pch=c(1,2,NA,NA),
       bty='n',cex=.75)
#title('Male')
#legend('topright','Male', bty='n')

#female
lines(aggregate(fppd$y[female],by=list(fppd$age[female]),mean), type='p', pch=2,ylim=yl,xaxt='n',xlab='')
f.lm=apply(aggregate(f.yrep[female,],by=list(fppd$age[female]),mean),1,quantile,prob=pr)
r.lm=apply(aggregate(r.yrep[female,],by=list(rppd$age[female]),mean),1,quantile,prob=pr)
segments(xn-.2, f.lm[1,],
         xn-.2, f.lm[2,], lty=1)
segments(xn+.2, r.lm[1,],
         xn+.2, r.lm[2,], lty=4)
lines(xn-.2,f.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
lines(xn+.2,r.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
#title('female')
#legend('topright','Female', bty='n')
axis(1,at=xn,labels=xn+65)


#employment/unemployment (by time)
yl=c(2,3.75)
emp=fppd$x5==1
unemp=fppd$x6==1
xn=seq(-6,4,by=2)
pr = c(0.08,.92,.5)


plot(aggregate(fppd$y[emp],by=list(fppd$age[emp]),mean), ylim=yl,xaxt='n', xlab='', ylab='')
  f.lm=apply(aggregate(f.yrep[emp,],by=list(fppd$age[emp]),mean),1,quantile,prob=pr)
  r.lm=apply(aggregate(r.yrep[emp,],by=list(rppd$age[emp]),mean),1,quantile,prob=pr)
  segments(xn-.1, f.lm[1,],
         xn-.1, f.lm[2,], lty=1)
  segments(xn+.1, r.lm[1,],
         xn+.1, r.lm[2,], lty=4)
  lines(xn-0.1,f.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
  lines(xn+0.1,r.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))



lines(aggregate(fppd$y[unemp],by=list(fppd$age[unemp]),mean), type='p',pch=2)
  f.lm=apply(aggregate(f.yrep[unemp,],by=list(fppd$age[unemp]),mean),1,quantile,prob=pr)
  r.lm=apply(aggregate(r.yrep[unemp,],by=list(rppd$age[unemp]),mean),1,quantile,prob=pr)
  segments(xn-.2, f.lm[1,],
           xn-.2, f.lm[2,], lty=1)
  segments(xn+.2, r.lm[1,],
           xn+.2, r.lm[2,], lty=4)
  lines(xn-0.2,f.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
  lines(xn+0.2,r.lm[3,],type='p',pch=3, col=rgb(0,0,0,alpha=1))
axis(1,at=xn,labels=xn+65)
legend('topright',
       c('Employed (obs)','Unemployed (obs)','Fixed PPD','Random PPD'),
       lty=c(0,0,1,4),
       pch=c(1,2,NA,NA),
       bty='n', cex=.75)




#wealth plots 
yl=c(1,5)
neg=fppd$x2 < 0
#qmil=fppd$x2 > 0 & fppd$x2 <= log(250*1000) #close to median>0, higher than mean
#threeqmil=fppd$x2>log(250*1000) & fppd$x2 <= log(750*1000)
top = fppd$x2>log(1.680*10^6) #top 20% of positive
mid = fppd$x2 > 0 & fppd$x2 < log(1500*1000)

cuts = cbind(neg,mid,top)

xn=seq(-6,4,by=2)
pr = c(0.08,.92,.5)


par(mfrow=c(3,1),mar=c(1,2,0,0),oma=c(3,3,1,1))

for(w in 1:ncol(cuts)){
  print(w)

  plot(aggregate(fppd$y[cuts[,w]],by=list(fppd$age[cuts[,w]]),mean), xlab='',ylab='',ylim=yl)
  f.lm=apply(aggregate(f.yrep[cuts[,w],],by=list(fppd$age[cuts[,w]]),mean),1,quantile,prob=pr)
  r.lm=apply(aggregate(r.yrep[cuts[,w],],by=list(rppd$age[cuts[,w]]),mean),1,quantile,prob=pr)
  segments(xn-.1, f.lm[1,],
           xn-.1, f.lm[2,], lty=1)
  segments(xn+.1, r.lm[1,],
           xn+.1, r.lm[2,], lty=4)
  lines(xn-0.1,f.lm[3,],type='p',pch='--', col=rgb(0,0,0,alpha=1))
  lines(xn+0.1,r.lm[3,],type='p',pch='--', col=rgb(0,0,0,alpha=1))

}



