#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Dev R 3.1.3 "Smooth Sidewalk"; x86_64-w64-mingw32/x64
#Script Loads and cleans HRS data to compare 
#growth curve (hierarchical) /
#with "fixed effects" (causal) models
#Bryce Bartlett
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#load universals configuration file
source("H:/projects/hlm_final/code/config.R",
echo =T, print.eval = T, keep.source=T)

#@@@@@@@@@@@@@@@@@@@@@
#Load and subset Raw HRS Data and select relevant subset of data
#Raw data downloaded on 1/29/2015 from https://ssl.isr.umich.edu/hrs/files2.php?versid=34  
#RAND center on aging pooled HRS data file.
#@@@@@@@@@@@@@@@@@@@@@

if (file.exists(subref) | file.exists(cleanref)) {
  print('Data already subset, skipping import of raw data.')
  
} else {
  
  print(paste("Subsetting HRS data and placing in",subref))
  
  #unzip on the fly
  rawzip = paste(rawdir,"randnstata.zip",sep='')
  zipdat = unzip(rawzip,"rndhrs_n.dta")
  
  library(foreign)
  rawdat = read.dta(zipdat, convert.factors = FALSE)
  #rawdat.df = data.frame(rawdat)
  
  
  #lmited to wave 6 (2002) to wave 11 (2012) 
  waves = 6:11
  years = c(rep(NA,5),2002,2004,2006,2008,2010,2012)
  
  #this function builds character arrays based on
  #rand data and the waves selected
  makeseq = function(pre,w,post){
    return(paste(pre,w,post,sep=''))
  }

  #select variables to subset
  vars = c(
          #independant id variable
          'hhidpn',
          #key dependant (growth) variable
          #makeseq('r',waves,'cesd'),
          
          #alt growth variable percentages (leaving retirement,suriviving,continue working)
          #makeseq('r',waves,'work62'),
          makeseq('r',waves,'shlt'), #self-reported health
          
          #@@time variable baseline to be calculated from birthyear
          'rabyear',
          
          #@@time-invariant variables associated with depressive symptoms
                    
          #gender 1=male, 2=female
          'ragender',
          
          #education
          'raedyrs',
          
          #race 1=white, 2=Black, 3=other
          'raracem',
          
          #@@time varying controls
                    
          #income (household; rand imputed: dollars)--may need separate SSI
          makeseq('h',waves,'itot'),
          
          #wealth (household; rand imputed: dollars; includes all but second house? 
          #(there are multiple options))
          makeseq('h',waves,'atota'),
          
          #current marital status: married/cohab (1-3), sep/div(4-6), widowed (7) nm (8)
          makeseq('r',waves,'mstat'),
          
          #work status (ft or pt, unemployed, retired, homemaker)
          #.A=presumed retired, .Q not asked, .T worked last 2 year: 1=FT,2=PT,3=UE,
          #4=part ret, 5=retired, 6=disabled, 7=not in labor force
          makeseq('r',waves,'lbrf'),
          
          #retirement plans rwplnyr (plan) rwplnya (thinks) rwsayret (consideres self)
          #retirement dates rwretyr and rwretmon
          #work full time, want, etc
          #cool risk aversion and related questions, too
          
          #@@weights
          makeseq('r',waves,'wtresp')
  )

  #subset dataframe
  subdat = subset(rawdat,select=c(vars))
  
  #remove raw data from memory and working directory
  rm(rawdat)
  file.remove("rndhrs_n.dta")
  
  #save raw subset in "output" directory
  write.csv(subdat, file=paste(subref))
}

#@@@@@@@@@@@@@@@@@@@@@
#Output "spot checks" for subset accuracy
#@@@@@@@@@@@@@@@@@@@@@

sink(paste(outdir,'private~/output-mean.txt',sep=''))
  print(Sys.Date(),quote="F")
  summary(subdat)  
sink()

#@@@@@@@@@@@@@@@@@@@@@
#Recode, subset to workable size, and transition to "long"
#1. build a wave-specific set and append
#@@@@@@@@@@@@@@@@@@@@@

#summarize variables and initialize clean dataset
#fixed
fvars = c('hhidpn','rabyear','ragender','raedyrs','raracem')
#respondent change variables
#rcvars = c('cesd','mstat','lbrf')
rcvars = c('shlt','mstat','lbrf')
#household change variables
hcvars = c('itot','atota')

longdat = as.data.frame(setNames(replicate(length(c(fvars,rcvars,hcvars))+1,numeric(0),
                                           simplify=F),
                                           c(fvars,rcvars,hcvars,'wave')))


for(w in waves){

  print(paste('Constructing wave',w))
  vars = c(fvars,paste('r',w,rcvars,sep=''),paste('h',w,hcvars,sep=''))
  #print(vars)
  
  tempdat=subset(subdat,
       select=vars)
  
  tempdat$wave=w
  colnames(tempdat)=colnames(longdat)
  
  #append for spot cheking
  sink(paste(outdir,'private~/output-mean.txt',sep=''),append=T)
    cat('\n\n@@@@@@@@@@@@@@@@@@@\nWAVE ')
    cat(w)
    cat(' changed to long\n@@@@@@@@@@@@@@@@@@@@@\n\n')
    print(summary(tempdat)) 
  sink()
  
  longdat=rbind(longdat,tempdat)
  
}

rm(subdat, tempdat)

#@@@@@@@@@@@@@@@@@@@@@
#Recode, subset to workable size, and transition to "long"
#2. recode and listwise delete
#@@@@@@@@@@@@@@@@@@@@@

longdat$age=years[longdat$wave] - longdat$rabyear
longdat$male=longdat$ragender
  longdat$male[longdat$ragender==2] = 0
longdat$black=NA
  longdat$black[longdat$raracem %in% c(1,3)] = 0
  longdat$black[longdat$raracem ==2] = 1
longdat$orace=NA
  longdat$orace[longdat$raracem %in% c(1,2)] = 0
  longdat$orace[longdat$raracem ==3] = 1
longdat$marr=NA
  longdat$marr[longdat$mstat %in% c(1:2)] = 1
  longdat$marr[longdat$mstat %in% c(3:8)] = 0
longdat$ret = NA
  longdat$ret[longdat$lbrf == 4:5] = 1 #includes "partly retired, i.e. ret with pt job
  longdat$ret[longdat$lbrf %in% c(1:3,6:7)] = 0
longdat$emp = NA
  longdat$emp[longdat$lbrf %in% c(3:7)] = 0 
  longdat$emp[longdat$lbrf %in% c(1:2)] = 1
longdat$unemp = NA
  longdat$unemp[longdat$lbrf %in% c(1:2,4:5)] = 0
  longdat$unemp[longdat$lbrf %in% c(3,6:7)] = 1

sink(paste(outdir,'private~/output-mean.txt',sep=''),append=T)
  cat('\n\n@@@@@@@@@@@@@@@\nCleaning of data\n@@@@@@@@@@@@@@@\n\n')
  table(longdat$ragender,longdat$male,dnn=c('ragender','male'))
  cat('\nRace dummy series (ref=white)\n')
  table(longdat$raracem,longdat$orace,dnn=c('raracem','orace'))
  cat('\n')
  table(longdat$raracem,longdat$black,dnn=c('raracem','black'))
  cat('\nMarried dummy\n')
  table(longdat$mstat,longdat$marr,dnn=c('mstat','marr'))
  cat('\n')
  cat('\n Labor Force dummy series (inclusive)\n')
  table(longdat$lbrf,longdat$emp,dnn=c('lbrf','ret'))
  cat('\n')
  table(longdat$lbrf,longdat$emp,dnn=c('lbrf','emp'))
  cat('\n')
  table(longdat$lbrf,longdat$unemp,dnn=c('lbrf','unemp'))

sink()

#@@@@@@@@@@@@@@@@@@@@@
#Recode, subset to workable size, and transition to "long"
#3. arbitrary limit to make size workable (can be removed for substantive anlaysis)
#@@@@@@@@@@@@@@@@@@@@@

#listwise delete for complete (within wave) cases
cleandat=na.omit(longdat[,!(names(longdat) %in% c('ragender','raracem','mstat','lbrf'))])

#print overview of deletions
sink(paste(outdir,'cleandat-overview.txt',sep=''))
  print(Sys.Date(),quote=F)
  cat('\n                             \t\tPersons\t\tObservations')
  cat('\nTotals                      ',length(unique(longdat$hhidpn)),nrow(longdat),sep='\t\t')
  cat('\nListwise Delete             ',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
  #iobs = data.frame(aggregate(cleandat$hhidpn, by=list(cleandat$hhidpn), length))
  #cleandat=cleandat[cleandat$hhidpn %in% iobs[iobs[2]>4,1],]
  #cat('\nIndividuals observed 5+ waves',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
  cleandat=cleandat[cleandat$orace==0,]
  cat('\nDelete Orace                ',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
  cleandat=cleandat[cleandat$rabyear==(2008-65),]
  cat('\nLimit 65 at recession (2008)',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
  #create random subsample of 300 from remaining
  samp = sample(unique(cleandat$hhidpn),size=300,replace=F)
  cleandat=cleandat[cleandat$hhidpn %in% samp,]
  cat('\n300 random sample (person)  ',length(unique(cleandat$hhidpn)),nrow(cleandat),sep='\t\t')
sink()

#@@@@@@@@@@@@@@@@@@@@@
#Write clean dataset
#@@@@@@@@@@@@@@@@@@@@@

write.csv(cleandat,file=cleanref)

#@@@@@@@@@@@@@@@@@@@@@
#output descriptive statistics for clean set
#@@@@@@@@@@@@@@@@@@@@@

attach(cleandat)
agg=aggregate(cleandat,by=list(wave), FUN=function(x) 
  c(mn=mean(x),sdev=sd(x),min=min(x),max=max(x),n=length(x) ) )
detach(cleandat)

sink(paste(outdir,'cleandat-overview.txt',sep=''),append=T)
  cat('\n\nDescriptives\n')
  print(round(t(agg),digits=2))
sink()

rm(agg,longdat)


#@@@@@@@@@@@@@@@@@@@@@
#Output subset suitable for analysis in Winbugs
# and stata for basic HLM and individual fixed effects
#long data by individual
#@@@@@@@@@@@@@@@@@@@@@

y = 'shlt'
t = 'age'

#invariant
z = c('raedyrs','male','black')

#variant
x = c('itot','atota','marr','ret','emp','unemp')
cx = paste('c',x,sep='')

#change id to serialized number
cleandat$sid = NA # intialize
id = unique(cleandat$hhidpn) #collect unique ids

for(i in 1:length(id)){
  cleandat$sid[cleandat$hhidpn == id[i]] = i 
}

sink(paste(outdir,'transform-dat.txt',sep=''))
  print(Sys.Date(),quote="F")
  cat('Test of recode from unique ID to serial ID (10 random ids)\n\n')
  tst = cleandat[cleandat$hhidpn %in% sample(id,10),c('hhidpn','sid','wave','age','male')]
  print(tst[sort(tst$hhidpn, index.return=T)$ix,], row.names=F)
sink()

bugsdat = cbind(
            cleandat[,'sid'],
            cleandat[,y],
            cleandat[,t],
            cleandat[,z],
            cleandat[,x]
                )
oldcol = colnames(bugsdat)
colnames(bugsdat) = c(
                        'id',
                        'y',
                        'age',
                        paste('z',1:length(z),sep=''),
                        paste('x',1:length(x),sep='')
                      )

#reorder by id
resort = sort(bugsdat[,'id'], index.return=T)$ix
bugsdat = bugsdat[resort,]


sink(paste(outdir,'transform-dat.txt',sep=''),append=T)
  cat('\n\n@@@@@@@\nRenames of variable columns\n@@@@@@@\n\n')
    print(cbind(oldcol,colnames(bugsdat)))

  cat('\n\n@@@@@@@\nRecodes\n@@@@@@@\n\n')
  cat('\n\tCenter age at 65')
    bugsdat[,'age'] = bugsdat[,'age']-65
  cat('\n\tCenter education at grand mean')
    print(mean(bugsdat[,'z1']))
    bugsdat[,'z1'] = bugsdat[,'z1'] - mean(bugsdat[,'z1'])
  cat('\n\tLog income after adding 1 (to prevent log(0))')
    bugsdat[,'x1'] = log(bugsdat[,'x1']+1)
  cat('\n\tLog abs(wealth+1)*(wealth+1/abs(wealth+1)) (to prevent NaN)')
    bugsdat[,'x2'] = log(abs(bugsdat[,'x2']+1))*((bugsdat[,'x2']+1)/abs(bugsdat[,'x2']+1))
  cat('\n\tCenter time variant variables within individuals (group mean centering\n)')
    #calculate means
    imean = aggregate(
      bugsdat[,c('y',paste('x',1:length(x),sep=''))], 
      by=list(bugsdat$id), FUN=mean)

    #demean
    tvar = c('y',paste('x',1:length(x),sep=''))
    bugsdat[,paste('c',tvar,sep='')] = as.numeric(NA)
    for(i in 1:nrow(imean)){
      for(j in tvar){
          bugsdat[bugsdat['id']==i,paste('c',j,sep='')] = bugsdat[bugsdat['id']==i,j] - imean[imean['Group.1']==i,j]
        }
      }
  cat('\n\tPrint individual and centered means for 5 random individuals to confirm coding.\n\n')
    rn=sample(unique(bugsdat$id))
    print(bugsdat[bugsdat$id %in% c(sample(unique(bugsdat$id),5)),c('id','age',tvar,paste('c',tvar,sep=''))])
  cat('\n\tProportion of individuals with change in time-varying dummy variables any time:\n')
      apply(imean[,c(5:ncol(imean))],2,function(j) 1-sum(j ==0 | j==1)/length(j))
  cat('\n\tProportion of observations with change on dep variable: ')
    print(1-sum(bugsdat$cy==0)/nrow(bugsdat))
  cat('\n\tProportion of individuals with change on dep variable across at least 1 wave: ')
    print(1-length(unique(bugsdat[bugsdat$cy==0,'id']))/length(unique(bugsdat$id)))

  cat('\n\n@@@@@@@\nDescriptives (by wave for comparison)\n@@@@@@@\n\n')
    attach(bugsdat)
      agg=aggregate(bugsdat, by=list(cleandat[resort,'wave']), FUN=function(x) 
      c(mn=mean(x),sdev=sd(x),min=min(x),max=max(x),n=length(x) ) )
    detach(bugsdat)
  print(round(t(agg),digits=2), row.names=F)
sink()


#second datasat limited to individual fixed time characteristics
bugsidat = unique(bugsdat[,c(1,4:6)])


#need to make a few edits to make compatible with BUGS; 
#"long" data model follwed from example in /Examples/Lizards.odc
#sub exampel in ../output/ibugs.dat (includes invidudual level data)

#need to manually replace cap L
sink(paste(outdir,'bugs.dat',sep=''))

  cat('i=')
  length(unique(bugsdat$id))

  #individual level data
  cat('nobs=')
  nrow(bugsdat)
  cat('id=') #need to delete list statement
  dput(as.list(bugsidat[,1])) 
  cat(',z=.Data=')
  dput(matrix(bugsidat[,c(2:4)])) 

  #time level data
  cat('\n obsid=')
  dput(bugsdat[,1])
  cat('y=')
  dput(bugsdat[,2])
  cat('age=')
  dput(bugsdat[,3])
  cat('x=')
  dput(bugsdat[,paste('x',1:length(x),sep='')])
  cat('cy=')
  dput(bugsdat[,'cy'])
  cat('cx=')
  dput(bugsdat[,paste('cx',1:length(x),sep='')])


sink()

#export data for use in stata
library(foreign)
write.dta(bugsdat,paste(outdir,'basic_hlm.dta',sep=''))

