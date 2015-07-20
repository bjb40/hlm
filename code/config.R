#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Dev R 3.1.3 "Smooth Sidewalk"; x86_64-w64-mingw32/x64
#Script sets universal configuration and objects
#Bryce Bartlett
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#@@
#options

#turn off sicentific notation
options(scipen=999, digits=3)

#@@
#Directories

#datasource
rawdir = "H:/Academic Projects/Data Files/HRS/"
#parent directory
projdir = "H:/projects/hlm_final/"
#directory for output
outdir = "H:/projects/hlm_final/output/"

#@@
#Files and temp folders

#test for and create private~ output folder
if(file.exists(paste(outdir, "private~", sep=''))== F)
  {dir.create(paste(outdir,"private~",sep=''))}

#note that the tilde excludes folder from git repository (to keep private or draft binary)
subref = paste(outdir,"private~/subdat.csv",sep='')
cleanref = paste(outdir,"private~/cleandat.csv",sep='')

#test for and create draft_img~ folder
if(file.exists(paste(projdir,'draft_img~',sep=''))==F)
  {dir.create(paste(projdir,'draft_img~',sep=''))}

finalimg = paste(projdir,'img',sep='')
draftimg = paste(projdir,'draft_img~',sep='')