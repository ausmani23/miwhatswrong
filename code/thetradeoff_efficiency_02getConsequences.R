########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(ggplot2)
require(rprojroot)

homedir<-find_root(
  criterion=has_file('_miwhatswrong.Rproj')
)
datadir<-file.path(
  homedir,"data"
)
filesdir<-file.path(
  homedir,"files"
)
codedir<-file.path(
  homedir,"code"
)
outputdir<-file.path(
  homedir,"output"
)

#########################################################
#########################################################

#load image
setwd(filesdir); dir()
load('thetradeoff_data.RData')

#load functions
setwd(codedir); dir()
source('calculate_homicides.R')
source('calculate_costsbenefits.R')

#########################################################
#########################################################

#loop through the possible space
loopdf <- expand.grid(
  #the full two-dimensional space 
  prisonrate = seq(from=0,1000,by=10),
  policerate = seq(from=0,1000,by=10),
  #other choices
  myMethod = c('stepwise','direct'), #elasticity: stepwise or direct?
  myUnits = c('yearsoflife','lives'), #unit: years of life or lives
  myOrientation=c(
    'bestguess',
    'pessimistic_police_crime', # you think police do not really reduce crime
    'pessimistic_police_arrests', #you think police will result in lots of arrests
    'pessimistic_arrest', #you think arrest is really bad
    'pessimistic_police_killings', #you think police killings will increase
    'optimistic_prison' #optimistic about prison reducing crim
  ),
  myElasticities=c('constant','changing'), #elasticities: constant of changing?
  myPrizChoice=c('standard','deflated'), #prison: how bad is it as a year of life?
  pointType = '2d'
)

#plus key points
tmpdf <- data.frame(
  policerate=c(
    solutiondf$x,
    0.75 * 10^5 * police_2021/pop_2021, #defund
    10^5 * police_2021/pop_2021
  ),
  prisonrate=c(
    solutiondf$y,
    50, #defund
    incrateusa_2021
  ),
  pointType=c(
    'fwbalance',
    'defund',
    'statusquo'
  )
)

tmpdf <- merge(
  tmpdf,
  expand.grid(
    myMethod = c('stepwise','direct'), #elasticity: stepwise or direct?
    myUnits = c('yearsoflife','lives'), #unit: years of life or lives
    myOrientation=c(
      'bestguess',
      'pessimistic_police_crime', # you think police do not really reduce crime
      'pessimistic_police_arrests', #you think police will result in lots of arrests
      'pessimistic_arrest', #you think arrest is really bad
      'pessimistic_police_killings', #you think police killings will increase
      'optimistic_prison' #you think prison reduces crime
    ),
    myElasticities=c('constant','changing'), #elasticities: constant of changing?
    myPrizChoice=c('standard','deflated') #prison: how bad is it as a year of life?
  ),
  by=NULL
)
loopdf <- rbind.fill(
  loopdf,
  tmpdf
)

#we just want robustness checks
keyvars<-c(
  'myMethod',
  'myUnits',
  'myOrientation',
  'myElasticities',
  'myPrizChoice'
)
tmpdf <- unique(loopdf[,keyvars])
tmp_default<-tmpdf$myMethod=='stepwise' & 
  tmpdf$myUnits=='yearsoflife' &
  tmpdf$myOrientation=='bestguess' &
  tmpdf$myElasticities=='constant' &
  tmpdf$myPrizChoice=='standard' 
thisrow <- tmpdf[tmp_default,]
tmp<-lapply(names(tmpdf),function(tmpvar) {
  #tmpvar<-c('myMethod')
  tmp1<-tmpdf[[tmpvar]]!=tmpdf[[tmpvar]][tmp_default] 
  othvars<-names(tmpdf)[names(tmpdf)!=tmpvar]
  tmp2<-lapply(othvars,function(othvar) {
    tmpdf[[othvar]]==tmpdf[[othvar]][tmp_default]
  }) 
  tmp1 & Reduce(f="&",tmp2)
})
tmp_extra <- Reduce(f='|',tmp)
tmpdf<-tmpdf[tmp_default | tmp_extra,]
tmplevels<-c(
  #
  'Preferred Choices',
  #
  'Not Stepwise',
  #
  'Lives Lost',
  #
  'Police Dont Reduce Crime',
  'Police Lead to Lots of Arrests',
  'Arrest = 1 Week in Prison',
  'More Police More Police Violence',
  'Prison Does Reduce Crime',
  #
  'Changing Elasticities',
  #
  'Prison-Year Deflated'
)
tmpdf$choice<-tmplevels
tmpdf$choice<-factor(tmpdf$choice,tmplevels)
loopdf<-merge(
  loopdf,
  tmpdf
) %>% data.table

loopdf$i<-1:nrow(loopdf)
myoutput <- lapply(loopdf$i,function(i) {
  #i<-1
  print(paste(i,'of',max(loopdf$i)))
  #print(i)
  tmplist <- calculate_costsbenefits(
    policerate_proposed = loopdf$policerate[i],
    prisonrate_proposed = loopdf$prisonrate[i],
    myMethod = loopdf$myMethod[i],
    myUnits = loopdf$myUnits[i],
    myOrientation = loopdf$myOrientation[i],
    myElasticities = loopdf$myElasticities[i],
    myPrizChoice = loopdf$myPrizChoice[i]
  )
  consequencesdf<-tmplist$consequencesdf
  if(!is.null(consequencesdf)) {
    consequencesdf$i<-i
  }
  list(
    returndf=data.frame(
      i = i,
      finalguess = tmplist$finalguess,
      moneysaved = tmplist$moneysaved
    ),
    consequencesdf=consequencesdf
  )
})

#get consequences
outputdf<-lapply(myoutput,function(x) x$returndf) %>% rbind.fill %>% data.table

#drop the points which are effecitvely impossible to reach
outputdf<-outputdf[!is.na(outputdf$finalguess),]
sumdf <- merge(loopdf,outputdf) %>% data.table

#save out
setwd(filesdir)
write.csv(
  sumdf,
  "thetradeoff_sumdf.csv",
  row.names=F
)

#also output consequences
consequencesdf<-lapply(myoutput,function(x) {
  x$consequencesdf
}) %>% rbind.fill %>% data.table
consequencesdf <- merge(loopdf,consequencesdf,by='i') %>% data.table

#save out
setwd(filesdir)
write.csv(
  consequencesdf,
  "thetradeoff_consequences.csv",
  row.names=F
)

#########################################################
#########################################################



