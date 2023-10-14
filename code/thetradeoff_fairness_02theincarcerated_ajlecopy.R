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
codedir<-file.path(
  homedir,"code"
)
outputdir<-file.path(
  homedir,"output"
)
filesdir<-file.path(
  homedir,'files'
)

setwd(codedir)
source('theme_black.R')

#########################################################
#########################################################

setwd(datadir); dir()
require(haven)
spidf<-read_dta(
  '37692-0001-Data.dta'
) %>% data.table
names(spidf)<-tolower(names(spidf))

#########################################################
#########################################################

myvars<-c(
  'v1953', #black
  'v0935', #education
  'v1585' #weight
)
spidf<-spidf[,myvars,with=F]
names(spidf)<-c(
  'black',
  'highested',
  'weight_f'
)

#########################################################
#########################################################

#recodes

#race
spidf$black_f<-0
tmp<-spidf$black==1
spidf$black_f[tmp]<-1

#class
spidf$hsdropout<-0
tmp<-spidf$highested%in%c(1:11)
spidf$hsdropout[tmp]<-1

#raceXclass
spidf$raceXclass<-0
tmp<-spidf$black_f==1 & 
  spidf$hsdropout==1
spidf$raceXclass[tmp]<-1

#summary stats
tmptab<-weighted.mean(
  spidf$black_f,
  spidf$weight_f,
  na.rm=T
) 

#41% of the incarcerated are balck
returndf<-data.frame(
  contact='incarcerated',
  category='black',
  val=tmptab
)

tmptab<-weighted.mean(
  spidf$hsdropout,
  spidf$weight_f,
  na.rm=T
) 
#54% of them are hs dropouts
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact=c('incarcerated'),
    category='hsdropout',
    val=tmptab
  )
)


tmptab<-weighted.mean(
  spidf$raceXclass,
  spidf$weight_f,
  na.rm=T
)
#24 % of those behind bars are
#black hsdropouts
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact=c('incarcerated'),
    category='blackXhsdropout',
    val=tmptab
  )
)

#########################################################
#########################################################

#what about relative probabilities?
#need pop data.. 

#########################################################
#########################################################

setwd(filesdir)
write.csv(
  returndf,
  "theincarcerated.csv",
  row.names=F
)

