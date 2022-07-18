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

#########################################################
#########################################################

#BLACK AND WHITE ARRESTEES

setwd(datadir); dir()
require(haven)
anesdf<-read_dta('anes_timeseries_2016.dta') %>% 
  data.table
myvars<-c(
  "V160102", #weight
  "V160201", #stratum
  "V160202", #psu
  "V162298", #everarrested
  "V161310x", #race
  "V161319x", #hispanic
  "V161270", #education
  "V161267", #age
  "V161342" #gender
)
anesdf <- anesdf[,myvars,with=F]
names(anesdf)<-c(
  'weight',
  'stratum',
  'psu',
  'everarrested',
  'race',
  'hispanic',
  'education',
  'age',
  'gender'
)

#recodes

#adjust weights to give us the population of usa
usapop <- 224.1 * 10^6 #codebook says reps this pop
anesdf$weight_f <- anesdf$weight * usapop/sum(anesdf$weight)

#race
anesdf$race_f<-NA_character_
tmp<-anesdf$race==1
anesdf$race_f[tmp]<-"white"
tmp<-anesdf$race==2
anesdf$race_f[tmp]<-"black"
tmp<-anesdf$race==5
anesdf$race_f[tmp]<-"hispanic"
tmp<-anesdf$race==3
anesdf$race_f[tmp]<-"asian"
tmp<-anesdf$race==4
anesdf$race_f[tmp]<-'nativeamerican'
tmp<-anesdf$race==6
anesdf$race_f[tmp]<-'other'

#black/white
anesdf$black<-as.numeric(anesdf$race_f=='black')
anesdf$black[anesdf$black==1]<-'black'
anesdf$black[anesdf$black==0]<-'not black'

#class
anesdf$education
anesdf$ed_f<-NA
tmp<-anesdf$education%in%c(1:8)
anesdf$ed_f[tmp]<-'hsdropout'
tmp<-anesdf$education%in%c(9:95)
anesdf$ed_f[tmp]<-'hsgrad+' 
anesdf$class<-as.numeric(anesdf$ed_f=='hsdropout')
anesdf$class[anesdf$class==1]<-'hsdropout'
anesdf$class[anesdf$class==0]<-'hsgrad+'

#gender
anesdf$gender_f<-NA
tmp<-anesdf$gender==1
anesdf$gender_f[tmp]<-'male'
tmp<-anesdf$gender==2
anesdf$gender_f[tmp]<-'female'
tmp<-anesdf$gender==3
anesdf$gender_f[tmp]<-'other'

#arrest
anesdf$arrest_f<-NA
tmp<-anesdf$everarrested==1
anesdf$arrest_f[tmp]<-"everarrested"
tmp<-anesdf$everarrested==2
anesdf$arrest_f[tmp]<-"neverarrested"

#if everyone who was black and missing was arrested?
# anesdf$arrest_f[
#   is.na(anesdf$arrest_f) &
#     anesdf$race_f=='black' &
#     !is.na(anesdf$race_f)
# ]<-'everarrested'


require(questionr)
wtd.table(anesdf$arrest_f,weights=anesdf$weight_f) #46 mill

#raceXclass
anesdf$raceXclass<-paste0(
  anesdf$black,"X",anesdf$class 
)
anesdf$raceXclass[
  is.na(anesdf$black) |
    is.na(anesdf$class)
] <- NA
tmp<-anesdf$raceXclass!='blackXhsdropout' &
  !is.na(anesdf$raceXclass)
anesdf$raceXclass[tmp]<-'other'


#summary stats

#what % of blacks have been ever arrested
tmp<-anesdf$black=='black'
weighted.mean(
  anesdf$arrest_f[tmp]=='everarrested',
  anesdf$weight[tmp],
  na.rm=T
)
#about 20%.. 

#by race
tmptable <- wtd.table(
  anesdf$race_f,
  anesdf$arrest_f,
  weights=anesdf$weight
)
tmptable/apply(tmptable,1,sum)
#blacks are not really overrepresented
#amongst the ranks of the ever arrested
#whites are slightly underrepresented,
#but not by much.. (70% vs. 67%)

#########################################################
#########################################################






















