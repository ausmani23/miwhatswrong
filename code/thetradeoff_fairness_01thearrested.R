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

#profile of the arrested

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
tmptab<-table(anesdf$everarrested)
sum(tmptab[names(tmptab)%in%c('1','2')])/sum(tmptab)
# 
# if everyone who was black and missing was arrested?
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

tmptab <- tmptable[,1]/apply(tmptable,2,sum)[1]
#10% of the ever-arrested are black,
#14% hispanic, 1% native american

returndf<-data.frame(
  contact='everarrested',
  category='black',
  val=tmptab['black'] %>% unname
)
tmptable[,2]/apply(tmptable,2,sum)[2]


#by class
tmptable <- wtd.table(
  anesdf$class,
  anesdf$arrest_f,
  weights=anesdf$weight
)
tmptable/apply(tmptable,1,sum)

tmptab<-tmptable[,1]/apply(tmptable,2,sum)[1]
#15% of the ever arrested are hs dropouts
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact='everarrested',
    category='hsdropout',
    val=tmptab['hsdropout']
  )
)
tmptable[,2]/apply(tmptable,2,sum)[2]


#by gender
tmptable <- wtd.table(
  anesdf$gender_f,
  anesdf$arrest_f,
  weights=anesdf$weight
)
tmptable/apply(tmptable,1,sum)
tmptable[,1]/apply(tmptable,2,sum)[1]
tmptable[,2]/apply(tmptable,2,sum)[2]
#about 30% of men say htey have been arrested,
#only about 12% of women; 70% of the ever arrested are male

#by raceXclass
tmptable <- wtd.table(
  anesdf$raceXclass,
  anesdf$arrest_f,
  weights=anesdf$weight
)
tmptable/apply(tmptable,1,sum)
tmptab<-tmptable[,1]/apply(tmptable,2,sum)[1]
#2% of the ever-arrested are black HS dropouts
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact='everarrested',
    category='blackXhsdropout',
    val=tmptab['blackXhsdropout'] %>% unname
  )
)



tmptable[,2]/apply(tmptable,2,sum)[2]


#########################################################
#########################################################

#some of these numbers are a bit weird
#so complement this with some FBI data

totalpop <- 229735355
require(readxl); dir()

tmpdf<-readxl::read_xls('table-43a.xls') 
tmpdf<-tmpdf[8:37,1:7]
names(tmpdf)<-c(
  'offense',
  'total',
  'white',
  'black',
  'nativeamerican',
  'asian',
  'api'
)
tmpdf$year<-2019
tmpdf$offensecat<-'misc'
tmp<-tmpdf$offense%in%c(
  'Murder and nonnegligent manslaughter',
  'Rape3',
  'Robbery',
  'Aggravated assault',
  'Burglary', 
  'Larceny-theft',
  'Motor vehicle theft'
)
tmpdf$offensecat[tmp]<-'serious'

tmp<-tmpdf$offense%in%c(
  'Disorderly conduct',
  'Suspicion',
  'Curfew and loitering law violations',
  'All other offenses (except traffic)',
  'Vandalism',
  'Vagrancy',
  'Gambling',
  'Drunkenness',
  'Liquor laws',
  'Drug abuse violations'
)
tmpdf$offensecat[tmp]<-'petty'

tmpdf$offense<-NULL
tmpdf$total<-NULL
tmpdf<-gather(
  tmpdf,
  race,
  arrested,
  white:api
)
tmpdf$arrested <-as.numeric(tmpdf$arrested)


tmpdf<-data.table(tmpdf)
tmpdf<-tmpdf[
  ,
  .(
    arrested=sum(arrested)
  )
  ,
  by=c('offensecat','race')
]
tmpdf <- spread(
  tmpdf,
  offensecat,
  arrested
)
tmpdf$misc <- tmpdf$misc/sum(tmpdf$misc)
tmpdf$petty <-  tmpdf$petty/sum(tmpdf$petty)
tmpdf$serious <-  tmpdf$serious/sum(tmpdf$serious)

tmpdf

#(from above, 10% of the ever arrested are black)
#26% of the pettily annually arrested are black
#32% of the seriously annually arrested are black

returndf<-rbind.fill(
  returndf,
  data.frame(
    contact=c('seriouslyarrested','pettilyarrested'),
    category='black',
    val=c(
      tmpdf$serious[tmpdf$race=='black'],
      tmpdf$petty[tmpdf$race=='black']
    )
  )
)


#########################################################
#########################################################

#killed by police
setwd(datadir); dir()
tmpdf<-fread('fatalencounters.csv')
tmp<-tmpdf$`Date (Year)`==2019
tmpdf<-tmpdf[tmp,]
names(tmpdf)[names(tmpdf)=="Subject's race with imputations"]<-'race'
tmptab<-table(tmpdf$race)
tmptab<-tmptab/sum(tmptab)
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact=c('killedbypolice'),
    category='black',
    val=tmptab['African-American/Black']
  )
)

#########################################################
#########################################################


#output
setwd(filesdir)
write.csv(
  returndf,
  'thearrested.csv',
  row.names=F
)




