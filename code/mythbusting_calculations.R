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

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$|\\.png$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$|\\.png$",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}

#########################################################
#########################################################

#2021 ACS estimates
# setwd(datadir); dir()
# tmpdf<-fread(
#   's2603_groupquarters.csv'
# )
#correctional data are from
#https://data.census.gov/cedsci/table?q=B02001
totalcorrectionalpop <- 2069113
blackshare <- 0.4220
whiteshare <- 0.3750 #white alone
blackcorrectionalpop <- blackshare * totalcorrectionalpop
whitecorrectionalpop <- whiteshare * totalcorrectionalpop

#population data are from
#https://data.census.gov/cedsci/table?q=ACS%20DEMOGRAPHIC&t=Populations%20and%20People&y=2019&tid=ACSST1Y2019.S0101
blackpop <- 46713850
whitepop <- 196789401
totalpop<-328239523

#rates
blackrate <- 10^5 * (blackcorrectionalpop/blackpop)
whiterate <- 10^5 * (whitecorrectionalpop/whitepop)

#disparity
blackrate/whiterate

#federal sentencing disparity as %
25/374

#########################################################
#########################################################

#drug share, 2022

#data from
#https://www.prisonpolicy.org/reports/pie2022.html

prisoners <- c(
  State=1042000,
  Jail=547000,
  Federal=208000,
  Juvenile=36000
)
prisoners_drug <-c(
  State=146000,
  Jail=113000 + 24000,
  Federal=21000 + 67000,
  Juvenile=2500
)
prisoners_violent <-c(
  State=606000,
  Jail=22000 + 141000,
  Federal=11000,
  Juvenile=0
)
prisoners_property <-c(
  State=159000,
  Jail=111000 + 25000,
  Federal=7000,
  Juvenile=9900
)

#random stats for writing
sum(prisoners_drug)/sum(prisoners) #20%
sum(prisoners_violent)/sum(prisoners) #42%
sum(prisoners_property)/sum(prisoners) #17%

#incarceration rate w/o drugs
10^5 * sum(prisoners)/totalpop
10^5 * (sum(prisoners)-sum(prisoners_drug))/totalpop

#share by type
shares <- c(
  100 * prisoners_drug/prisoners,
  Overall=100 * sum(prisoners_drug)/sum(prisoners)
)
plotdf<-data.frame(
  var=names(shares),
  val=shares %>% unname
)
plotdf$var<-factor(
  plotdf$var,
  c('Juvenile','Jail','State','Federal','Overall')
)
tmpcolors<-c('blue','red','red','red','red')
names(tmpcolors)<-levels(plotdf$var) %>% rev
g.tmp<-ggplot(
  plotdf,
  aes(
    x=var,
    y=val,
    fill=var
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    color='black'
  ) +
  scale_fill_manual(
    guide='none',
    values=tmpcolors
  ) +
  ylab('\nDrug Prisoners as %\n') +
  xlab('') +
  theme_bw() +
  coord_flip()

setwd(outputdir)
ggsave(
  filename='fig_mythbusting_drugshare.png',
  plot=g.tmp,
  width=4*1.5,
  height=2*1.5
)

#########################################################
#########################################################

#get the other data from the UNODC
setwd(datadir)
undf<-fread('unodc_contemporary.csv')
undf[statistic=='prisoners_sentenced']
undf<-undf[
  statistic%in%c(
    'prisoners',
    'prisoners_sentenced',
    'prisoners_sentenced_drug',
    'prisoners_sentenced_drugpossession',
    'prisoners_sentenced_drugtrafficking',
    ###
    'arrests',
    'arrests_drug',
    'arrests_drugpossession',
    'arrests_drugtrafficking'
  ) &
    gender=="" & 
    age==""
  ,
  c(
    'countryname',
    'statistic',
    'year',
    'value'
  )
]
undf$id<-paste0(undf$countryname," x ",undf$year)
undf<-spread(undf,statistic,value)
undf$prisoners_sentenced_drug2 <-
  undf$prisoners_sentenced_drugpossession + 
  undf$prisoners_sentenced_drugtrafficking
undf$arrests_drug2 <-
  undf$arrests_drugpossession + 
  undf$arrests_drugtrafficking
#we'll use the alternative estimate where drug is missing
undf$prisoners_sentenced_drug[is.na(undf$prisoners_sentenced_drug)]<-
  undf$prisoners_sentenced_drug2[is.na(undf$prisoners_sentenced_drug)]
undf$arrests_drug[is.na(undf$arrests_drug)]<-
  undf$arrests_drug2[is.na(undf$arrests_drug)]

#our best estimate is 
#prisoners_drug/prisoners_sentenced
undf$prisoners_drugshare <- undf$prisoners_sentenced_drug/undf$prisoners_sentenced
undf$arrests_drugshare <- undf$arrests_drug/undf$arrests

#we'll use US-specific data instead of the UN data
#(this is biased against our conclusion, since UN seems to underestimte US share)
undf$group<-'RoW'
undf<-undf[undf$countryname!='United States of America',]

#add the US specific data
#add US-specific data
#prisoner data comes from above
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-29
tmpdf <- data.frame(
  countryname='USA',
  arrests_drugshare=1558862 / 10085207,
  prisoners_drugshare=sum(prisoners_drug)/sum(prisoners)
)
undf<-rbind.fill(
  undf,
  tmpdf
) %>% data.table

#collapse to country median
plotdf <- undf[
  ,
  .(
    arrests_drugshare=100 * median(arrests_drugshare,na.rm=T),
    prisoners_drugshare=100 * median(prisoners_drugshare,na.rm=T)
  )
  ,
  by=c(
    'countryname'
  )
]
plotdf$group<-'Rest of World'
plotdf$group[plotdf$countryname=='USA']<-'USA'
plotdf <- plotdf[
  ,
  .(
    type=c('ymin','y','ymax'),
    arrests=quantile(arrests_drugshare,c(0.2,0.5,0.8),na.rm=T),
    prisoners=quantile(prisoners_drugshare,c(0.2,0.5,0.8),na.rm=T)
  )
  ,
  by=c(
    'group'
  )
]
plotdf<-pivot_longer(
  plotdf,
  c('arrests','prisoners')
)
toproper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
plotdf$name<-toproper(plotdf$name)
plotdf<-spread(plotdf,type,value)
g.tmp <- ggplot(
  plotdf,
  aes(
    x=group,
    y=y,
    ymin=ymin,
    ymax=ymax
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    fill='red',
    color='black'
  ) +
  geom_errorbar(
    width=0.1
  ) +
  facet_wrap(
    ~ name
  ) +
  coord_flip() +
  theme_bw() +
  xlab('') +
  ylab('\nDrug Share as %')


setwd(outputdir)
ggsave(
  filename='fig_mythbusting_drugshare_worldwide.png',
  plot=g.tmp,
  width=5*1.5,
  height=2*1.5
)

#########################################################
#########################################################

#use the SPI to identify drug share 
#and racial disparities in and out
setwd(datadir); dir()
require(haven)
spidf<-read_dta(
  '37692-0001-Data.dta'
) %>% data.table
names(spidf)<-tolower(names(spidf))
myvars<-c(
  'v1951', #hispanic
  'v1952', #white
  'v1953', #black
  'v0935', #education
  'v0062', #offense type
  'v1585' #weight
)
spidf<-spidf[,myvars,with=F]
names(spidf)<-c(
  'hispanic',
  'white',
  'black',
  'highested',
  'offense_type',
  'weight_f'
)

#race
spidf$hispanic_f<-0
tmp<-spidf$hispanic==1
spidf$hispanic_f[tmp]<-1
spidf$white_f<-0
tmp<-spidf$white==1
spidf$white_f[tmp]<-1
spidf$black_f<-0
tmp<-spidf$black==1
spidf$black_f[tmp]<-1
spidf$race_f<-4
spidf$race_f[spidf$white_f==1 & spidf$hispanic_f!=1]<-1
spidf$race_f[spidf$black_f==1 & spidf$hispanic_f!=1]<-2
spidf$race_f[spidf$hispanic_f==1]<-3

#offense
spidf$offense_f[spidf$offense_type==1 & !is.na(spidf$offense_type)]<-'violent'
spidf$offense_f[spidf$offense_type==2 & !is.na(spidf$offense_type)]<-'property'
spidf$offense_f[spidf$offense_type==3 & !is.na(spidf$offense_type)]<-'drug'
spidf$offense_f[spidf$offense_type==4 & !is.na(spidf$offense_type)]<-'publicorder'
spidf$offense_f[spidf$offense_type==5 & !is.na(spidf$offense_type)]<-'other'
spidf$offense_f[is.na(spidf$offense_type)]<-'not given'
table(spidf$offense_f,spidf$offense_type,useNA='a') #some are missing due to interview issue

#get numbers to compute disparities, w/ and w/o drugs
#https://data.census.gov/cedsci/table?q=ACS%20DEMOGRAPHIC&t=Populations%20and%20People&y=2019&tid=ACSST1Y2019.S0101
blackpop <- 46713850
whitepop <- 196789401
prisoners_white <- sum(spidf$weight_f[spidf$race_f==1])
prisoners_black <- sum(spidf$weight_f[spidf$race_f==2])
prisoners_drug <- sum(spidf$weight_f[spidf$offense_f=='drug'])
prisoners_black_drug <- sum(spidf$weight_f[spidf$offense_f=='drug' & spidf$race_f==2])
prisoners_white_drug <- sum(spidf$weight_f[spidf$offense_f=='drug' & spidf$race_f==1])
prisoners_black_nondrug <- sum(spidf$weight_f[spidf$offense_f!='drug' & spidf$race_f==2])
prisoners_white_nondrug <- sum(spidf$weight_f[spidf$offense_f!='drug' & spidf$race_f==1])
prisoners_black_violent <- sum(spidf$weight_f[spidf$offense_f%in%c('violent') & spidf$race_f==2])
prisoners_white_violent <- sum(spidf$weight_f[spidf$offense_f%in%c('violent') & spidf$race_f==1])

#the disparity
bwdisparity <- (10^5 * prisoners_black/blackpop) / 
  (10^5 * prisoners_white/whitepop)
bwdisparity_drug <- (10^5 * prisoners_black_drug/blackpop) / 
  (10^5 * prisoners_white_drug/whitepop)
bwdisparity_nondrug <- (10^5 * prisoners_black_nondrug/blackpop) / 
  (10^5 * prisoners_white_nondrug/whitepop)
bwdisparity_violent <- (10^5 * prisoners_black_violent/blackpop) / 
  (10^5 * prisoners_white_violent/whitepop)

#so, if we only incarcerated for violence,
#the disparity would only be 3% lower
bwdisparity_violent/bwdisparity 
plotdf<-data.frame(
  var=c('All','Drug','All Non-Drug','Violent'),
  val=c(bwdisparity,bwdisparity_drug,bwdisparity_nondrug,bwdisparity_violent)
)
plotdf$var<-factor(plotdf$var,c('Violent','All Non-Drug','All','Drug'))
tmpcolors<-c('red','red','red','blue')
names(tmpcolors)<-levels(plotdf$var)
g.tmp<-ggplot(
  plotdf,
  aes(
    x=var,
    y=val,
    fill=var
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    color='black'
  ) +
  scale_fill_manual(
    values=tmpcolors,
    guide='none'
  ) +
  ylab('\nBlack/White Ratio\n') +
  xlab('Type of Offense\n') +
  theme_bw() +
  coord_flip()

tmpname<-'fig_mythbusting_bwratio.png'
setwd(outputdir)
ggsave(
  filename=tmpname,
  plot=g.tmp,
  width=4*1.5,
  height=2*1.5
)
output(plotdf,tmpname)

#########################################################
#########################################################

#possession vs. distribution? 
#from 2004 SPI

#load the federal survey of inmates
setwd(datadir); dir()
setwd(file.path(datadir,"ICPSR_04572","DS0001")); dir()
spidf_federal<-fread(
  '04572-0001-Data.tsv'
)
names(spidf_federal)<-tolower(names(spidf_federal))

#load the state survey of inmates
setwd(datadir); dir()
setwd(file.path(datadir,"ICPSR_04572","DS0002")); dir()
spidf_state<-fread(
  '04572-0002-Data.tsv'
)
names(spidf_state)<-tolower(names(spidf_state))

#put them together
spidf<-rbind(
  spidf_federal,
  spidf_state
)
spidf_federal<-spidf_state<-NULL

#fix weights

#names(spidf)[names(spidf)=="v2614"]<-"pweight_wrong" 
names(spidf)[names(spidf)=="v2622"]<-"pweight_male_state"
names(spidf)[names(spidf)=="v2832"]<-"pweight_female_state"
names(spidf)[names(spidf)=='v2888']<-"pweight_male_federal"
names(spidf)[names(spidf)=='v2919']<-"pweight_female_federal"

#blank weights should be '0'
#in fed dataset these are 9999.999, in staet these are already NA
weightvars<-c(
  'pweight_male_state',
  'pweight_female_state',
  'pweight_male_federal',
  'pweight_female_federal'
)
for(w in weightvars) {
  tmp<-spidf[[w]]==9999.9999 |
    is.na(spidf[[w]])
  print(sum(tmp))
  spidf[[w]][tmp]<-0
}

tmpnames<-names(spidf)[str_detect(names(spidf),"pweight")]
tmpdf<-spidf[,tmpnames,with=F]
#this confirms that there are no duplicate weights
table(apply(tmpdf,1,function(x) sum(x==0)))

#this is the final weight
spidf$pweight<-spidf$pweight_male_federal + 
  spidf$pweight_female_federal + 
  spidf$pweight_male_state + 
  spidf$pweight_female_state

#get how many are there for possession
tmp<-spidf$co_currentoffense==16 #possession
possession_share <- 100 * sum(spidf$pweight[tmp])/sum(spidf$pweight)
tmp<-spidf$co_currentoffense==17 #trafficking
trafficking_share <- 100 * sum(spidf$pweight[tmp])/sum(spidf$pweight)
tmp<-spidf$co_currentoffense==18 #other drug offense
otherdrug_share <- 100 * sum(spidf$pweight[tmp])/sum(spidf$pweight)
possession_share/(trafficking_share + otherdrug_share + possession_share)

#########################################################
#########################################################

#overdose deaths, black vs. white
setwd(datadir); dir()
tmpdf<-read.delim('drugdeaths_cdcwonder.txt',sep='\t',nrows=130)
names(tmpdf) <- tolower(names(tmpdf))
tmpdf$deathrate <- 
  str_extract(tmpdf$crude.rate,'[0-9]{1,2}\\.[0-9]') %>%
  as.numeric
plotdf<-tmpdf[!is.na(tmpdf$year),]
g.tmp <- ggplot(
  plotdf,
  aes(
    x=year,
    y=deathrate,
    color=race
  )
) +
  geom_line(
    size=1
  ) +
  facet_wrap(
    ~ gender
  ) +
  scale_color_discrete(
    name=''
  ) +
  xlab('') +
  ylab('Drug Overdose Deaths\n') +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )
tmpname<-'fig_mythbusting_drugoverdose.png'
setwd(outputdir)
ggsave(
  filename=tmpname,
  plot=g.tmp,
  width=4*1.5,
  height=2*1.5
)
output(plotdf,tmpname)
