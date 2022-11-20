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
outputdir<-file.path(
  homedir,"output"
)
codedir<-file.path(
  homedir,"code"
)

# setwd(homedir)
# source('theme_black.R')
require(ggrepel)

#########################################################
#########################################################

#get data 
setwd(datadir); dir()
hpundf<-fread('histpundf_221009.csv')

#restrict to big countries
popdf<-hpundf[
  year>2000 &
    statistic=='population' &
    maindata==T
  ,
  .(population=mean(value,na.rm=T))
  ,
  by=c('cownum')
]
bigcows<-popdf$cownum[popdf$population>(3 * 10^6) & !is.na(popdf$population)]
hpundf$bigcountries<-hpundf$cownum%in%bigcows
hpundf<-hpundf[bigcountries==T]

#get the stats
fulldf<-hpundf[
  advanced==T &
    maindata==T &
    year>2015 &
    statistic%in%c(
      'homicides',
      'prisoners',
      'police',
      'population'
    ) &
    #compare only to the UNODC observations
    #that we have for other countries, for police
    (
      (
      statistic=='police' & 
        str_detect(source,'UNODC|Burnham')
    ) 
    |
      statistic!='police'
    )
    
  ,
  .(
    period = 'post2015',
    value = median(value,na.rm=T),
    advanced =unique(advanced)
  )
  ,
  by=c(
    'countryname',
    'statistic'
  )
]
fulldf<-spread(fulldf,statistic,value)

#fix the E+W pop number https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/internationalmigration/bulletins/internationalmigrationenglandandwales/census2021#:~:text=2.-,Migration%20between%20censuses,2011%20to%2059%2C597%2C542%20in%202021.
fulldf$population[fulldf$countryname=='England and Wales']<-59597542

#########################################################
#########################################################

#add usa estimates

#population
pop_2017 <- 325.1 * 10^6 # https://www.worldometers.info/world-population/us-population/
pop_2019 <- (329.064917 * 10^6) #https://www.worldometers.info/world-population/us-population/
pop_2020 <-  (331.002 * 10^6)
pop_2021 <- (331.893745 * 10^6) # https://www.census.gov/quickfacts/fact/table/US/PST045221

#police in the USA
unodc_2017 <- 670279 #https://dataunodc.un.org/
#this is 'most comparable', since all reports to UNODC are supposed to follow their definition of police
# p.114 'personnel in public agencies whose principal functions are prevention, detection , apprehension. support staff excluded'
#https://www.unodc.org/documents/data-and-analysis/Crime-statistics/International_Statistics_on_Crime_and_Justice.pdf
fbi_2019 <- 697195 #https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-74
fbi_adjusted_2019 <- 697195 * 329/288 + 100000 #adjusted for undercounting
census_2019 <- 1088027 #count of all active people saying they are 'police' in the census
#NB: I made a mistake when sending this figure to Alec; the figure I sent him
#included retired + unemployed 'police', who should obvi be excluded

#homicides in the USA
homrate_2017 <- 5.3 #https://ucr.fbi.gov/crime-in-the-u.s/2017/crime-in-the-u.s.-2017/topic-pages/murder
homrate_2019 <- 6 #https://www.cnn.com/2021/10/06/health/us-homicide-rate-increase-nchs-study/index.html
homrate_2020 <- 7.8 #https://www.cnn.com/2021/10/06/health/us-homicide-rate-increase-nchs-study/index.html
homicides_2017 <- pop_2017 * homrate_2017/10^5
homicides_2019 <- pop_2019 * homrate_2019/10^5
homicides_2020 <- pop_2020 * homrate_2020/10^5
homicides_2021 <- 22900 #https://www.theguardian.com/us-news/2022/oct/05/fbi-crime-data-2021-homicides
homrate_2021 <- 10^5 * homicides_2021/pop_2021

#prisoners
prisoners_2017 <- 2.3 * 10^6 #https://www.prisonpolicy.org/reports/pie2017.html
prisoners_2019 <- 2.3 * 10^6 #https://www.prisonpolicy.org/reports/pie2019.html
prisoners_2021 <- 2.1 * 10^6 #https://www.prisonpolicy.org/reports/pie2022.html/see also https://data.census.gov/cedsci/table?q=B02001&tid=ACSST1Y2021.S2603

#add fwb for illustration (proposal is lifted from the original piece)
#(note that these are outdated; in current manuscript, new proposal is different in details)
police_fwb <- fbi_2019 + 500000 
homicides_fwb <- homicides_2019 - 4240
prisoners_fwb <- prisoners_2019 -  2000000

#add these rows
extradf <- data.frame(
  #different estimates
  countryname=c(
    'UNODC', #unodc (most comparable)
    'FBI', #fbi ucr, original (used in the piece)
    'FBI Adjusted', #fbi, adjusted for under-countring
    'Census', #census
    'Our Proposal' #our proposal
  ),
  #each pertains to a recent year
  period=c(
    2017,
    2019,
    2019,
    2019,
    2019
  ),
  police=c(
    unodc_2017,
    fbi_2019,
    fbi_adjusted_2019,
    census_2019,
    police_fwb
  ),
  #get the corresponding homicide estimates
  homicides=c(
    homicides_2017,
    homicides_2019,
    homicides_2019,
    homicides_2019,
    homicides_fwb
  ),
  #get the corresponding prisoner estimates
  prisoners=c(
    prisoners_2017,
    prisoners_2019,
    prisoners_2019,
    prisoners_2019,
    prisoners_fwb
  )
  ,
  #get the corresponding population estimates
  population=c(
    pop_2017,
    pop_2019,
    pop_2019,
    pop_2019,
    pop_2019
  )
)

#count as usa
extradf$usa<-T

#drop usa
fulldf<-fulldf[fulldf$countryname!='USA',]

#add the extra estimates
fulldf<-rbind.fill(
  fulldf,
  extradf
) %>% data.table

#add per capita rates
fulldf$policerate<-10^5 * fulldf$police/fulldf$population
fulldf$prisonersrate<-10^5 * fulldf$prisoners/fulldf$population
fulldf$polhomratio <- fulldf$police/fulldf$homicides
fulldf$prizhomratio <- fulldf$prisoners/fulldf$homicides
fulldf$prispolratio<- fulldf$prisoners/fulldf$police

#########################################################
#########################################################

#FIGURE 
#what does the per homicide graph look like
#when you use other estimates instead? 

plotdf<-fulldf[
  !is.na(policerate) & 
    !is.na(prisonersrate) 
]
plotdf$usa[is.na(plotdf$usa)]<-F

tmpsizes<-c(3,2)
tmpalphas<-c(1,0.5)
names(tmpsizes)<-names(tmpalphas)<-c(T,F)

prispol_slope<-median(plotdf$prispolratio[!plotdf$usa])
firstworld_fun <- function(x) prispol_slope * x
prispol_slope_usa <- median(plotdf$prispolratio[plotdf$usa & !str_detect(plotdf$countryname,'Our Proposal')])
usa_fun <- function(x) prispol_slope_usa * x

g.tmp <- ggplot(
  plotdf,
  aes(
    label=countryname,
    x=polhomratio,
    y=prizhomratio,
    size=usa,
    alpha=usa
  )
) +
  geom_vline(
    xintercept=median(
      plotdf$polhomratio[!plotdf$usa]
    ),
    linetype='dashed',
    color='darkblue'
  ) +
  geom_text_repel() +
  stat_function(
    fun=firstworld_fun,
    size=1,
    color='darkred',
    linetype='dashed',
    alpha=0.5
  ) +
  stat_function(
    fun=usa_fun,
    size=1,
    color='darkred',
    linetype='dashed',
    alpha=0.5
  ) +
  scale_alpha_manual(
    guide='none',
    values=tmpalphas
  ) +
  scale_size_manual(
    guide='none',
    values=tmpsizes
  ) +
  geom_point(
    size=1,
    alpha=0.5
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  scale_y_continuous(
    breaks=c(0,50,100,150,200),
    limits=c(0,205)
  ) +
  xlab('\nPolice per Homicide') +
  ylab('Prisoners per Homicide\n') +
  theme_bw() 

setwd(outputdir)
ggsave(
  filename='fig_policeprisoners_perhomicide_alec.png',
  plot=g.tmp,
  width=6,
  height=6
)

#########################################################
#########################################################

#summarize how much below/above the median each point is
tmp<-plotdf$usa==T
usas <- plotdf$countryname[tmp]
lapply(usas,function(x) {
  data.frame(
    point=x,
    polhomratio=100 * plotdf$polhomratio[plotdf$countryname==x]/
    median(plotdf$polhomratio[!plotdf$usa==T]),
    policerate=100 * plotdf$policerate[plotdf$countryname==x]/
      median(plotdf$policerate[!plotdf$usa==T]),
    prisonrate=100 * plotdf$prisonersrate[plotdf$countryname==x]/
      median(plotdf$prisonersrate[!plotdf$usa==T])
  )
}) %>% rbind.fill

#summarize ratios: prisoners per 10 police officers
10 * median(plotdf$prispolratio[!plotdf$usa==T])
10 * plotdf$prispolratio[plotdf$usa==T & plotdf$countryname!='Our Proposal']

#to have developed world median, US would have to:
extra_police_needed <- (median(plotdf$polhomratio[!plotdf$usa==T & plotdf$countryname!='Our Proposal']) * 
    plotdf$homicides[plotdf$usa==T & plotdf$countryname!='Our Proposal']) - 
  plotdf$police[plotdf$usa==T & plotdf$countryname!='Our Proposal']
sort(extra_police_needed)

