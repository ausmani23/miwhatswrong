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
fulldf<-fread('histpundf_221009.csv')

#limit to big countries
popdf<-fulldf[
  year>2000 &
    statistic=='population' &
    maindata==T
  ,
  .(population=mean(value,na.rm=T))
  ,
  by=c('cownum')
]
bigcows<-popdf$cownum[popdf$population>(3 * 10^6) & !is.na(popdf$population)]
fulldf$bigcountries<-fulldf$cownum%in%bigcows
fulldf<-fulldf[bigcountries==T,]

#choose to use the IPUMS police values at the national level for the USA
#these are not our best estimates, but they are biased against our conclusions
#that the US is 'underpoliced', which invites so much push back
tmp<-fulldf$cownum==2 & 
  fulldf$statistic=='police' & 
  !str_detect(fulldf$source,'IPUMS') & 
  fulldf$maindata==T
fulldf<-fulldf[!tmp,]

#subset
fulldf<-fulldf[
  maindata==T &
  !is.na(advanced) & 
    year>2010 &
    statistic%in%c(
      'homicides',
      'prisoners',
      'police',
      'policekillings',
      'population'
    ) 
  ,
  .(
    period = 'post2000',
    value = median(value,na.rm=T),
    advanced =unique(advanced)
  )
  ,
  by=c(
    'countryname',
    'statistic'
  )
]


#spread
fulldf<-spread(fulldf,statistic,value)

#put in per capita terms
fulldf$prisoners_rate <- 10^5 * fulldf$prisoners/fulldf$population
fulldf$police_rate <- 10^5 * fulldf$police/fulldf$population
fulldf$polk_rate <- 10^5 * fulldf$policekillings/fulldf$population
fulldf$homicides_rate <- 10^5 * fulldf$homicides/fulldf$population

#########################################################
#########################################################

#add UK manually
fulldf$prisoners_rate[fulldf$countryname=='United Kingdom']<-167

#these ratios are necessary
fulldf$prishomratio <- fulldf$prisoners/fulldf$homicides
fulldf$polhomratio <- fulldf$police/fulldf$homicides
fulldf$prispolratio<- fulldf$prisoners/fulldf$police

#########################################################
#########################################################

#CALCULATIONS FOR THE TRADEOFF
usa<-fulldf$countryname=='USA'
advanced<-fulldf$advanced

#incarceration rates in other advanced countries
rates_othcountries <- fulldf$prisoners_rate[!usa & advanced]
rates_quantiles <- quantile(rates_othcountries,c(0.2,0.5,0.8),na.rm=T)

#police rates in other countries
polrates_othcountries <- fulldf$police_rate[!usa & advanced]
polrates_quantiles <- quantile(polrates_othcountries,c(0.2,0.5,0.8),na.rm=T)

#prispol ratios in other countries
prispolratios_othcountries <- fulldf$prispolratio[!usa & advanced]
prispolratios_quantiles <- quantile(prispolratios_othcountries,c(0.2,0.5,0.8),na.rm=T)
prispolratio_usa <- fulldf$prispolratio[usa]

#polhom ratios
polhomratios_othcountries <- fulldf$polhomratio[!usa & advanced]
polhomratios_quantiles <- quantile(polhomratios_othcountries,c(0.2,0.5,0.8),na.rm=T)
polhomratio_usa <- fulldf$polhomratio[usa]

#prishomratios in other coutnries
prishomratio_othcountries <- fulldf$prishomratio[!usa & advanced]
prishomratios_quantiles<-quantile(prishomratio_othcountries,c(0.2,0.5,0.8),na.rm=T)
prishomratio_usa <- fulldf$prishomratio[usa]

#########################################################
#########################################################

#UPDATE USA NUMBERS TO BE CURRENT STATUS QUO

#population counts
#https://www.worldometers.info/world-population/us-population/
pop_2019 <- (329.064917 * 10^6)
pop_2020 <-  (331.002 * 10^6)
pop_2021 <- (331.893745 * 10^6) # https://www.census.gov/quickfacts/fact/table/US/PST045221

#the homicide rate in this dataset is not recent
#so we prefer to use today's homicide rates, for our calculations
homrate_thisdata <- fulldf$homicides_rate[usa]
homrate_2019 <- 6 #https://www.cnn.com/2021/10/06/health/us-homicide-rate-increase-nchs-study/index.html
homrate_2020 <- 7.8 #https://www.cnn.com/2021/10/06/health/us-homicide-rate-increase-nchs-study/index.html
#homrate_2021 calculated below

#ditto for prisoner counts
prisoners_2019 <- prisoners_2020 <- 2.3 * 10^6 
#https://www.prisonpolicy.org/reports/pie2020.html
#https://www.prisonpolicy.org/reports/pie2019.html
prisoners_2021 <- 1.9 * 10^6 #https://www.prisonpolicy.org/reports/pie2022.html (2021 not availasble)

#ditto for police counts
#police_2019 <- 697195 #https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-74
#police_2020 <- police_2019 #until we have other data..
police_2021 <- 860000 #https://crime-data-explorer.app.cloud.gov/pages/le/uof 
#'The Uniform Crime Reporting (UCR) program recommends the FBI utilize a police employee count of 860,000...'
policerate_2021 <- 10^5 * police_2021/pop_2021

#incarceration rates..
incrateusa_2019 <- 10^5 * prisoners_2019/pop_2019
incrateusa_2020 <- 10^5 * prisoners_2020/pop_2020
incrateusa_2021 <- 10^5 * prisoners_2021/pop_2021

#homicides
homicides_2019 <- pop_2019 * homrate_2019/10^5
homicides_2020 <- pop_2020 * homrate_2020/10^5
homicides_2021 <- 22900 #https://www.theguardian.com/us-news/2022/oct/05/fbi-crime-data-2021-homicides
homrate_2021 <- 10^5 * homicides_2021/pop_2021

# #robbery
# robberies_2019 <- 267988
# #https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/robbery
# 
# #rape
# rape_2019 <- 139815
# #https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/rape
# 
# #assault
# assaults_2019 <- 821182
# #https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/aggravated-assault

#murder, robberies, rape, assault in 2021
require(readxl); dir()
setwd(file.path(datadir,"crime2021"))
tmpdf<-readxl::read_xlsx(
  'Table_8_Offenses_Known_to_Law_Enforcement_by_State_by_City_2021.xlsx',
  skip=3
)
names(tmpdf)<-c(
  'state',
  'city',
  'population',
  'violentcrime',
  'murder',
  'rape',
  'robbery',
  'assault',
  'propertycrime',
  'burglary',
  'larceny',
  'motorvehicletheft',
  'arson'
)
tmpdf<-tmpdf[!is.na(tmpdf$population),]
#homrate_2021 <- 10^5 * sum(tmpdf$murder)/sum(tmpdf$population) #alternative estimate; higher
robberyrate_2021 <- 10^5 * sum(tmpdf$robbery)/sum(tmpdf$population)
robberies_2021 <- robberyrate_2021 * pop_2021/10^5
raperate_2021 <- 10^5 * sum(tmpdf$rape)/sum(tmpdf$population)
rape_2021 <- raperate_2021 * pop_2021/10^5
assaultrate_2021 <- 10^5 * sum(tmpdf$assault)/sum(tmpdf$population)
assaults_2021 <- assaultrate_2021 * pop_2021/10^5

# #arrests, 2019
require(readxl);
setwd(datadir)
tmpdf<-readxl::read_xls('table-29.xls')
tmpdf<-tmpdf[3:34,1:2]
names(tmpdf)<-c(
  'offense',
  'total'
)
total_arrests_2019 <- tmpdf$total[tmpdf$offense=='Total1']
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-29

#arrests, 2020
setwd(file.path(datadir,'persons-arrested-2020'))
tmpdf<-readxl::read_xls('Table_69_Arrest_by_State_2020.xls',skip=3)
tmpdf<-tmpdf[,c(1:3,ncol(tmpdf))]
names(tmpdf)<-c('state','class','total','population')
tmpdf$population<-na.locf(tmpdf$population)
tmpdf<-tmpdf[str_detect(tmpdf$class,"Total"),]
total_arrestsrate_2020 <- 10^5 * sum(tmpdf$total,na.rm=T)/
  sum(tmpdf$population,na.rm=T)
total_arrests_2020 <- total_arrestsrate_2020 * pop_2020/10^5
#reassuring thing is that this imputed total
#matches the imputed total reported in Table 29
tmpdf<-readxl::read_xls('Table_29_Estimated_Number_of_Arrests_United_States_2020.xls',skip=2)
total_arrests_2020<-unlist(tmpdf[tmpdf[,1]=='Total1',2]) %>% unname

#arrests, 2021
setwd(file.path(datadir,'persons-arrested-2021'))
tmpdf<-readxl::read_xls('Table_69_Arrest_by_State_2021.xls',skip=3)
tmpdf<-tmpdf[,c(1:3,ncol(tmpdf))]
names(tmpdf)<-c('state','class','total','population')
tmpdf$population<-na.locf(tmpdf$population)
tmpdf<-tmpdf[str_detect(tmpdf$class,"Total"),]
total_arrestsrate_2021 <- 10^5 * sum(tmpdf$total,na.rm=T)/
  sum(tmpdf$population,na.rm=T)
total_arrests_2021 <- total_arrestsrate_2021 * pop_2021/10^5

#########################################################
#########################################################

#what *should* incarceration rate be?

#the US has historically had a higher prisoner/homicide ratio
#than the median developed country. 

incrateusa_calibrated_old <- homrate_2019 *
  prishomratios_quantiles['50%'] 

#this was no longer true after the fall in incarceration and 
#rise in violence that took place 2019-2021, during the pandemic

incrateusa_calibrated <- homrate_2021 *
  prishomratios_quantiles['50%'] 

#given today's homicide rate, if the US incarcerated 
#at prisoner/homicide ratio of median developed country
#there should be 850 per 100,000 people in prison

pop_2021 * 
  (incrateusa_2021 - incrateusa_calibrated)/10^5

#this would be the equivalent of adding 800,000 people to prison!

#########################################################
#########################################################

#ILLUSTRATE THE USA VS RoW

plotdf<-fulldf[
  !is.na(police_rate) & 
    !is.na(prisoners_rate) &
    advanced
]
plotdf$prisoners_rate[plotdf$countryname=='USA']<-
  incrateusa_2021
plotdf$police_rate[plotdf$countryname=='USA'] <- 
  policerate_2021
plotdf$usa<-plotdf$countryname=='USA'
tmpsizes<-c(4,3)
tmpalphas<-c(1,0.5)
names(tmpsizes)<-names(tmpalphas)<-c(T,F)

#add slopes
prispol_slope<-prispolratios_quantiles['50%']
firstworld_fun <- function(x) prispol_slope * x
prispol_slope_usa <- prisoners_2021/police_2021
usa_fun <- function(x) prispol_slope_usa * x

g.tmp <- ggplot(
  plotdf,
  aes(
    label=countryname,
    x=police_rate,
    y=prisoners_rate,
    size=usa,
    alpha=usa
  )
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
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw() 

setwd(outputdir)
ggsave(
  filename='fig_policeprisoners.png',
  plot=g.tmp,
  width=5,
  height=5
)



#########################################################
#########################################################

#ILLUSTRATE THE TRADEOFF w/ DECARCERATION STOPPING POINT

plotdf <- fulldf[countryname=='USA']
plotdf$prisoners_rate <- incrateusa_2021 
plotdf$police_rate <- policerate_2021 
plotdf$label<-'Status Quo'
extradf <- data.frame(
  police_rate=plotdf$police_rate,
  prisoners_rate=incrateusa_calibrated,
  label='Calibrated'
)
# arrowdf <- data.frame(
#   x1=plotdf$police,
#   y1=plotdf$prisoners,
#   x2=extradf$police,
#   y2=extradf$prisoners
# )
plotdf<-rbind.fill(
  plotdf,
  extradf
)
tradeoffdf<-data.frame(
  xmin=0,
  ymin=0,
  ymax=incrateusa_calibrated,
  xmax=unique(plotdf$police_rate)
)


g.tmp <- ggplot(
  plotdf,
) +
  geom_point(
    aes(
      y=prisoners_rate,
      x=police_rate,
      color=label
    ),
    size=3
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,1000)
  ) + 
  scale_y_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,1000)
  ) +
  scale_color_manual(
    name="",
    values=c(
      'Status Quo'='red',
      'Calibrated'='black'
    )
  ) +
  # geom_segment(
  #   data=arrowdf,
  #   aes(
  #     x=x1,
  #     y=y1,
  #     xend=x2,
  #     yend=y2
  #   )
  #   ,
  #   arrow= arrow(length = unit(0.03, "npc"))
  # ) +
# geom_rect(
#   data=tradeoffdf,
#   aes(
#     xmin=xmin,
#     xmax=xmax,
#     ymin=ymin,
#     ymax=ymax,
#   ),
#   alpha=0.25,
#   fill='darkred',
#   color='black'
# ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw() +
  theme(
    legend.position = 'top',
    legend.direction = 'horizontal'
  )

setwd(outputdir)
ggsave(
  filename='fig_tradeoff.png',
  plot=g.tmp,
  width=5,
  height=5
)

#########################################################
#########################################################

#ADD PRIZPOLRATIO LINES

#median developed country
prispol_slope<-prispolratios_quantiles['50%']
firstworld_fun <- function(x) prispol_slope * x

prispol_slope_usa <- prisoners_2021/police_2021
usa_fun <- function(x) prispol_slope_usa * x

g.tmp<- g.tmp + 
  stat_function(
    fun=firstworld_fun,
    size=1,
    color='grey'
  ) +
  stat_function(
    fun=usa_fun,
    size=1,
    color='grey'
  ) 


setwd(outputdir)
ggsave(
  filename='fig_tradeoff_wratios.png',
  plot=g.tmp,
  width=5,
  height=5
)

#########################################################
#########################################################

#ADD FIRST-WORLD BALANCE TO THE TRADEOFF

cost_policeofficer <- 130000 #chaflin and mccary 2017, p. 182
#cost_prisoner <- 31000 #roodman, conclusion
cost_prisoner <- 33089 #chalfin and mccary 2017, p. 183
setwd(codedir); dir()

#we assume these numbers give slope of substitution 
prispol_tradeoff_slope <- -1 * (cost_policeofficer/cost_prisoner) 
y0 <- incrateusa_2021 - (prispol_tradeoff_slope * unique(plotdf$police_rate))
solution_fun <- function(x) y0 + prispol_tradeoff_slope * x

#find intersection of the two lines
#based on paper =
x_intersect = y0 / (prispol_slope - prispol_tradeoff_slope)
y_intersect = firstworld_fun(x_intersect)
solution_fun(x_intersect) #should be the same
solutiondf<-data.frame(x=x_intersect,y=y_intersect,label='The First-World Balance')
police_added <- (solutiondf$x - unique(plotdf$police_rate))/10^5 * pop_2021 #how many more police
prisoners_added <- (solutiondf$y - incrateusa_2021)/10^5 * pop_2021 #how many fewer prisoners
(prisoners_added)/prisoners_2021
(police_2021+police_added)/police_2021
stop()
#ILLUSTRATE

g.tmp <- g.tmp + 
  stat_function(
    fun=solution_fun,
    size=1,
    color='grey',
    linetype=2,
    arrow = arrow(length = unit(0.5, "cm"))
  ) +
  geom_point(
    data=solutiondf,
    aes(
      x=x,
      y=y
    )
  ) + 
  annotate(
    geom='text',
    x=solutiondf$x+50,
    y=solutiondf$y+50,
    label=paste0("(",round(solutiondf$x),",",round(solutiondf$y),")")
  )

setwd(outputdir)
ggsave(
  filename='fig_tradeoff_wsolution.png',
  plot=g.tmp,
  width=5,
  height=5
)

#########################################################
#########################################################

#MAKE A POLICE KILLINGS GRAPH

setwd(datadir); dir()
plotdf<-fread('histpundf_national_220128.csv')

#get avg in rest of developed world
tmpdf<-plotdf[
  countryname!='United States of America' & 
    advanced==T &
    statistic=='policekillings',
  mean(value),
  by='countryname'
]
mean(tmpdf$V1)
median(tmpdf$V1)

plotdf<-plotdf[
  year>=1996 & #the modern period
    !is.na(advanced) & 
    statistic%in%c(
      'policekillings',
      'population',
      'homicides',
      'police'
    )
  ,
  .(
    period = 'post1996',
    value = median(value,na.rm=T)
  )
  ,
  by=c(
    'countryname',
    'statistic'
  )
]
plotdf<-spread(plotdf,statistic,value)
tmp<-plotdf$countryname=='United States of America'
plotdf$countryname[tmp]<-'USA'

#we only want countries for which we have all four
tmp<-apply(plotdf,1,function(x) sum(is.na(x)))==0
plotdf<-plotdf[tmp,]

#get some numbers
plotdf[,c('countryname','policekillings')]

#there is a negative relationship in logs
#between police/homicide and police killings/capita
plotdf$polhomratio <- plotdf$police/plotdf$homicide #log(plotdf$police/plotdf$homicides)
plotdf$polkillings_percapita <- 10^6 * (0.1+plotdf$policekillings)/plotdf$population#log((0.1 + plotdf$policekillings)/plotdf$population)

#identify usa
plotdf$usa<-plotdf$countryname=='USA'
tmpsizes<-c(4,2)
tmpalphas<-c(1,0.5)
names(tmpsizes)<-names(tmpalphas)<-c(T,F)

#save out policekilling dataset
#we'll use this for calculations later.
polkdf<-plotdf

g.tmp <- ggplot(
  plotdf,
  aes(
    label=countryname,
    x=polhomratio,
    y=polkillings_percapita,
    size=usa,
    alpha=usa
  )
) + 
  geom_point(
    size=0.8,
    alpha=0.5
  ) +
  geom_text_repel() +
  scale_alpha_manual(
    guide='none',
    values=tmpalphas
  ) +
  scale_size_manual(
    guide='none',
    values=tmpsizes
  ) +
  geom_line(
    stat="smooth",
    method = "lm", 
    formula = y ~ x,
    color='red',
    size = 1.5,
    alpha = 0.5
  ) +
  scale_x_log10(
    breaks=c(5,50,500),
    labels=c("5","50","500")
  ) +
  scale_y_log10(
    breaks=c(10^(-1:2)),
    labels=c(".1","1","10","100")
  ) +
  xlab('\nPolice Per Homicide') +
  ylab('Police Killings Per Million\n') +
  theme_bw()

setwd(outputdir)
ggsave(
  filename='fig_policeviolence.png',
  plot=g.tmp,
  width=5,
  height=5
)

#########################################################
#########################################################

#save out workspace for CB calculations
setwd(filesdir)
#but drop the directories
tmp<-str_detect(ls(),'dir')
rm(list=ls()[tmp])
save.image(
  'thetradeoff_data.RData'
)

#########################################################
#########################################################
