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
fulldf<-fread('histpun.csv')
fulldf<-fulldf[
  !is.na(advanced) & 
    year>2010 &
    statistic%in%c(
      'homicides',
      'prisoners',
      'police'
    ) &
    unit=='percapita' &
    population>5* 10^6
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
fulldf<-spread(fulldf,statistic,value)

#########################################################
#########################################################

#add police killing data (can be integrated w/ above, once updated)
setwd(datadir); dir()
polkdf<-fread('histpundf_national_211020.csv')
polkdf<-polkdf[
  !is.na(advanced) & 
    year>2010 & 
    statistic%in%c(
      'policekillings',
      'population',
      'homicides',
      'police'
    )
  ,
  .(
    period = 'post2000',
    value = median(value,na.rm=T)
  )
  ,
  by=c(
    'countryname',
    'statistic'
  )
]
polkdf<-spread(polkdf,statistic,value)

#we only want countries for which we have all four
tmp<-apply(polkdf,1,function(x) sum(is.na(x)))==0
polkdf<-polkdf[tmp,]

#########################################################
#########################################################

#add UK manually
fulldf$prisoners[fulldf$countryname=='United Kingdom']<-167

#these ratios are necessary
fulldf$prishomratio <- fulldf$prisoners/fulldf$homicides
fulldf$polhomratio <- fulldf$police/fulldf$homicides
fulldf$prispolratio<- fulldf$prisoners/fulldf$police

#########################################################
#########################################################

#CALCULATIONS FOR THE TRADEOFF
usa<-fulldf$countryname=='United States of America'
advanced<-fulldf$advanced

#incarceration rates in other advanced countries
rates_othcountries <- fulldf$prisoners[!usa & advanced]
rates_quantiles <- quantile(rates_othcountries,c(0.2,0.5,0.8),na.rm=T)

#police rates in other countries
polrates_othcountries <- fulldf$police[!usa & advanced]
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

#the homicide rate in this dataset is not recent
#so we prefer to use today's homicide rates, for our calculations
homrate_thisdata <- fulldf$homicides[usa]
homrate_2019 <- 6 #https://www.cnn.com/2021/10/06/health/us-homicide-rate-increase-nchs-study/index.html
homrate_2020 <- 7.8 #https://www.cnn.com/2021/10/06/health/us-homicide-rate-increase-nchs-study/index.html

#ditto for prisoner counts
prisoners_2019 <- prisoners_2020 <- 2.3 * 10^6 
#https://www.prisonpolicy.org/reports/pie2020.html
#https://www.prisonpolicy.org/reports/pie2019.html

#ditto for police counts
police_2019 <- 697195 #https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-74
police_2020 <- police_2019 #until we have other data..

#population counts
#https://www.worldometers.info/world-population/us-population/
pop_2019 <- (329.064917 * 10^6)
pop_2020 <-  (331.002 * 10^6)

#incarceration rates..
incrateusa_2019 <- 10^5 * prisoners_2019/pop_2019
incrateusa_2020 <- 10^5 * prisoners_2020/pop_2020

#homicides
homicides_2019 <- pop_2019 * homrate_2019/10^5
homicides_2020 <- pop_2020 * homrate_2020/10^5

#robberies
robberies_2019 <- 267988
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/robbery

#rape
rape_2019 <- 139815
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/rape

#assault
assaults_2019 <- 821182
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/aggravated-assault

#arrests
require(readxl); dir();
tmpdf<-readxl::read_xls('table-29.xls') 
tmpdf<-tmpdf[3:34,1:2]
names(tmpdf)<-c(
  'offense',
  'total'
)
total_arrests_2019 <- tmpdf$total[tmpdf$offense=='Total1']
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-29

#########################################################
#########################################################

#what should incarceration rate be, if it were calibrated by median prishom in other countries
incrateusa_calibrated <- homrate_2019 *
  prishomratios_quantiles['50%'] #about 676
incrateusa_calibrated_optimistic <-
  homrate_2019 * prishomratios_quantiles['20%'] #super optimistic (using lenient country)
incrateusa_calibrated_pessimistic <-
  homrate_2020 * prishomratios_quantiles['50%'] #more pessimistic (using today's hom)

#how much less than today
(incrateusa_2019 -incrateusa_calibrated)/incrateusa_2019

#how many people would be released from prison
pop_2019 * 
  (incrateusa_2019 - incrateusa_calibrated)/10^5

#########################################################
#########################################################

#ILLUSTRATE THE USA VS RoW
plotdf<-fulldf[
  !is.na(police) & 
    !is.na(prisoners) &
    advanced
]
plotdf$prisoners[plotdf$countryname=='United States of America']<-
  incrateusa_2019
plotdf$police[plotdf$countryname=='United States of America'] <- 
  10^5 * police_2019/pop_2019
plotdf$countryname[plotdf$countryname=='Germany 1990-2014'] <- 'Germany'
plotdf$usa<-plotdf$countryname=='United States of America'
tmpsizes<-c(4,3)
tmpalphas<-c(1,0.5)
names(tmpsizes)<-names(tmpalphas)<-c(T,F)

#add slopes
prispol_slope<-prispolratios_quantiles['50%']
firstworld_fun <- function(x) prispol_slope * x
prispol_slope_usa <- prisoners_2019/police_2019
usa_fun <- function(x) prispol_slope_usa * x

g.tmp <- ggplot(
  plotdf,
  aes(
    label=countryname,
    x=police,
    y=prisoners,
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

plotdf <- fulldf[countryname=='United States of America']
plotdf$prisoners <- incrateusa_2019 #the real incrate today
plotdf$police <- 10^5 * police_2019/pop_2019
plotdf$label<-'Status Quo'
# extradf <- data.frame(
#   police=plotdf$police,
#   prisoners=incrateusa_calibrated,
#   label='After Decarceration'
# )
# arrowdf <- data.frame(
#   x1=plotdf$police,
#   y1=plotdf$prisoners,
#   x2=extradf$police,
#   y2=extradf$prisoners
# )
# plotdf<-rbind.fill(
#   plotdf,
#   extradf
# )
tradeoffdf<-data.frame(
  xmin=0,
  ymin=0,
  ymax=incrateusa_calibrated,
  xmax=unique(plotdf$police)
)


g.tmp <- ggplot(
  plotdf,
) +
  geom_point(
    aes(
      y=prisoners,
      x=police,
      color=label
    ),
    size=3
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) + 
  scale_y_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  # scale_color_manual(
  #   name="",
  #   values=c(
  #     'Status Quo'='red',
  #     'Cost-Free Decarceration'='black'
  #   )
  # ) +
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

prispol_slope_usa <- prisoners_2019/police_2019
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
y0 <- incrateusa_2019 - (prispol_tradeoff_slope * unique(plotdf$police))
solution_fun <- function(x) y0 + prispol_tradeoff_slope * x

#find intersection of the two lines
#based on paper =
x_intersect = y0 / (prispol_slope - prispol_tradeoff_slope)
y_intersect = firstworld_fun(x_intersect)
solution_fun(x_intersect) #should be the same
solutiondf<-data.frame(x=x_intersect,y=y_intersect,label='The First-World Balance')
police_added <- (solutiondf$x - unique(plotdf$police))/10^5 * pop_2019 #how many more police
prisoners_added <- (solutiondf$y - incrateusa_2019)/10^5 * pop_2019 #how many fewer prisoners

rates_quantiles

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

#there is a negative relationship in logs
#between police/homicide and police killings/capita
polkdf$polhomratio <- log(polkdf$police/polkdf$homicides)
polkdf$polkillings_percapita <- 
  log((0.1 + polkdf$policekillings)/polkdf$population)

#identify usa
polkdf$usa<-polkdf$countryname=='United States of America'
tmpsizes<-c(4,3)
tmpalphas<-c(1,0.5)
names(tmpsizes)<-names(tmpalphas)<-c(T,F)

g.tmp <- ggplot(
  polkdf,
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
  xlab('\nPolice Per Homicide (Log)') +
  ylab('Police Killings Per Capita (Log)\n') +
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
save.image(
  'thetradeoff_data.RData'
)

#########################################################
#########################################################
