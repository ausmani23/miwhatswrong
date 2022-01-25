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

#DEPRECATED

# #NOW. 
# #calculate the relevant costs/benefits
# 
# #########################################################
# #########################################################
# 
# #IMPRISONMENT
# 
# #we want to convert this to prisoner-lives lost
# prisoners_added #this is prisoner-years added
# prisoners_added_percent <- 1 - (prisoners_added + prisoners_2019)/prisoners_2019
# life_expectancy_prisoner <- 65 #we say 65, as a best guess, of prisoner life expectancy
# discount_prison <- 0.5 #life in prison is not death, but half-death
# decarceration_costs <- (prisoners_added / life_expectancy_prisoner) * discount_prison
# #this gives us gains of decarceration ('costs', but negative)
# 
# #########################################################
# 
# #POLICE
# 
# police_added
# police_added_percent <- (police_added + police_2019)/police_2019 - 1
# 
# #two kinds of policing costs to consider
# #first, change in police killings
# 
# #if someone is pessimistic, this just goes up with number of police
# policekillings_2020 <- 2084 #taken from fatalencounters.org
# policeexpansion_costs_policekillings_pessimistic <- 
#   (police_added_percent * policekillings_2020) 
# 
# m.polk <- lm(
#   data=polkdf,
#   formula = polkillings_percapita ~ polhomratio
# )
# #we will only be able to estimate police killings 
# #once we estimate homicide rate
# #(i do this below)
# 
# #second, change in police arrests, serious and petty
# petty_arrests_perofficer <- c(7,15,22) #chalfin et al 2020, p. 11
# serious_arrests_perofficer <- c(-0.97,(-0.97 + -1.6)/2,-1.6) #chalfin et al 2020, p12
# 
# #chalfin and mccary give this as increase in arrests per officer hired
# #rather than as increase in arrests by % increas in policing
# #so we use that, noting that, given the very large increase we are contemplating
# #this is probably biased against our argument.. 
# policeexpansion_costs_pettyarrests <- petty_arrests_perofficer * police_added
# policeexpansion_costs_seriousarrests <- serious_arrests_perofficer * police_added
# 
# #########################################################
# 
# #HOMICIDE/CRIME
# 
# #by how much would this change the homicide rate? 
# 
# #this is difficult to estimate, so we proceed in two ways
# 
# #leaning on the elasticities in literature
# elasticity_police <- -0.67 #chalfin and mccary 2017, p. 180
# elasticity_prison <- 0 #roodman 2017
# 
# #first, optimistically for our argument, 
# #we assume constant elasticities
# #this means increase in police leads to reduction in homicide
# #and the decline in imprisonment has no implications
# homicides_costs <- homicides_2019 * (police_added_percent * elasticity_police)
# 
# #second, pessimisticially for our argument, 
# #we assume changing elasticities
# #declining for police, as police increase
# #increasing for decarceration, as prisoners decrease
# 
# #there are an infinite number of combinations of increase/decrease
# #so we have to decide on some way to anchor this exercise
# 
# #we do this by assuming that the median developed country
# #is optimizing. 
# 
# #that is, at the point they're at, $ spent on police buys
# #the same crime reduction as $ spent on prison
# 
# #this doesn't mean the elasticities are equivalent
# #it means that %homicide/$_prison = %homicide/$_police
# 
# #this means that:
# #%homicide/%prison * %prison/$_prison = %homicide/%police * %police/$_prison
# 
# #therefore, %homicide/%prison = %homicide/%police * (%police/$prison)/(%prison/$prison)
# 
# #what is %police/$prison, %prison/$prison? 
# 
# #the first is the % increase in policing you'd get for a dollar 
# #the second is the % increase in prison that you'd get for a dollar
# #at the levels of policing and imprisonment that characterize europe
# 
# #it turns out that this equals
# multiplier <- cost_policeofficer/cost_prisoner * #costs..
#   (solutiondf$x/solutiondf$y) #europe ratio
# 
# #so in our pessimistic world, police_elasticity will be 13 times prison_elasticity
# 
# #now we have to make one more assumption 
# #to yield our pessimistic numbers
# 
# #we have to anchor police elasticity in something
# #we assume that it declines to some smaller rate
# #and then we use this final rate to calculate prison_elasticity
# elasticity_police_pessimistic <- elasticity_police
# #elasticity_prison_pessimistic <- elasticity_police_pessimistic/multiplier
# elasticity_prison_pessimistic <- c(-0.05,-0.15)
# homicides_costs_pessimistic <- 
#   homicides_2019 * (police_added_percent * elasticity_police_pessimistic) -
#   homicides_2019 * (prisoners_added_percent * elasticity_prison_pessimistic)
# 
# #sumamrizing
# homicides_costs<-c(
#   'best' = homicides_costs,
#   'pessimistic' = homicides_costs_pessimistic
# )
# 
# 
# #########################################################
# #########################################################
# 
# #SUMMARIZING COSTS/BENEFITS
# 
# #first calculate police killings, under optimistic scenario
# homicides_cfactual <- homicides_2019 + homicides_costs['best']
# homrate_cfactual <- 10^5 * homicides_cfactual/pop_2019
# police_cfactual <- police_2019 + police_added
# prisoners_cfactual <- prisoners_2019 + prisoners_added
# 
# polhomratio_cfactual <- police_cfactual/homicides_cfactual
# polhomratios_quantiles
# polhomratio_usa #old
# 
# predictdf<-data.frame(
#   polhomratio=log((police_cfactual)/homicides_cfactual)
# )
# #the us is above the line, so we need to take this into account
# usa_residual <- m.polk$residuals[which(polkdf$countryname=='United States of America')]
# 
# policekillings_cfactual <- 
#   exp(predict(m.polk,newdata=predictdf) + usa_residual) * pop_2019
# 
# #this is our best guess, then.
# policeexpansion_costs_policekillings <- c(
#   'pessimistic'=policeexpansion_costs_policekillings_pessimistic,
#   'best'=unname(policekillings_cfactual - policekillings_2020)
# )
# 
# #OUR STANDARD SUMMARY
# 
# #costs
# policeexpansion_costs_pettyarrests[2]
# 
# #benefits
# homicides_costs
# decarceration_costs
# policeexpansion_costs_seriousarrests[2]
# policeexpansion_costs_policekillings['best']
# 
# #OUR PESSIMISTIC SUMMARY
# 
# #costs
# policeexpansion_costs_pettyarrests[3]
# policeexpansion_costs_policekillings['pessimistic']
# 
# #benefits
# homicides_costs['pessimistic']
# decarceration_costs
# policeexpansion_costs_seriousarrests[1]
# 
