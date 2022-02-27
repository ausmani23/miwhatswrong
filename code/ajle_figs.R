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
    year>2015 &
    statistic%in%c(
      'homicides',
      'prisoners',
      'police'
    ) &
    unit=='percapita' &
    population>5* 10^6
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

#########################################################
#########################################################

#add data
setwd(datadir); dir()
extradf<-fread('histpundf_national_220128.csv')

#need this data point
homicides_2006 <- extradf[
  cownum==2 & statistic=='homicides' & year==2006,
  median(value)
]

extradf<-extradf[
  !is.na(advanced) & 
    year>2015 & 
    statistic%in%c(
      'population',
      'homicides',
      'police',
      'arrests',
      'arrests_homicide',
      'convictions_homicide',
      'prisoners'
    )
  ,
  .(
    advanced = unique(advanced),
    period = 'post2015',
    value = median(value,na.rm=T)
  )
  ,
  by=c(
    'countryname',
    'statistic'
  )
]
extradf<-spread(extradf,statistic,value)


#big countries only?
extradf<-extradf[population>4*10^6]

# #we only want countries for which we have all four
# tmp<-apply(extradf,1,function(x) sum(is.na(x)))==0
# extradf<-extradf[tmp,]

#########################################################
#########################################################

tmp<-fulldf$countryname=='United States of America'
fulldf$countryname[tmp]<-'USA'
tmp<-extradf$countryname=='United States of America'
extradf$countryname[tmp]<-'USA'

tmp<-fulldf$countryname=='Germany 1990-2014'
fulldf$countryname[tmp]<-'Germany'
tmp<-extradf$countryname=='Germany 1990-2014'
extradf$countryname[tmp]<-'Germany'

#########################################################
#########################################################

#add UK manually
#https://commonslibrary.parliament.uk/research-briefings/sn04334/#:~:text=Prison%20population%20per%20capita&text=167%20prisoners%20per%20100%2C000%20of,in%20England%20and%20Wales%20(2020)
fulldf$prisoners[fulldf$countryname=='United Kingdom']<-167

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
(polhomratio_usa/polhomratios_quantiles[2]) 

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
arrests_2019 <- tmpdf$total[tmpdf$offense=='Total1']
arrests_homicides_2019 <- tmpdf$total[tmpdf$offense=="Murder and nonnegligent manslaughter"]
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/table-29

#########################################################
#########################################################

#figure 1: police per capita and prisoners per capita
plotdf<-fulldf[
  !is.na(police) & 
    !is.na(prisoners) &
    advanced
]

plotdf$prisoners[plotdf$countryname=='USA']<-
  incrateusa_2019
plotdf$police[plotdf$countryname=='USA'] <- 
  10^5 * police_2019/pop_2019
plotdf$usa<-plotdf$countryname=='USA'
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
  filename='fig_policeprisoners_percapita.png',
  plot=g.tmp,
  width=5,
  height=5
)



#########################################################
#########################################################


#figure 2: police per homicide and prisoners per homicide
plotdf<-fulldf[
  !is.na(police) & 
    !is.na(prisoners) &
    advanced
]

plotdf$prishomratio[plotdf$countryname=='United States of America']<-
  prisoners_2019/homicides_2019
plotdf$polhomratio[plotdf$countryname=='United States of America'] <- 
  police_2019/homicides_2019
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


g.tmp<- ggplot(
  plotdf,
  aes(
    label=countryname,
    x=polhomratio,
    y=prishomratio,
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
    breaks=c(0,50,100,150),
    limits=c(0,200)
  ) +
  xlab('\nPolice per Homicide') +
  ylab('Prisoners per Homicide\n') +
  theme_bw() 

setwd(outputdir)
ggsave(
  filename='fig_policeprisoners_homicide.png',
  plot=g.tmp,
  width=5,
  height=5
)

mydf<-plotdf

#########################################################
#########################################################

# POLICE KILLINGS)
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

#CLEARANCE RATE

plotdf<-extradf[advanced==T]

#drop netherlands, which has outlying values that make little sense
plotdf<-plotdf[countryname!='Netherlands']

#get facts from usa today
tmp<-plotdf$countryname=='USA'
plotdf$homicides[tmp] <- homicides_2019
plotdf$arrests_homicide[tmp] <- arrests_homicides_2019
plotdf$police[tmp] <- police_2019

#stats
plotdf$polhomratio <- plotdf$police/plotdf$homicides
plotdf$clearance_rate <- plotdf$arrests_homicide/
  plotdf$homicides
plotdf$police_efficiency <- plotdf$arrests_homicide/plotdf$police
plotdf$clearance_rate2 <- plotdf$convictions_homicide/plotdf$homicides

#arrests is not perfectly comparable, so consider convictions
#https://bjs.ojp.gov/content/pub/pdf/fssc06st.pdf, table 1.6
tmp<-plotdf$countryname=='USA'
plotdf$clearance_rate2[tmp] <- 8816/homicides_2006

#back out police efficiency, using clearance rate and polhomratio
plotdf$police_efficiency2 <- plotdf$clearance_rate2/plotdf$polhomratio

# setwd(outputdir)
# write.csv(
#   plotdf,
#   'tmpdf.csv',
#   row.names=F
# )

tmpvars<-c(
  'countryname',
  'clearance_rate',
  'police_efficiency',
  'polhomratio'
)

plotdf<-plotdf[,tmpvars,with=F]
tmp<-apply(plotdf,1,function(x) sum(is.na(x))==0)
plotdf<-plotdf[tmp,]

plotdf<-gather(
  plotdf,
  var,
  val,
  clearance_rate:polhomratio
) %>% data.table


tmplevels<-c(
  'clearance_rate',
  'polhomratio',
  'police_efficiency'
)
tmplabels<-c(
  'Clearance Rate (Homicide Arrests/Homicides)',
  'Police Footprint (Police/Homicides)',
  'Police Efficiency (Homicide Arrests/Police)'
)
plotdf$var<-factor(plotdf$var,tmplevels,tmplabels)

#colors
plotdf$color<-'grey'
tmp<-str_detect(plotdf$countryname,'USA$')
plotdf$color[tmp]<-'red'
tmp<-str_detect(plotdf$countryname,'USA\\s\\(')
plotdf$color[tmp]<-'blue'
plotdf$color<-factor(plotdf$color)
tmpcolors<-levels(plotdf$color)
names(tmpcolors)<-levels(plotdf$color)


#get median values for other countries
tmp<-str_detect(plotdf$countryname,"USA")
linedf<-plotdf[
  !str_detect(countryname,'USA')
  ,
  .(medianval = median(val))
  ,
  by=c(
    'var'
  )
]

#order countries by clearance rate
tmpdf<-plotdf[plotdf$var=='Clearance Rate (Homicide Arrests/Homicides)']
tmplevels<-tmpdf$countryname[order(-tmpdf$val)]
plotdf$countryname <- factor(plotdf$countryname,tmplevels)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=countryname,
    y=val,
    fill=color
  )
) +
  geom_bar(
    stat='identity',
    color='black'
  ) +
  geom_hline(
    data=linedf,
    aes(
      yintercept=medianval
    ),
    linetype='dashed',
    alpha=0.5
  ) +
  scale_fill_manual(
    guide=F,
    values=tmpcolors
  ) +
  facet_grid(
    ~ var,
    scales='free'
  ) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() 



setwd(outputdir)
ggsave(
  filename='fig_clearancerate.png',
  plot=g.tmp,
  width=8*1.25,
  height=5*1.25
)

#make another graph, which is just USA and developed world median
plotdf<-plotdf[
  ,
  .(
    countryname=paste0(countryname,collapse=''),
    val=median(val)
  )
  ,
  by=c(
    'var',
    'color'
  )
]
tmp<-plotdf$countryname!='USA'
plotdf$countryname[tmp]<-'First World'

g.tmp <- ggplot(
  plotdf,
  aes(
    x=countryname,
    y=val,
    fill=color
  )
) +
  geom_bar(
    width=0.7,
    stat='identity',
    color='black'
  ) +
  scale_fill_manual(
    guide=F,
    values=tmpcolors
  ) +
  facet_wrap(
    ncol=1,
    ~ var,
    scales='free'
  ) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() 

setwd(outputdir)
ggsave(
  filename='fig_clearancerate2.png',
  plot=g.tmp,
  width=5*1.25,
  height=4*1.25
)

#########################################################
#########################################################

#plot arrests/homicide and prisoners/homicide
#but add rows for blacks and whites in the US, separately


plotdf<-extradf[advanced==T] 

#get facts from usa today
tmp<-plotdf$countryname=='USA'
plotdf$homicides[tmp] <- homicides_2019
plotdf$arrests[tmp] <- arrests_2019
plotdf$arrests_homicide[tmp] <- arrests_homicides_2019
plotdf$police[tmp] <- police_2019
plotdf$prisoners[tmp] <- prisoners_2019

#add black/white
#sources
#arrest %'s from https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/tables/table-43
#homicide %'s from https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/expanded-homicide
#prisoner %'s from the whole pie // https://www.prisonpolicy.org/reports/pie2019.html
#i use %'s rather than raw counts, because some arrests/murders don't have race reported

#the one tricky thing is 'homicides'
#we have a count of victims; but what we really want is a count of likely offenders
#one way to do this is to just use the victim count
#this is pretty safe, given that most crime is intra-racial
#but a better estimate would use what we know about inter-racial crime
#so we do that, using additional info in:
#https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/tables/expanded-homicide-data-table-6.xls

blackvictims_homicides_2019 <- 0.547 * homicides_2019
whitevictims_homicides_2019 <- 0.423 * homicides_2019
blackoffenders_homicides_2019 <- 
  2574/2906 * blackvictims_homicides_2019 + #share of blackvictims killed by blacks
  566/3299 * whitevictims_homicides_2019 #share of white victims killed by whites
whiteoffenders_homicides_2019 <- 
  246/2906 * blackvictims_homicides_2019 + #share of black victims killed by whites
  2594/3299 * whitevictims_homicides_2019

blackvictims_homicides_2019/blackoffenders_homicides_2019
whitevictims_homicides_2019/whiteoffenders_homicides_2019
#using offenders rather than victims makes little difference for blacks
#a bigger difference for whites, but in a direction that 
#makes our bigger argument even more dramatic..

newdf<-data.frame(
  countryname=c('USA (Black Americans)','USA (White Americans)'),
  arrests=c(0.266 * arrests_2019,0.694 * arrests_2019),
  homicides=c(blackoffenders_homicides_2019,whiteoffenders_homicides_2019),
  prisoners=c(0.4 * prisoners_2019,0.39 * prisoners_2019)
)

plotdf<-rbind.fill(
  plotdf,
  newdf
) %>% data.table

plotdf$punitiveness <- plotdf$prisoners/plotdf$homicides
plotdf$certainty <- plotdf$arrests/plotdf$homicides
plotdf$severity <- plotdf$prisoners/plotdf$arrests


tmpvars<-c(
  'countryname',
  'punitiveness',
  'certainty',
  'severity'
)

plotdf<-plotdf[,tmpvars,with=F]
tmp<-apply(plotdf,1,function(x) sum(is.na(x))==0)
plotdf<-plotdf[tmp,]

plotdf<-gather(
  plotdf,
  var,
  val,
  punitiveness:severity
) %>% data.table

tmplevels<-c(
  'punitiveness',
  'certainty',
  'severity'
)
tmplabels<-c(
  'Punitiveness (Prisoners/Homicides)',
  'Certainty (Arrests/Homicides)',
  'Severity (Prisoners/Arrests)'
)
plotdf$var<-factor(plotdf$var,tmplevels,tmplabels)

plotdf$color<-'grey'
tmp<-str_detect(plotdf$countryname,'USA$')
plotdf$color[tmp]<-'red'
tmp<-str_detect(plotdf$countryname,'USA\\s\\(')
plotdf$color[tmp]<-'blue'
plotdf$color<-factor(plotdf$color)
tmpcolors<-levels(plotdf$color)
names(tmpcolors)<-levels(plotdf$color)

#get median values for other countries
tmp<-str_detect(plotdf$countryname,"USA")
linedf<-plotdf[
  !str_detect(countryname,'USA')
  ,
  .(medianval = median(val))
  ,
  by=c(
    'var'
  )
]

#order countries by certainty,
#least to most, sinc ehtis is the standout point
tmpdf<-plotdf[plotdf$var=='Certainty (Arrests/Homicides)']
tmplevels<-tmpdf$countryname[order(-tmpdf$val)]
plotdf$countryname <- factor(plotdf$countryname,tmplevels)



g.tmp <- ggplot(
  plotdf,
  aes(
    x=countryname,
    y=val,
    fill=color
  )
) +
  geom_bar(
    stat='identity',
    color='black'
  ) +
  geom_hline(
    data=linedf,
    aes(
      yintercept=medianval
    ),
    linetype='dashed',
    alpha=0.5
  ) +
  scale_fill_manual(
    guide=F,
    values=tmpcolors
  ) +
  facet_grid(
    ~ var,
    scales='free'
  ) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() 


setwd(outputdir)
ggsave(
  filename='fig_certaintyseverity.png',
  plot=g.tmp,
  width=7*1.25,
  height=5*1.25
)

#make another graph, which is just USA and developed world median
plotdf1<-plotdf[str_detect(countryname,'USA') & !str_detect(countryname,'White Americans')]
plotdf2<-plotdf[
  !str_detect(countryname,'USA')
  ,
  .(
    countryname='First World',
    val=median(val)
  )
  ,
  by=c(
    'var',
    'color'
  )
]
plotdf<-rbind.fill(plotdf1,plotdf2)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=countryname,
    y=val,
    fill=color
  )
) +
  geom_bar(
    width=0.7,
    stat='identity',
    color='black'
  ) +
  scale_fill_manual(
    guide=F,
    values=tmpcolors
  ) +
  facet_wrap(
    ncol=1,
    ~ var,
    scales='free'
  ) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() 

setwd(outputdir)
ggsave(
  filename='fig_certaintyseverity2.png',
  plot=g.tmp,
  width=5*1.25,
  height=5*1.25
)

stop()
#########################################################
#########################################################

#calculations for welfare section


arrests<-+ 7808300
prisoners<- -1932000
arrest_to_prisonyear<-3/365
arrests_prisonyears <- arrests * arrest_to_prisonyear
prisonyear_to_lifeyear <-1/2
arrests_lifeyears <- arrests_prisonyears * prisonyear_to_lifeyear
lifeyears_to_life <- 1/65
arrests_lives <- arrests_lifeyears * lifeyears_to_life
homicides<--4240
policekillings<--910
homicides*40
policekillings*40


#police and prisoners in TFWB
police_fwb <- police_2019 + 500000
homicides_fwb <- homicides_2019 - 4240
prisoners_fwb <- prisoners_2019 -  2000000
arrests_fwb <- arrests_2019 + 7808300
arrests_fwb/homicides_fwb
arrests_fwb/homicides_fwb/linedf$medianval[linedf$var=='Certainty (Arrests/Homicides)']
police_fwb/homicides_fwb
10^5 *police_fwb/pop_2019


#make the plot
newdf<-data.frame(
  countryname='TFWB',
  polhomratio=police_fwb/homicides_fwb,
  prishomratio=prisoners_fwb/homicides_fwb
)

plotdf<-rbind.fill(
  mydf,
  newdf
)

g.tmp<- ggplot(
  plotdf,
  aes(
    label=countryname,
    x=polhomratio,
    y=prishomratio,
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
    breaks=c(0,50,100,150),
    limits=c(0,200)
  ) +
  xlab('\nPolice per Homicide') +
  ylab('Prisoners per Homicide\n') +
  theme_bw() 

setwd(outputdir)
ggsave(
  filename='fig_policeprisoners_tfwb_homicide.png',
  plot=g.tmp,
  width=5,
  height=5
)




