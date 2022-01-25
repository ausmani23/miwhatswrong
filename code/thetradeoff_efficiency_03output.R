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
sumdf <- fread('thetradeoff_sumdf.csv')

tmp<-sumdf$choice=='Pessimistic about Police'
sumdf$choice[tmp]<-'Pessimistic About Police'
tmp<-sumdf$choice=='Optimistic about Prison'
sumdf$choice[tmp]<-'Optimistic About Prison'

#load functions
setwd(codedir); dir()
source('calculate_homicides.R')
source('calculate_costsbenefits.R')

#########################################################
#########################################################

#PREFERRED SPECIFICATION

plotdf <- sumdf[
  moneysaved<=0 & 
  myMethod=='stepwise' &
    myUnits=='yearsoflife' &
    myOrientation=='bestguess' &
    pointType=='2d' &
    myPrizChoice=='standard'
]

#make a plot
#plotdf <- plotdf[plotdf$moneysaved<=0]
plotdf$finalguess<- -1 * plotdf$finalguess/10^5

#make all the negatives equivalent
tmp<-!is.finite(plotdf$finalguess) |
  plotdf$finalguess < 0
plotdf$finalguess[tmp]<- 0

#illustate these points
extradf <- rbind.fill(
  solutiondf,
  data.frame(
    y=incrateusa_2019,
    x=10^5 * police_2019/pop_2019,
    label='Mass Incarceration'
  )
)
extradf <- rbind.fill(
  extradf,
  data.frame(
    x=0.75 * 10^5 * police_2019/pop_2019 ,
    y=50, #in line w/ Scandinavia
    label='Defund'
  )
)


g.tmp <- 
  ggplot(
    plotdf,
    aes(
      x=policerate,
      y=prisonrate,
      fill=finalguess
    )
  ) + 
  geom_tile() + 
  scale_fill_gradient2(
    name = 'Years of Life Saved (100,000s)',
    low = "#fdc086",
    mid = 'white',
    high = "#7fc97f"
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  scale_y_continuous(
    breaks=c(0,250,500,750,1000),
    limits=c(0,1000)
  ) +
  stat_function(
    fun=firstworld_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=usa_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=solution_fun,
    size=1,
    color='black',
    linetype=2,
    arrow = arrow(length = unit(0.75, "cm"))
  ) +
  geom_point(
    data=extradf,
    aes(
      x=x,
      y=y,
      fill=NULL
    )
  ) +
  annotate(
    geom='label',
    x=extradf$x+30,
    y=extradf$y+50,
    label=paste0(
      extradf$label,
      " (",
      round(extradf$x),
      ",",
      round(extradf$y),
      ")"
    )
  ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal',
    panel.grid=element_blank()
  )

setwd(outputdir)
ggsave(
  filename='fig_thetradeoff_consequences.png',
  plot=g.tmp,
  width=5*1.5,
  height=6*1.5
)

#########################################################
#########################################################

#make a cool graph w/ arrows pointing in direction of efficiency

plotdf <- sumdf[
  myMethod=='stepwise' &
    myUnits=='yearsoflife' &
    myOrientation=='bestguess' &
    myPrizChoice=='standard' & 
    pointType=='2d'
]
plotdf$finalguess<- -1 * plotdf$finalguess/10^3

plotdf$prisonrate_100 <- 50 * floor(plotdf$prisonrate/50)
plotdf$policerate_100 <- 50 * floor(plotdf$policerate/50)

tmpdf <- plotdf[
  finalguess!=0 &
    moneysaved<0
  ,
  .(
    xminpoint = min(policerate[finalguess==min(finalguess)]),
    yminpoint = min(prisonrate[finalguess==min(finalguess)]),
    xmaxpoint = max(policerate[finalguess==max(finalguess)]),
    ymaxpoint = max(prisonrate[finalguess==max(finalguess)])
  )
  ,
  by=c(
    'prisonrate_100',
    'policerate_100'
  )
]
tmpdf$group<-1:nrow(tmpdf)

tmpdf<-gather(
  tmpdf,
  point,
  val,
  xminpoint:ymaxpoint
) %>% data.table
tmpdf$var<-str_extract(tmpdf$point,"x|y")
tmpdf$point<-str_replace(tmpdf$point,"x|y","")
tmpdf<-spread(
  tmpdf,
  var,
  val
)
tmpdf$point<-as.numeric(tmpdf$point=='maxpoint')
tmpdf<-tmpdf[order(tmpdf$group,tmpdf$point),]

g.tmp<-ggplot(
  tmpdf,
  aes(
    x=x,
    y=y,
    group=group
  )
) +
  stat_function(
    fun=firstworld_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=usa_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=solution_fun,
    size=1,
    color='black',
    linetype=2,
    arrow = arrow(length = unit(0.75, "cm"))
  ) +
  geom_line(
    arrow=arrow(length = unit(0.15, "cm")),
    color='grey'
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  scale_y_continuous(
    breaks=c(0,250,500,750,1000),
    limits=c(0,1000)
  ) +
  geom_point(
    data=extradf,
    aes(
      x=x,
      y=y,
      fill=NULL,
      group=NULL
    )
  ) +
  annotate(
    geom='label',
    x=extradf$x+30,
    y=extradf$y+50,
    label=paste0(
      extradf$label,
      " (",
      round(extradf$x),
      ",",
      round(extradf$y),
      ")"
    )
  ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw() +
  theme(
    panel.grid = element_blank()
  )

setwd(outputdir)
ggsave(
  filename='fig_thetradeoff_consequences_arrows.png',
  plot=g.tmp,
  width=5*1.5,
  height=6*1.5
)

#########################################################
#########################################################

#summarize how the efficient point changes
#depending on which choices you make

tmpdf<-sumdf[
  moneysaved<=0
  ,
  .(finalguess=min(finalguess))
  ,
  by=c('choice')
]
plotdf1 <- merge(sumdf,tmpdf,by=c('choice','finalguess'))
plotdf1$choicetype <- 'Constrained by $'

tmpdf<-sumdf[
  ,
  .(finalguess=min(finalguess))
  ,
  by=c('choice')
]
plotdf2 <- merge(sumdf,tmpdf,by=c('choice','finalguess'))
plotdf2$choicetype <- 'Unconstrained by $'
plotdf <- rbind.fill(
  plotdf1,
  plotdf2
)

require(ggrepel)

g.tmp <- ggplot(
  plotdf[plotdf$choicetype=='Constrained by $',],
  aes(
    x=policerate,
    y=prisonrate,
    label=choice,
    color=choice
  )
) +
  stat_function(
    fun=firstworld_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=usa_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=solution_fun,
    size=1,
    color='black',
    linetype=2,
    arrow = arrow(length = unit(0.75, "cm"))
  ) +
  geom_label_repel() +
  geom_point(
    size=2,
    color='black'
  ) +
  # facet_wrap(
  #   ~ choicetype
  # ) +
  scale_color_discrete(
    guide=F
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  scale_y_continuous(
    breaks=c(0,250,500,750,1000),
    limits=c(0,1000)
  ) +
  geom_hline(
    yintercept=quantile(rates_othcountries,c(0,0.5,1)),
    linetype=2,
    alpha=0.5,
    color='grey'
  ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw()


setwd(outputdir)
ggsave(
  filename='fig_theefficientpoints.png',
  plot=g.tmp,
  width=5*1.5,
  height=6*1.5
)


#also make a table
tmplevels<-c(
  'Preferred Choices',
  'Not Stepwise',
  'Lives Lost',
  'Pessimistic About Police',
  'Optimistic About Prison',
  'Changing Elasticities',
  'Prison-Year Deflated'
) %>% rev
plotdf$choice <- factor(plotdf$choice,tmplevels)
g.tmp <- ggplot(
  plotdf,
  aes(
    x=choicetype,
    y=choice,
    label=paste0("(",round(policerate),", ",round(prisonrate),")")
  )
) +
  geom_tile(fill='white',color='black') + 
  geom_text() +
  xlab("") +
  ylab("") +
  scale_x_discrete(position='top') + 
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
ggsave(
  filename='fig_theefficientpoints_table.png',
  plot=g.tmp,
  width=4*1.5,
  height=2*1.5
)
#########################################################
#########################################################

#summarizing the consequences of the first world balance
setwd(filesdir); dir()
plotdf <- fread('thetradeoff_consequences.csv')

#quick fix for typo
tmp<-plotdf$choice=='Pessimistic about Police'
plotdf$choice[tmp]<-'Pessimistic About Police'
tmp<-plotdf$choice=='Optimistic about Prison'
plotdf$choice[tmp]<-'Optimistic About Prison'

tmp <- str_detect(
  plotdf$name,
  'raw'
)
plotdf$units<-'converted'
plotdf$units[tmp]<-'raw'
plotdf$name <- str_replace(
  plotdf$name,"\\_raw",""
)
tmpvars<-c(
  'prisoners',
  'arrests',
  'homicides',
  'othercrime',
  'policekillings',
  'prisoners',
  'sum'
)
plotdf<-plotdf[name%in%tmpvars]

#the efficient points plus other points of interest.
#decarcerate (210,110) #we get decarceration we want but not police
#refund (370,700) #we get policing we want but not prison
#fwbalance

policerates<-c(plotdf1$policerate,solutiondf$x,160,210,370)
prisonrates<-c(plotdf1$prisonrate,solutiondf$y,50,110,700)
tmpseq.i<-seq_along(policerates)
tmp<-lapply(tmpseq.i,function(i){
  tmp<-round(plotdf$policerate - policerates[i])==0 & 
    round(plotdf$prisonrate - prisonrates[i])==0
})
tmp<-Reduce(f='|',tmp)
plotdf <- plotdf[tmp,]

#color the most efficient point in each category in green
plotdf$point<-
  paste0(
    "(",
    round(plotdf$policerate),", ",
    round(plotdf$prisonrate),")"
  )
tmplevels<-unique(plotdf$point[order(plotdf$policerate)])
plotdf$point <- factor(plotdf$point,tmplevels)

tmpdf<-plotdf[
  name=='sum'
  ,
  .(bestpoint = point[finalguess==min(finalguess)])
  ,
  by=c('choice')
]
plotdf<-merge(
  plotdf,
  tmpdf,
  by='choice'
)
plotdf$bestpoint <- as.numeric(plotdf$point==plotdf$bestpoint)
tmplevels<-c(0,1); tmplabels<-c('Other Point','Most Efficient Point')
plotdf$bestpoint <- factor(plotdf$bestpoint,tmplevels,tmplabels)

plotdf$finalguess <- 
  prettyNum(round(plotdf$finalguess/10^3,2),big.mark=',')

tmplevels<-c(
  'Preferred Choices',
  'Not Stepwise',
  'Lives Lost',
  'Pessimistic About Police',
  'Optimistic About Prison',
  'Changing Elasticities',
  'Prison-Year Deflated'
)
plotdf$choice <- factor(plotdf$choice,tmplevels)

tmplevels<-c(
  'raw',
  'converted'
)
plotdf$units <- factor(
  plotdf$units,
  tmplevels
)

tmplevels<-c(
  'prisoners',
  'police',
  'homicides',
  'othercrime',
  'arrests',
  'policekillings',
  'sum'
) %>% rev
plotdf$name <- factor(plotdf$name,tmplevels)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=units,
    y=name,
    label=finalguess,
    fill=bestpoint
  )
) +
  geom_tile(color='black') + 
  geom_text() +
  facet_grid(
    point ~ choice
  ) +
  scale_fill_discrete(
    name=""
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank(),
    legend.position='bottom',
    legend.direction='horizontal'
  ) +
  xlab("") +
  ylab("") 


setwd(outputdir)
ggsave(
  filename='fig_theefficientpoints_consequences_table.png',
  plot=g.tmp,
  width=8*1.75,
  height=6*1.75
)

#get the distance between all these points
tmpdf<-unique(plotdf[,c('prisonrate','policerate')])
newrow<-data.frame(prisonrate=700,policerate=212)
tmpdf<-rbind.fill(
  tmpdf,
  newrow
)
loopdf<-expand.grid(
  a=1:nrow(tmpdf),
  b=1:nrow(tmpdf)
)
loopdf<-loopdf[loopdf$a!=loopdf$b,]
loopdf$distance<-sapply(1:nrow(loopdf),function(i) {
  #i<-1
  thisrow<-loopdf[i,]
  pointa<-unlist(tmpdf[thisrow$a,])
  pointb<-unlist(tmpdf[thisrow$b,])
  sqrt(abs(pointa[1]-pointb[1])^2 + abs(pointa[2]-pointb[2])^2) %>% unname
})
dist1 <- loopdf$distance[loopdf$a==6 & loopdf$b==9] #fw balance to mass incarceration
dist2 <- loopdf$distance[loopdf$a==1 & loopdf$b==6] #fw balance to pesssimistic point
dist3 <- loopdf$distance[loopdf$a==1 & loopdf$b==9] #pessimistic point to mass incarceration 
dist4 <- loopdf$distance[loopdf$a==1 & loopdf$b==7] #pessimistic point to defund

#pessimist point is:
dist3/dist2 #5.3 times 
dist4/dist2  #0.87

#########################################################
#########################################################

#make a graph showing where the efficient points are
#if all we care about is homicides and nothing else
setwd(filesdir); dir()
sumdf <- fread('thetradeoff_consequences.csv')
sumdf <- sumdf[name%in%c('homicides','othercrime','resources'),]
sumdf<-spread(
  sumdf,
  name,
  finalguess
) %>% data.table
sumdf$crime<-sumdf$homicides+sumdf$othercrime

#what's the best we can do, w/ the homicide rate? 
# setwd(filesdir); dir()
# sumdf <- fread('thetradeoff_consequences.csv')
# sumdf <- sumdf[name%in%c('homicides_raw','resources'),]
# sumdf<-spread(
#   sumdf,
#   name,
#   finalguess
# ) %>% data.table
# tmpdf<-sumdf[
#   resources<=0
#   ,
#   .(homicides=min(homicides_raw))
#   ,
#   by=c('choice')
# ]
#most optimistic is 11,000
#but more likely it seems to be a decline in about 4,000

tmpdf<-sumdf[
  resources<=0
  ,
  .(crime=min(crime))
  ,
  by=c('choice')
]
plotdf1 <- merge(sumdf,tmpdf,by=c('choice','crime'))
plotdf1$choicetype <- 'Constrained by $'

tmpdf<-sumdf[
  ,
  .(crime=min(crime))
  ,
  by=c('choice')
]
plotdf2 <- merge(sumdf,tmpdf,by=c('choice','crime'))
plotdf2$choicetype <- 'Unconstrained by $'

plotdf <- rbind.fill(
  plotdf1,
  plotdf2
)


g.tmp <- ggplot(
  plotdf,
  aes(
    x=policerate,
    y=prisonrate,
    label=choice,
    color=choice
  )
) +
  stat_function(
    fun=firstworld_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=usa_fun,
    size=1,
    color='black'
  ) +
  stat_function(
    fun=solution_fun,
    size=1,
    color='black',
    linetype=2,
    arrow = arrow(length = unit(0.75, "cm"))
  ) +
  geom_label_repel() +
  geom_point(
    size=2,
    color='black'
  ) +
  facet_wrap(
    ~ choicetype
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  scale_y_continuous(
    breaks=c(0,250,500,750,1000),
    limits=c(0,1000)
  ) +
  geom_hline(
    yintercept=quantile(rates_othcountries,c(0,0.5,1)),
    linetype=2,
    alpha=0.5,
    color='grey'
  ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw()


setwd(outputdir)
ggsave(
  filename='fig_theefficientpoints_crime.png',
  plot=g.tmp,
  width=10*1.5,
  height=6*1.5
)


#########################################################
#########################################################

