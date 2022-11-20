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

#########################################################
#########################################################

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

#GET #'S OF THE EFFICIENT POINT
sumdf[pointType=='fwbalance']
sumdf[pointType=='statusquo']

#########################################################
#########################################################

#FIGURE

#make a gradient graph
#this graph colors the points by efficiency

plotdf <- sumdf[
  moneysaved<=0 & 
  myMethod=='stepwise' &
    myUnits=='yearsoflife' &
    myOrientation=='bestguess' &
    myElasticities=='constant' & 
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
    y=incrateusa_2021,
    x=10^5 * police_2021/pop_2021,
    label='Mass Incarceration'
  )
)
extradf <- rbind.fill(
  extradf,
  data.frame(
    x=0.75 * 10^5 * police_2021/pop_2021,
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

#FIGURE

#this graph has arrows pointing in the direction of the efficient point

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

#FIGURE/TABLE

#this graph summarizes how the efficient point changes
#as you tweak the robustness checks.. 

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
  geom_point(
    size=2
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

#ALSO MAKE A TABLE

tmplevels<-c(
  'Preferred Choices',
  'Not Stepwise',
  'Lives Lost',
  'Changing Elasticities',
  'Prison-Year Deflated',
  'Police Lead to Lots of Arrests',
  'Arrest = 1 Week in Prison',
  'Prison Does Reduce Crime',
  'More Police More Police Violence',
  'Police Dont Reduce Crime'
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

#FIGURE 

#how does the efficient point change
#as we increase the harms of state violence?

#these are all the worlds w/ full consequences
setwd(filesdir)
mydf <- fread('thetradeoff_consequences.csv')

#limit to those which save money
these.is <- sumdf$i[sumdf$moneysaved<=0] %>% unique
mydf<-mydf[i%in%these.is,]

#limit to default choices
mydf <- mydf[
    myMethod=='stepwise' &
    myUnits=='yearsoflife' &
    myOrientation=='bestguess' &
    myElasticities=='constant' & 
    myPrizChoice=='standard'
]

weights<-c(1:10)
tmpdf <- lapply(weights,function(w) {
  #w<-1
  returndf<-mydf[
    ,
    .(
      sum=finalguess[name=='prisoners'] * w +
        finalguess[name=='arrests'] * w +
        finalguess[name=='policekillings'] * w +
        finalguess[name=='homicides'] +
        finalguess[name=='othercrime']
    )
    ,
    by=c('i')
  ]
  returndf$w<-w
  returndf
}) %>% rbind.fill

tmpdf <- merge(
  tmpdf,
  unique(
    mydf[,c('i','prisonrate','policerate')]
  )
) %>% data.table

plotdf <- by(tmpdf,tmpdf$w,function(df) {
  #df<-tmpdf[tmpdf$w==1,]
  tmp<-df$sum==min(df$sum)
  data.frame(
    w=df$w[tmp],
    policerate=df$policerate[tmp],
    prisonrate=df$prisonrate[tmp]
  )
}) %>% rbind.fill

ggplot(
  plotdf,
  aes(
    x=policerate,
    y=prisonrate,
    label=w
  )
) +
  geom_label() +
  # scale_x_continuous(
  #   breaks=c(0,250,500,750),
  #   limits=c(0,750)
  # ) +
  # scale_y_continuous(
  #   breaks=c(0,250,500,750,1000),
  #   limits=c(0,1000)
  # ) +
  theme_bw()

#########################################################
#########################################################

#TABLE

#summarize the consequences of the first world balance, in full
setwd(filesdir); dir()
plotdf <- fread('thetradeoff_consequences.csv')

# #quick fix for typo
# tmp<-plotdf$choice=='Pessimistic about Police'
# plotdf$choice[tmp]<-'Pessimistic About Police'
# tmp<-plotdf$choice=='Optimistic about Prison'
# plotdf$choice[tmp]<-'Optimistic About Prison'

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
  'Changing Elasticities',
  'Prison-Year Deflated',
  'Police Lead to Lots of Arrests',
  'Arrest = 1 Week in Prison',
  'Prison Does Reduce Crime',
  'More Police More Police Violence',
  'Police Dont Reduce Crime'
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

#FIGURE

#make a fairness graph
#based on A, B, C, D, E

#A (gangster) cares about all costs
#B (grandmother) cares about crime
#C (young black poor man) cares about crime and arrest
#D (young black rich man) cares about arrest
#E (rest of us) care about resources

characters<-list(
  'A'=c('prisoners','arrests','homicides','othercrime','policekillings'),
  'B'=c('homicides','othercrime'),
  'C'=c('homicides','othercrime','arrests','policekillings'),
  'D'=c('arrests'),
  'E'=c('arrests','incarceration'),
  'F'=c('incarceration'),
  'G'=c('homicides','othercrime','incarceration'),
  'H'=c('resources_raw')
)

setwd(filesdir); dir()
sumdf <- fread('thetradeoff_consequences.csv')

#get info for table, just restricted to the worlds

tabdf<-lapply(names(characters),function(c) {
  #c<-'D'
  print(c)
  tmp<-names(characters)==c
  cats <- characters[tmp] %>% unlist %>% unname
  tmpdf<-sumdf[
    pointType!='2d' &
      choice=='Preferred Choices' &
      name%in%c(cats,'resources_raw')
  ]
  tmpdf<-tmpdf[
    ,
    .(
      finalguess=sum(finalguess),
      prisonrate=unique(prisonrate),
      policerate=unique(policerate)
    )
    ,
    by='pointType'
  ]
  c(c,tmpdf$pointType[order(tmpdf$finalguess)])
})

plotdf<-lapply(names(characters),function(c) {
  #c<-'richminority'
  print(c)
  tmp<-names(characters)==c
  cats <- characters[tmp] %>% unlist %>% unname
  these.is<-sumdf$i[sumdf$name=='resources_raw' & sumdf$finalguess<0]
  tmpdf<-sumdf[
    i%in%these.is &
    choice=='Preferred Choices' &
      name%in%c(cats,'resources_raw')
  ]
  tmpdf<-tmpdf[
    ,
    .(
      finalguess=sum(finalguess),
      prisonrate=unique(prisonrate),
      policerate=unique(policerate)
    )
    ,
    by='i'
  ]
  data.frame(
    character=c,
    policerate=tmpdf$policerate[tmpdf$finalguess==min(tmpdf$finalguess)],
    prisonrate=tmpdf$prisonrate[tmpdf$finalguess==min(tmpdf$finalguess)]
  )
}) %>% rbind.fill

g.tmp <- ggplot(
  plotdf,
  aes(
    x=policerate,
    y=prisonrate,
    label=character,
    color=character
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
  geom_point() +
  scale_color_discrete(
    guide='none'
  ) +
  scale_x_continuous(
    breaks=c(0,250,500,750),
    limits=c(0,750)
  ) +
  scale_y_continuous(
    breaks=c(0,250,500,750,1000),
    limits=c(0,1000)
  ) +
  xlab('\nPolice per 100,000') +
  ylab('Prisoners per 100,000\n') +
  theme_bw()
  
setwd(outputdir)
ggsave(
  filename='fig_thefairpoints_graph.png',
  plot=g.tmp,
  width=5*1.5,
  height=6*1.5
)



#########################################################
#########################################################

