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

setwd(filesdir); dir()

plotdf<-rbind(
  rbind(
    fread('thearrested.csv'),
    fread('theincarcerated.csv')
  ),
  fread('themurdered.csv')
)

plotdf$contact
tmplevels<-c(
  'genpop',
  'everarrested',
  'pettilyarrested',
  'seriouslyarrested',
  'killedbypolice',
  'incarcerated',
  'murdered'
)
tmplabels<-c(
  'Adult Population',
  'Ever Arrested',
  'Petty Arrest in 2019',
  'Serious Arrest in 2019',
  'Killed by Police in 2019',
  'Incarcerated',
  'Murdered'
)
plotdf$contact <- factor(
  plotdf$contact,
  tmplevels %>% rev,
  tmplabels %>% rev
)

tmplevels<-c(
  'black',
  'hsdropout',
  'blackXhsdropout'
)
tmplabels<-c(
  '% Black',
  '% HS Dropout',
  '% Black and HS Dropout'
)
plotdf$category <- factor(
  plotdf$category,
  tmplevels,
  tmplabels
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=contact,
    y=100 * val,
    label=paste0(round(100 * val),"%"),
  )
) +
  geom_bar(
    stat='identity',
    fill='red',
    color='black', 
    width=0.5
  ) +
  geom_text(
    aes(
      y=(100 * val) + 4
    ),
    size=3
  ) +
  facet_wrap(
    ~ category,
    ncol=1
  ) +
  coord_flip() +
  theme_bw() +
  xlab("") + 
  ylab("")

setwd(outputdir)
ggsave(
  plot =g.tmp,
  filename = 'fig_thedisadvantaged.png',
  width=4,
  height=6
)

#########################################################
#########################################################

#lifetime calculations
adultpop_black <- 31 * 10^6
pettyarrests_black <- 1008872
seriousarrests_black <- 358880
killed_black <- 11393
policekilled_black <- 475
incarcerated_black <- .41 * 600000

#probabilities
p_pettyarrest <- 1-(1-(pettyarrests_black/adultpop_black))^20
p_seriousarrest <- 1-(1-(seriousarrests_black/adultpop_black))^20
p_policekilled <- 1-(1-(policekilled_black/adultpop_black))^20
p_incarcerated <- 1-(1-(incarcerated_black/adultpop_black))^20
p_killed <- 1-(1-(killed_black/adultpop_black))^20

#utility multipliers
u_pettyarrest <- 0.1 * 1/52 + 0.01 * (50-1/52)
u_seriousarrest <- 0.5 * 1/52 + 0.02 * (50-1/52)
u_policekilled <- 1 * 50
u_incarcerated <- 0.5 * 2 + 0.10 * 48
u_killed <- 1 * 50

#E(U) for A
u_mi <- 
  50 -
  p_killed * u_killed -
  p_incarcerated * u_incarcerated -
  p_policekilled * u_policekilled - 
  p_pettyarrest * u_pettyarrest

u_fw <- 
  50 -
  0.5 * p_killed * u_killed -
  0.15 * p_incarcerated * u_incarcerated -
  1 * p_policekilled * u_policekilled - 
  2.5 * p_pettyarrest * u_pettyarrest

u_mi
u_fw

#E(U) for C
u_mi <- 
  50 -
  (p_killed * u_killed) -
  #p_incarcerated * u_incarcerated -
  p_policekilled * u_policekilled - 
  p_pettyarrest * u_pettyarrest

u_fw <- 
  50 -
  0.5 * p_killed * u_killed -
  #0.15 * p_incarcerated * u_incarcerated -
  0.5 * p_policekilled * u_policekilled - 
  2.5 * p_pettyarrest * u_pettyarrest 

u_mi
u_fw
