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

#calculations from the whole pie 2022
#https://www.prisonpolicy.org/reports/pie2022.html#:~:text=Can%20it%20really%20be%20true,when%20they%20break%20the%20law%3F

state<-1042
federal<-208
local<-547
juvenile<-36

drug_federal<-67
drug_local_unconvicted<-113
drug_local_convicted<-24
drug_local<-drug_local_convicted+drug_local_unconvicted
drug_state_possession<-146
drug_state_other<-106
drug_state<-drug_state_possession+drug_state_other
drug_juvenile<-2.5

#upper limit of drug estimate
drugs_all <- drug_federal + 
  drug_local_unconvicted + 
  drug_local_convicted + 
  drug_state_possession + 
  drug_state_other
drugs_prisoners <- drugs_all/(state+federal+local) #25%

#########################################################
#########################################################

#using the sevigny and caulkins estimates, p. 417
#where 'll' means
#no concurrent convictions, no firearms, no drug group
#no high-level role, no nondrug convictions, no priors

federal_ll_pct <- 10977/57076 
state_ll_pct <- 29160/217248

drugs_lowerlevel <- drug_state * state_ll_pct + 
  drug_federal * federal_ll_pct +
  drug_local + 2 * max(c(state_ll_pct,federal_ll_pct))
drugs_lowerlevel/(state+federal+local) #10%

#########################################################
#########################################################

#make a plot

plotdf<-data.frame(
  drug=c(drugs_lowerlevel,drugs_all),
  rest=c(state+federal+local-drugs_lowerlevel,state+federal+local-drugs_all),
  type=c('Low-Level Drug vs. Rest','Drug vs. Rest')
)
plotdf<-gather(
  plotdf,
  var,
  val,
  drug:rest
)

tmplevels<-c('drug','rest') %>% rev
plotdf$var<-factor(plotdf$var,tmplevels)
tmpcolors<-c('red','blue')
names(tmpcolors)<-tmplevels

plotdf$type<-factor(plotdf$type,unique(sort(plotdf$type)) %>% rev)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=type,
    y=val,
    var=var,
    fill=var
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    color='black'
  ) +
  # scale_color_manual(
  #   values=tmpcolors,
  #   guide=F
  # ) +
  scale_fill_manual(
    values=tmpcolors,
    guide='none'
  ) +
  xlab("") +
  ylab("\nNumber of Prisoners (1000's)") +
  theme_bw() +
  coord_flip()

setwd(outputdir); dir()
ggsave(
  plot=g.tmp,
  file='drugspct.png',
  width=6,
  height=3
)
