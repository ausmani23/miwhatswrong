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

# goal: a graph showing the life course

#cbt
#earlychildhood
#violenceinterruption
#summerjobs
#greenspaces
#reentry
#restorative justice

tmplist <- list(
  expand.grid(
    program='Early Childhood',
    age=0:4,
    risk=1:10
  ),
  expand.grid(
    program='Summer Jobs',
    age=16:20,
    risk=1:10
  ),
  expand.grid(
    program='Re-Entry Programs',
    age=20:45,
    risk=1
  ),
  expand.grid(
    program='Violence Interruption',
    age=18:35,
    risk=1:2
  ),
  expand.grid(
    program='Restorative Justice',
    age=18:35,
    risk=1
  )#,
  # expand.grid(
  #   program='Green Spaces',
  #   age=0:80,
  #   risk=1:15
  # )
)
tmpdf <- Reduce( rbind, tmplist)
tmpdf$group <- 'Targeted Social Policy'

tmplist <- list(
  expand.grid(
    program='Education',
    age=4:18,
    risk=0:100
  ),
  expand.grid(
    program='Welfare and Income Support',
    age=0:65,
    risk=1:33
  ),
  expand.grid(
    program='Medicaid',
    age=0:65,
    risk=0:20
  ),
  expand.grid(
    program='Social Security',
    age=65:80,
    risk=1:100
  ),
  expand.grid(
    program='Medicare',
    age=65:80,
    risk=1:100
  )
)
tmpdf2 <- Reduce( rbind, tmplist)
tmpdf2$group <- 'Traditional Social Policy'

tmplist <- list(
  expand.grid(
    program='Police',
    age=15:35,
    risk=1:5
  ),
  expand.grid(
    program='Prisons',
    age=20:45,
    risk=1
  ),
  expand.grid(
    program='Courts',
    age=15:45,
    risk=1:3
  )
)
tmpdf3 <- Reduce( rbind, tmplist)
tmpdf3$group <- 'Penal Policy'

#########################################################
#########################################################

tmplist<-list(
  tmpdf2,
  tmpdf3,
  tmpdf
)
plotdf <- Reduce( rbind, tmplist) %>% data.table


#factors
plotdf$group<-factor(
  plotdf$group,
  unique(plotdf$group)
)
plotdf$program<-factor(
  plotdf$program,
  unique(plotdf$program)
)

#we want to put a label in the midpoints
labeldf<-plotdf[
  ,
  .(
    age=mean(age),
    risk=mean(risk)
  )
  ,
  by=c(
    'group',
    'program'
  )
]

require(ggrepel)

g.tmp <- ggplot(
  plotdf[group!="Penal Policy"],
  aes(
    x=age,
    y=100 + -1*risk,
    fill=program
  )
) + 
  geom_tile(
    alpha=0.25
  ) +
  # geom_point(
  #   data=labeldf
  # ) +
  geom_text_repel(
    data=labeldf[group!="Penal Policy"],
    aes(
      label=program
    )
  ) +
  facet_wrap(
    ~ group,
    ncol=1
  ) +
  scale_x_continuous(
    breaks=seq(0,80,10),
    limits=c(0,80)
  ) + 
  scale_y_continuous(
    #breaks=seq(0,100,10),
    limits=c(0,100)
  ) +
  scale_fill_discrete(
    name="",
    guide='none'
  ) +
  xlab("\nAge") +
  ylab("Risk of Offending\n") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  filename='fig_spendingprofile_ageXrisk.png',
  plot = g.tmp,
  width = 8, 
  height = 8
)

#########################################################
#########################################################

tmplist <- list(
  expand.grid(
    program='Expensive Pre-School (e.g., Perry Preschool)',
    type='Early Childhood',
    feasibility=-1,
    efficiency=3
  ),
  expand.grid(
    program='Inexpensive Pre-school (e.g., Head Start)',
    type='Early Childhood',
    feasibility=3,
    efficiency=-1
  ),
  expand.grid(
    program='Inexpensive Jobs Program (e.g., Job Corps)',
    type='At-Risk Youth',
    feasibility=2,
    efficiency=-2
  ),
  expand.grid(
    program='Expensive Jobs Program',
    type='At-Risk Youth',
    feasibility=-2,
    efficiency=2
  ),
  expand.grid(
    program='Inexpensive Training (e.g. Prison Education)',
    type='The Incarcerated',
    efficiency=-3,
    feasibility=1
  ),
  expand.grid(
    program='Expensive Training (e.g. Vocational Training)',
    type='The Incarcerated',
    efficiency=1,
    feasibility=-3
  )
)
plotdf <- Reduce( rbind, tmplist)


g.tmp <- ggplot(
  plotdf,
  aes(
    x=efficiency,
    y=feasibility,
    label=program,
    group=type,
    color=type
  )
) +
  geom_text() +
  geom_line(
    alpha=0.5,
    linetype='dashed'
  ) +
  scale_color_discrete(
    name=""
  ) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  xlim(-4,4) +
  ylim(-4,4) +
  xlab("\nEfficiency") + 
  ylab("Feasibility\n") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    legend.position='bottom',
    legend.direction='horizontal'
  )

setwd(outputdir)
ggsave(
  filename='fig_efficiencyfeasibilitytradeoff.png',
  plot = g.tmp,
  width = 8*1.25, 
  height = 6*1.25
)


