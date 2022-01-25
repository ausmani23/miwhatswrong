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

setwd(datadir); dir()
tmpdf<-fread(
  'SHR76_20.csv'
)
names(tmpdf)<-tolower(names(tmpdf))
tmpdf<-tmpdf[year==2020]

tmptable<-table(tmpdf$vicrace)
tmptable<-tmptable/sum(tmptable)
#53% black

returndf<-data.frame(
  contact='murdered',
  category='black',
  val=tmptable['Black'] %>% unname
)

tmptable<-table(tmpdf$vicsex)
tmptable/sum(tmptable)
#79% male

#########################################################
#########################################################

#add data on the population from IPUMS
#helper funcitons
isodd<-function(x) x%%2!=0
iseven<-function(x) x%%2==0
genfw<-function(x) { #takes columns you want, and generates fwf request  
  #format has to be startcol,endcol,startcol,endcol,startcol,endcol...  
  fwf<-c()  
  for (i in 1:length(x)) {
    if (i==1) 
      fwf<-append(fwf,-(x[1]-1)) 
    if (isodd(i) & i>1) {
      if ((x[i]-x[i-1])>1) 
        fwf<-append(fwf,-(x[i]-x[i-1])+1)
    }
    if (iseven(i)) 
      fwf<-append(fwf,x[i]-x[i-1]+1)
  }
  if (fwf[1]==0) fwf<-fwf[-1] #if we're getting first col, need to do this 
  return(fwf)
}
genfw(c(1,4))

setwd(datadir); dir()
tmpfname<-"usa_00039"
tmp<-readLines(paste0(tmpfname,".txt")) #get info from the codebook
stline <- str_detect(tmp,"Variable") %>% which %>% min + 1
endline <- str_detect(tmp,"Variable Availability Key") %>% which - 2
if(!is.finite(stline) | !is.finite(endline))
  stop('var missing')

tmp<-tmp[stline:endline]
tmp<-str_extract(tmp,"(.*?)\\sX\\s")
colnames<-str_extract(tmp,"[A-Z0-9]+") %>% 
  tolower
locs<-str_extract(tmp,"(H|P)\\s+[0-9]+(\\-[0-9]+)?")
locs<-str_replace(locs,"(H|P)\\s+","")
stloc<-str_extract(locs,"^[0-9]+") %>%
  as.numeric
endloc<-str_extract(locs,"[0-9]+$") %>%
  as.numeric

#order stloc
neworder<-order(stloc)
colnames<-colnames[neworder]
stloc<-stloc[neworder]
endloc<-endloc[neworder]

#need to generate fwf format for cols
tmpmatrix<-matrix(
  c(stloc,endloc),
  ncol=2
) 
tmpfwf<-c(t(tmpmatrix))
widths<-genfw(tmpfwf)
types<-rep("i",length(colnames)) %>%
  paste0(collapse="")
types<-rep("integer",length(colnames))

#histid is charcter
types[colnames%in%c('histid')]<-"character"

#LOAD DATASET

#this is data to construct
setwd(datadir); dir()
require(LaF)
raw<-laf_open_fwf(
  filename=paste0(tmpfname,".dat"),
  column_types=types,
  column_names=colnames,
  column_widths=widths
)
thisdf<-data.table(raw[1:nrow(raw),colnames])

#weights (combines geographic weight w/ person weight)
thisdf[,weight_f := perwt/100]
thisdf[,hhweight_f := hhwt/100]


#sex
thisdf[ sex==1 , sex_f := 1] 
thisdf[ sex==2 , sex_f := 2] 
#race/hispanic
#whites, non-hispanic
#blacks, non-hispanic
#hispanics
#other
#also, make those who choose black AND.. := black
blackcodes<-c(801,830:845) 
#also, make sure that hispanic is always reported
# tmp<-sum(df$hispan==9)
# if(tmp>0)
#   stop()
thisdf[ , race_f := 4] #make everyone other
thisdf[ race==1 & hispan==0, race_f := 1] #whites, non-hispanics
thisdf[ (race==2 | raced%in%blackcodes) & hispan==0, race_f := 2] #blacks, non-hisapnics
thisdf[ hispan != 0, race_f := 3] #hispanics, all races
#tmptab<-table( df$race_f, useNA="always" )
#100 * tmptab/sum(tmptab) 

#race
thisdf[,black_f:=0]
thisdf[ race_f==2,black_f:=1]

tmp<-thisdf[black_f==1 & age>=18,sum(weight_f)]/
  sum(thisdf$weight_f[thisdf$age>=18])
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact='genpop',
    category='black',
    val=tmp
  )
)


#class
thisdf[,hsdropout:=0]
thisdf[educd%in%c(2:50),hsdropout := 1]
tmp<-thisdf[hsdropout==1 & age>=18,sum(weight_f)]/sum(thisdf$weight_f[thisdf$age>=18])
returndf<-rbind.fill(
  returndf,
  data.frame(
    contact='genpop',
    category='hsdropout',
    val=tmp
  )
)


#raceXclass
thisdf[,raceXclass:=0]
thisdf[black_f==1 & hsdropout==1,raceXclass:=1]
tmp<-thisdf[raceXclass==1 & age>=18,sum(weight_f)]/
  sum(thisdf$weight_f[thisdf$age>=18])

returndf<-rbind.fill(
  returndf,
  data.frame(
    contact='genpop',
    category='blackXhsdropout',
    val=tmp
  )
)

#########################################################
#########################################################

setwd(filesdir)
write.csv(
  returndf,
  'themurdered.csv',
  row.names=F
)



