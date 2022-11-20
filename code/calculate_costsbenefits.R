
#########################################################
#########################################################

#goal: 

#give me an x,y point
#and use that x,y point to calculate costs/benefits

#what are costs/benefits? 

#crime (homicides + other crimes)
#costs of incarceration (prisoner-years reduced)
#costs of policing (arrests, killings)
#$ saved (if any; this can be a measure of SP optimism)

#########################################################
#########################################################

#load and clean whatever estimates will be helpful
setwd(datadir); dir()
require(readxl); dir();

#additional arrests per officer
tmpdf<-readxl::read_xlsx('chalfin2020.xlsx') 
arrests_perofficer_base <-c(
  sum(as.numeric(unlist(tmpdf[c(9:15,34:43,58:71),4]))),
  sum(as.numeric(unlist(tmpdf[c(24:30,45:54,73:86),4])))
)  

#mean age of homicide victims
tmpdf<-fread('SHR76_20.csv')
mean_age_homvictim <- mean(tmpdf$OffAge[tmpdf$OffAge!="999"])

#mean age of those killed by police
tmpdf<-fread('fatalencounters.csv') #from fatalencounters.org
mean_age_policevictim <- suppressWarnings(mean(as.numeric(tmpdf$Age),na.rm=T))
tmpdf$year<-str_extract(tmpdf$`Date of injury resulting in death (month/day/year)`,"[0-9]{4}$") %>%
  as.numeric
tmptab<-table(tmpdf$year)
policekillings_2019 <- tmptab[names(tmptab)==2019] 
policekillings_2020 <- tmptab[names(tmptab)==2020]
policekillings_2021 <- tmptab[names(tmptab)==2021]

#this runs some numbers on what the likely effects
#of hiring an additional police officer on prisoners is
setwd(codedir); source('thetradeoff_fwbalancecalcs.R')

#########################################################
#########################################################

#this function takes an x-y coordinate
#and returns costs+benefits, based on our assumptions
calculate_costsbenefits <- function(
    policerate_proposed,
    prisonrate_proposed,
    myUnits=c('yearsoflife','lives'), #whether years of life lost or lives lost
    myMethod=c('stepwise','direct'), #whether step or direct
    myOrientation=c(
      'bestguess',
      'pessimistic_police_crime', # you think police do not really reduce crime
      'pessimistic_police_arrests', #you think police will result in lots of arrests
      'pessimistic_arrest', #you think arrest is really bad
      'pessimistic_police_killings', #you think police killings will increase
      'optimistic_prison' #you think prison reduces crime
    ), 
    myElasticities=c('constant','changing'), #whether changing or constant
    myPrizChoice=c('standard','deflated')
) {
  
  # policerate_proposed = 370
  # prisonrate_proposed = 50
  # myMethod = 'stepwise'
  # myUnits = 'yearsoflife'
  # myOrientation = 'pessimistic_police'
  # myElasticities = 'constant'
  # myPrizChoice = 'standard'
  # i<-1
  # policerate_proposed = loopdf$policerate[i]
  # prisonrate_proposed = loopdf$prisonrate[i]
  # myMethod = loopdf$myMethod[i]
  # myUnits = loopdf$myUnits[i]
  # myOrientation = loopdf$myOrientation[i]
  # myElasticities = loopdf$myElasticities[i]
  # myPrizChoice = loopdf$myPrizChoice[i]
  
  #this is what we are proposing to do to prisoners, police
  prisoners_proposed <- (prisonrate_proposed * pop_2021)/10^5
  police_proposed <- (policerate_proposed * pop_2021)/10^5 
  prisoners_added <- (prisoners_proposed - prisoners_2021)
  prisoners_added_percent <- (prisoners_added + prisoners_2021)/prisoners_2021 - 1
  police_added <- (police_proposed - police_2021)
  police_added_percent <- (police_added + police_2021)/police_2021 - 1
  
  #now, we have to account for the fact that, if we are hiring more police officers,
  #our decline in prisoners is actually going to have to be effectively more aggressive
  #this is because every police officer added leads to a little extra incarceration
  #and so you have to 'effectively' increase incarceration by a bit more..
  
  #how many extra prisoners are effectively added depends on how many police officers we have hired
  extrapriz_frompolice_thisestimate <- ifelse(
    myOrientation=='pessimistic_police_arrests', #if pessimistic..
    extrapriz_frompolice['pessimistic_police'],
    extrapriz_frompolice['bestguess']
  )
  prisoners_added_effective <- 
    prisoners_added + #the original added
    -1 * (police_added * extrapriz_frompolice_thisestimate) #the extra decarceration necessary
  prisonrate_proposed_effective <- 
    round(10^5 * (prisoners_2021 + prisoners_added_effective)/pop_2021)
  prisoners_added_percent_effective <- 
    (prisoners_added_effective + prisoners_2021)/prisoners_2021 - 1
  
  #nb: this can result in us feeding a negative value for prisonrate_proposed
  #these are, in effect, impossible points to reach. so we skip the whole routine
  #and return empty if this is th case
  if(prisonrate_proposed_effective<0) {
    return( 
      list(
        finalguess = NA,
        moneysaved = NA,
        consequencesdf = NULL
      )
    )
  } 
  
  
  
  #initiate a list to store consequences
  consequences_raw <- list()
  consequences <- list()
  
  ###consequences of change in prisoners
  consequences_raw[['prisoners']] <- prisoners_added
  
  #how do we want to count prison, compared to life outside
  #our best guess is it is worth 25% of a life
  #this means that 1 - this value is our multiplier
  prisonlife_multiplier <- 1 - 0.25 
  #our argument ends up relying heavily on this, so it is helpful
  #to examine robustness of it specifically to this; decrease by order of mag
  prisonlife_multiplier <- ifelse(
    myPrizChoice=='deflated',
    prisonlife_multiplier/10,
    prisonlife_multiplier
  )
  
  if(myUnits=='yearsoflife') {
    
    consequences[['prisoners']] <- 
      prisoners_added * ( prisonlife_multiplier )
    
  } else if (myUnits=='lives') {
    
    #best guess is 65ish
    life_expectancy_prisoner <- 65 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4984780/
    consequences[['prisoners']] <- ( 
      prisoners_added / 
        life_expectancy_prisoner
    ) * (prisonlife_multiplier)
    
  }
  
  ###consequences of change in police
  consequences_raw[['police']] <- police_added
  
  #(1) police killings 
  #we require the homicide rate to calculate this, per our best guess
  #so calculation is postponed to below
  
  #(2) change in police arrests, serious and petty
  #(we are ignoring estimation uncertainty, 
  #where we have bounds, it is b/c of uncertainty across estimates)
  
  #we take estimates of the effect on arrests, from chalfin (read above)
  #they provide two kinds of estimates, from two different IV's
  #one estimate is smaller than the other
  #we take our best guess to be the midpoint of the two
  #and our pessimistic about police guess to be the max of the two
  arrests_perofficer <-
    ifelse(
      myOrientation%in%c('bestguess','pessimistic_police_arrests'),
      mean(arrests_perofficer_base),
      max(arrests_perofficer_base)
    )
  consequences_raw[['arrests']] <- 
    arrests_perofficer *
    police_added
  
  #what does an arrest mean in terms of lives lost?
  #bestguess: about as bad as a day in prison; pessimistic, as bad as a week
  arrest_to_prisoneryear <- ifelse(myOrientation=='pessimistic_arrest',1/52,1/365)
  
  if(myUnits=='yearsoflife') {
    
    consequences[['arrests']] <- 
      consequences_raw[['arrests']] * 
      arrest_to_prisoneryear *
      #(1/life_expectancy_prisoner) * 
      (prisonlife_multiplier)
    
  } else if(myUnits=='lives') {
    
    consequences[['arrests']] <- 
      consequences_raw[['arrests']] * 
      arrest_to_prisoneryear *
      (1/life_expectancy_prisoner) * 
      (prisonlife_multiplier)
    
  }
  
  
  #(3) change in homicide
  
  if(myMethod=='stepwise') {
    
    consequences_raw[['homicides']] <- calculate_homicides(
      priz0 = round(10^5 * prisoners_2021/pop_2021),
      prizf = prisonrate_proposed_effective,
      pol0 = 10^5 * police_2021/pop_2021,
      polf = policerate_proposed,
      y0 = homicides_2021,
      delta = 1,
      myOrientation=myOrientation,
      myElasticities=myElasticities
    ) - homicides_2021
    
  } else if(myMethod=='direct') {
    
    consequences_raw[['homicides']] <-
      homicides_2021 * (police_added_percent * getPolElast(myOrientation=myOrientation)) +
      homicides_2021 * (prisoners_added_percent_effective * getPrizElast(myOrientation=myOrientation))
    
  } 
  
  if(myUnits=='yearsoflife') {
    
    life_expectancy_homvictim <- 65 #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4984780/
    consequences[['homicides']] <-
      consequences_raw[['homicides']] * 
      (life_expectancy_homvictim - mean_age_homvictim)
    
  } else {
    
    consequences[['homicides']] <-
      consequences_raw[['homicides']]
    
  }
  
  #(3b) change in other crime
  
  #(we'd need relevant elasticities for prisons and police)
  #(and some estimate of how they change, if they change)
  #robbery
  #assault
  #rape
  #and then we'd need to convert these to 'years of life'
  #for now, as shorthand, we take our cues from Chalfin and McCary 2017
  #and assume that homicide is ~70% of the costs of crime
  consequences_raw[['othercrime']] <-
    consequences_raw[['homicides']] * (995-693)/995
  consequences[['othercrime']] <- 
    consequences[['homicides']] * (995-693)/995 
  
  #now we have a homicide rate, we can calculate police kilings
  if(myOrientation!='pessimistic_police_killings') {
    
    polkdf$y<-log(0.1 + polkdf$polkillings_percapita)
    polkdf$x<-log(polkdf$polhomratio)
    m.polk <- lm(
      data=polkdf,
      formula = y ~ x
    ) 
    homicides_cfactual <- homicides_2021 + consequences_raw[['homicides']]
    #if prediction is <0 homicides, needs adjusting.. 
    if(homicides_cfactual<=0)
      homicides_cfactual<-0.1
    #homrate_cfactual <- 10^5 * homicides_cfactual/pop_2021
    police_cfactual <- police_2021 + police_added
    # #ditto for police, but this just makes it strange
    # if(police_cfactual<=0) {
    #   police_cfactual<-0.1
    # }
    predictdf<-data.frame(
      x=log(police_cfactual/homicides_cfactual)
    )
    tmp<-which(polkdf$countryname=='USA')
    usa_residual <- m.polk$residuals[tmp]
    policekillings_cfactual <- 
      exp(predict(m.polk,newdata=predictdf) + usa_residual) * pop_2021/10^6
    consequences_raw[['policekillings']] <- 
      unname(policekillings_cfactual) - unname(policekillings_2021)
    
    
  } else if (myOrientation=='pessimistic_police_killings') {
    
    consequences_raw[['policekillings']] <- 
      (police_added_percent * policekillings_2021) 
    
  }
  
  
  if(myUnits=='yearsoflife') {
    
    #assume life expectancy is about 65
    life_expectancy_policevictim <- 65
    consequences[['policekillings']] <- 
      consequences_raw[['policekillings']] * (
        life_expectancy_policevictim - mean_age_policevictim
      )
  } else {
    consequences[['policekillings']] <- consequences_raw[['policekillings']]
  }
  
  
  #(4) change in resources, if any
  consequences_raw[['resources']] <- round(
    (prisoners_added * cost_prisoner + 
       police_added * cost_policeofficer) / 10^9
  )
  
  
  #consequences
  finalguess <- sum(unlist(consequences) %>% unname)
  consequencesdf<- rbind.fill(
    data.frame(
      name=c(names(consequences),'sum'),
      finalguess=c(unlist(consequences) %>% unname,finalguess)
    ),
    data.frame(
      name=paste0(names(consequences_raw),"_raw"),
      finalguess=unlist(consequences_raw) %>% unname
    )
  )
  #return this
  list(
    finalguess = finalguess,
    moneysaved = consequences_raw[['resources']],
    consequencesdf = consequencesdf
  )
  
}