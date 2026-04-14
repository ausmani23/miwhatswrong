
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

#arrests_perofficer_base is defined inside thetradeoff_fwbalancecalcs.R
#(sourced below) using the same Chalfin 2020 row indices.

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

#crime-specific parameters for non-homicide crimes
#used to replace the proportional othercrime shorthand

#NCVS 2023 victimization volumes (from ICPSR_38962, weighted)
#police elasticities from Chalfin & McCary 2017 Table 3 col 9 (GMM)
#linear betas from Chalfin et al 2022 (midpoint ASG/COPS IVs)
#prison elasticities from Donohue 2009 synthesis of Table 9.1
#  murder responds less (-0.05); other crimes ~-0.15
#  (Roodman 2017 argues net effect may be zero at current margins)
#welfare weights: cost per crime from CM2017 Table 1,
#  converted to YoL via VSLY = $7M / 33.9 = $206,490

#homicide prison elasticity (used in calculate_homicides, kept separate)
homicide_prizElast <- -0.05

crime_params <- list(
  rape = list(
    ncvs_volume = 386246,
    polElast = -0.26,   #CM2017 GMM
    polBeta = -0.071,   #Chalfin et al 2022 midpoint
    prizElast = -0.15,  #Donohue 2009: violent crime, upper range
    costPerCrime = 140000,
    yolPerCrime = 140000 / (7000000 / 33.9)  #0.678
  ),
  robbery = list(
    ncvs_volume = 526181,
    polElast = -0.56,
    polBeta = -3.559,
    prizElast = -0.15,  #Donohue 2009
    costPerCrime = 12000,
    yolPerCrime = 12000 / (7000000 / 33.9)  #0.058
  ),
  aggravated_assault = list(
    ncvs_volume = 984997,
    polElast = -0.10,
    polBeta = -0.775,
    prizElast = -0.15,  #Donohue 2009
    costPerCrime = 40000,
    yolPerCrime = 40000 / (7000000 / 33.9)  #0.194
  ),
  burglary = list(
    ncvs_volume = 1578526,
    polElast = -0.23,
    polBeta = -4.452,
    prizElast = -0.15,  #Donohue 2009: property crime
    costPerCrime = 2000,
    yolPerCrime = 2000 / (7000000 / 33.9)  #0.0097
  ),
  theft = list(
    ncvs_volume = 10685819 + 104140, #household + personal larceny
    polElast = -0.08,
    polBeta = -6.484,
    prizElast = -0.15,  #Donohue 2009
    costPerCrime = 500,
    yolPerCrime = 500 / (7000000 / 33.9)  #0.0024
  ),
  motor_vehicle_theft = list(
    ncvs_volume = 803027,
    polElast = -0.34,
    polBeta = -5.180,
    prizElast = -0.15,  #Donohue 2009
    costPerCrime = 6000,
    yolPerCrime = 6000 / (7000000 / 33.9)  #0.029
  )
)

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
    myPrizChoice=c('standard','deflated'),
    myStateViolenceMultiplier=1.67, #Ang 2021: police killings have ~1/0.6 the effect of civilian killings
    myPolFunctionalForm=c('loglog','linear'), #Chalfin et al 2022: linear means constant absolute effect
    myLinearPolBeta=-0.1, #Chalfin et al 2022: homicides averted per officer hired
    myPoliceEffectiveness=1.0, #multiplier on all police crime-reduction elasticities (0=no effect, 1=best guess, 2=double)
    myPrisonEffectiveness=1.0 #multiplier on all prison crime-reduction elasticities (0=Roodman, 1=Donohue best guess)
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
  
  #Adding police affects the prison population via the arrests
  #they make. This is captured by `extrapriz_frompolice`, which is
  #derived in thetradeoff_fwbalancecalcs.R from Chalfin 2020 arrest
  #coefficients * BJS per-arrest prison-years. At current values
  #the number is NEGATIVE — a marginal officer reduces prison-years
  #on net, because Chalfin's negative effect on index-crime arrests
  #(more police -> fewer violent/property crimes -> fewer felony
  #arrests) dominates the small positive effect on QoL arrests.
  #
  #`extrapriz_frompolice` is calibrated around the crime environment
  #of Chalfin's sample. If the proposed policy also changes the
  #prison rate (or applies other crime-reducing interventions),
  #total crime shifts, and the marginal officer encounters more or
  #less arrestable volume than Chalfin assumes. We correct by
  #scaling the offset with `crime_ratio` = (crime under non-police
  #interventions) / (crime at baseline). We exclude the police
  #channel from the ratio because that feedback is already in
  #Chalfin's arrest coefficients.
  extrapriz_frompolice_thisestimate <- ifelse(
    myOrientation=='pessimistic_police_arrests',
    extrapriz_frompolice['pessimistic_police'],
    extrapriz_frompolice['bestguess']
  )

  #Crime ratio: prison channel only (police channel baked into
  #Chalfin). Uses the uncorrected prisonrate_proposed as a first-
  #order approximation; the residual Picard iteration is small
  #because extrapriz_frompolice is already small.
  prisonrate_baseline <- 10^5 * prisoners_2021 / pop_2021
  prison_ratio_uncorrected <- prisonrate_proposed / prisonrate_baseline
  if(prison_ratio_uncorrected <= 0) prison_ratio_uncorrected <- 0.001

  crime_baseline <- homicides_2021 + sum(sapply(crime_params, function(cp) cp$ncvs_volume))
  crime_no_police <- homicides_2021 * prison_ratio_uncorrected^homicide_prizElast
  for(cp in crime_params) {
    scaled_prizElast_ratio <- cp$prizElast * myPrisonEffectiveness
    crime_no_police <- crime_no_police +
      cp$ncvs_volume * prison_ratio_uncorrected^scaled_prizElast_ratio
  }
  crime_ratio <- crime_no_police / crime_baseline

  prisoners_added_effective <-
    prisoners_added +
    -1 * (police_added * extrapriz_frompolice_thisestimate * crime_ratio)
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
      (prisonlife_multiplier) *
      myStateViolenceMultiplier

  } else if(myUnits=='lives') {

    consequences[['arrests']] <-
      consequences_raw[['arrests']] *
      arrest_to_prisoneryear *
      (1/life_expectancy_prisoner) *
      (prisonlife_multiplier) *
      myStateViolenceMultiplier

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
      myElasticities=myElasticities,
      myPolFunctionalForm=myPolFunctionalForm,
      myLinearPolBeta=myLinearPolBeta,
      myPoliceEffectiveness=myPoliceEffectiveness,
      myPrisonEffectiveness=myPrisonEffectiveness
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
  #crime-specific calculation using NCVS volumes, CM2017 elasticities,
  #Chalfin et al 2022 linear betas, and CM2017 welfare weights

  consequences_raw[['othercrime']] <- 0
  consequences[['othercrime']] <- 0

  for(ctype in names(crime_params)) {
    cp <- crime_params[[ctype]]

    if(myPolFunctionalForm == 'linear') {
      #linear: change = beta * officers_added, scaled by police effectiveness
      crime_change_from_police <- cp$polBeta * myPoliceEffectiveness * police_added
    } else {
      #log-log: closed-form for police effect, elasticity scaled by multiplier
      police_ratio <- policerate_proposed / (10^5 * police_2021/pop_2021)
      if(police_ratio <= 0) police_ratio <- 0.001
      scaled_polElast <- cp$polElast * myPoliceEffectiveness
      crime_change_from_police <- cp$ncvs_volume * (police_ratio^scaled_polElast - 1)
    }

    #prison effect (log-log, crime-specific elasticity from Donohue, scaled by multiplier)
    prison_ratio <- prisonrate_proposed_effective / round(10^5 * prisoners_2021/pop_2021)
    if(prison_ratio <= 0) prison_ratio <- 0.001
    scaled_prizElast <- cp$prizElast * myPrisonEffectiveness
    crime_change_from_prison <- cp$ncvs_volume * (prison_ratio^scaled_prizElast - 1)

    raw_change <- crime_change_from_police + crime_change_from_prison
    consequences_raw[['othercrime']] <- consequences_raw[['othercrime']] + raw_change

    if(myUnits == 'yearsoflife') {
      consequences[['othercrime']] <- consequences[['othercrime']] + raw_change * cp$yolPerCrime
    } else {
      #in "lives" mode: convert to murder-equivalents
      consequences[['othercrime']] <- consequences[['othercrime']] +
        raw_change * cp$yolPerCrime / (life_expectancy_homvictim - mean_age_homvictim)
    }
  }
  
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
      ) * myStateViolenceMultiplier
  } else {
    consequences[['policekillings']] <-
      consequences_raw[['policekillings']] * myStateViolenceMultiplier
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