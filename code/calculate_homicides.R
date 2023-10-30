
#########################################################
#########################################################

#suppose EU has 100 prisoners
#and EU has 320 police officers
#and suppose we have $130000

#this will get us 4 prisoners
#and 1 police officer
x<-320
pctchange_police <- (100 * (x+1)/x) - 100
pctchange_priz <- (100 * (x/3.2 + 4.2)/(x/3.2)) - 100
#an increase of 4 prisoners is an increase that is roughly 13 times, 
#in percentage terms, an increase of 1 police officer
pctchange_priz/pctchange_police

#what this means: 
#the assumption that prisons and police are, dollar for dollar, 
#equally efficient in the world of the first-world balance
#is a reason to believe that the elasticity of crime wrt police
#is roughly -0.67, if we take -0.05 to be prison elasticity there
-0.05 * pctchange_priz/pctchange_police # = -0.67

#(note that if we take it to be anything larger, it implies increaseing elasticities
#increasing elasticities is in fact compatible w/ chalfin finding that 
#effect of prison does not diminish with increase in policing. 
#but it is biased in our favor, so we do not make this move)

#this is roughly the same as the estimated elasticity in the Chalfin
#this is a reason to believe in constant elasticities, in other worsd

#where do these constant elasticities come from come from?
#homicide
#chalfin and mccary 2017, p. 180 and 182
#-0.47 is the cost-weighted elasticity, brought downward b/c
#police are less effective at non-murder reduction
#-0.67 is the elasticity wrt murder

#-0.05 to -0.15 is the the range reported in Donohue 2009, pp. 282-283
#0 would be what we would take from the Roodman, 
#but it is not really our best guess, just the most optimistic reading

#that said, we also here give an option to use changing elasticities
#we anchor these in what we observe, in the EU/US, plus above observations

#(note that changing elasticites lead to a situation in which 
#prisons are in fact *more* efficient than police at the European blaance. 
#if we are right about how these elasticiteis change, they shoudl be using
#prisons more than they are, and police less. but maybe, there, prisons cost more
#than they do here, so this might be also compatible w/ our observation

#nonetheless, all this is a reason not to make changing elastiices are default view.. )

prizhomratio_usa <- 143.6925
prizhomratio_median <- 112.6971
prizhomratio_leader <- 60.41227
polhomratio_usa <- 44.02872
polhomratio_median <- 319.3811
polhomratio_leader <- 522.2526

getPrizElast<-function(
  prizhomratio=700/6,
  myElasticities='constant',
  myOrientation='bestguess'
) {
  
  # priz<-11
  # hom<-6
  
  if(myElasticities=='changing') {
    
    if(prizhomratio<prizhomratio_leader) {
      y <- -0.15
    } else if (prizhomratio>=prizhomratio_leader & prizhomratio<prizhomratio_median) {
      #line from -0.05 to -0.15, from these two points
      #y = mx + b
      m <- ( -0.15 - (-0.05) )/ (prizhomratio_leader - prizhomratio_median)
      b <- -0.15 - m * prizhomratio_leader
      y <- m * (prizhomratio) + b 
      
    } else if (prizhomratio>=prizhomratio_median & prizhomratio<prizhomratio_usa ) {
      
      #line from -0.05 to 0, from these two points
      m <- (-0.05 - 0) / ( prizhomratio_median - prizhomratio_usa)
      b <- -0.05 - m * prizhomratio_median
      y <- m * (prizhomratio) + b
      
    } else if (prizhomratio>=prizhomratio_usa) {
      y <- 0
    }
  } else {
    #pessimistic about prison means biased against our conclusions, prison
    y <- ifelse(myOrientation=='optimistic_prison',-0.15,-0.05)
  }
  
  y
  
}

# this is what it returns 
# is<-0:200
# es<-sapply(is,getPrizElast,myElasticities='changing')
# qplot(is,es,geom='line')

#deflated
olddir<-getwd()
setwd(datadir); tmpdf<-fread('chalfin2017.csv')
metaElasticity <- weighted.mean(
  tmpdf$mu,
  w=1/tmpdf$se^2,
  na.rm=T
)
setwd(olddir)

getPolElast<-function(
  polhomratio=212/6,
  myElasticities='constant',
  myOrientation='bestguess'
) {
  
  #DEPRECATED;
  #we don't have good reasons to make
  #elasticity of homicide with respect to police decline
  #(Chalfin et al suggests it is increasing!)
  #if(myElasticities=='changing') {
  #   if(polhomratio<polhomratio_usa) {
  #     y <- -0.67
  #   } else if (polhomratio>=polhomratio_usa & polhomratio<polhomratio_median) {
  #     #y = mx + b
  #     m <- ( -0.67 - (0.75 * -0.67) )/ (polhomratio_usa - polhomratio_median)
  #     b <- -0.67 - m * polhomratio_usa
  #     y <- m * (polhomratio) + b 
  #     
  #   } else if (polhomratio>=polhomratio_median) {
  #     y <- -0.75 * -0.67
  #   }
  # } else {
  # }
  #chalfin and mccary 2017 report 9 estimates
  #their preferred estimate is among the lowest
  #we have the option of (a) trusting them; (b) using meta-analysis
  #if (a), elasticity is -0.67
  #if (b): elasticity is -0.36 (see above)
  #(b) leans very heavily on an old, OLS-based study by Marvell and Moody
  #so we prefer (a) to (b)
  #our pessimsitic estimate will be the bottom of the Chalfin CI
  y <- ifelse(myOrientation=='pessimistic_police_crime',-0.67 + 2*0.24,-0.67)
  y
}

# # this is what it returns 
# is<-0:500
# es<-sapply(is,getPolElast,myElasticities='changing')
# qplot(is,es,geom='line')


#########################################################
#########################################################

#what will this function do? 

#we start at a given point
#pol0,priz0 (i.e., where in the 2-dimensional space)

#at this point we have two elasticities
#one for imprisonment, one for policing

#we begin by cutting one, using it to fund the other
#and we do this by an amount given by delta

#at each stage when we do this, 
#the point we are at plus the current level of y
#gives us the slope of the line along which we move

#and we then move an interval-sized distance towards a new point
#and at that new point, we calculate a new pair of elasticities/slopes

#to do this, default method is to use priz/hom and pol/hom to give us elasticities
#rn we assume that they are basically constant across the space

#but we can also by direct about this and specify starting and ending elasticities
#when doing this, i simply assume that elasticities move towards ef linearly
#this gives enough information to calculate what we need.

#we move, repetitively, towards our final destination in this way

calculate_homicides <- function(
  #necessary inputs
  priz0, #starting incarceration rate
  prizf, #end incarceration rate
  pol0, #starting police rate
  polf, #end police rate
  y0, #starting outcome
  delta, #size of interval for incrate change
  myOrientation=c('bestguess','pessimistic'),
  myElasticities=c('constant','changing'),
  #OPTIONAL PARAMETERS: if we want to specify elasticity
  #rather than let them be calculated from pris/hom and pol/hom
  epriz0=NULL, #elasticity of incarceration at start
  eprizf=NULL, #elasticity of incarceration at end
  epol0=NULL, #elasticity of police at start
  epolf=NULL #elasticity of police at end
) {
  
  
  priz0 = round(10^5 * prisoners_2019/pop_2019)
  prizf = prisonrate_proposed_effective
  pol0 = 10^5 * (police_2019)/pop_2019
  polf = 10^5 * (police_2019+500000)/pop_2019
  y0 = homicides_2019
  delta = 1
  myOrientation='bestguess'
  myElasticities='constant'
  
  # priz0<-700
  # prizf<-211
  # pol0<-212
  # polf<-250
  # y0<-20000
  # delta<-1
  epriz0<-eprizf<-epol0<-epolf<-NULL
  # myOrientation='bestguess'
  
  if(is.null(eprizf) & is.null(epriz0))  {
    epriz0 <- getPrizElast(priz0,y0)
    epol0 <- getPolElast(pol0,y0)
  }
  
    #starting points for the change
  #i.e., for a one-unit change in prison/police rate, 
  #what will happen (given the assumed elasticities)
  #to the number of homicides, assuming we're starting at y0
  spriz0 = y0/priz0 * epriz0 #simplifies from: y0 * (((priz0 + 1)/priz0) - 1) * epriz0
  spol0 = y0/pol0 * epol0  #ditto..
  
  #set up the ticks and the loop
  chg_pol = polf - pol0 #this is the change in the police rate
  chg_priz = prizf - priz0 #this is the change in the prison rate
  
  #the way we will calculate the change in homicides
  #is by slowly adjusting the number of prisoners and police
  #until we reach the final number
  #some proposals require changing one a large amount and the other very little
  #in these, we are basically just changing one; so this if-else loop
  #helps detect which one this is, and makes that one the centerpriece
  #of the counterfactual reform
  
  if(chg_pol==0 & chg_priz ==0) {
    
    #in case the proposed destination
    #is the same as the starting point
    return(y0)
    
    #if the change in police is larger than the change in prisoners
  } else if ( abs(chg_pol)>=abs(chg_priz) ) {
    
    tmpseq.i <- 1:(abs((polf - pol0)/delta))
    
    #if the change in police is larger...
  } else if (abs(chg_pol)<abs(chg_priz)) {
    
    tmpseq.i <- 1:(abs((prizf - priz0)/delta))
  }
  
  #this will give us the rate at which
  #we chantge prisoners and police
  delta_pol <- (polf - pol0)/max(tmpseq.i)
  delta_priz <- (prizf - priz0)/max(tmpseq.i)
  
  #if specifying elasticities..
  #input parameters give us the rate of change of elasticity_priz
  if(!is.null(eprizf) & !is.null(epriz0))  {
    epriz_delta <- (eprizf - epriz0) * delta_priz/(prizf - priz0)
    epol_delta <- (epolf - epol0) / max(tmpseq.i) 
  }
  
  #initiate loop
  prizs <- rep(priz0,max(tmpseq.i) + 1)
  pols <- rep(pol0,max(tmpseq.i) + 1)
  ys <- rep(y0,max(tmpseq.i) + 1)
  sprizs <- rep(spriz0,max(tmpseq.i) + 1)
  spols <- rep(spol0,max(tmpseq.i) + 1)
  eprizs <- rep(epriz0,max(tmpseq.i) + 1)
  epols <- rep(epol0,max(tmpseq.i) + 1)
  
  
  for(i in (1+tmpseq.i)) {
    
    #i<-2
    
    #next level of policerate, incrate
    prizs[i] <- prizs[i-1] + delta_priz
    pols[i] <- pols[i-1] + delta_pol
    
    #next level of the outcome
    #here we assume impact is simultaneous
    y_change_priz <- sprizs[i-1] * delta_priz
    y_change_pol <- spols[i-1] * delta_pol
    ys[i] <- ys[i-1] + y_change_priz + y_change_pol
    
    #now we adjust the elasticities
    #based on our assumptions about these elasticities
    #i.e., whether they are constant or changing
    eprizs[i] <- getPrizElast(
      prizs[i-1]/ys[i-1],
      myElasticities=myElasticities,
      myOrientation = myOrientation
    )
    epols[i] <- getPolElast(
      pols[i-1]/ys[i-1],
      myElasticities=myElasticities,
      myOrientation = myOrientation
    )
    
    #this uses a delta method
    #but I don't particularly like this
    #since we want to be consistent over this whole area
    # eprizs[i] <- eprizs[i-1] + epriz_delta
    # epols[i] <- epols[i-1] + epol_delta
    
    #and then, based on the new level of homicides
    #we recalculate what the change in homicides will be
    #for a one-unit change in the rate of police/prison
    sprizs[i] <- ys[i]/prizs[i] * eprizs[i]
    spols[i] <- ys[i]/pols[i] * epols[i]
    
  }
  
  #ys[1+max(tmpseq.i)]
  return(ys[1+max(tmpseq.i)])
  
}





























