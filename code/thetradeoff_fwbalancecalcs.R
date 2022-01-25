#one concern is that increasing arrests will result in increasing incarcerations
#we want to know how big of an issue that is, when thinking about the first-world balance

#one reason to think that it is not such a big issue is to note that the 
#increase in arrests is concentrated in qol/drug-type arretss

#so what we want to calculate here, following Chalfin and McCary 2017:
#what is: 
#E(prisonyears|drugarrest) 
#E(prisonyears|indexarrest)

#we expect to see many more drug-type arrests
#and slightly fewer indexarrests

#is the sum of these two things going to change the game? 

###########################

#we take these numbers from 
#https://ucr.fbi.gov/crime-in-the-u.s/2006
drugarrests_2006 <- 1889810
violentarrests_2006 <- 611523
propertyarrests_2006 <- 1540297

#https://bjs.ojp.gov/content/pub/pdf/fssc06st.pdf incarcerations
#Table 1.6
drugconvictions_state_2006 <- 377860
drugconvictions_fed_2006 <- 27361
pctincarcerated_state_2006 <- 0.65
pctincarcerated_fed_2006 <- 0.93
meansentence_state_2006 <- 31/12
meansentence_fed_2006 <- 87/12
prisonyears_drug_2006 <- 
  drugconvictions_state_2006 * pctincarcerated_state_2006 * meansentence_state_2006 +
  drugconvictions_fed_2006 * pctincarcerated_fed_2006 * meansentence_fed_2006 

#nb:
#Chalfin and McCary note, on pg. 183, that for index crimes,
#people only serve about 47.5% of their sentence
servepercent <- 0.475

#so, each drug conviction yields prison-years in sentences
Eprisonyears_perdrugconviction <-servepercent * prisonyears_drug_2006/drugarrests_2006

#now, let's do the same for index crimes
#Table 1.6
convictions_state_violent_2006 <- 206140
convictions_fed_violent_2006 <- 2451
convictions_state_property_2006 <- 321570
convictions_fed_property_2006 <- 10922
pctincarcerated_state_violent_2006 <- 0.77
pctincarcerated_fed_violent_2006 <- 0.94
pctincarcerated_state_property_2006 <- 0.67
pctincarcerated_fed_property_2006 <- 0.59
meansentence_state_violent_2006 <- 71/12
meansentence_fed_violent_2006 <- 108/12
meansentence_state_property_2006 <- 30/12
meansentence_fed_property_2006 <- 29/12

#for violence
prisonyears_violent_2006 <- 
  convictions_state_violent_2006 * pctincarcerated_state_violent_2006 * meansentence_state_violent_2006 +
  convictions_fed_violent_2006 * pctincarcerated_fed_violent_2006 * meansentence_fed_violent_2006 
Eprisonyears_perviolentconviction <- servepercent * prisonyears_violent_2006/violentarrests_2006

#for property
prisonyears_property_2006 <- 
  convictions_state_property_2006 * pctincarcerated_state_property_2006 * meansentence_state_property_2006 +
  convictions_fed_property_2006 * pctincarcerated_fed_property_2006 * meansentence_fed_property_2006 
Eprisonyears_perpropertyconviction <- servepercent * prisonyears_property_2006/propertyarrests_2006

###summary

#in sum: 
#we remove 4.2 prison-years and get an additional police officer
#in the worst-case scenario, that police officer gives us 
#about 6 new drug arrests (also 14 new liquor arrests)
#liquor is much less likely to yield convictgion/incarceration
#so let's just say there are about 8 new drug arrests, rounding up
#and 1 fewer index arrest.. (this is worst case scenario)

#how many of these additional arrests turn into prisoner-years
#Table 1.6, again
drugconvictions_perarrest <- 405221/drugarrests_2006
Eprisonyears_perdrugarrest <- drugconvictions_perarrest * Eprisonyears_perdrugconviction

violentconvictions_perarrest <- 208591/violentarrests_2006
Eprisonyears_perviolentarrest <- violentconvictions_perarrest * Eprisonyears_perviolentconviction

propertyconvictions_perarrest <- 322492/propertyarrests_2006
Eprisonyears_perpropertyarrest <- propertyconvictions_perarrest * Eprisonyears_perpropertyconviction

#tables 11-13 in chalfin et al 2020 let us anticipate the effect

#we need a 'pessimistic about policing' version
#and a best guess version.

#our best guess is midpoint of the two 
#our pessimistic guess is max
olddir<-getwd()
setwd(datadir)
tmpdf<-readxl::read_xlsx('chalfin2020.xlsx') 
arrests_perofficer_violent <-c(
  sum(as.numeric(unlist(tmpdf[c(9:12),4]))),
  sum(as.numeric(unlist(tmpdf[c(24:27),4])))
)  
arrests_perofficer_property <-c(
  sum(as.numeric(unlist(tmpdf[c(13:15),4]))),
  sum(as.numeric(unlist(tmpdf[c(28:30),4])))
) 
arrests_perofficer_drug <-c(
  sum(as.numeric(unlist(tmpdf[c(42),4]))),
  sum(as.numeric(unlist(tmpdf[c(53),4])))
) 
arrests_perofficer_liquor <-c(
  sum(as.numeric(unlist(tmpdf[c(41),4]))),
  sum(as.numeric(unlist(tmpdf[c(52),4])))
) 
setwd(olddir)

extrapriz_frompolice <- 
  arrests_perofficer_violent * Eprisonyears_perviolentarrest + 
  arrests_perofficer_property * Eprisonyears_perpropertyarrest +
  arrests_perofficer_drug * Eprisonyears_perdrugarrest +
  #we assume, w/o any other data, that liquor arrests 
  #are about 1/2 as likely to lead to an arrest as drug arrest
  #this seems like a reasonable best guess
  arrests_perofficer_liquor * Eprisonyears_perdrugarrest/2

#best estimate is mean of the two
extrapriz_frompolice[1] <- mean(extrapriz_frompolice)
names(extrapriz_frompolice)<-c('bestguess','pessimistic_police')

#thus, our best estimate is that adding a single police officer, 
#with the kinds of effects shown in Chalfin et al 2021 (in general),
#is to result in an additional 0.29 prisoner-years

#but if someone is pessimistic about policing, 
#then they will expect more policing to result in even more prisoners

#the ratio of police price to prisoner price is about 130/33 (see below)
#what this in effect means is that we have to be extra-aggressive
#when cutting the incarceration rate. we are not just releasing 
#this number of prisoners, but more

#this calculation will have to figure in our elasticity calculations
#for the effect of prisoner release on homicide, but not in our calculations
#of the effect of prisoner release on decline in prison-years

n<-100
pol<-rep(212,n)
priz<-rep(700,n)
money<-rep(0,n)
cost_pol <- 130000
cost_priz <- 33089
nprisoners<-cost_pol/cost_priz + extrapriz_frompolice
((cost_pol/cost_priz)+extrapriz_frompolice)/(cost_pol/cost_priz)
#the release is going to have to be about 10% more aggressive
#in fact than it appears to be, on paper.. 
nprisoners<-cost_pol/cost_priz + extrapriz_frompolice


# for(i in 1:n) {
#   
#   #i<-1
#   
#   #equivalently
#   #release 5 prisoners
#   
#   priz[i+1]<-priz[i] - nprisoners
#   money[i+1]<-money[i] + cost_priz * nprisoners
#   #add 1 police officer
#   pol[i+1]<-pol[i]+1
#   money[i+1]<-money[i+1] - cost_pol*1
#   #and this adds extra prisoner
#   priz[i+1]<-priz[i+1]+ extrapriz_frompolice
#   money[i+1]<-money[i+1] - cost_priz*extrapriz_frompolice
#   
# }
# 
# data.frame(
#   pol,
#   priz,
#   round(money)
# )




