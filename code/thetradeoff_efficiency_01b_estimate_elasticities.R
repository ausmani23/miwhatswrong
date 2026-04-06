
#########################################################
#########################################################

#goal:

#estimate crime-specific effects of police in two specifications:
#  (a) levels (linear): crimes per officer hired
#      estimated from Chalfin et al. 2022 replication data
#      using their ASG and COPS IV strategies
#  (b) log-log: elasticity of crime rate wrt police rate
#      taken from Chalfin & McCary 2017 Table 3, column 9 (GMM)
#      (we cannot re-estimate log-log from these data because the
#       ASG measurement-error-correction IV does not work in logs:
#       classical errors become non-classical under log transform)

#output: files/crime_elasticities.csv

#########################################################
#########################################################

rm(list=ls())

require(haven)
require(data.table)
require(fixest) #for feols with IV and clustering
require(rprojroot)

homedir <- find_root(
  criterion=has_file('_miwhatswrong.Rproj')
)
datadir <- file.path(homedir, "data")
codedir <- file.path(homedir, "code")
filesdir <- file.path(homedir, "files")
outputdir <- file.path(homedir, "output")

#########################################################
#########################################################

#Chalfin & McCary 2017, Table 3, Column 9 (GMM, preferred)
#these are elasticities: % change in crime per % change in police
#estimated using growth-rate specification with measurement-error IV
cm2017 <- data.frame(
  crime = c('Murder', 'Rape', 'Robbery', 'Aggravated Assault',
            'Burglary', 'Larceny', 'Motor Vehicle Theft',
            'Violent Crime', 'Property Crime', 'Cost-Weighted Crime'),
  elast = c(-0.67, -0.26, -0.56, -0.10,
            -0.23, -0.08, -0.34,
            -0.34, -0.17, -0.47),
  se_elast = c(0.24, 0.22, 0.12, 0.13,
               0.09, 0.07, 0.10,
               0.10, 0.06, 0.17),
  stringsAsFactors=FALSE
)

cat('=== Chalfin & McCary 2017 GMM elasticities ===\n')
print(cm2017, row.names=FALSE)

#########################################################
#########################################################

#load Chalfin et al. 2022 replication data for linear estimates
df <- read_dta(file.path(datadir, '135761-V1/dataprocess/main_data_all.dta'))
df <- data.table(df)

#create lags (the do-file uses xtset xid year then l.variable)
setorder(df, xid, year)
df[, L_S := shift(S, 1), by=xid]
df[, L_cops_asg2 := shift(cops_asg2, 1), by=xid]
df[, L_copseligible_hiring := shift(copseligible_hiring, 1), by=xid]
df[, L_award_nonhiring := shift(award_nonhiring, 1), by=xid]
df[, L_apply_hiring := shift(apply_hiring, 1), by=xid]
df[, L_apply_nonhiring := shift(apply_nonhiring, 1), by=xid]

#create P2 if not already there (quadratic in population)
if(!'P2' %in% names(df)) {
  df[, P2 := population_census^2]
}

#convert FE variables to factors
df[, xid := as.factor(xid)]
df[, styr := as.factor(styr)]

#covariates (same as do-file)
covs <- c('population_census', 'P2', 'totalexpenditure', 'totalrevenue',
           'totaltaxes', 'Male_Pct', 'Age_0_14_Pct', 'Age_15_24_Pct',
           'Age_25_44_Pct', 'Female_HH_Pct', 'Never_Married_Pct',
           'Educ_Less_HS_Pct', 'Unemployment_Rate', 'Median_HH_Income',
           'Poverty', 'NH_Black_Pct', 'Hispanic_Pct')
cov_formula <- paste(covs, collapse=' + ')

#crime types to estimate
crimes <- data.frame(
  var = c('total', 'ucr_murder', 'ucr_rape', 'ucr_robbery',
          'ucr_aggassault', 'ucr_burglary', 'ucr_theft',
          'ucr_vehicletheft', 'ucr_total'),
  label = c('Homicides (SHR)', 'Murder (UCR)', 'Rape', 'Robbery',
            'Aggravated Assault', 'Burglary', 'Theft',
            'Motor Vehicle Theft', 'Total Index Crime'),
  stringsAsFactors=FALSE
)

#########################################################
#########################################################

#run the linear (levels) IV regressions
results <- list()

for(i in 1:nrow(crimes)) {

  cvar <- crimes$var[i]
  clab <- crimes$label[i]
  cat('\n=== Estimating:', clab, '===\n')

  #ASG IV, levels
  fml_asg <- as.formula(paste0(
    cvar, ' ~ ', cov_formula, ' | xid + styr | L_S ~ L_cops_asg2'
  ))
  m_asg <- feols(fml_asg, data=df, weights=~w, cluster=~xid)

  #COPS IV, levels (year >= 1990)
  fml_cops <- as.formula(paste0(
    cvar, ' ~ ', cov_formula,
    ' + L_award_nonhiring + L_apply_hiring + L_apply_nonhiring',
    ' | xid + styr | L_S ~ L_copseligible_hiring'
  ))
  m_cops <- feols(fml_cops,
                   data=df[year >= 1990],
                   weights=~w, cluster=~xid)

  #first stage F-stats
  f_asg <- fitstat(m_asg, 'ivwald')[[1]]$stat
  f_cops <- fitstat(m_cops, 'ivwald')[[1]]$stat

  results[[clab]] <- data.frame(
    crime = clab,
    var = cvar,
    beta_asg = coef(m_asg)['fit_L_S'],
    se_beta_asg = se(m_asg)['fit_L_S'],
    f_asg = f_asg,
    beta_cops = coef(m_cops)['fit_L_S'],
    se_beta_cops = se(m_cops)['fit_L_S'],
    f_cops = f_cops,
    n_asg = nobs(m_asg),
    n_cops = nobs(m_cops),
    row.names=NULL
  )

  cat('  ASG:', round(coef(m_asg)['fit_L_S'], 4),
      '(F=', round(f_asg, 1), ')',
      '  COPS:', round(coef(m_cops)['fit_L_S'], 4),
      '(F=', round(f_cops, 1), ')\n')
}

#########################################################
#########################################################

#combine and display
resultsdf <- do.call(rbind, results)
rownames(resultsdf) <- NULL

#add midpoint of ASG and COPS
resultsdf$beta_mid <- (resultsdf$beta_asg + resultsdf$beta_cops) / 2

cat('\n\n=============================================\n')
cat('LINEAR ESTIMATES (crimes per officer)\n')
cat('=============================================\n\n')
print(resultsdf[, c('crime', 'beta_asg', 'beta_cops', 'beta_mid')],
      digits=4, row.names=FALSE)

#########################################################
#########################################################

#combine linear and log-log into final output
#match crime types between our estimates and CM2017

#mapping from our labels to CM2017 labels
cm_map <- c(
  'Homicides (SHR)' = 'Murder',
  'Murder (UCR)' = 'Murder',
  'Rape' = 'Rape',
  'Robbery' = 'Robbery',
  'Aggravated Assault' = 'Aggravated Assault',
  'Burglary' = 'Burglary',
  'Theft' = 'Larceny',
  'Motor Vehicle Theft' = 'Motor Vehicle Theft',
  'Total Index Crime' = 'Cost-Weighted Crime'
)

resultsdf$cm2017_elast <- NA
resultsdf$cm2017_se <- NA
for(i in 1:nrow(resultsdf)) {
  cm_crime <- cm_map[resultsdf$crime[i]]
  j <- which(cm2017$crime == cm_crime)
  if(length(j) == 1) {
    resultsdf$cm2017_elast[i] <- cm2017$elast[j]
    resultsdf$cm2017_se[i] <- cm2017$se_elast[j]
  }
}

#########################################################
#########################################################

cat('\n\n=============================================\n')
cat('COMBINED ESTIMATES\n')
cat('=============================================\n\n')

cat('Linear beta = midpoint of ASG and COPS (crimes per officer)\n')
cat('Log-log elasticity = Chalfin & McCary 2017 GMM preferred\n\n')
print(resultsdf[, c('crime', 'beta_mid', 'cm2017_elast', 'cm2017_se')],
      digits=4, row.names=FALSE)

#save
setwd(filesdir)
write.csv(resultsdf, 'crime_elasticities.csv', row.names=FALSE)
cat('\nResults saved to files/crime_elasticities.csv\n')

#########################################################
#########################################################

#summary for use in calculate_costsbenefits.R
cat('\n\n=============================================\n')
cat('FOR USE IN calculate_costsbenefits.R:\n')
cat('=============================================\n\n')

cat('crime_params <- list(\n')
for(i in 1:nrow(resultsdf)) {
  r <- resultsdf[i,]
  nm <- gsub(' ', '_', gsub('[()]', '', tolower(r$crime)))
  elast_str <- ifelse(is.na(r$cm2017_elast), 'NA', sprintf('%.2f', r$cm2017_elast))
  cat(sprintf('  %s = list(beta = %.4f, elast = %s)',
              nm, r$beta_mid, elast_str))
  if(i < nrow(resultsdf)) cat(',')
  cat(sprintf('  # beta ASG: %.4f, COPS: %.4f\n',
              r$beta_asg, r$beta_cops))
}
cat(')\n')
