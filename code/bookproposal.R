#########################################################
#########################################################
##
## bookproposal.R
##
## Reproduces every figure in the book proposal for
## *What's Wrong with Mass Incarceration?* (Lewis and Usmani).
##
## Data:
##  - World History of Punishment Database, vintage 261020
##    (built 2026-10-20), as harmonized by the replication
##    pipeline of *From Plantation to Prison* (stage 0,
##    00_adjustments.R -> files/p2pdf.RDS). The database and
##    that pipeline are not yet public; this script runs once
##    they are.
##  - IPUMS USA (American Community Survey), as tabulated by
##    the same pipeline (05_inequality_09class_01ipums_process.R
##    -> output/04_inequality_class_ipums_byEdRaceSex.csv).
##  - OECD Income Distribution Database, downloaded below.
##
## Conventions (same as From Plantation to Prison): big
## countries only (population > 3 million), maindata rows
## only, median across sources within a country-year, rates
## per 100,000. "Rich democracies" = the book's Rich
## comparison set; Britain enters as England and Wales,
## Scotland and Northern Ireland.
##
## Run from the repository root:
##   Rscript code/bookproposal.R
## Set P2P_DIR if the From Plantation to Prison repository is
## not at ../p2p.
##
#########################################################
#########################################################

rm(list=ls())
require(data.table)

p2pdir <- Sys.getenv('P2P_DIR', unset=file.path('..','p2p'))
p2pdf_file <- file.path(p2pdir,'files','p2pdf.RDS')
ipums_file <- file.path(p2pdir,'output','04_inequality_class_ipums_byEdRaceSex.csv')
stopifnot(file.exists(p2pdf_file), file.exists(ipums_file))

#everything we report is collected here
out <- list()
report <- function(claim, value, note='') {
  out[[length(out)+1]] <<- data.table(claim=claim, value=value, note=note)
  cat(sprintf('%-62s %12s  %s\n', claim, format(value, big.mark=',', digits=3), note))
}

#########################################################
#########################################################

#LOAD AND HARMONIZE

fulldf <- readRDS(p2pdf_file)
setDT(fulldf)

#drop the UK-wide aggregates, so Britain is not double-counted
fulldf <- fulldf[!countryname %in% c('United Kingdom (composite)','United Kingdom')]

fulldf <- fulldf[bigcountries==T & maindata==T]

#the pipeline also carries a copy of the US in the poor comparison set; drop it
fulldf <- fulldf[!grepl('Fake', cownum)]

#one value per country-year-statistic: the median across sources
cy <- fulldf[
  statistic %in% c(
    'prisoners','police','homicides','population','policekillings',
    'govspending','socialspending','policespending','prisonspending','courtspending'
  ),
  .(value=median(value)),
  by=c('countryname','countrycompset','usa','year','fiveyear','statistic')
]
cy <- dcast(cy, countryname + countrycompset + usa + year + fiveyear ~ statistic, value.var='value')

#rates per 100,000
cy[, prisoners_rate := 10^5 * prisoners/population]
cy[, police_rate := 10^5 * police/population]
cy[, homicides_rate := 10^5 * homicides/population]
cy[, policekillings_rate := 10^5 * policekillings/population]

rich <- cy[countrycompset=='Rich']

#a country's value over a window of years: the mean of its annual values
over <- function(df, var, years) {
  df[year %in% years & !is.na(get(var)), .(v=mean(get(var))), by=c('countryname','usa')]
}

recent <- 2015:2024 #window for today's cross-national comparisons

#########################################################
#########################################################

#1. THE SCALE OF AMERICAN INCARCERATION

cat('\n--- Scale of incarceration ---\n')

us <- cy[usa==T & !is.na(prisoners)][order(year)]
report('US prisoners and jail inmates, latest year', round(tail(us$prisoners,1)),
       paste('year', tail(us$year,1)))

#US vs each rich democracy, recent window
tmp <- over(rich, 'prisoners_rate', recent)
usrate <- tmp[usa==T, v]
others <- tmp[usa==F][, ratio := usrate/v][order(ratio)]
report('US incarceration rate per 100k, 2015-24', round(usrate))
report('US rate / median rich democracy', round(usrate/median(others$v),1))
report('US rate / rich democracy, lowest multiple', round(min(others$ratio),1), others[1, countryname])
report('US rate / rich democracy, highest multiple', round(max(others$ratio),1), others[.N, countryname])
report('Rich democracies with US multiple between 5 and 10', others[ratio>=5 & ratio<=10, .N],
       paste('of', nrow(others)))

#US today vs fifty years earlier (five-year means)
inc_7074 <- over(rich[usa==T], 'prisoners_rate', 1970:1974)$v
inc_last <- us[year >= max(year)-4, mean(prisoners_rate)]
report('US incarceration rate, 1970-74', round(inc_7074))
report('US incarceration rate, latest five years', round(inc_last),
       paste0(max(us$year)-4,'-',max(us$year)))
report('Today / fifty years earlier', round(inc_last/inc_7074,1))

#########################################################
#########################################################

#2. CLASS AND RACE (IPUMS)

cat('\n--- Class and race (IPUMS ACS) ---\n')

ip <- fread(ipums_file)
ip <- ip[sex=='Male' & age_band=='18-50' & statistic %in% c('institutionalized','population')]
cellrate <- function(r, e, yrs) {
  x <- ip[race==r & ed4==e & year %in% yrs, .(v=sum(value)), by=statistic]
  x[statistic=='institutionalized', v]/x[statistic=='population', v]
}
for (yrs in list(2007, 2021:2024)) {
  lab <- paste(unique(range(yrs)), collapse='-')
  b <- cellrate('black','lt_hs',yrs); w <- cellrate('white','ba_plus',yrs)
  report(paste('Black dropouts / white BA+ (men 18-50),', lab), round(b/w),
         sprintf('%.1f%% vs %.3f%% institutionalized', 100*b, 100*w))
}

#########################################################
#########################################################

#3. CRIME AND PUNISHMENT BEFORE THE RISE

cat('\n--- Crime and punishment before the rise ---\n')

for (yrs in list(1965:1969, 1970:1974)) {
  lab <- paste(range(yrs), collapse='-')
  h <- over(rich, 'homicides_rate', yrs)
  report(paste('US homicide rate / mean of rich democracies,', lab),
         round(h[usa==T, v]/h[usa==F, mean(v)],1), paste(h[usa==F, .N], 'countries'))
  ph <- rich[year %in% yrs & !is.na(prisoners) & !is.na(homicides),
             .(v=mean(prisoners/homicides)), by=c('countryname','usa')]
  if (!any(ph$usa)) next #no US prisoner count in this window (census years only)
  report(paste('Prisoners per homicide, rich mean / US,', lab),
         round(ph[usa==F, mean(v)]/ph[usa==T, v],1),
         sprintf('US %.0f vs %.0f (%d countries)', ph[usa==T, v], ph[usa==F, mean(v)], ph[usa==F, .N]))
}

#########################################################
#########################################################

#4. DRUG OFFENDERS (UNODC prisoners by offense)

#Share of sentenced prisoners held for drug offenses. Numerator: UNODC's
#'drug' total where reported, otherwise trafficking plus possession; the
#denominator is UNODC's count of sentenced prisoners. Median across UNODC
#releases within a country-year (these rows are not maindata).

cat('\n--- Drug offenders ---\n')

drugdf <- readRDS(p2pdf_file)
setDT(drugdf)
drugdf <- drugdf[
  bigcountries==T & countrycompset=='Rich' & !grepl('Fake', cownum) &
    !countryname %in% c('United Kingdom (composite)','United Kingdom') &
    grepl('unodc', source) & statistic %in% c('prisoners','prisoners_sentenced') &
    is.na(gender) & is.na(age) & is.na(sentence) & year %in% recent
]
num <- drugdf[crime %in% c('drug','drugtraffic','drugpossession'),
              .(v=median(value)), by=c('countryname','usa','year','crime')]
num <- dcast(num, countryname + usa + year ~ crime, value.var='v')
for (v in c('drug','drugtraffic','drugpossession')) if (!v %in% names(num)) num[, (v) := NA_real_]
num[, drugs := fifelse(!is.na(drug), drug, rowSums(cbind(drugtraffic, drugpossession), na.rm=T))]
den <- drugdf[statistic=='prisoners_sentenced' & is.na(crime),
              .(sentenced=median(value)), by=c('countryname','year')]
drugs <- merge(num, den, by=c('countryname','year'))[, .(v=mean(100*drugs/sentenced)), by=c('countryname','usa')]
rm(drugdf)

report('Drug offenders, % of sentenced prisoners, US (2015-24)', round(drugs[usa==T, v],1))
report('Drug offenders, % of sentenced prisoners, median rich', round(drugs[usa==F, median(v)],1),
       sprintf('%d countries; 20th-80th pct %.0f-%.0f%%', drugs[usa==F, .N],
               drugs[usa==F, quantile(v,.2)], drugs[usa==F, quantile(v,.8)]))

#########################################################
#########################################################

#5. SPENDING

cat('\n--- Penal and social spending ---\n')

sp <- cy[usa==T & !is.na(policespending) & !is.na(prisonspending) & !is.na(courtspending) &
           !is.na(govspending) & !is.na(socialspending)][order(year)]
spy <- tail(sp, 1)
penal <- spy[, policespending + prisonspending + courtspending]
report('US penal spending, $ billion', round(penal/10^9), paste('year', spy$year))
report('US social spending, $ billion', round(spy$socialspending/10^9))
report('Penal spending, % of US government spending', round(100*penal/spy$govspending,1))
report('Penal spending as % of social spending', round(100*penal/spy$socialspending,1))

ratio <- rich[year %in% recent & !is.na(socialspending) & !is.na(policespending) & !is.na(prisonspending),
              .(v=mean(socialspending/(policespending + prisonspending + ifelse(is.na(courtspending),0,courtspending)))),
              by=c('countryname','usa')]
report('Social / penal spending, US', round(ratio[usa==T, v],1))
report('Social / penal spending, median rich democracy', round(ratio[usa==F, median(v)],1),
       paste(ratio[usa==F, .N], 'countries'))

#########################################################
#########################################################

#6. PRISONERS AND POLICE

cat('\n--- Prisoners and police ---\n')

pp <- cy[year %in% recent & !is.na(prisoners) & !is.na(police),
         .(v=mean(prisoners/police)), by=c('countryname','countrycompset','usa')]
us_pp <- pp[usa==T, v]
report('US prisoners per police officer, 2015-24', round(us_pp,2))
report('Median rich democracy: police officers per prisoner', round(pp[countrycompset=='Rich' & usa==F, median(1/v)],2),
       paste(pp[countrycompset=='Rich' & usa==F, .N], 'countries'))
above <- pp[v > us_pp][order(-v)]
report('Countries with more prisoners per officer than the US', nrow(above),
       paste0(paste(above$countryname, collapse=', '), ' (of ', nrow(pp), ')'))

#police killings
pk <- over(rich, 'policekillings_rate', recent)
report('Police killings per million, US', round(10*pk[usa==T, v],1))
report('Police killings per million, highest other rich democracy', round(10*pk[usa==F, max(v)],2),
       pk[usa==F][which.max(v), countryname])

#social democracy: crime and punishment in the Nordic countries
nordic <- c('Denmark','Finland','Norway','Sweden')
for (var in c('homicides_rate','prisoners_rate')) {
  x <- over(cy, var, recent)
  report(paste('US /', 'Nordic mean,', var), round(x[usa==T, v]/x[countryname %in% nordic, mean(v)],1))
}

#########################################################
#########################################################

#7. THE FIRST WORLD BALANCE

#The budget-neutral swap from prisons to police (Lewis and Usmani 2022):
#move along the line that holds penal spending fixed, at the relative costs
#of a police officer and a prisoner, until the US prisoner/police ratio
#equals the median rich democracy's.

cat('\n--- First World Balance ---\n')

cost_police <- 130000   #per officer per year (Chalfin and McCrary 2017)
cost_prisoner <- 33089  #per prisoner per year (Chalfin and McCrary 2017)
c_ratio <- cost_police/cost_prisoner

lastyear <- cy[usa==T & !is.na(prisoners) & !is.na(police), max(year)]
P <- cy[usa==T & year==lastyear, prisoners]
Q <- cy[usa==T & year==lastyear, police]
r <- pp[countrycompset=='Rich' & usa==F, median(v)] #prisoners per officer

#intersection of P' = r Q' with P' = P - c_ratio (Q' - Q)
Q_new <- (P + c_ratio*Q)/(r + c_ratio)
P_new <- r*Q_new
report('First World Balance: fewer prisoners', round(P - P_new), paste('from', lastyear, 'levels'))
report('First World Balance: additional police officers', round(Q_new - Q))

#########################################################
#########################################################

#8. REDISTRIBUTION (OECD)

cat('\n--- Redistribution (OECD Income Distribution Database) ---\n')

oecd_url <- paste0(
  'https://sdmx.oecd.org/public/rest/data/OECD.WISE.INE,DSD_WISE_IDD@DF_IDD,/',
  '.A.INC_DISP_GINI+INC_MRKT_GINI..._T.METH2012.D_CUR.',
  '?startPeriod=2015&dimensionAtObservation=AllDimensions&format=csv'
)
oecd <- tryCatch(fread(oecd_url), error=function(e) NULL)
if (is.null(oecd)) {
  cat('Could not reach the OECD API; skipping.\n')
} else {
  #the rich comparison set, by ISO code
  richiso <- c('AUS','AUT','BEL','CAN','DNK','FIN','FRA','DEU','ITA','JPN',
               'NLD','NZL','NOR','PRT','ESP','SWE','CHE','GBR','USA')
  g <- oecd[REF_AREA %in% richiso, .(REF_AREA, MEASURE, year=TIME_PERIOD, OBS_VALUE)]
  g <- dcast(g, REF_AREA + year ~ MEASURE, value.var='OBS_VALUE')[!is.na(INC_DISP_GINI) & !is.na(INC_MRKT_GINI)]
  g <- g[, .SD[which.max(year)], by=REF_AREA] #latest year per country
  g[, reduction := 100*(1 - INC_DISP_GINI/INC_MRKT_GINI)]
  g <- g[order(reduction)]
  report('US: % reduction in Gini from taxes and transfers', round(g[REF_AREA=='USA', reduction],1),
         paste('year', g[REF_AREA=='USA', year]))
  report('US rank, least redistributive rich democracy', which(g$REF_AREA=='USA'),
         paste0('of ', nrow(g), '; less: ', paste(g[reduction < g[REF_AREA=='USA', reduction], REF_AREA], collapse=', ')))
  report('US rank, highest disposable-income Gini', which(g[order(-INC_DISP_GINI)]$REF_AREA=='USA'))
}

#########################################################
#########################################################

#SAVE

outdir <- 'output'
dir.create(outdir, showWarnings=F)
fwrite(rbindlist(out), file.path(outdir,'bookproposal_numbers.csv'))
cat('\nWrote', file.path(outdir,'bookproposal_numbers.csv'), '\n')
