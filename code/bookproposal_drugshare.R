#########################################################
#########################################################
##
## bookproposal_drugshare.R
##
## Companion to bookproposal.R. Checks the proposal's claim that
## the share of arrestees held for drug offenses in the United
## States is comparable to other developed countries, after
## harmonizing what each country counts.
##
## Why this is needed. UNODC's "persons brought into formal
## contact with the police" lets each country report suspects,
## arrests, or cautions. Most of Europe reports registered
## suspects; the US reports FBI arrests. A registered suspect
## (German Tatverdaechtiger, Swedish skaeligen misstaenkt, French
## mis en cause, Dutch geregistreerde verdachte) is a named person
## formally recorded by the police as the offender, normally after
## a hearing, whose file goes to the prosecutor. The FBI arrest
## count already includes in-person citations and summonses. So
## the numerators are closer than the labels suggest. The real
## differences are (a) which offenses are in the denominator
## (traffic and DUI, public drunkenness, fare evasion, insult,
## immigration) and (b) the counting unit (events vs persons vs
## person-offense pairs).
##
## What it does. Section 1 reproduces the raw UNODC shares from the
## World History of Punishment Database. Sections 2-3 transcribe
## the national police statistics (FBI UCR; BKA PKS; SSMSI/INSEE;
## Bra; BFS; CBS; BMI Austria; SSB; Home Office; ABS; StatCan) and
## rebuild each denominator under common scopes:
##   S0  published total
##   S1  minus road traffic (incl. DUI), drunkenness, liquor laws
##   S2  minus petty public-order and status offenses (the headline)
##   S2b minus catch-all / administration-of-justice offenses
##   S3  "core": drugs / (violent + property + weapons + drugs)
## Section 4 builds the headline table. Every transcribed number
## carries its source in the comments.
##
## Result (2019 or nearest year). On S2 the US share is 19.0%
## against a median of 17.3% across ten rich democracies (rank 5 of
## 11); in 2022/23, 13.7% against 14.0%. The raw UNODC comparison
## (15.4% vs 13.2%) therefore understates neither side much. On the
## core scope S3 the US is near the top (32.6% vs 20.1%), because a
## third of US arrests fall in "all other offenses". We do not use
## S3: in countries with many police per homicide, police spend
## much of their effort on minor offenses, and that share of
## activity is part of what the comparison should capture.
##
## Counting units differ and cannot be harmonized from published
## tables. Each row carries a unit code:
##   EV  events, one principal offense each (US, E&W, Canada)
##   PP  distinct persons per year, principal offense (Norway, Australia)
##   PX  distinct persons, counted once in EVERY category (Germany,
##       Switzerland, Sweden): their S1/S2 shares are upper bounds
##   PO  person-offense pairs (Austria, Netherlands)
##
## Data: as bookproposal.R (P2P_DIR), plus the transcribed national
## figures below. Run from the repository root:
##   Rscript code/bookproposal_drugshare.R
## Writes output/bookproposal_drugshare.csv.
##
#########################################################
#########################################################

rm(list=ls())
require(data.table)

p2pdir <- Sys.getenv('P2P_DIR', unset=file.path('..','p2p'))
p2pdf_file <- file.path(p2pdir,'files','p2pdf.RDS')
stopifnot(file.exists(p2pdf_file))


# ------------------------------------------------------------------------------------------
# 1. Raw UNODC shares from the World History of Punishment db (same conventions as bookproposal.R)
# ------------------------------------------------------------------------------------------
df <- readRDS(p2pdf_file); setDT(df)
df <- df[!countryname %in% c('United Kingdom (composite)','United Kingdom') & !grepl('Fake',cownum)]
rich <- df[bigcountries==T & countrycompset=='Rich']
a <- rich[grepl('unodc',source) & statistic=='arrests' & is.na(gender) & is.na(age) & year %in% 2016:2022]
num <- a[crime %in% c('drug','drugtraffic','drugpossession'), .(v=median(value)), by=c('countryname','year','crime')]
num <- dcast(num, countryname+year~crime, value.var='v')
num[, drugs := fifelse(!is.na(drug), drug, rowSums(cbind(drugtraffic,drugpossession), na.rm=T))]
den <- a[is.na(crime), .(total=median(value)), by=c('countryname','year')]
m <- merge(num, den, by=c('countryname','year'))[!is.na(drugs)]
m[, share := 100*drugs/total]
raw <- m[, .(raw=mean(share), raw_years=paste(range(year), collapse='-')), by=countryname]
raw[countryname=='USA', countryname:='United States']
print(raw[order(-raw)])

# ------------------------------------------------------------------------------------------
# 2. United States
# ------------------------------------------------------------------------------------------
# 2016: CIUS 2016 Table 18; 2017-2019: CIUS Table 29 "Estimated Number of Arrests, United States"
# (ucr.fbi.gov, read via web.archive.org). Categories sum exactly to total (suspicion excluded).
us_t29 <- rbindlist(list(
 data.table(year=2016,total=10662252,murder=11788,rape=23632,robbery=95754,aggassault=383977,burglary=207325,larceny=1050058,mvt=86088,arson=9812,otherassault=1078808,forgery=56661,fraud=128531,embezzle=15937,stolen=93981,vandalism=195951,weapons=156777,prostitution=38306,sexoff=51063,drugs=1572579,gambling=3705,family=88748,dui=1017808,liquor=234899,drunk=376433,disorderly=369733,vagrancy=24851,allother=3254871,curfew=34176),
 data.table(year=2017,total=10554985,murder=12208,rape=23436,robbery=94046,aggassault=388927,burglary=199266,larceny=950357,mvt=91023,arson=9111,otherassault=1062370,forgery=55604,fraud=124232,embezzle=15967,stolen=98660,vandalism=188350,weapons=164984,prostitution=36247,sexoff=48525,drugs=1632921,gambling=3237,family=94062,dui=990678,liquor=207332,drunk=366824,disorderly=353151,vagrancy=23321,allother=3290015,curfew=30131),
 data.table(year=2018,total=10310960,murder=11970,rape=25205,robbery=88128,aggassault=395800,burglary=178611,larceny=887622,mvt=91676,arson=9387,otherassault=1063535,forgery=50072,fraud=119951,embezzle=14765,stolen=93161,vandalism=179828,weapons=168403,prostitution=31147,sexoff=46937,drugs=1654282,gambling=3323,family=87475,dui=1001329,liquor=173152,drunk=328772,disorderly=329152,vagrancy=23546,allother=3231700,curfew=22031),
 data.table(year=2019,total=10085207,murder=11060,rape=24986,robbery=74547,aggassault=385278,burglary=171590,larceny=813073,mvt=80636,arson=9068,otherassault=1025711,forgery=45183,fraud=112707,embezzle=13497,stolen=88272,vandalism=180501,weapons=153161,prostitution=26713,sexoff=40796,drugs=1558862,gambling=2458,family=85687,dui=1024508,liquor=175548,drunk=316032,disorderly=310331,vagrancy=21896,allother=3318453,curfew=14653)))
us_t29[, chk := total - (rowSums(.SD) - total), .SDcols=setdiff(names(us_t29),'year')]
stopifnot(all(us_t29$chk == 0))   # categories sum to total exactly

# 2022-2023: FBI Crime Data Explorer reported (not estimated) arrests,
# https://cde.ucr.cjis.gov/LATEST/arrest/national/all?type=totals&from=01-YYYY&to=12-YYYY (pulled 2026-10-22).
# The "(Unspecified)" drug rows are SRS subtotals double-counted in the API; dropping them makes the
# category sum equal the sex sum (2023: 7,081,986 vs 7,077,330) and reproduces the FBI national
# estimates' drug share (2022 12.37% vs 907,958/7,364,553 = 12.33%; 2023 11.52% vs 870,874/7,555,863 = 11.53%).
# Drunkenness (NIBRS 90E) was retired 1 Jan 2021 and is now reported under "All Other Offenses".
us_cde <- rbindlist(list(
 data.table(year=2022, catsum=6800485, unspec_drug=258366, suspicion=881, drugs=707590+101219,
   homicide=12732, rape=18590, robbery=52692, aggassault=301221, otherassault=844983, sexoff=22839,
   burglary=107391, larceny=518327, mvt=68532, arson=8385, forgery=25918, fraud=69922, embezzle=8464, stolen=73031, vandalism=144827,
   weapons=160237, dui=693235, liquor=83635, drunk=20178, disorderly=221942, vagloit=20464, allother=2196329),
 data.table(year=2023, catsum=7184563, unspec_drug=102577, suspicion=905, drugs=713414+102064,
   homicide=12905, rape=18772, robbery=65336, aggassault=330191, otherassault=950008, sexoff=23086,
   burglary=116277, larceny=634177, mvt=74696, arson=8465, forgery=33805, fraud=75304, embezzle=9324, stolen=76246, vandalism=159764,
   weapons=165688, dui=755175, liquor=89654, drunk=14292, disorderly=248888, vagloit=25291, allother=2315493)))
us_cde[, total := catsum - unspec_drug - suspicion]

us_scopes <- function(d, viol, prop) {
  d[, `:=`(S0 = total,
           S1 = total - dui - liquor - drunk)]
  d[, S2  := S1 - disorderly - vag]
  d[, S2b := S2 - allother]
  d[, S3  := rowSums(.SD[, c(viol, prop, 'weapons', 'drugs'), with=FALSE])]
  d
}
us_t29[, vag := vagrancy + curfew]
us_cde[, vag := vagloit]
viol16 <- c('murder','rape','robbery','aggassault','otherassault','sexoff')
prop   <- c('burglary','larceny','mvt','arson','forgery','fraud','embezzle','stolen','vandalism')
us_t29 <- us_scopes(us_t29, viol16, prop)
us_cde <- us_scopes(us_cde, c('homicide','rape','robbery','aggassault','otherassault','sexoff'), prop)
us <- rbind(us_t29[, .(year, drugs, S0, S1, S2, S2b, S3)], us_cde[, .(year, drugs, S0, S1, S2, S2b, S3)])
us_sh <- us[, .(year, S0=100*drugs/S0, S1=100*drugs/S1, S2=100*drugs/S2, S2b=100*drugs/S2b, S3=100*drugs/S3)]
print(us); print(us_sh, digits=3)

# ------------------------------------------------------------------------------------------
# 3. Comparison countries: numerator and denominators (counts), from national sources.
#    Each denominator is built in the comment from the published components; see notes file.
#    NA = not computable from published tables.
# ------------------------------------------------------------------------------------------
R <- function(country, year, unit, drugs, S0, S1=NA, S2=NA, S2b=NA, S3=NA, note='')
  data.table(country, year=as.character(year), unit, drugs, S0, S1, S2, S2b, S3, note)

oth <- rbindlist(list(
 # Germany, BKA PKS T20, Tatverdaechtige (persons; once per category). Traffic offences are outside PKS -> S1=S0.
 # S2 = key 890000 (total excl. immigration, de-duplicated) - fare evasion 515000 - insult 673000.
 # S3 = violent (000000+100000+210000+220000) + property (theft ****00 + 500000 - 515000 + 674000 + 631000 + 632000) + weapons 726000 + drugs 730000.
 R('Germany',2019,'PX',284390,2019211,2019211,1896221-130447-183082,NA,(3838+52322+26678+458379)+(377425+459010-130447+124216+1248+10839)+39691+284390),
 R('Germany',2022,'PX',267808,2093782,2093782,1921553-89297-189026,NA,1728650),
 R('Germany',2023,'PX',266280,2246767,2246767,2017552-100085-192772,NA,1808216),
 # France, old etat-4001 basis (per person per procedure, principal offence), SSMSI base via INSEE Securite et societe 2021 fiche 4.2.
 # 2019 total 1,151,100; ILS 16.1% (~185,300). S2 removes immigration (idx 69-71, 13,344) + outrage (idx 72, 29,748), procedure counts as proxy.
 # S3 = (physical 19.8 + sexual 3.3 + theft 18.2 + damage 4.5 + fraud/econ 8.1 + ILS 16.1)% of total, + weapons proxy (idx 74, 37,495).
 R('France',2019,'EV',185300,1151100,1151100,1151100-13344-29748,NA,1151100*(19.8+3.3+18.2+4.5+8.1+16.1)/100+37495),
 # Sweden, Bra Tabell 220 misstankta personer (persons, overlapping). Drugs = narkotikastrafflagen.
 # S1/S2: subtract all traffic suspects (38,255) and defamation+immigration (2,087) -> lower bound on denominator (upper bound on share).
 R('Sweden',2019,'PX',52214,189190,189190-38255,189190-38255-1307-780,NA,192930,'S1/S2 are upper bounds; true value lies between S0 and these'),
 R('Sweden',2023,'PX',55776,194503,NA,NA,NA,55776/0.289,'S1/S2 upper bounds: 34.8 / 35.2'),
 # Sweden alternative: Tabell 260 handled suspicions (person-offence pairs), sums to total.
 R('Sweden (offence pairs)',2019,'PO',108659,566104,503946,501419,NA,354288),
 # Switzerland, BFS PKS: S0 = StGB + BetmG + AIG persons (law totals; persons may appear in >1 law). SVG traffic outside PKS -> S1=S0.
 # S2 = StGB - insult(177) - defamation(173) - calumny(174) - fare evasion(150) + BetmG (AIG dropped).
 # S3 = (Leib und Leben + Raub + Sexual) + (Vermoegen - Raub - Art150) + BetmG; no weapons-law count.
 R('Switzerland',2019,'PX',32974,137567,137567,(81956-9651-1350-1149-660)+32974,NA,(20392+1212+5376)+(38252-1212-660)+32974),
 R('Switzerland',2023,'PX',24245,142156,142156,(90582-10518-1473-1182-1300)+24245,NA,93226),
 # Netherlands, CBS 86081NED registraties van verdachten (person-offence pairs, misdrijven only), sums to total.
 # S1 = minus verkeersmisdrijven; S2 = minus 245 (ongewenste vreemdeling), 241, 242 (orders/resisting).
 R('Netherlands',2019,'PO',19480,274200,226360,220880,NA,178970),
 R('Netherlands',2023,'PO',17270,257020,199450,193540,NA,156660),
 # Netherlands unique suspects (persons, overlapping), 86081NED.
 R('Netherlands (persons)',2019,'PX',16840,166610,166610-41240,NA,NA,124440,'S1 upper bound'),
 # Austria, BK PKS (since 2019 person-offence pairs, sums to total). Drugs = known SMG suspects (Suchtmittelbericht).
 # S1 = minus StGB traffic negligence (Kz 503, 32,772); S2 = minus fare evasion (177) and FPG (959); insult not in PKS.
 # S3 = (Leib und Leben - traffic + Raub + Sexual) + (Vermoegen - Raub) + WaffG + SMG.
 R('Austria',2019,'PO',40060,304422,304422-32772,304422-32772-177-959,NA,((85351-32772)+2198+6033)+(118233-2198)+2576+40060),
 R('Austria',2022,'PO',32751,302530,302530-28631,NA,NA,NA,'S1 approximate (traffic split from ages 14+ table)'),
 # Norway, SSB 09416 siktede by hovedlovbrudd (persons, principal offence), sums to total.
 # S1 = minus traffic violations, DUI, drunken disorder; S2 = minus immigration law.
 # S3 = (violence + sexual) + property (theft + receiving/laundering + fraud + embezzlement + damage) + weapons + narcotics.
 R('Norway',2019,'PP',10301,69345,69345-22297-3569-638,69345-22297-3569-638-292,NA,(8337+1762)+10419+1877+10301),
 R('Norway',2023,'PP',4396,60751,60751-20790-4588-262,60751-20790-4588-262-152,NA,(7881+1730)+10957+1487+4396),
 # England & Wales, Home Office arrests (events, primary offence), notifiable offences only -> S1=S0.
 # 2019/20 and 2022/23 on the mar23 second edition basis (all forces). S2 = minus public order; S2b = minus misc crimes v society.
 R('England & Wales','2019/20','EV',71811,676476,676476,676476-49684,676476-49684-42468,584324),
 R('England & Wales','2022/23','EV',60013,668979,668979,668979-52269,668979-52269-36367,580343),
 # Australia, ABS Recorded Crime - Offenders, Table 1 (distinct persons, principal offence; traffic div 14 excluded -> S1=S0).
 # S2 = minus public order (13), misc (16), residual; S2b = also minus offences against justice (15).
 R('Australia','2018/19','PP',77208,394638,394638,394638-53583-15051-28331,394638-53583-15051-28331-27466,91661+80081+15313+77208),
 R('Australia','2022/23','PP',52315,347742,347742,347742-36841-11254-22095,347742-36841-11254-22095-31041,105181+69775+12757+52315),
 # Canada, StatCan 35-10-0177 persons charged (per incident, most serious violation). Criminal Code + federal statutes.
 # S1 = minus CC traffic [900]; S2 = minus disturb the peace [3430], YCJA [6450], other federal [610]; S2b = minus admin of justice [330].
 R('Canada',2019,'EV',35339,645614,645614-65773,645614-65773-2005-2302-5097,645614-65773-2005-2302-5097-190685,187940+132408+10840+35339),
 R('Canada',2023,'EV',23749,594563,594563-55678,594563-55678-1441-912-2838,594563-55678-1441-912-2838-166916,NA,'S3 = 6.7%, from StatCan 2023 components not transcribed here')
))
oth[country=='Canada' & year=='2023', S3 := 23749/0.067]   # StatCan 2023 core components not transcribed
sh <- oth[, .(country, year, unit, S0=100*drugs/S0, S1=100*drugs/S1, S2=100*drugs/S2, S2b=100*drugs/S2b, S3=100*drugs/S3, note)]
print(sh, digits=3)

# ------------------------------------------------------------------------------------------
# 4. Headline table: one preferred series per country, pre-COVID (2019 or nearest) and latest (2022/23)
# ------------------------------------------------------------------------------------------
pref <- c('Germany','France','Sweden','Switzerland','Netherlands','Austria','Norway','England & Wales','Australia','Canada')
H <- rbind(
  data.table(country='United States', period='pre', year='2019', unit='EV', us_sh[year==2019, .(S0,S1,S2,S2b,S3)]),
  data.table(country='United States', period='late', year='2023', unit='EV', us_sh[year==2023, .(S0,S1,S2,S2b,S3)]),
  sh[country %in% pref & !(country=='Germany' & year=='2022'), .(country, period=fifelse(year %in% c('2019','2019/20','2018/19'),'pre','late'), year, unit, S0,S1,S2,S2b,S3)])
# France late: only the new SSMSI basis exists (per person per offence, secondary offences included): S0 only.
H <- rbind(H, data.table(country='France', period='late', year='2023', unit='PO*', S0=100*299300/1894942, S1=100*299300/1894942, S2=NA, S2b=NA, S3=NA))
# "Harmonized" headline = S2 (all criminal offences minus traffic/alcohol/petty), and S3 (core).
H[, H1 := S2][, H2 := S3]
# For Sweden S2 is an upper bound; use the midpoint of [S0, S2-upper] for the headline and show the range.
H[country=='Sweden' & period=='pre',  H1 := (100*52214/189190 + 100*52214/(189190-38255-1307-780))/2]
H[country=='Sweden' & period=='late', H1 := (28.7+35.2)/2]
# Austria late: only S1 available
H[country=='Austria' & period=='late', H1 := S1]
# France late: S0 only (scope already excludes traffic/contraventions), flagged non-comparable unit
H[country=='France' & period=='late', H1 := S0]
H <- merge(H, raw, by.x='country', by.y='countryname', all.x=TRUE)
H[country=='England & Wales', `:=`(raw=100*68736/639950, raw_years='2019/20 (HO)')]
print(H[order(period, -H1)], digits=3)

med <- function(p, v) H[period==p & country!='United States', median(get(v), na.rm=TRUE)]
rk  <- function(p, v) { x <- H[period==p & !is.na(get(v))][order(-get(v))]; c(which(x$country=='United States'), nrow(x)) }
summ <- data.table(period=c('pre','late'),
  us_H1=c(H[country=='United States' & period=='pre',H1], H[country=='United States' & period=='late',H1]),
  med_H1=c(med('pre','H1'), med('late','H1')),
  us_H2=c(H[country=='United States' & period=='pre',H2], H[country=='United States' & period=='late',H2]),
  med_H2=c(med('pre','H2'), med('late','H2')),
  rank_H1=c(paste(rk('pre','H1'),collapse=' of '), paste(rk('late','H1'),collapse=' of ')),
  rank_H2=c(paste(rk('pre','H2'),collapse=' of '), paste(rk('late','H2'),collapse=' of ')),
  us_raw=H[country=='United States',raw][1], med_raw=H[country!='United States' & period=='pre', median(raw, na.rm=T)])
print(summ, digits=3)

#########################################################
#########################################################

#WRITE

dir.create('output', showWarnings=F)
fwrite(H[order(period, -H1)], file.path('output','bookproposal_drugshare.csv'))
cat('\nWrote output/bookproposal_drugshare.csv\n')
