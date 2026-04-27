#============================================================
# Derivation of extrapriz_frompolice  [v2: 2019 data + jails +
# full Chalfin coverage + fine-grained pub-order splits]
#
# extrapriz_frompolice = the number of extra CONFINEMENT-years
# (prison + jail) generated in expectation by hiring one
# additional police officer, via the arrests they make.
#
# Used in calculate_costsbenefits.R to adjust the proposed prison
# reduction when police are added: if a marginal officer implies
# +0.X confinement-years, a truly budget-neutral police-for-prison
# swap must release an extra 0.X on top of the nominal release.
#
# ------- WHAT CHANGED FROM v1 (−0.53) -------
#
# v1 summed β_c × (prison_c / arrests_c) over only 5 Chalfin
# categories (index violent, index property, drug possession,
# drug sale, liquor-cluster). It omitted most of Chalfin's
# Tables 12 and 13, and it used prison stock only (no jail).
#
# v2 does three things differently:
#   1) Includes ALL Chalfin arrest categories (Tables 11, 12, 13)
#      so the positive QoL and non-index contributions are in
#      scope.
#   2) Adds jail stock (Census of Jails 2019 × SILJ 2002 shares)
#      since pretrial detention and misdemeanor sentencing are
#      real confinement caused by arrests.
#   3) Uses 2019 pre-COVID inputs throughout.
#
# Point estimate +0.01 (bestguess mean across the two IVs),
# with pessimistic_police +1.45. The bestguess collapse from
# v4's +0.45 came in two steps: SPI 2016 robustness on state-
# prison pub-order splits dropped it to +0.22, then JDI 2024
# robustness on jail "Other public-order" liquor share dropped
# it to ~zero. Pessimistic_police remains comfortably positive.
# The sign of bestguess is now ambiguous: small positive at
# point estimate, with sensitivity easily flipping it negative
# under reasonable parameter perturbations.
#
# ------- METHODOLOGY --------
#
# For each Chalfin arrest category c (at minimum-common-
# denominator grain with BJS/SILJ stock categorizations):
#
#   E[confinement-yrs | arrest_c]
#     = (state prison + federal prison + jail stock for c)
#       / (annual arrests for c)
#
# This is Little's Law — in long-run steady state, the stock
# classified as c equals the inflow times expected time-in-
# system.
#
#   extrapriz_frompolice = Σ_c β_c × E[confinement-yrs | arrest_c]
#
# where β_c are Chalfin 2020 "arrests per officer" coefficients
# (Tables 11-13, both IVs).
#
# ------- PLEA-DOWN BIAS (KNOWN, SIGNED) -------
#
# Stock_c includes people arrested for c who stayed classified
# as c AND people arrested for higher-severity offences who
# pleaded down into c. Similarly, some arrests of c plea out to
# lower categories, removing their prison-years from stock_c.
# Net effect: stock_c / arrests_c underestimates E[…|arrest_c]
# for high-severity categories (pleas-out dominate) and
# overestimates it for low-severity (pleas-in dominate).
#
# At MAJOR-GROUP level (violent/property/drug/public-order),
# within-group plea-downs wash out exactly. A group-level
# alternative gives +4.44 (upper bound). The fine-grained
# calculation below gives +0.01 — the gap reflects combined
# plea-down bias + the homogeneity bias of group-level
# aggregation (treats weapons and liquor arrests identically
# within pub-order). Neither is perfect; fine-grained below is
# closer to reality.
#
# Between-group plea-downs (e.g., robbery → larceny) are not
# corrected. They are likely small at the Chalfin β-weighted
# sum level since Chalfin's large β's are mostly on QoL/non-
# index where between-group plea-downs are rare.
#
# ------- SPI 2016 ROBUSTNESS CHECK (DONE 2026-04-27) -------
#
# Earlier versions used judgment-call shares for splitting the
# BJS state-prison "Other public-order" 73,800 bucket: 55%
# prob/parole, 15% court, 10% morals, 5% liquor, 15% other.
# Replaced with empirical SPI 2016 shares (RV0036==12 EXCL. DUI,
# weighted): 0.1% prob/parole, 29% court, 16% morals/comm.vice,
# 0.2% liquor, 54% other. The 55% prob/parole prior turned out
# to be a 500x overstatement — almost no SPI respondents have
# NCRP code 490 or 500 as their controlling offense, because
# prob/parole violators are typically classified by their
# underlying offense, not as "490/500 violation". Of the five
# splits only morals (now 0.109) and liquor (now 0.002) enter
# the offset; the change in liquor (5% → 0.2%, 25× lower)
# accounts for nearly all of the bestguess drop from +0.45 to
# +0.22, since β_liquor in Chalfin Table 12 is large positive.
# Derivation: claude_workspace/spi_pubord_v2.R
#============================================================

#------------------------------------------------------------
# STEP 1: STATE PRISON STOCKS — BJS Prisoners 2019, Table 14
#         (sentenced prisoners, Dec 31 2018; pre-COVID)
#------------------------------------------------------------
s_murder     <- 177700 + 18600  # murder/nonneg.mans + neg.mans
s_rape_sex   <- 162700          # rape + sexual assault
s_robbery    <- 155000
s_assault    <- 135900          # agg + simple lumped by BJS
s_otherviol  <-  43500
s_burglary   <- 106500
s_larceny    <-  38700
s_mvt        <-  10200
s_fraud      <-  22400
s_otherprop_bjs  <-  21900      # catchall: arson, vandalism, stolen, other
s_drugposs   <-  46500
s_drugtraf   <- 129900          # BJS "Other" drug (≈ trafficking/sale)
s_weapons    <-  58000
s_dui        <-  21400
s_otherpub_bjs <- 73800         # catchall: see split below

#------------------------------------------------------------
# STEP 2: FEDERAL PRISON STOCKS — BJS Prisoners 2019, Table 16
#         (sentenced prisoners, Sept 30 2019; pre-COVID)
#------------------------------------------------------------
f_homicide   <-  2663
f_rape_sex   <-  1321
f_robbery    <-  5521
f_otherviol  <-  2707
f_burglary   <-   292
f_fraud      <-  6637  # fed fraud bucket = fraud + embezzlement + forgery
f_otherprop  <-  1471
f_drug       <- 73210  # >99% trafficking per BJS footnote
f_weapons    <- 29327
f_immig      <-  8324
f_otherpub   <- 26040

#------------------------------------------------------------
# STEP 3: JAIL STOCK — Census of Jails 2019 × SILJ 2002 shares
#         (midyear 2019)
#------------------------------------------------------------
j_total       <- 734470
j_convicted   <- 253730
j_unconvicted <- 480740

# SILJ 2002 Profile of Jail Inmates, Table 3 — sub-category
# offense shares among all jail inmates (Total column). Used
# to apportion 2019 jail stock into fine categories. Footnotes:
#   drunk_morals bucket = drunkenness + vagrancy + disorderly
#     conduct + unlawful assembly + morals + commercialized vice
#   other_pub bucket = rioting + abandonment + nonsupport +
#     invasion of privacy + liquor law + tax evasion
silj <- list(
  # violent sub-shares
  murder = 0.020, neg_mans = 0.005, kidnap = 0.007, rape = 0.006,
  other_sex = 0.028, robbery = 0.056, assault = 0.117, other_viol = 0.014,
  # property sub-shares
  burglary = 0.067, larceny = 0.070, mvt = 0.020, arson = 0.003,
  fraud = 0.049, stolen = 0.017, other_prop = 0.018,
  # drug sub-shares
  drug_poss = 0.108, drug_traf = 0.121, drug_other = 0.018,
  # public-order sub-shares
  weapons = 0.020, obstruction = 0.039, traffic = 0.037, dwi = 0.064,
  drunk_morals = 0.017, prob_parole_viol = 0.029, immig = 0.018,
  other_pub = 0.025
)

jfine <- function(share) j_total * share

#------------------------------------------------------------
# STEP 4: INTERNAL SPLITS OF BJS CATCHALL BUCKETS
#------------------------------------------------------------
# BJS state "Other public-order" (73,800): footnote e of BJS
# Prisoners 2019 Table 14 lists five components but does NOT
# provide percentages. Earlier code used judgment-call shares
# (55/15/10/5/15) which turned out to overstate prob/parole and
# liquor by ~25× and understate court offenses by ~3×.
#
# Replaced with SPI 2016 empirical shares. Method: among RV0036==12
# (Other Public Order, n=1,827, weighted ≈102k state prisoners,
# matches the 73,800 + 21,400 DUI sum closely after excluding DUI),
# we take the first NCRP3 code in the public-order range across
# all 12 CJ-status blocks × 5 positions, then bucket per the BJS
# NCRP code dictionary. Derivation script:
# claude_workspace/spi_pubord_v2.R
#
# Empirical shares within (RV0036==12 EXCL. DUI), N≈71,558:
#   prob/parole viol (490+500):                  0.001
#   court offenses (530 contempt + 540 perjury): 0.292
#   morals/comm.vice (600 lewdness + 640 vice):  0.161
#   liquor (660):                                0.002
#   other (escape, flight, riot, habitual,
#     family, drunken, immig, obstr, etc):       0.544
#
# Of these only s_pubord_morals and s_pubord_liquor enter the
# offset calc (Step 8); the others are documentation only.
# Note s_pubord_morals uses the NARROW match (NCRP 590 drunken/
# vagrant/disorder + 640 commercial vice) = 0.109, since that
# matches the UCR arrest categories used in e_morals_cluster
# (disorder + drunken + vagrancy + prostitut + gambling). NCRP
# 600 lewdness is morals-decency but not in those UCR rows.
s_pubord_probparole <- 0.001 * s_otherpub_bjs
s_pubord_court      <- 0.292 * s_otherpub_bjs
s_pubord_morals     <- 0.109 * s_otherpub_bjs   # 590 + 640 (offset-relevant)
s_pubord_liquor     <- 0.002 * s_otherpub_bjs   # 660 (offset-relevant)
s_pubord_otherres   <- 0.596 * s_otherpub_bjs   # residual (1 - sum above)

# BJS state "Other property" (21,900): includes arson, vandalism,
# stolen property, other. Rough shares: arson 15%, vandalism 20%,
# stolen 30%, other 35%.
s_arson     <- 0.15 * s_otherprop_bjs
s_vandalism <- 0.20 * s_otherprop_bjs
s_stolen    <- 0.30 * s_otherprop_bjs
s_othprop_res <- 0.35 * s_otherprop_bjs

# SILJ jail "Other public-order" (2.5% bucket): liquor share
# anchored on PPI/JDI April 2025 analysis of Feb 2024 snapshot
# (251,671 inmates × 977,728 charges across 865 jail rosters):
# liquor laws are 0.3% of all jail charges nationally. Applying
# that share to 2019 jail stock 734,470 ≈ 2,200 inmates with
# liquor as controlling charge → 2,200 / 18,362 ≈ 12% within
# the SILJ "other_pub" bucket (was 30% by judgment, a 2.5×
# overstatement). Caveats: JDI is a non-probability sample,
# 2024 snapshot mapped to 2019 stock, and "0.3% of charges" vs
# "0.3% of inmates" differ slightly in interpretation. Source:
# https://www.prisonpolicy.org/blog/2025/04/17/jdi_jail_offenses/
j_otherpub_total  <- jfine(silj$other_pub)
j_pubord_liquor   <- 0.12 * j_otherpub_total
j_pubord_otherres <- 0.88 * j_otherpub_total

# SILJ jail "Other property" (1.8% bucket) has no explicit
# vandalism row. Assume 40% is vandalism.
j_otherprop_total  <- jfine(silj$other_prop)
j_vandalism_jail   <- 0.40 * j_otherprop_total
j_othprop_res_jail <- 0.60 * j_otherprop_total

#------------------------------------------------------------
# STEP 5: UCR 2019 ARRESTS — Table 29 + Arrest Table drug split
#------------------------------------------------------------
a_murder    <-   11060
a_rape      <-   24986
a_robbery   <-   74547
a_aggassault<-  385278
a_burglary  <-  171590
a_larceny   <-  813073
a_mvt       <-   80636
a_arson     <-    9068
a_otherassault <- 1025711   # simple assault primarily
a_forgery   <-   45183
a_fraud     <-  112707
a_embezzle  <-   13497
a_stolen    <-   88272
a_vandalism <-  180501
a_weapons   <-  153161
a_prostitut <-   26713
a_sexoffens <-   40796      # non-rape non-prostitution
a_drug_total<- 1558862
a_drug_poss <- round(a_drug_total * 0.867)  # UCR 2019 Arrest Table: 86.7%
a_drug_sale <- a_drug_total - a_drug_poss   # 13.3%
a_gambling  <-    2458
a_family    <-   85687
a_dui       <- 1024508
a_liquor    <-  175548
a_drunken   <-  316032
a_disorder  <-  310331
a_vagrancy  <-   21896
a_suspicion <-     579
a_curfew    <-   14653
# (UCR "All other offenses" excluded — catchall not in Chalfin)

#------------------------------------------------------------
# STEP 6: E[confinement-yrs | arrest_c] at fine grain
#------------------------------------------------------------
# Each Chalfin arrest category is paired with the
# narrowest-matching stock slice across state prison, federal
# prison, and jail.

# Violent
e_murder     <- (s_murder + f_homicide + jfine(silj$murder + silj$neg_mans)) / a_murder
e_rape_sex   <- (s_rape_sex + f_rape_sex + jfine(silj$rape + silj$other_sex)) / (a_rape + a_sexoffens)
e_robbery    <- (s_robbery + f_robbery + jfine(silj$robbery)) / a_robbery
e_assault    <- (s_assault + jfine(silj$assault)) / (a_aggassault + a_otherassault)
e_otherviol  <- (s_otherviol + f_otherviol + jfine(silj$other_viol + silj$kidnap)) / a_family
# ^ BJS "other violent" matched to UCR "family offenses" (domestic
#   violence primarily). Imperfect but closest available.

# Property
e_burglary   <- (s_burglary + f_burglary + jfine(silj$burglary)) / a_burglary
e_larceny    <- (s_larceny + jfine(silj$larceny)) / a_larceny
e_mvt        <- (s_mvt + jfine(silj$mvt)) / a_mvt
e_fraud_grp  <- (s_fraud + f_fraud + jfine(silj$fraud)) / (a_fraud + a_forgery + a_embezzle)
# ^ BJS fed "Fraud" bucket (f_fraud=6637) covers fraud + forgery +
#   embezzlement + counterfeiting, so we divide by UCR sum.
e_arson      <- (s_arson + jfine(silj$arson)) / a_arson
e_vandalism  <- (s_vandalism + j_vandalism_jail) / a_vandalism
e_stolenprop <- (s_stolen + jfine(silj$stolen)) / a_stolen

# Drug
e_drug_poss  <- (s_drugposs + f_drug * 0.01 + jfine(silj$drug_poss)) / a_drug_poss
e_drug_sale  <- (s_drugtraf + f_drug * 0.99 + jfine(silj$drug_traf + silj$drug_other)) / a_drug_sale

# Public-order
e_weapons    <- (s_weapons + f_weapons + jfine(silj$weapons)) / a_weapons
e_dui        <- (s_dui + jfine(silj$dwi)) / a_dui
e_liquor     <- (s_pubord_liquor + j_pubord_liquor) / a_liquor
# Drunk/morals cluster: disorderly, drunkenness, vagrancy,
# prostitution, gambling all fall into SILJ "drunk_morals" (1.7%)
# and BJS state "morals" split (10% of other pubord).
a_morals_cluster <- a_disorder + a_drunken + a_vagrancy + a_prostitut + a_gambling
e_morals_cluster <- (s_pubord_morals + jfine(silj$drunk_morals)) / a_morals_cluster
# Curfew, suspicion: essentially no confinement stock (juvenile /
# held-brief). Placeholder lower-bound.
e_curfew_susp <- 0.003

#------------------------------------------------------------
# STEP 7: CHALFIN 2020 COEFFICIENTS (Tables 11, 12, 13)
#------------------------------------------------------------
# chalfin2020.xlsx reproduces Tables 11, 12, 13 of the paper.
# Column 4 holds the β coefficient (arrests per officer).
# Panel A rows use the ASG Employment IV; Panel B rows use the
# COPS Eligible Hires IV.
olddir <- getwd()
setwd(datadir)
tmpdf <- readxl::read_xlsx('chalfin2020.xlsx')
setwd(olddir)
val <- function(r) as.numeric(unlist(tmpdf[r, 4]))

# Table 11 — Index-crime ARRESTS.
# Panel A rows 9-15: Murder, Rape, Robbery, Agg.Assault,
#                    Burglary, Theft, MVT.
# Panel B rows 24-30: same order.
c11A <- val(9:15);  c11B <- val(24:30)
b_murder     <- c(c11A[1], c11B[1])
b_rape       <- c(c11A[2], c11B[2])
b_robbery    <- c(c11A[3], c11B[3])
b_aggassault <- c(c11A[4], c11B[4])
b_burglary   <- c(c11A[5], c11B[5])
b_larceny    <- c(c11A[6], c11B[6])
b_mvt        <- c(c11A[7], c11B[7])

# Table 12 — Quality-of-life arrests.
# Panel A rows 34-43: Disorderly, Suspicious, Curfew, Vandalism,
#                     Vagrancy, Gambling, Drunkenness, Liquor,
#                     Drug Possession, Uncategorized.
# Panel B rows 45-54: same order.
c12A <- val(34:43);  c12B <- val(45:54)
b_disorder   <- c(c12A[1], c12B[1])
b_suspicion  <- c(c12A[2], c12B[2])
b_curfew     <- c(c12A[3], c12B[3])
b_vandalism  <- c(c12A[4], c12B[4])
b_vagrancy   <- c(c12A[5], c12B[5])
b_gambling   <- c(c12A[6], c12B[6])
b_drunken    <- c(c12A[7], c12B[7])
b_liquor     <- c(c12A[8], c12B[8])
b_drug_poss  <- c(c12A[9], c12B[9])
# (Uncategorized Arrests at c12[10] is a residual — excluded.)

# Table 13 — Non-index arrests.
# Panel A rows 58-71: NeglMans, Arson, OtherAssault, FamilyOffense,
#                     Weapons, Prostitution, OtherSex, Runaway,
#                     DUI, DrugSale, Forgery, Fraud, Embezzlement,
#                     StolenProperty.
# Panel B rows 73-86: same order.
c13A <- val(58:71);  c13B <- val(73:86)
b_neg_mans    <- c(c13A[1],  c13B[1])
b_arson       <- c(c13A[2],  c13B[2])
b_otherassault<- c(c13A[3],  c13B[3])
b_family      <- c(c13A[4],  c13B[4])
b_weapons     <- c(c13A[5],  c13B[5])
b_prostitut   <- c(c13A[6],  c13B[6])
b_othersex    <- c(c13A[7],  c13B[7])
# b_runaway omitted — juvenile, no state prison or jail stock match
b_dui         <- c(c13A[9],  c13B[9])
b_drug_sale   <- c(c13A[10], c13B[10])
b_forgery     <- c(c13A[11], c13B[11])
b_fraud       <- c(c13A[12], c13B[12])
b_embezzle    <- c(c13A[13], c13B[13])
b_stolen      <- c(c13A[14], c13B[14])

#------------------------------------------------------------
# STEP 8: Aggregate β_c × E_c → extrapriz_frompolice
#------------------------------------------------------------
# Each Chalfin arrest category contributes β × E confinement-yrs.
# Summing over all categories gives total extra confinement
# years per marginal officer per year.
extrapriz_frompolice <-
  b_murder      * e_murder      +
  b_neg_mans    * e_murder      +   # neg.mans paired with murder bucket
  b_rape        * e_rape_sex    +
  b_othersex    * e_rape_sex    +
  b_robbery     * e_robbery     +
  b_aggassault  * e_assault     +
  b_otherassault* e_assault     +
  b_family      * e_otherviol   +
  b_burglary    * e_burglary    +
  b_larceny     * e_larceny     +
  b_mvt         * e_mvt         +
  b_arson       * e_arson       +
  b_fraud       * e_fraud_grp   +
  b_forgery     * e_fraud_grp   +
  b_embezzle    * e_fraud_grp   +
  b_stolen      * e_stolenprop  +
  b_vandalism   * e_vandalism   +
  b_drug_poss   * e_drug_poss   +
  b_drug_sale   * e_drug_sale   +
  b_weapons     * e_weapons     +
  b_dui         * e_dui         +
  b_liquor      * e_liquor      +
  b_drunken     * e_morals_cluster +
  b_disorder    * e_morals_cluster +
  b_vagrancy    * e_morals_cluster +
  b_prostitut   * e_morals_cluster +
  b_gambling    * e_morals_cluster +
  b_curfew      * e_curfew_susp +
  b_suspicion   * e_curfew_susp

# extrapriz_frompolice now holds [IV_A, IV_B].
# Summarise as bestguess = mean, pessimistic_police = max (max =
# most pessimistic about police's ability to reduce prison, i.e.
# largest confinement-years added per marginal officer).
extrapriz_frompolice <- c(
  bestguess          = mean(extrapriz_frompolice),
  pessimistic_police = max(extrapriz_frompolice)
)

# With 2019 inputs, fine-grained pub-order splits, SPI 2016
# state-prison shares, and JDI 2024 jail liquor anchor, this
# yields approximately:
#   bestguess          ≈ +0.01
#   pessimistic_police ≈ +1.45
# At the bestguess point estimate the marginal officer is
# roughly neutral on confinement-years; the band straddles
# zero. Pessimistic_police remains substantially positive.

#------------------------------------------------------------
# STEP 9: Arrests per officer (TOTAL), for welfare cost of
# arrests in calculate_costsbenefits.R. Sum across ALL Chalfin
# rows (including Uncategorized and Runaway — these are real
# marginal arrests that each carry a welfare cost, even though
# they're excluded from the offset calc because we can't match
# them to stock categories). Matches historical behavior.
#------------------------------------------------------------
arrests_perofficer_base <- c(
  sum(c11A) + sum(c12A) + sum(c13A),  # IV_A
  sum(c11B) + sum(c12B) + sum(c13B)   # IV_B
)
