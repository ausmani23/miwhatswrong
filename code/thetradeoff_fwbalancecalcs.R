#============================================================
# Derivation of extrapriz_frompolice
#
# extrapriz_frompolice = the number of extra prison-years
# generated (in expectation, in the long run) by hiring one
# additional police officer, via the arrests they make that turn
# into convictions that turn into prison time.
#
# Used in calculate_costsbenefits.R to adjust the proposed prison
# reduction when police are added: if a marginal officer implies
# +0.X prison-years, a truly budget-neutral police-for-prison
# swap must release an extra 0.X on top of the nominal release.
#
# ------- METHODOLOGY --------
#
# We back the number out of BJS prison composition using
# Little's Law. For each arrest category c:
#
#   E[prison-years | arrest_c]
#     = (state + federal prisoners held on offence c, stock)
#       / (annual arrests for offence c, flow)
#
# In long-run steady state this equals the expected prison-years
# generated per arrest of that type. This handles the critical
# fact that most MARGINAL arrests caused by hiring police are
# misdemeanor-class (drug possession, liquor, disorderly) and
# almost never result in state prison, while index-crime arrests
# (violent, property) have much higher per-arrest prison-year
# values.
#
# Then we weight each category by Chalfin et al (2020) "arrests
# per officer" coefficients (Tables 11-13), which tell us how a
# marginal added officer reshuffles arrest composition. Critically,
# Chalfin finds NEGATIVE effects on index-crime arrests (fewer
# violent and property arrests are made per marginal officer,
# presumably because fewer such crimes occur in cities with more
# police) and POSITIVE effects on QoL arrests. So the marginal
# officer generates more drug/liquor arrests (with negligible
# per-arrest prison time) and fewer index arrests (with high
# per-arrest prison time), and the net can easily be NEGATIVE:
# hiring police REDUCES incarceration on net.
#
# ------- IMPORTANT CAVEAT — JAILS --------
#
# Misdemeanor confinement happens in local jails, not state
# prison. BJS prison stocks therefore UNDERSTATE total state-
# inflicted confinement from a marginal misdemeanor arrest.
# Including jail data would raise the per-arrest values for
# drug-possession and liquor categories, shrinking the magnitude
# of the negative net offset. TODO: supplement with BJS Jail
# Inmates series or State Court Processing Statistics for
# misdemeanor sentencing. See memory/project_jails_todo.md.
#============================================================

#------------------------------------------------------------
# STEP 1: prison stocks by offence, 2021
# Source: BJS Prisoners in 2021 — Statistical Tables (NCJ 305125),
#         Table 12 (state) + federal BOP composition.
#------------------------------------------------------------
# State prisoners, 2021 total ≈ 1,042,300 (Table 1)
prisoners_state_violent  <- 619800  # 59.5% — murder, rape, robbery, assault, other violent
prisoners_state_property <- 145200  # 13.9% — burglary, larceny, MVT, fraud, other property
prisoners_state_drug     <- 135000  # 13.0% — possession + trafficking
prisoners_state_pubord   <- 116900  # 11.2% — DUI, weapons, QoL, court offences
prisoners_state_other    <-  25400  #  2.4%

# Federal prisoners, 2021 total ≈ 156,500 (BOP composition)
prisoners_fed_violent  <- 12500  #  ~8%
prisoners_fed_property <-  9400  #  ~6%
prisoners_fed_drug     <- 70000  # ~45% (overwhelmingly trafficking)
prisoners_fed_other    <- 64600  # ~41% (immigration, weapons, other)

# Federal drug prisoners are almost entirely trafficking; simple
# possession rarely reaches federal court.
drug_state_possession_share <- 0.15  # NCRP pattern: ~15% of state drug prisoners are possession-only
drug_state_sale_share       <- 0.85
drug_fed_possession_share   <- 0.01
drug_fed_sale_share         <- 0.99

prisoners_drug_possession <-
  prisoners_state_drug * drug_state_possession_share +
  prisoners_fed_drug   * drug_fed_possession_share
prisoners_drug_sale <-
  prisoners_state_drug * drug_state_sale_share +
  prisoners_fed_drug   * drug_fed_sale_share

# Liquor-law prisoners: very few. They fall in the "public order"
# BJS category alongside DUI and weapons, which are dominant.
# Assume ~5% of state public-order are strictly liquor.
prisoners_liquor <- 0.05 * prisoners_state_pubord

# Index-crime stocks (state + federal)
prisoners_violent  <- prisoners_state_violent  + prisoners_fed_violent
prisoners_property <- prisoners_state_property + prisoners_fed_property

#------------------------------------------------------------
# STEP 2: annual arrest flows by offence, 2021
# Source: FBI UCR 2021, Persons Arrested / Table 29
#------------------------------------------------------------
arrests_violent_idx  <-  440000   # murder + rape + robbery + agg assault
arrests_property_idx <- 1100000   # burglary + larceny + MVT
arrests_drug_total   <- 1160000
arrests_drug_possession <- round(0.85 * arrests_drug_total)   # FBI: ~85% of drug arrests are possession
arrests_drug_sale       <- arrests_drug_total - arrests_drug_possession
arrests_liquor       <- 1100000   # DUI + drunkenness + liquor laws

#------------------------------------------------------------
# STEP 3: E[prison-years | arrest] = stock / flow (Little's Law)
#------------------------------------------------------------
Eprisonyears_per_violent_arrest       <- prisoners_violent          / arrests_violent_idx
Eprisonyears_per_property_arrest      <- prisoners_property         / arrests_property_idx
Eprisonyears_per_drug_possession_arrest <- prisoners_drug_possession  / arrests_drug_possession
Eprisonyears_per_drug_sale_arrest     <- prisoners_drug_sale        / arrests_drug_sale
Eprisonyears_per_liquor_arrest        <- prisoners_liquor           / arrests_liquor

#------------------------------------------------------------
# STEP 4: Chalfin 2020 arrests per marginal officer (Tables 11-13)
#------------------------------------------------------------
# chalfin2020.xlsx reproduces Tables 11, 12, 13 of the paper.
# Column 4 holds the coefficient (β = arrests per officer).
# Panel A rows are the ASG Employment IV; Panel B rows are the
# COPS Eligible Hires IV. We take the mean across IVs as
# "bestguess" and the max as "pessimistic_police".
olddir <- getwd()
setwd(datadir)
tmpdf <- readxl::read_xlsx('chalfin2020.xlsx')
setwd(olddir)
val <- function(r) as.numeric(unlist(tmpdf[r, 4]))

# Table 11 — Index-crime arrests
# Panel A: rows 9-12 (violent sub-types), rows 13-15 (property sub-types)
# Panel B: rows 24-27 (violent),           rows 28-30 (property)
arrests_perofficer_violent_idx  <- c(sum(val(9:12)),  sum(val(24:27)))
arrests_perofficer_property_idx <- c(sum(val(13:15)), sum(val(28:30)))

# Table 12 — Quality-of-life arrests
# Rows 41-42 (Panel A) and 52-53 (Panel B) cover liquor + drug possession.
arrests_perofficer_drug_poss <- c(val(42), val(53))
arrests_perofficer_liquor    <- c(val(41), val(52))

# Table 13 — Non-index arrests
# Rows 67 (Panel A) and 82 (Panel B) are Drug Sale.
arrests_perofficer_drug_sale <- c(val(67), val(82))

#------------------------------------------------------------
# STEP 5: aggregate to prison-years per marginal officer
#------------------------------------------------------------
# Calculation per IV: sum of (arrests_c * Eprisonyears_per_arrest_c)
# across the Chalfin categories we have per-arrest data for.
# Note that arrests_perofficer_violent_idx and _property_idx are
# negative in Chalfin's estimates (adding police reduces index
# arrests), so these terms SUBTRACT from the offset.
extrapriz_frompolice <-
  arrests_perofficer_violent_idx  * Eprisonyears_per_violent_arrest       +
  arrests_perofficer_property_idx * Eprisonyears_per_property_arrest      +
  arrests_perofficer_drug_poss    * Eprisonyears_per_drug_possession_arrest +
  arrests_perofficer_drug_sale    * Eprisonyears_per_drug_sale_arrest     +
  arrests_perofficer_liquor       * Eprisonyears_per_liquor_arrest

# extrapriz_frompolice now holds [IV_A, IV_B].
# Summarise as bestguess = mean, pessimistic_police = max
# (max = least-negative = most pessimistic about police's
# ability to reduce prison).
extrapriz_frompolice <- c(
  bestguess          = mean(extrapriz_frompolice),
  pessimistic_police = max(extrapriz_frompolice)
)

# With 2021 BJS/UCR inputs and current Chalfin coefficients, this
# yields approximately:
#   bestguess           ≈ -0.53
#   pessimistic_police  ≈ -0.51
# i.e. the marginal officer REDUCES prison-years on net, because
# Chalfin's negative index-arrest effects dominate the small
# positive QoL contributions. See caveat above re: jails.

#------------------------------------------------------------
# Arrests per officer (TOTAL, used by calculate_costsbenefits.R
# for computing welfare costs of arrests). Sum across all
# categories in all three Chalfin tables.
#------------------------------------------------------------
# Table 11 (index): rows 9-15 Panel A, 24-30 Panel B
# Table 12 (QoL):   rows 34-43 Panel A, 45-54 Panel B
# Table 13 (non-index): rows 58-71 Panel A, 73-86 Panel B
arrests_perofficer_base <- c(
  sum(val(9:15))  + sum(val(34:43)) + sum(val(58:71)),  # IV_A
  sum(val(24:30)) + sum(val(45:54)) + sum(val(73:86))   # IV_B
)
