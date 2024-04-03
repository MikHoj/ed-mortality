#### Meta-regression (table 4)

## Function to extract data
extract.t4 <- function (x) {
  # Extract results from MR-objects
  n         <- sapply(x, function (x) {x$k.eff})
  es        <- sapply(x, function (x) {format(round(x$beta[[2]], 3), nsmall = 3)})
  ci_lb     <- sapply(x, function (x) {format(round(x$ci.lb[[2]], 3), nsmall = 3)})
  ci_ub     <- sapply(x, function (x) {format(round(x$ci.ub[[2]], 3), nsmall = 3)})
  p1        <- sapply(x, function (x) {ifelse(x$pval[[2]] <0.0001, "<0.0001", format(signif(x$pval[[2]], 2), scientific = FALSE))})
  # Assemble RR with CIs
  rr       <- paste(es, " (", ci_lb, " to ", ci_ub, ")", sep = "")
  # Create subtable
  results <- cbind(n, rr, p1)  
  return(results)
}

## Load data sets
data.ac <- readRDS("input/ac_genpop.rds")
data.su <- readRDS("input/su_genpop.rds")
data.nc <- readRDS("input/nc_genpop.rds")

## Meta-analysis objects
ac <- metagen(mort_es,
              lower = mort_lb,
              upper = mort_ub,
              data = data.ac[[1]],
              common = FALSE
)
ac

su <- metagen(mort_es,
              lower = mort_lb,
              upper = mort_ub,
              data = data.su[[1]],
              common = FALSE
)
su

nc <- metagen(mort_es,
              lower = mort_lb,
              upper = mort_ub,
              data = data.nc[[1]],
              common = FALSE
)
nc

## Meta-regression AC
x <- ac
age  <- metareg(x, ~ ed_age_mean)
sex  <- metareg(x, ~ sex_ed_male)
bmi  <- metareg(x, ~ bmi_ed_mean)
year <- metareg(x, ~ fu_mediandatayear)
fu   <- metareg(x, ~ fu_mean_mod)
size <- metareg(x, ~ sizemod)
eco  <- metareg(x, ~ sdi)
# mdd  <- metareg(x, ~ comorb_ed_mdd)
# anx  <- metareg(x, ~ comorb_ed_anx)
# sud  <- metareg(x, ~ comorb_ed_sud)

ac.list <- list(age, sex, bmi, year, fu, size, eco)
names(ac.list) <- c("  Mean age", "  % male", "  Mean BMI", "  Median year of data collection", "  Mean follow-up (years)", "  Sample size (1000)", "  SDI")


# Extract results
res1 <- extract.t4(ac.list)
res1 <- cbind(
  names(ac.list),
  res1
)
res1

## Meta-regression SU
x <- su
age  <- metareg(x, ~ ed_age_mean)
sex  <- metareg(x, ~ sex_ed_male)
bmi  <- metareg(x, ~ bmi_ed_mean)
year <- metareg(x, ~ fu_mediandatayear)
fu   <- metareg(x, ~ fu_mean_mod)
size <- metareg(x, ~ sizemod)
eco  <- metareg(x, ~ sdi)

su.list <- list(age, sex, bmi, year, fu, size, eco)
names(su.list) <- c("  Mean age", "  % male", "  Mean BMI", "  Median year of data collection", "  Mean follow-up (years)", "  Sample size (1000)", "  SDI")


# Extract results
res2 <- extract.t4(su.list)
res2 <- cbind(
  names(su.list),
  res2
)
res2

## Meta-regression NC
x <- nc
age  <- metareg(x, ~ ed_age_mean)
sex  <- metareg(x, ~ sex_ed_male)
# bmi  <- metareg(x, ~ bmi_ed_mean)
year <- metareg(x, ~ fu_mediandatayear)
fu   <- metareg(x, ~ fu_mean_mod)
size <- metareg(x, ~ sizemod)
eco  <- metareg(x, ~ sdi)

nc.list <- list(age, sex, year, fu, size, eco)
names(nc.list) <- c("  Mean age", "  % male", "  Median year of data collection", "  Mean follow-up (years)", "  Sample size (1000)", "  SDI")


# Extract results
res3 <- extract.t4(nc.list)
res3 <- cbind(
  names(nc.list),
  res3
)
res3

## Collect table -------------------------------------------------------------
header     <- c("Outcome/Moderator", "N studies", "Coef (95%CI)", "p-value")
subheader1 <- c("All-cause mortality (Any ED)", "", "", "")
subheader2 <- c("Mortality from suicide (Any ED)", "", "", "")
subheader3 <- c("Mortality from natural causes (Any ED)", "", "", "")
t4 <- rbind(
  header,
  subheader1,
  res1,
  subheader2,
  res2,
  subheader3,
  res3
)

## Export to excel -----------------------------------------------------------
write.xlsx(t4, file = "output/tables.xlsx", sheetName = "T4 metaregression", row.names = FALSE, col.names = FALSE, append = TRUE)
