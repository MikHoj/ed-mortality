#### Main analysis (table 2)

## Functions --------------------------------------------------------
# Function to meta-analyze
re.ma <- function (x) {
  zz <- metagen(mort_es,
                lower = mort_lb,
                upper = mort_ub,
                data = x,
                common = FALSE
  )
  return(zz)
}

# Function to extract results from MA objects (main analysis)
extract.t2 <- function (x) {
  # Extract results from MA-objects
  k         <- sapply(x, function (x) {x[["k"]]})
  es        <- sapply(x, function (x) {format(round(x$TE.random, 2), nsmall = 2)})
  ci_lb     <- sapply(x, function (x) {format(round(x$lower.random, 2), nsmall = 2)})
  ci_ub     <- sapply(x, function (x) {format(round(x$upper.random, 2), nsmall = 2)})
  pval      <- sapply(x, function (x) {ifelse(x$pval.random < 0.001, "<0.001", format(signif(x$pval.random, 2), scientific = FALSE))})
  i2        <- sapply(x, function (x) {ifelse(x[["k"]] > 1, round(x$I2 * 100, 0), "NA")})
  rr        <- paste0(es, " (", ci_lb, "-", ci_ub, ")")
  # Eggers test
  egger <- sapply(x, function (x) {
    if(x$k >= 10) {
      pval2 <- ifelse(metabias(x)$pval < 0.001, "<0.001", format(signif(metabias(x)$pval, 2), scientific = FALSE))
      return(pval2)
    } else {
      egger <- "NA"
    }
  })
  # collect and return
  res <- cbind(k, rr, pval, i2, egger)
  return(res)
}


### Outcome: All-cause mortality ----------------------------------------------
outcome <- "All-cause mortality"

## Load data set
data.ac <- readRDS("input/dataac.rds")

## Split by comparison
data.ac.comp <- split(data.ac, data.ac$study_comparison_mod)

## ED vs. general population
comparison <- "ED vs. general population"
xx <- data.ac.comp[[comparison]]
unique(xx$ed_diagnosis_mod)

# Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
bn    <- all %>% filter(ed_diagnosis_mod == "BN")
ednos <- all %>% filter(ed_diagnosis_mod == "EDNOS")
bed   <- all %>% filter(ed_diagnosis_mod == "BED")
mixed <- all %>% filter(ed_diagnosis_mod == "Mixed ED")
yy    <- list(all, an, bn, ednos, bed, mixed)
names(yy) <- c("Any ED", "AN", "BN", "EDNOS", "BED", "Mixed ED")
saveRDS(yy, file = "input/ac_genpop.rds")

# Analyze
ma <- lapply(yy, re.ma)

# Extract and collect subtable
res <- extract.t2(ma)
res <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)
header <- c(outcome, rep("", ncol(res) - 1))
res.ac.genpop <- rbind(header, res)


## ED vs. matched population
comparison <- "ED vs. matched population"
xx <- data.ac.comp[[comparison]]
unique(xx$ed_diagnosis_mod)

# Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
bn    <- all %>% filter(ed_diagnosis_mod == "BN")
yy    <- list(all, an, bn)
names(yy) <- c("Any ED", "AN", "BN")
saveRDS(yy, file = "input/ac_matchpop.rds")

# Analyze
ma <- lapply(yy, re.ma)

# Extract and collect subtable
res <- extract.t2(ma)
res.ac.matchpop <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)


## ED males vs. females
comparison <- "ED males vs. ED females"
xx <- data.ac.comp[[comparison]]
unique(xx$ed_diagnosis_mod)
# Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
bn    <- all %>% filter(ed_diagnosis_mod == "BN")
ednos <- all %>% filter(ed_diagnosis_mod == "EDNOS")
yy    <- list(all, an, bn, ednos)
names(yy) <- c("Any ED", "AN", "BN", "EDNOS")
saveRDS(yy, file = "input/ac_sexpop.rds")

# Analyze
ma <- lapply(yy, re.ma)

# Extract and collect subtable
res <- extract.t2(ma)
res.ac.male <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)


### Outcome: Mortality from suicide -------------------------------------------
outcome <- "Mortality from suicide"

## Load data set
data.su <- readRDS("input/datasu.rds")

## Split by comparison
data.su.comp <- split(data.su, data.su$study_comparison_mod)


## ED vs. general population
comparison <- "ED vs. general population"
xx <- data.su.comp[[comparison]]
unique(xx$ed_diagnosis_mod)

# Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
bn    <- all %>% filter(ed_diagnosis_mod == "BN")
ednos <- all %>% filter(ed_diagnosis_mod == "EDNOS")
bed   <- all %>% filter(ed_diagnosis_mod == "BED")
mixed <- all %>% filter(ed_diagnosis_mod == "Mixed ED")
yy    <- list(all, an, bn, ednos, bed, mixed)
names(yy) <- c("Any ED", "AN", "BN", "EDNOS", "BED", "Mixed ED")
saveRDS(yy, file = "input/su_genpop.rds")

## Analyze 
ma <- lapply(yy, re.ma)

## Extract and collect subtable
res <- extract.t2(ma)
res <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)
header <- c(outcome, rep("", ncol(res) - 1))
res.su.genpop <- rbind(header, res)


## ED vs. matched population
comparison <- "ED vs. matched population"
xx <- data.su.comp[[comparison]]
unique(xx$ed_diagnosis_mod)

# Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
bn    <- all %>% filter(ed_diagnosis_mod == "BN")
yy    <- list(all, an, bn)
names(yy) <- c("Any ED", "AN", "BN")
saveRDS(yy, file = "input/su_matchpop.rds")

# Analyze
ma <- lapply(yy, re.ma)

# Extract and collect subtable
res <- extract.t2(ma)
res.su.matchpop <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)


### Outcome: Mortality from natural causes ------------------------------------
outcome <- "Mortality from natural causes"

## Load data set
data.nc <- readRDS("input/datanc.rds")

## Split by comparison
data.nc.comp <- split(data.nc, data.nc$study_comparison_mod)

## ED vs. general population
comparison <- "ED vs. general population"
xx <- data.nc.comp[[comparison]]
unique(xx$ed_diagnosis_mod)

## Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
yy <- list(all, an)
names(yy) <- c("Any ED", "AN")
saveRDS(yy, file = "input/nc_genpop.rds")

## Analyze 
ma <- lapply(yy, re.ma)

## Extract and collect subtable
res <- extract.t2(ma)
res <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)
header <- c(outcome, rep("", ncol(res) - 1))
res.nc.genpop <- rbind(header, res)


## ED vs. matched population
comparison <- "ED vs. matched population"
xx <- data.nc.comp[[comparison]]
unique(xx$ed_diagnosis_mod)

# Filter by ED type
all   <- xx %>% filter(mort_subgroup_mod == "All")
an    <- all %>% filter(ed_diagnosis_mod == "AN")
bn    <- all %>% filter(ed_diagnosis_mod == "BN")
yy    <- list(all, an, bn)
names(yy) <- c("Any ED", "AN", "BN")
saveRDS(yy, file = "input/nc_matchpop.rds")

# Analyze
ma <- lapply(yy, re.ma)

# Extract and collect subtable
res <- extract.t2(ma)
res.nc.matchpop <- cbind(c(comparison, rep("", nrow(res) - 1)), names(yy), res)

### Collect main analysis table -----------------------------------------------
header <- c("Outcome/comparison", "ED subtype", "k", "RR (95% CI)", "p-value", "I\u00B2", "Egger's p")
t2     <- rbind(header,
                res.ac.genpop,
                res.ac.matchpop,
                res.ac.male,
                res.su.genpop,
                res.su.matchpop,
                res.nc.genpop,
                res.nc.matchpop
                )
write.xlsx(t2, file = "output/tables.xlsx", sheetName = "T2 main analysis", row.names = FALSE, col.names = FALSE)
