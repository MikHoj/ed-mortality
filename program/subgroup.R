#### Subgroup analysis (table 3)

## Function to extract subgroup results
extract.t3 <- function (x,y) {
  res <- data.frame(
    subgroup = paste0("  ", x$subgroup.levels),
    studies = x$k.w,
    es = paste0(
      format(round(x$TE.random.w, 2), nsmall = 2), " (",
      format(round(x$lower.random.w, 2), nsmall = 2), "-",
      format(round(x$upper.random.w, 2), nsmall = 2), ")"),
    het = ifelse(x$k.w>1, round(x$I2.w * 100, 0), "NA"),
    pvalsg = ""
  )
  res[1, "pvalsg"] <- ifelse(
    x$pval.Q.b.random <0.001, "<0.001",
    format(signif(x$pval.Q.b.random, 2), scientific = FALSE)
  )
  
  res <- rbind(c(y, "", "", "", ""), res)
  
  return(res)
}

## Load data sets
data.ac <- readRDS("input/ac_genpop.rds")
data.su <- readRDS("input/su_genpop.rds")
data.nc <- readRDS("input/nc_genpop.rds")

## Subgroup analysis - Any ED AC GENPOP
data <- data.ac[[1]]
dsm <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(ed_diagsystem_mod),
               subgroup = ed_diagsystem_mod,
               common = FALSE
               )
rob <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(rob),
               subgroup = rob,
               common = FALSE
)
rep <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(study_represent),
               subgroup = study_represent,
               common = FALSE
)
adj <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(mort_adjusted),
               subgroup = mort_adjusted,
               common = FALSE
)

aa <- extract.t3(dsm, "Diagnosis system")
bb <- extract.t3(rob, "Quality rating")
cc <- extract.t3(rep, "Representativeness")
dd <- extract.t3(adj, "Adjusted estimates")


## Subgroup analysis - Any ED SU GENPOP
data <- data.su[[1]]
dsm <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(ed_diagsystem_mod),
               subgroup = ed_diagsystem_mod,
               common = FALSE
)
rob <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(rob),
               subgroup = rob,
               common = FALSE
)
rep <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(study_represent),
               subgroup = study_represent,
               common = FALSE
)
adj <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(mort_adjusted),
               subgroup = mort_adjusted,
               common = FALSE
)

ee <- extract.t3(dsm, "Diagnosis system")
ff <- extract.t3(rob, "Quality rating")
gg <- extract.t3(rep, "Representativeness")
hh <- extract.t3(adj, "Adjusted estimates")


## Subgroup analysis - Any ED NC GENPOP
data <- data.nc[[1]]
dsm <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(ed_diagsystem_mod),
               subgroup = ed_diagsystem_mod,
               common = FALSE
)
rob <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(rob),
               subgroup = rob,
               common = FALSE
)
rep <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(study_represent),
               subgroup = study_represent,
               common = FALSE
)
adj <- metagen(mort_es,
               lower = mort_lb,
               upper = mort_ub,
               data = data,
               subset = !is.na(mort_adjusted),
               subgroup = mort_adjusted,
               common = FALSE
)

ii <- extract.t3(dsm, "Diagnosis system")
jj <- extract.t3(rob, "Quality rating")
kk <- extract.t3(rep, "Representativeness")
ll <- extract.t3(adj, "Adjusted estimates")

## Assemble table
header <- c("Outcome/Subgroup", "k", "RR (95% CI)", "I\U00B2", "Subgroup diff.")
subheader1 <- c("All-cause mortality", "", "", "", "")
subheader2 <- c("Mortality from suicide", "", "", "", "")
subheader3 <- c("Mortality from natural causes", "", "", "", "")

t3 <- rbind(
  header,
  subheader1,
  aa,
  bb,
  cc,
  dd,
  subheader2,
  ee,
  ff,
  gg,
  hh,
  subheader3,
  ii,
  jj,
  kk,
  ll
)
write.xlsx(t3, file = "output/tables.xlsx", sheetName = "T3 subgroup analysis", row.names = FALSE, col.names = FALSE, append = TRUE)
