## Load data
# Extracted data (and clean variable names)
data <- read_excel("input/database_4.xlsx",
                   sheet = "Reconciled DE",
                   na = "--",
                   skip = 4
) %>%
  clean_names()

# Quality ratings
rob <- read_excel("input/quality3.xlsx",
                  sheet = "rob")

# Select relevant columns
datared <- data %>% 
  select(authoryear,
         study_n, n_ed, n_comp,
         study_comparison_mod,
         fu_mean_mod,
         fu_mediandatayear,
         study_setting,
         study_represent,
         ed_diagnosis_mod,
         ed_diagsystem_mod,
         ed_age_mean, comp_age_mean,
         sex_ed_male, sex_comp_male,
         bmi_ed_mean, bmi_comp_mean,
         comorb_ed_mdd, comorb_ed_anx, comorb_ed_sud,
         starts_with("mort_"),
         comments,
         sdi
  ) %>% 
  mutate_at(vars(mort_ed_deaths,
                 mort_comp_deaths,
                 mort_es,
                 mort_lb,
                 sdi),
            as.numeric)

# Add quality rating
datared <- datared %>% 
  left_join(rob, by = "authoryear")


## Output table with comparisons by ED and study
comparisons <- datared %>% 
  select(authoryear, study_comparison_mod, ed_diagnosis_mod) %>% 
  distinct() %>% 
  arrange(-desc(ed_diagnosis_mod), -desc(study_comparison_mod), -desc(authoryear))
write.xlsx(comparisons, "output/comparisons.xlsx", sheetName = "groups")


## Output table with individual study names
studies <- comparisons %>% 
  select(authoryear) %>% 
  unique() %>% 
  arrange(-desc(authoryear))
write.xlsx(studies, "output/studies.xlsx", sheetName = "studies")


## Clean data
# Representativeness
repres <- c("Non-representative",
            "Nonrepresentative",
            "Not representative",
            "non-representative"
            )
datared$study_represent <- ifelse(datared$study_represent %in% repres,
                                  "Non-representative",
                                  datared$study_represent
                                  )

# Decades of data collection
datared$decade <- cut(datared$fu_mediandatayear,
                      breaks = seq(1960, 2020, by = 10),
                      labels = c("1960-69", "1970-79", "1980-89", "1990-99", "2000-09", "2010-23"),
                      right = FALSE)

datared$sizemod <- datared$study_n / 1000

## Comparison labels
genpop <- c("ED vs. General population", 
            "ED vs general population??"
            )
datared$study_comparison_mod <- ifelse(datared$study_comparison_mod %in% genpop,
                                       "ED vs. general population",
                                       datared$study_comparison_mod
                                       )

match <- c("ED vs. Matched population",
           "ED vs. Matched population??",
           "ED vs. No ED (matched for breast cancer)",
           "ED vs. No ED (propensity-score matched)"
           )
datared$study_comparison_mod <- ifelse(datared$study_comparison_mod %in% match,
                                       "ED vs. matched population",
                                       datared$study_comparison_mod
                                       )


## Calculate RR with CI if other measure not reported
datared <- escalc(measure = "RR",
                  ai = mort_ed_deaths,
                  n1i = n_ed,
                  ci = mort_comp_deaths,
                  n2i = n_comp,
                  data = datared
)

# Drop if zero events in both groups
datared$obs <- ""
datared$obs[datared$mort_ed_deaths == 0 & datared$mort_comp_deaths == 0 & is.na(datared$mort_es)] <- "OBS"
datared <- datared %>% filter(obs != "OBS")

# Add correction factor 0.5 if zero events in one arm
datared$mort_ed_deaths[datared$mort_ed_deaths == 0] <- 0.5
datared$mort_comp_deaths[datared$mort_comp_deaths == 0] <- 0.5

# Lines to add data
datared$needsrr <- ifelse(is.na(datared$mort_es) & !is.na(datared$yi), "*", "")
datared$mort_metric <- ifelse(datared$needsrr == "*", "RR", datared$mort_metric)
datared$mort_es <- ifelse(datared$needsrr == "*", exp(datared$yi), datared$mort_es)
datared$mort_lb <- ifelse(datared$needsrr == "*", exp(datared$yi - 1.96 * datared$vi), datared$mort_lb)
datared$mort_ub <- ifelse(datared$needsrr == "*", exp(datared$yi + 1.96 * datared$vi), datared$mort_ub)


## Output table with outcomes
outcomes <- datared %>% 
  filter(!is.na(mort_es)) %>% 
  select(mort_outcome_mod) %>% 
  group_by(mort_outcome_mod) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))
write.xlsx(outcomes, "output/outcomes.xlsx", sheetName = "outcomes")


## Choose adjusted over unadjusted ES
datared$mort_adjusted[datared$mort_adjusted == "Yes"] <- "Y"
datared$mort_adjusted[datared$mort_adjusted == "No"] <- "N"
datared <- datared %>% 
  group_by(authoryear,
           study_comparison_mod,
           ed_diagnosis_mod,
           mort_outcome_mod,
           mort_subgroup_mod
           ) %>% 
  arrange(authoryear,
          study_comparison_mod,
          ed_diagnosis_mod,
          mort_outcome_mod,
          mort_subgroup_mod,
          mort_adjusted) %>% 
  slice_head(n = 1)

## Divide data set in 1) outcomes, 2) comparisons, 3) disorders
# All-cause mortality
primaryoutcome1 <- c("All-cause mortality")
data.ac <- datared %>%
  filter(mort_outcome_mod %in% primaryoutcome1)

# Death from suicide
primaryoutcome2 <- c("Mortality due to suicide")
data.su <- datared %>% 
  filter(mort_outcome_mod %in% primaryoutcome2)

# Death from natural causes
cancer <- c("Mortality due to any cancer",
            "Mortality due to head and neck cancer",
            "Mortality due to gastrointestinal cancer",
            "Mortality due to lung cancer",
            "Mortality due to sarcoma",
            "Mortality due to melanoma",
            "Mortality due to other skin cancer",
            "Mortality due to breast cancer",
            "Mortality due to gynecological cancer",
            "Mortality due to urogenital cancer", 
            "Mortality due to central nervous system cancer",
            "Mortality due to thyroid or other endocrine cancer",
            "Mortality due to non-specified cancers",
            "Mortality due to hematological cancers"
)
othernaturalcauses <- c("Mortality due to natural causes",
                        "Mortality due to cardiovascular disease",
                        "Mortality due to any respiratory diseases",
                        "Mortality due to any infectious diseases",
                        "Mortality due to endocrine diseases",
                        "Mortality due to gastrointestinal diseases",
                        "Mortality due to other diseases",
                        "Mortality due to ED complications"
)
primaryoutcome3 <- c(cancer, othernaturalcauses)
data.nc <- datared %>% 
  filter(mort_outcome_mod %in% primaryoutcome3)


## Save data sets for the three primary outcomes
saveRDS(datared, file = "input/data.rds")
saveRDS(data.ac, file = "input/dataac.rds")
saveRDS(data.su, file = "input/datasu.rds")
saveRDS(data.nc, file = "input/datanc.rds")

## Cleaning
rm("comparisons", "outcomes", "repres", "rob", "studies")
rm("data", "datared", "data.ac", "data.su", "data.nc")
