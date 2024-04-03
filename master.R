### Mortality in ED - meta-analysis ###
### Created 21/12/2023
### Updated 21/12/2023
### Mikkel Hoejlund

## Required packages
library(here)
library(readxl)
library(janitor)
library(dplyr)
library(xlsx)
library(meta)
library(metafor)
library(purrr)

## Set working directory
setwd("C:/Users/mhoejlund/OneDrive - Syddansk Universitet/72_ED_mortality")

source("program/cleaning.R")
source("program/analysis.R")
source("program/subgroup.R")
source("program/metareg.R")

# Calculate pooled RR for all-cause mortality
# Versus general population
# Versus matched population
# Versus with and without risk factor

# Mortality from suicide

# Mortality from natural causes

# Cause-specific mortality in EDs


# Sensitivity analyses
# DSM/ICD
# High quality studies
# Representative studies
# Adjusted studies

## Subgroup analyses
# Sex
# 5-year windows / decades?

# Meta-regression
# >= 10 studies
# Mean age
# Prop sex
# Mean BMI
# Prop comorb
# Prop subtype ED (isn't that a subgroup analysis?)
# Prop treatment at any time
# SDI
# Median year of data collection