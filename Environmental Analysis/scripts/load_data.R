# ==================================================
# Xenopus Environmental Analysis Pipeline
#
# Script: 01_load_data.R
# Author: Tom Burns-Cox
#
# Purpose:
# Load raw datasets used throughout the analysis.
# ==================================================

library(tidyverse)
library(lubridate)
library(lme4)
library(mgcv)
library(ggeffects)
library(here)
library(survival)

females <- read_csv(
  here("data","raw","females_df_bulk_trial_2.csv")
)

males <- read_csv(
  here("data","raw","males_df_bulk.csv")
)

environmental <- read_csv(
  here("data","raw","Conditions_Data.csv")
)

health_data <- read_csv(
  here("data","raw","Health_categorised.csv")
)

salts <- read_csv(
  here("data","raw","trop salinity.csv")
)