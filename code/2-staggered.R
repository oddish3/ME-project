# ------------------------------------------
#           Staggered DiD Script 
# ------------------------------------------

rm(list = ls())
setwd("~/Documents/R_folder/MSc/ME/ME-project/code")
source("~/Documents/R_folder/MSc/ME/ME-project/code/1.prelim-DiD.R")

library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)
library(readr)
library(pacman)
library(magrittr)

df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>%
  filter(twfe_sample == 1)

# merge with /home/oddish3/Documents/R_folder/MSc/ME/ME-project/data/processed/cohort_earn_panel.csv') to get entry, exit times
cohort_earn_panel <- read_csv("../data/processed/cohort_earn_panel.csv") %>% select(c("exposure_time", "enter_time", "exit_time_date", "UNITID", "super_opeid", "AY_FALL"))

df %<>%
  left_join(cohort_earn_panel, by = c("UNITID", "super_opeid", "AY_FALL"))

# Estimate the baseline DiD model - PULKIT ------------------------------------------

library(haven)
library(tidyverse)
library(fixest)
library(fastDummies)
library(lmtest)

analysis_sample <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>% 
  filter(twfe_sample == 1 & late_adopter == 0)


analysis_sample$k_rank <- analysis_sample$k_rank * 100
#excluding a specific observation 
analysis_sample <- filter(analysis_sample, UNITID != 184694)
# create + recode simplebarrons variable
analysis_sample$simplebarrons <- analysis_sample$barrons
analysis_sample$simplebarrons <- as.numeric(as.character(analysis_sample$simplebarrons))
analysis_sample$simplebarrons <- dplyr::recode(analysis_sample$simplebarrons,
                                               `0` = 1, `1` = 1,
                                               `2` = 2, `3` = 2,
                                               `4` = 3,
                                               `5` = 4,
                                               `999` = 9)

# group data and generate identifiers
analysis_sample$simpletiershock <- as.integer(interaction(analysis_sample$simplebarrons, analysis_sample$AY_FALL, drop = TRUE))
analysis_sample <- fastDummies::dummy_cols(analysis_sample, select_columns = "simpletiershock")



analysis_sample$D <- ifelse(analysis_sample$EXPOSURE_4YR > 0, 1, 0)

identical_elements <- analysis_sample$D == analysis_sample$EXPOSED
identical_elements # basically same thing - few differences probably bc of samples (main / twfe?)


analysis_sample <- analysis_sample %>% mutate(t = case_when(
  AY_FALL == 1998 ~ 1, 
  AY_FALL == 1999 ~ 2,
  AY_FALL == 2000 ~ 3,
  AY_FALL == 2001 ~ 4,
  AY_FALL == 2002 ~ 5,
  AY_FALL == 2003 ~ 6,
  AY_FALL == 2004 ~ 7,
  AY_FALL == 2005 ~ 8
))

analysis_sample$G <- 0
for (i in unique(analysis_sample$UNITID)){
  analysis_sample$G[analysis_sample$UNITID == i] <- min(analysis_sample$t[analysis_sample$UNITID == i & analysis_sample$D == 1])
}  


analysis_sample$R <- analysis_sample$t - analysis_sample$G + 1
analysis_sample <- dummy_cols(analysis_sample, select_columns = "R")
R <- grep("R_", names(analysis_sample)[300:374], value = T)


# Whats trending in diff-n-diff paper introduces these two estimators under the multiple
# treatment periods and differntial treatment times. 
# The Static TWFE assumes there is no hetergeneity in treatment effects over time or across
# different units 
# Dynamic TWFE assumes there is treatment heterogeneity over time (so different treatment effect
# if you got the treatment sooner rather than later) but no heterogeneity across units. 

# static TWFE
M1 <- feols(k_rank ~ D | UNITID + simpletiershock, data = analysis_sample, vcov = "cluster")
print(M1)


# dynamic TWFE 
M2 <- feols(k_rank ~ `R_-1` + `R_-2` + `R_1` + `R_2` + `R_3` + `R_4` + `R_5` + 
              `R_6` | UNITID + simpletiershock, data = analysis_sample, vcov = "cluster")
print(M2)


# staggered stuff ---------------------------------------------------------------


df$DateJoinedFB <- as.Date(df$DateJoinedFB, format="%m/%d/%Y")
df$enter_time <- as.Date(df$enter_time, format="%m/%d/%Y")
harvard <- df %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, enter_time, exit_time_date) #, k_rank

# following code creates COHORT WAVES () ----


calculate_waves <- function(dataset) {
  dataset %>%
    mutate(
      # Ensure DateJoinedFB and exit_time_date are Date objects
      DateJoinedFB = as.Date(DateJoinedFB),
      exit_time_date = as.Date(exit_time_date),
      # Calculate numeric wave identifier
      wave1 = ifelse(is.na(DateJoinedFB), 
                           -1,  # Code for 'general release'
                           {
                             diff_years <- as.integer(exit_time_date - DateJoinedFB) / 365.25
                             ifelse(diff_years < 0, 0, floor(diff_years)+1)  # Code 0 for 'Pre-FB'
                           }),
      # Map numeric wave identifiers to labels
      Wave1Character = case_when(
        wave1 == -1 ~ "general release",
        wave1 == 0  ~ "Pre-FB",
        TRUE              ~ paste0("Wave ", wave1)
      )
    )
}

harvard_waves <- calculate_waves(harvard) 
print(harvard_waves)

df <- calculate_waves(df) 



# following code creates introduction waves (within 2 days of eachothers - arbitrarily chosen)

# Convert 'DateJoinedFB' to Date type and sort
df$DateJoinedFB <- as.Date(df$DateJoinedFB)
data_sorted <- arrange(df, DateJoinedFB)

# Identify the start of a new wave
data_sorted$wave2 <- ifelse(!is.na(data_sorted$DateJoinedFB) & data_sorted$exit_time_date - data_sorted$DateJoinedFB >=0, cumsum(c(1, diff(as_date(data_sorted$DateJoinedFB)) > 2)), 0)

# # Summarize wave information
# wave_summary <- data_sorted %>%
#   group_by(NewWave) %>%
#   summarise(
#     WaveStart = min(DateJoinedFB),
#     WaveEnd = max(DateJoinedFB),
#   ) %>%
#   ungroup() # To remove the grouping structure
# 
# data_sorted %>% filter(FBName == "Harvard") %>% select(DateJoinedFB, wave1, wave2) %>% print()

df_sorted = data_sorted %>% ungroup()




