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
library(DIDmultiplegtDYN)

df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>%
  filter(twfe_sample == 1)

# merge with /home/oddish3/Documents/R_folder/MSc/ME/ME-project/data/processed/cohort_earn_panel.csv') to get entry, exit times
cohort_earn_panel <- read_csv("../data/processed/cohort_earn_panel.csv") %>% select(c("exposure_time", "enter_time", "exit_time_date", "UNITID", "super_opeid", "AY_FALL"))

df %<>%
  left_join(cohort_earn_panel, by = c("UNITID", "super_opeid", "AY_FALL"))

# Estimate the baseline DiD model - PULKIT ------------------------------------------




# staggered stuff ---------------------------------------------------------------


df$DateJoinedFB <- as.Date(df$DateJoinedFB, format="%m/%d/%Y")
df$enter_time <- as.Date(df$enter_time, format="%m/%d/%Y")
harvard <- df %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, enter_time, exit_time_date) #, k_rank


# get year from datejoinedfb
df_sorted$year <- ifelse(!is.na(df_sorted$DateJoinedFB) & df_sorted$exit_time_date - df_sorted$DateJoinedFB >=0, as.numeric(format(df_sorted$DateJoinedFB, "%Y")), NA)

harvard <- df_sorted %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, enter_time, exit_time_date, wave1, wave2, year) #, k_rank
print(harvard)
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


df <- calculate_waves(df) 



# following code creates introduction waves (within 2 days of eachothers - arbitrarily chosen)

# Convert 'DateJoinedFB' to Date type and sort
df$DateJoinedFB <- as.Date(df$DateJoinedFB)
data_sorted <- arrange(df, DateJoinedFB)

# Identify the start of a new wave
data_sorted$wave2 <- ifelse(!is.na(data_sorted$DateJoinedFB) & data_sorted$exit_time_date - data_sorted$DateJoinedFB >=0, cumsum(c(1, diff(as_date(data_sorted$DateJoinedFB)) > 2)), 0)



df_sorted = data_sorted %>% ungroup()
rm(data_sorted)
harvard <- df_sorted %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, enter_time, exit_time_date, wave1, wave2) #, k_rank
print(harvard)














