# ==============================================================================
# Social Media and Labour market Outcomes
# ==============================================================================
#
# Author: 10710007 &  &
# Version: 13-03-2024
#
# ==============================================================================

rm(list = ls()) 

# Packages  
library(dplyr)
library(haven)
library(magrittr)
library(tidyverse)
library(fixest)
library(did)
library(dplyr)
library(broom)
library(fastDummies)

# Data 


# load dataset + filter
analysis_sample <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>%
  filter(twfe_sample == 1)

# Script
# ==============================================================================
# create_exhbits_do (creating macro) ----

eqopp_demos <- c("k_married", "par_q1", "par_q2", "par_q3", "par_q4", "par_d9",
                 "par_top10pc", "par_top5pc", "par_top1pc", "par_toppt1pc", "par_rank")
ipeds_demos <- c("female", "hispanic", "asian", "black", "nativeamerican", "alien", "unknown",
                 "satmt25", "satmt75", "mi_sat", "use_act_score", "admssn_rate", "mi_admission_rate")
major_controls <- grep("^major_", names(analysis_sample), value = TRUE)

major_controls <- c(major_controls, "gradrate_150p")

### TWOWAYFE.do ----

#modify k rank variable
analysis_sample$k_rank <- analysis_sample$k_rank * 100
# 'panel data setup'
analysis_sample <- analysis_sample %>% group_by(UNITID, AY_FALL)
 #  or
library(plm)
pdata <- pdata.frame(analysis_sample, index = c("UNITID", "AY_FALL"))

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


# ============================
#  DiD model 
# ============================
analysis_sample <- fastDummies::dummy_cols(analysis_sample, select_columns = "simpletiershock")
# Get names of all dummy variables excluding one (e.g., exclude the first dummy variable)
dummy_vars <- grep("simpletiershock_", names(analysis_sample), value = TRUE)[-1]  # Excludes the first dummy variable



  
  
  
  
  
  

