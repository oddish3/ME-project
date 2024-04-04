# ------------------------------------------
#           Staggered DiD Script 
# ------------------------------------------

rm(list = ls())

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

# Install DIDmultiplegt from GitHub if not already installed
if (!"HonestDiD" %in% rownames(installed.packages())) {
  remotes::install_github("asheshrambachan/HonestDiD")
}

# Load DIDmultiplegt
pacman::p_load(DIDmultiplegt, readr)

df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>%
  filter(twfe_sample == 1)

# df1 <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>%
#   filter(main_sample == 1)

Y <- "k_rank"
G <- "UNITID"
T <- "AY_FALL"
D <- "EXPOSED"

### 


# Define your variable lists
eqopp_demos <- c("k_married", "par_q1", "par_q2", "par_q3", "par_q4", "par_d9",
                 "par_top10pc", "par_top5pc", "par_top1pc", "par_toppt1pc", "par_rank")
ipeds_demos <- c("female", "hispanic", "asian", "black", "nativeamerican", "alien", "unknown",
                 "satmt25", "satmt75", "mi_sat", "use_act_score", "admssn_rate", "mi_admission_rate")

# Assuming 'analysis_sample' was a mistake and your dataframe is 'df', let's adjust that:
major_controls <- grep("^major_", names(df), value = TRUE)
simpletiershock_star <- grep("^simpletiershock_", names(df), value = TRUE)

# Append 'gradrate_150p' to 'major_controls'
major_controls <- c(major_controls, "gradrate_150p")

# Combine all controls for a full check
controls <- c(eqopp_demos, ipeds_demos, major_controls, simpletiershock_star)

# Define individual variables
Y <- "k_rank"
G <- "UNITID"
T <- "AY_FALL"
D <- "EXPOSED"

# Function to check and print variables not in dataframe
check_vars_in_df <- function(var_list, df) {
  not_present <- var_list[!var_list %in% names(df)]
  if(length(not_present) > 0) {
    return(not_present)
  } else {
    return("All variables are present.")
  }
}

# Perform checks
eqopp_check <- check_vars_in_df(eqopp_demos, df)
ipeds_check <- check_vars_in_df(ipeds_demos, df)
controls_check <- check_vars_in_df(controls, df)
individual_vars_check <- check_vars_in_df(c(Y, G, T, D), df)

# missing the following variables (since only replicated 1 file cleaning)
list(eqopp_check = eqopp_check, ipeds_check = ipeds_check, controls_check = controls_check, individual_vars_check = individual_vars_check)

head <- df %>% select(all_of(Y), all_of(T), all_of(D), all_of(G))
head(head)

#### honestdid github page ----




head(df, 5)


# The data is a state-level panel with information on health insurance coverage and Medicaid expansion. 
# The variable dins shows the share of low-income childless adults with health insurance in the state. 
# The variable yexp2 gives the year that a state expanded Medicaid coverage under the Affordable Care Act,
# and is missing if the state never expanded.

# Estimate the baseline DiD model - PULKIT 

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
  
  






