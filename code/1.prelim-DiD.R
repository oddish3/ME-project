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
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(dplyr, haven, magrittr, tidyverse, fixest, did, broom, fastDummies, sandwich, lmtest)


# DID_MULTIPLEGT FUNCTION 
# Ensure remotes is available for GitHub installations
pacman::p_load(remotes)

# Install DIDmultiplegt from GitHub if not already installed
if (!"DIDmultiplegt" %in% rownames(installed.packages())) {
  remotes::install_github("shuo-zhang-ucsb/did_multiplegt")
}

# Load DIDmultiplegt
pacman::p_load(DIDmultiplegt)

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
# analysis_sample <- analysis_sample %>% group_by(UNITID, AY_FALL)
 #  or
# library(plm)
# pdata <- pdata.frame(analysis_sample, index = c("UNITID", "AY_FALL"))

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


#  DiD model 
set.seed(9)

# Create dummy variables for simpletiershock
analysis_sample <- fastDummies::dummy_cols(analysis_sample, select_columns = "simpletiershock")

# Get names of all dummy variables 
simpletiershock_star <- grep("^simpletiershock_", names(analysis_sample), value = TRUE)





##### models ----

# filter data
analysis_sample_filtered <- subset(analysis_sample, late_adopter == 0)

# Define vectors for 'eqopp_demos' and 'ipeds_demos' as before
eqopp_demos <- c("k_married", "par_q1", "par_q2", "par_q3", "par_q4", "par_d9", "par_top10pc", "par_top5pc", "par_top1pc", "par_toppt1pc", "par_rank")
ipeds_demos <- c("female", "hispanic", "asian", "black", "nativeamerican", "alien", "unknown", "satmt25", "satmt75", "mi_sat", "use_act_score", "admssn_rate", "mi_admission_rate")

# Dynamically construct your formula
independent_vars <- c("EXPOSED", eqopp_demos, ipeds_demos)
formula_str <- paste("k_rank ~", paste(independent_vars, collapse = " + "), "| UNITID + simpletiershock")
# Note: If 'k_married' should not be doubly included, adjust 'eqopp_demos' accordingly

# Use the formula with the feols function
m1 <- feols(as.formula(formula_str), data = analysis_sample_filtered, vcov = "cluster")

# Print the summary
summary(m1)


# model 2

# Dynamically construct your formula for m2 (same as m1 if variables are unchanged)
independent_vars_m2 <- c("EXPOSED", eqopp_demos, ipeds_demos)
formula_str_m2 <- paste("k_rank ~", paste(independent_vars_m2, collapse = " + "), "| UNITID + simpletiershock")

# Use the formula with the feols function for m2
m2 <- feols(as.formula(formula_str_m2), data = analysis_sample, vcov = "cluster")

# Print the summary for m2
summary(m2)

# model 3

# subset data
analysis_sample_sub <- analysis_sample[analysis_sample$AY_FALL <= 2001, ]
# 

Y <- "k_rank"
G <- "UNITID"
T <- "AY_FALL"
D <- "EXPOSED"
controls <- c(eqopp_demos, simpletiershock_star, ipeds_demos)

# results <-did_multiplegt( #no SEs until next script
#   df = analysis_sample_sub,
#   Y = Y,
#   G = G,
#   T = T,
#   D = D,
#   controls = controls
# )



#### !!!!! takes a while to run

# 
analysis_sample1 <- analysis_sample_sub

results1 <- did_multiplegt(
  df = analysis_sample1,
  Y = Y,
  G = G,
  T = T,
  D = D,
  controls = controls,
  brep      = 10,                  # no. of bootstraps (required for SEs)
  cluster   = 'UNITID',                # variable to cluster SEs on
  parallel  = TRUE                 # run the bootstraps in parallel
)

summary(m1) #1 more ob
summary(m2) # 100 mroe ob
print(results1)




