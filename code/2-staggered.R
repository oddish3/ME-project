# ------------------------------------------
#           Staggered DiD Script 
# ------------------------------------------

rm(list = ls())
# Install DIDmultiplegt from GitHub if not already installed
if (!"HonestDiD" %in% rownames(installed.packages())) {
  remotes::install_github("asheshrambachan/HonestDiD")
}

# Load DIDmultiplegt
pacman::p_load(DIDmultiplegt, readr)

# cohort_earn_panel
class(df$k_married)

# analysis_sample <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>%
#   filter(twfe_sample == 1)

Y <- "k_rank"
G <- "UNITID"
T <- "AY_FALL"
D <- "EXPOSED"

head <- df %>% select(all_of(Y), all_of(T), all_of(D), all_of(G))
head(head)


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


#### honestdid github page ----


library(here)
library(dplyr)
library(did)
library(haven)
library(ggplot2)
library(fixest)
library(HonestDiD)
library(readr)

df <- read_csv("~/Documents/R_folder/MSc/ME/ME-project/data/processed/cohort_earn_panel.csv")
head(df, 5)


# The data is a state-level panel with information on health insurance coverage and Medicaid expansion. 
# The variable dins shows the share of low-income childless adults with health insurance in the state. 
# The variable yexp2 gives the year that a state expanded Medicaid coverage under the Affordable Care Act,
# and is missing if the state never expanded.

# Estimate the baseline DiD

#where D is 1 if a unit is first treated in 2014 and 0 otherwise.

#Keep years before 2016. Drop the 2016 cohort
df_nonstaggered <- df %>% filter(year < 2016 &
                                   (is.na(yexp2)| yexp2 != 2015) )

#Create a treatment dummy
df_nonstaggered <- df_nonstaggered %>% mutate(D = case_when( yexp2 == 2014 ~ 1,
                                                             T ~ 0))

#Run the TWFE spec
twfe_results <- fixest::feols(dins ~ i(year, D, ref = 2013) | stfips + year,
                              cluster = "stfips",
                              data = df_nonstaggered)


betahat <- summary(twfe_results)$coefficients #save the coefficients
sigma <- summary(twfe_results)$cov.scaled #save the covariance matrix


fixest::iplot(twfe_results)





