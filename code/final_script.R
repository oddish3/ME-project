# NOTE 
# binscatteR requires Java to be installed on your machine.




# ==============================================================================
# Social Media and Labour market Outcomes Continuous DiD
# ==============================================================================
#
# Author: 10710007 &  &
# Version: 13-03-2024
#
# ==============================================================================

## Section 1 : preliminary diff in diff, replication of ARMONA TWOWAYFE.do file --------
rm(list = ls()) 

# Packages
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(dplyr, haven, magrittr, tidyverse, fixest, did, broom, fastDummies, sandwich, lmtest, TwoWayFEWeights)


# DID_MULTIPLEGT FUNCTION 
# Ensure remotes is available for GitHub installations
pacman::p_load(remotes)

# Install DIDmultiplegt from GitHub if not already installed
if (!"DIDmultiplegt"  %in% rownames(installed.packages())) {
  remotes::install_github("shuo-zhang-ucsb/did_multiplegt")
}

# Load DIDmultiplegt
pacman::p_load(DIDmultiplegt, DIDmultiplegtDYN)

# Data 
# ==============================================================================
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



#modify k rank variable
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
  #brep      = 4,                  # no. of bootstraps (required for SEs)
  cluster   = 'UNITID',                # variable to cluster SEs on
)

weights1 <- TwoWayFEWeights::twowayfeweights(analysis_sample1, Y, G, T, D, type = ("feTR"), 
                                             controls = controls)
# model summary of weights1 for exporting to google doc
weights1


summary(m1) #1 more ob
summary(m2) # 100 mroe ob

# Section 1 Appendix (omitted for brevity)
# print(results1)
# 
# results2 <- did_multiplegt(
#   df = analysis_sample1,
#   Y = Y,
#   G = G,
#   T = T,
#   D = "EXPOSURE_4YR",
#   controls = controls,
#   #brep      = 4,                  # no. of bootstraps (required for SEs)
#   cluster   = 'UNITID',                # variable to cluster SEs on
# )
# print(results2)
# 
# results11 <- did_multiplegt_dyn(analysis_sample1, 
#                                 Y, 
#                                 G,
#                                 T, 
#                                 D, 
#                                 controls = controls, 
#                                 #cluster = 'UNITID'
# )
# print(results11)
# 
# results22 <- did_multiplegt_dyn(analysis_sample1, 
#                                 Y, 
#                                 G,
#                                 T, 
#                                 "EXPOSURE_4YR", 
#                                 controls = controls)
# print(results22)


### Section 2 : CONTINUOUS DIFF IN DIFF SIMULATION ----
# credit to https://mixtape-sessions.github.io/Frontiers-in-DID/Exercises/Exercise-2/exercise2a_sol.html
# for the 2 cont DiD functions

rm(list=ls())

# Load necessary libraries
pacman::p_load(devtools, remotes)

# Install DIDmultiplegt from GitHub if not already installed
if (!"binscatteR" %in% rownames(installed.packages())) {
  remotes::install_github("shommazumder/binscatteR")
}
pacman::p_load(ggplot2, readxl, truncnorm, np, binscatteR)

# Set seed for reproducibility
set.seed(42)

# Generate dataset with specific statistical characteristics
generate_dataset <- function() {
  # Define the number of hospitals
  num_schools <- 748
  
  # Generate a binary treatment indicator where 10% are untreated (0) and 90% are treated (1)
  treatment <- rbinom(num_schools, 1, 0.9)
  
  # Initialize EXPOSURE_4YR with zeros for all hospitals
  EXPOSURE_4YR <- rep(0, num_schools)
  
  # For the treated hospitals, generate dose from a normal distribution centered around 0.5 with sd of 0.16
  EXPOSURE_4YR[treatment == 1] <- rtruncnorm(sum(treatment), a = 0, b = 1, mean = 0.5, sd = 0.16)
  
  # Calculate the dependent variable d_k_rank using the quadratic equation
  d_k_rank <- -4 * (EXPOSURE_4YR-.5)^2 + 1 
  
  # Create DataFrame
  data.frame(
    uni_ident = 1:num_schools,
    d_k_rank = d_k_rank,
    EXPOSURE_4YR = EXPOSURE_4YR
  )
}

# Generate the dataset

analysis_sample_simulated <- generate_dataset()
summary(analysis_sample_simulated)


#' @param l a particular value of the treatment for which to compute weights
#' @param D an nx1 vector containing doses for all units
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}


#' nonparametric estimates of att(d|d) and acrt(d|d)
#' @param dy the change in the outcome over time
#' @param dose the amount of the treatment
#' @return list( 
#'            local_effects - data frame containing the dose and estimates of 
#'              att(dose) and acrt(dose)
#'            att.overall - an estimate of the overall att
#'            acrt.overall - an estimate of the overall acrt
#'          )
cont_did <- function(dy, dose) {
  # choose bandwidth
  bw <- np::npregbw(formula=dy ~ dose,
                    regtype="ll",
                    bws=1.06,
                    bwscaling=TRUE,
                    bandwidth.compute=FALSE)
  # estimate att and acrt nonparametrically
  out <- np::npreg(bws=bw, gradients=TRUE, exdat=dose)
  
  # order from smallest to largest dose and drop untreated
  this_order <- order(dose)
  dose <- dose[this_order]
  dy <- dy[this_order]
  att.d <- out$mean[this_order]
  acrt.d <- out$grad[,1][this_order]
  att.d <- att.d[dose>0]
  acrt.d <- acrt.d[dose>0]
  att.overall <- mean(att.d)
  acrt.overall <- mean(acrt.d)
  
  return(list(local_effects=data.frame(dose=dose[dose>0],
                                       att.d=att.d,
                                       acrt.d=acrt.d),
              att.overall=att.overall,
              acrt.overall=acrt.overall))
}

cont_did_est <- function(data) {
  # Convert dose and dy using the 'data' argument
  dose <- as.vector(data$EXPOSURE_4YR)
  dy <- as.vector(data$d_k_rank)
  
  # First Plot: Histogram of dose
  p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
    geom_histogram()
  
  # Binscatter plot of the change in the outcome over time with respect to the dose
  binnedout <- binscatter(data=data, x="EXPOSURE_4YR", y="d_k_rank")
  
  # TWFE model
  twfe <- lm(dy ~ dose)
  twfe_summary <- summary(twfe)$coefficients
  twfe_est <- summary(twfe)$coefficients[2,1]
  
  # Using the cont_did function to estimate the ATT etc.
  cont_res <- cont_did(dy, dose)
  ATT <- cont_res$att.overall
  ACRT <- cont_res$acrt.overall
  # Plots as functions of the dose and estimates of ATT etc.
  plot_df <- cont_res$local_effects
  colnames(plot_df) <- c("dose", "att", "acrt")
  
  att_plot <- ggplot(plot_df, aes(x=dose, y=att)) +
    geom_hline(yintercept=0, color="red", linetype="dashed") +
    geom_line() +
    theme_bw()
  
  acrt_plot <- ggplot(plot_df, aes(x=dose, y=acrt)) +
    geom_hline(yintercept=0, color="red", linetype="dashed") +
    geom_line() +
    theme_bw()
  
  # Density of the dose
  dL <- min(dose[dose>0])
  dU <- max(dose)
  dose_grid <- seq(dL, dU, length.out=100)
  frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
    geom_density(colour = "darkblue", linewidth = 1.2) +
    xlim(c(min(dose_grid), max(dose_grid))) +
    ylab("Density weights") +
    xlab("Dose") +
    ylim(c(0,3)) + 
    labs(title="Density of dose")
  
  # TWFE weights
  twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)
  plot_df <- cbind.data.frame(twfe_weights, dose_grid)
  
  twfe_weights_plot <- ggplot(data=plot_df,
                              mapping=aes(x = dose_grid, y = twfe_weights)) +
    geom_line(colour = "darkblue", linewidth = 1.2) +
    xlim(c(min(dose_grid), max(dose_grid))) +
    ylab("TWFE weights") +
    xlab("Dose") +
    geom_vline(xintercept = mean(dose), colour="black", linewidth = 0.5, linetype = "dotted") +
    ylim(c(0,3)) +
    labs(title="TWFE weights")
  
  cat("ATT:", ATT)
  cat("\n")
  cat("ACRT:", ACRT)
  cat("\n")
  cat()
  cat("TWFE Estimate:", twfe_est)
  # Return a list of all the generated results
  return(list(ACRT = ACRT, ATT = ATT, histogram=p, binscatter=binnedout, twfe_summary=twfe_summary, 
              att_plot=att_plot, acrt_plot=acrt_plot, frq_weights_plot=frq_weights_plot, 
              twfe_weights_plot=twfe_weights_plot))
  
}

sim_results <- cont_did_est(analysis_sample_simulated)


sim_results$ATT


# SECTION 3 : CONTINUOUS DIFF IN DIFF ESTIMATION ------------------------------------------
# credit to https://mixtape-sessions.github.io/Frontiers-in-DID/Exercises/Exercise-2/exercise2a_sol.html
# for the 2 cont DiD functions

rm(list=ls())

pacman::p_load(dplyr, haven, magrittr, tidyverse, fixest, did, broom, fastDummies, sandwich, lmtest)

analysis_sample <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>% 
  filter(twfe_sample == 1 #& late_adopter == 0
  ) %>%  filter(AY_FALL <=2005)


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

#identical_elements <- analysis_sample$D == analysis_sample$EXPOSED
#identical_elements # basically same thing - few differences probably bc of samples (main / twfe?)


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

# continuous DiD ----------------------------------------------------------------
analysis_sample$DateJoinedFB <- as.Date(analysis_sample$DateJoinedFB, format = "%m/%d/%Y")

# To extract the year and create a new column for it
analysis_sample$year_treated <- format(analysis_sample$DateJoinedFB, "%Y")



# computes weights from TWFE regression ------------------------------------------

#' @param l a particular value of the treatment for which to compute weights
#' @param D an nx1 vector containing doses for all units
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

#' nonparametric estimates of att(d|d) and acrt(d|d)
#' @param dy the change in the outcome over time
#' @param dose the amount of the treatment
#' @return list( 
#'            local_effects - data frame containing the dose and estimates of 
#'              att(dose) and acrt(dose)
#'            att.overall - an estimate of the overall att
#'            acrt.overall - an estimate of the overall acrt
#'          )
cont_did <- function(dy, dose) {
  # choose bandwidth
  bw <- np::npregbw(formula=dy ~ dose,
                    regtype="ll",
                    bws=1.06,
                    bwscaling=TRUE,
                    bandwidth.compute=FALSE)
  # estimate att and acrt nonparametrically
  out <- np::npreg(bws=bw, gradients=TRUE, exdat=dose)
  
  # order from smallest to largest dose and drop untreated
  this_order <- order(dose)
  dose <- dose[this_order]
  dy <- dy[this_order]
  att.d <- out$mean[this_order]
  acrt.d <- out$grad[,1][this_order]
  att.d <- att.d[dose>0]
  acrt.d <- acrt.d[dose>0]
  att.overall <- mean(att.d)
  acrt.overall <- mean(acrt.d)
  
  return(list(local_effects=data.frame(dose=dose[dose>0],
                                       att.d=att.d,
                                       acrt.d=acrt.d),
              att.overall=att.overall,
              acrt.overall=acrt.overall))
}


# main analysis - break down by years treated ----------------------------------------------------------

# subset data
analysis_sample_cleaned <- analysis_sample %>%
  filter(!is.na(k_rank) & !is.na(AY_FALL)) %>%
  arrange(UNITID, AY_FALL)

# Step 2: Calculate Yearly Changes for each UNITID
analysis_sample_diffs <- analysis_sample_cleaned %>%
  group_by(UNITID) %>%
  mutate(next_k_rank = lead(k_rank), # Get next year's k_rank
         next_AY_FALL = lead(AY_FALL), # Get next year's AY_FALL
         d_k_rank = next_k_rank - k_rank) %>% # Calculate the difference
  ungroup() %>%
  select(FBName, UNITID, AY_FALL, next_AY_FALL, k_rank, d_k_rank, EXPOSURE_4YR) %>%
  filter(!is.na(d_k_rank))

# example of what the code does 
analysis_sample_diffs %>% filter(FBName == "Harvard") %>% head(5)

analysis_sample1999 <- analysis_sample_diffs[analysis_sample_diffs$AY_FALL <= 1999, ]
analysis_sample2000 <- analysis_sample_diffs[analysis_sample_diffs$AY_FALL <= 2000, ]
analysis_sample2001 <- analysis_sample_diffs[analysis_sample_diffs$AY_FALL <= 2001, ]
analysis_sample2002 <- analysis_sample_diffs[analysis_sample_diffs$AY_FALL <= 2002, ]
analysis_sample2003 <- analysis_sample_diffs[analysis_sample_diffs$AY_FALL <= 2003, ]
analysis_sample2004 <- analysis_sample_diffs[analysis_sample_diffs$AY_FALL <= 2004, ]

# Calculate the difference in k_rank between 2001 and 1999 for each UNITID
cont_did_est <- function(data) {
  # Convert dose and dy using the 'data' argument
  dose <- as.vector(data$EXPOSURE_4YR)
  dy <- as.vector(data$d_k_rank)
  
  # First Plot: Histogram of dose
  p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
    geom_histogram()
  
  # Binscatter plot of the change in the outcome over time with respect to the dose
  binnedout <- binscatter(data=data, x="EXPOSURE_4YR", y="d_k_rank")
  
  # TWFE model
  twfe <- lm(dy ~ dose)
  twfe_summary <- summary(twfe)$coefficients
  twfe_est <- summary(twfe)$coefficients[2,1]
  
  # Using the cont_did function to estimate the ATT etc.
  cont_res <- cont_did(dy, dose)
  ATT <- cont_res$att.overall
  ACRT <- cont_res$acrt.overall
  # Plots as functions of the dose and estimates of ATT etc.
  plot_df <- cont_res$local_effects
  colnames(plot_df) <- c("dose", "att", "acrt")
  
  att_plot <- ggplot(plot_df, aes(x=dose, y=att)) +
    geom_hline(yintercept=0, color="red", linetype="dashed") +
    geom_line() +
    theme_bw()
  
  acrt_plot <- ggplot(plot_df, aes(x=dose, y=acrt)) +
    geom_hline(yintercept=0, color="red", linetype="dashed") +
    geom_line() +
    theme_bw()
  
  # Density of the dose
  dL <- min(dose[dose>0])
  dU <- max(dose)
  dose_grid <- seq(dL, dU, length.out=100)
  frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
    geom_density(colour = "darkblue", linewidth = 1.2) +
    xlim(c(min(dose_grid), max(dose_grid))) +
    ylab("Density weights") +
    xlab("Dose") +
    ylim(c(0,3)) + 
    labs(title="Density of dose")
  
  # TWFE weights
  twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)
  plot_df <- cbind.data.frame(twfe_weights, dose_grid)
  
  twfe_weights_plot <- ggplot(data=plot_df,
                              mapping=aes(x = dose_grid, y = twfe_weights)) +
    geom_line(colour = "darkblue", linewidth = 1.2) +
    xlim(c(min(dose_grid), max(dose_grid))) +
    ylab("TWFE weights") +
    xlab("Dose") +
    geom_vline(xintercept = mean(dose), colour="black", linewidth = 0.5, linetype = "dotted") +
    ylim(c(0,3)) +
    labs(title="TWFE weights")
  
  cat("ATT:", ATT)
  cat("\n")
  cat("ACRT:", ACRT)
  cat("\n")
  cat()
  cat("TWFE Estimate:", twfe_est)
  # Return a list of all the generated results
  return(list(ACRT = ACRT, ATT = ATT, histogram=p, binscatter=binnedout, twfe_summary=twfe_summary, 
              att_plot=att_plot, acrt_plot=acrt_plot, frq_weights_plot=frq_weights_plot, 
              twfe_weights_plot=twfe_weights_plot))
  
}
results2000 <- cont_did_est(analysis_sample2000)
results2001 <- cont_did_est(analysis_sample2001)
results2002 <- cont_did_est(analysis_sample2002)
results2003 <- cont_did_est(analysis_sample2003)
results2004 <- cont_did_est(analysis_sample2004)

#main results
results_diff <- cont_did_est(analysis_sample_diffs)

# various plots have been saved from here by simply calling object and saving in IDE





# SECTION 4 : FINAL SECTION : Figures ----------------------------------------------------------------

## FIGURE 1 (from paper) ------------------------------------------
# figure works, table doesnt

rm(list=ls())

df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>% 
  filter(twfe_sample == 1) 

# Keep the necessary columns
df1 <- df %>% select(simpletier, DateJoinedFB, FBName, super_opeid, barrons, UNITID, late_adopter, EXPOSED, EXPOSURE_4YR, k_rank, AY_FALL)

# Handle duplicates and convert the date format
df1 <- df1 %>% distinct() %>% mutate(date = mdy(DateJoinedFB))

# Create a sequence of dates
date_seq <- seq(from = min(df1$date, na.rm = TRUE), to = max(df1$date, na.rm = TRUE), by = "day")

# Initialize a list to store data frames for each tier
tier_data <- list()

# Loop over each tier to calculate fraction of schools with FB access
# Assume df1, date_seq, first_day, and last_day are already defined
tiers <- unique(df1$barrons, na.rm = TRUE)
tier_data <- list() # Initialize the list to store data frames for each tier

for(t in tiers) {
  tier_df <- data.frame(date = date_seq) # Initialize data frame for the current tier
  
  # Conditional calculation based on tier
  tier_df$fb_access <- sapply(tier_df$date, function(d) {
    if(t == 7) {
      mean(df1$date <= d, na.rm = TRUE) # For the last tier, the condition does not depend on 'barrons'
    } else {
      mean(df1$date <= d & df1$barrons == t, na.rm = TRUE) # For other tiers
    }
  })
  
  tier_df$tier <- t
  
  # Optionally, calculate and append the label with the number of unique UNITID for the current tier
  # This part is more complex in R and might require additional steps not shown here
  
  tier_data[[as.character(t)]] <- tier_df # Store the data frame for the current tier in the list
}

# Combine all tiers into one data frame
combined_df <- do.call(rbind, tier_data)


# Plotting
ggplot(combined_df, aes(x = date, y = fb_access, color = factor(tier))) +
  geom_line() +
  labs(title = "Facebook Rollout Across School Tiers",
       x = "Date",
       y = "Fraction of Schools with FB Access",
       color = "Tier") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) # Customize as needed

# Save the plot
ggsave("../docs/figures/fb_rollout.png", width = 11, height = 8.5, dpi = 300)


# plots --------------

dfdf2 = df %>%  select(simpletier, DateJoinedFB, FBName, super_opeid, barrons, UNITID, late_adopter, EXPOSED, EXPOSURE_4YR, k_rank, AY_FALL)

dfdf2$DateJoinedFB <- as.Date(dfdf2$DateJoinedFB, format = "%m/%d/%Y")
dfdf2$year_joinedFB <- as.Date(format(dfdf2$DateJoinedFB, "%Y"), format = "%Y")

dfdf2 %<>% 
  mutate(
    year_treated = case_when(
      AY_FALL < 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL < 2005 & EXPOSED > 0 & year_joinedFB == 2005 ~ 2005,
      AY_FALL < 2000 ~0, 
      TRUE ~ 0 # Default case if neither condition is met
    )
  )

dfdf2 <- df %>% 
  select(simpletier, DateJoinedFB, FBName, super_opeid, barrons, UNITID, late_adopter, EXPOSED, EXPOSURE_4YR, k_rank, AY_FALL)

# Convert DateJoinedFB to Date format and create a year_joinedFB variable
dfdf2$DateJoinedFB <- as.Date(dfdf2$DateJoinedFB, format = "%m/%d/%Y")
dfdf2$year_joinedFB <- format(dfdf2$DateJoinedFB, "%Y")

# Create year_treated variable
dfdf1<- dfdf2 %>% 
  mutate(
    year_treated = case_when(
      AY_FALL <= 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB <= 2005 ~ 2005,
      TRUE ~ 0  # Use NA_real_ for numeric NA
    )
  )

# 1.5 binary treatment (2004) ------

dfdf1 <- dfdf1 %>%
  group_by(UNITID) %>%
  mutate(first.treat = ifelse(any(year_treated == 2004), 1, 0)) %>%
  ungroup()

dfdf2 <- dfdf1 #%>% filter(AY_FALL <= 2000) 

average_k_rank <- dfdf2 %>%
  group_by(AY_FALL, first.treat, EXPOSED) %>% summarise(average_k_rank = mean(k_rank, na.rm = TRUE))

ggplot(average_k_rank, aes(x = AY_FALL, y = average_k_rank, color = as.factor(first.treat))) +
  geom_line() + # Using geom_line to connect average points over AY_FALL
  geom_point()  + 
  # Optionally add points to highlight the actual averages
  labs(title = "Average K_Rank by Cohort and Year Treated",
       x = "Cohort",
       y = "Average K_Rank",
       color = "College Treated in 2004") +
  theme_minimal() +
  geom_vline(xintercept = 2000, linetype = "dashed")

ggsave("../docs/figures/average_k_rank_treat_Exposed.png", width = 11, height = 8.5, dpi = 300)

dfdf2$compet <- ifelse(dfdf2$barrons <=4, 1, 0)
average_k_rank <- dfdf2 %>%
  group_by(AY_FALL, first.treat, compet) %>% summarise(average_k_rank = mean(k_rank, na.rm = TRUE))

ggplot(average_k_rank, aes(x = AY_FALL, y = average_k_rank, 
                           color = as.factor(first.treat), 
                           group = interaction(compet, first.treat),
                           linetype = as.factor(compet))) +
  geom_line() + # Using geom_line to connect average points over AY_FALL for each tier and year_treated
  geom_point() + # Optionally add points to highlight the actual averages
  labs(title = "Average K_Rank by Cohort, Competitiveness and Year Treated",
       x = "Cohort",
       y = "Average K_Rank",
       color = "College Treated in 2004",
       linetype = "Classed as Competitive") + # Adding a label for linetype legend
  theme_minimal() +
  geom_vline(xintercept = 2000, color = "#00B0B9", linetype = "dashed") 

ggsave("../docs/figures/average_k_rank_compet.png", width = 11, height = 8.5, dpi = 300)

