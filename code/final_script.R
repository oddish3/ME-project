## preliminary diff in diff, replication of ARMONA -----


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

### TWOWAYFE.do ----

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

summary(m1) #1 more ob
summary(m2) # 100 mroe ob
print(results1)

results2 <- did_multiplegt(
  df = analysis_sample1,
  Y = Y,
  G = G,
  T = T,
  D = "EXPOSURE_4YR",
  controls = controls,
  #brep      = 4,                  # no. of bootstraps (required for SEs)
  cluster   = 'UNITID',                # variable to cluster SEs on
)
print(results2)

results11 <- did_multiplegt_dyn(analysis_sample1, 
                                Y, 
                                G,
                                T, 
                                D, 
                                controls = controls, 
                                #cluster = 'UNITID'
)
print(results11)

results22 <- did_multiplegt_dyn(analysis_sample1, 
                                Y, 
                                G,
                                T, 
                                "EXPOSURE_4YR", 
                                controls = controls)
print(results22)


### CONTINUOUS DIFF IN DIFF SIMULATION ----

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



#' @hospital_id - @hospital identifier
#' @d_capital_labor_ratio - the @change in the capital labor ratio for a hospital from 1983 to 1985, this is the (change in the) outcome variable
#' @medicare_share_1983 - the @fraction of medicare patients in the hospital in 1983, this is the continuous treatment variable.


# generating the ATT and plotting it
dose <- seq(.01,.99,by=.01)
ATT <- -4*(dose-.5)^2 + 1 
p <- ggplot(data.frame(ATT=ATT, dose=dose), aes(x=dose, y=ATT)) + 
  geom_line() + ylim(c(0,2))
p

# which implies the following average causal response to treatment
ACRT <- -8*(dose-.5) 
ggplot(data.frame(ACRT=ACRT, dose=dose), aes(x=dose, y=ACRT)) +
  geom_line() + ylim(c(-6,6))

# ex 1 - plotting histogram
dose <- analysis_sample_simulated$EXPOSURE_4YR
dy <- analysis_sample_simulated$d_k_rank

p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

binnedout <- binscatter(data=analysis_sample_simulated, x="EXPOSURE_4YR", y="d_k_rank")
binnedout

summary(dose)
summary(dy)

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients

cont_res <- cont_did(dy, dose)
cont_res$att.overall

cont_res$acrt.overall

plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()


ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

dL <- min(dose[dose>0])
dU <- max(dose)
# density of the dose
dose_grid <- seq(dL, dU, length.out=100)
frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid)))+
  ylab("Density weights") +
  xlab("Dose") +
  ylim(c(0,3)) + 
  labs(title="Density of dose")
frq_weights_plot

twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

plot_df <- cbind.data.frame(twfe_weights, dose_grid)

twfe_weights_plot <- ggplot(data=plot_df,
                            mapping=aes(x = dose_grid,
                                        y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid),
         max(dose_grid)))+
  ylab("TWFE weights") +
  xlab("Dose") +
  geom_vline(xintercept = mean(dose),
             colour="black",
             linewidth = 0.5,
             linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights")

twfe_weights_plot





# CONTINUOUS DIFF IN DIFF ESTIMATION

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


# break down by years treated ----------------------------------------------------------

# 1 years treated ----------------------------------------------------------

analysis_sample1 <- analysis_sample[analysis_sample$EXPOSURE_4YR <=1, ]
# plotting histogram
dose <- as.vector(analysis_sample1$EXPOSURE_4YR)
dy <- as.vector(analysis_sample1$k_rank)

p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

# The histogram show that a non-trivial fraction of units are untreated whist there is no real
# pattern to the rest, bar the bunching at 4 years.
# binscatter plot of the change in the outcome over time with respect to the dose
binnedout <- binscatter(data=analysis_sample1, x="EXPOSURE_4YR", y="k_rank")
binnedout

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients

# using the cont_did function provided above to estimate the ATT etc
#Plot and as functions of the dose and provide estimates of ATT etc

cont_res <- cont_did(dy, dose)

cont_res$att.overall
cont_res$acrt.overall

plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

dL <- min(dose[dose>0])
dU <- max(dose)
# density of the dose
dose_grid <- seq(dL, dU, length.out=100)
frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid)))+
  ylab("Density weights") +
  xlab("Dose") +
  ylim(c(0,3)) + 
  labs(title="Density of dose")
frq_weights_plot

twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

plot_df <- cbind.data.frame(twfe_weights, dose_grid)

twfe_weights_plot <- ggplot(data=plot_df,
                            mapping=aes(x = dose_grid,
                                        y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid),
         max(dose_grid)))+
  ylab("TWFE weights") +
  xlab("Dose") +
  geom_vline(xintercept = mean(dose),
             colour="black",
             linewidth = 0.5,
             linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights")

twfe_weights_plot

# year treated 2 ----------------------------------------------------------
analysis_sample1 <- analysis_sample[analysis_sample$EXPOSURE_4YR <=2, ]
# plotting histogram
dose <- as.vector(analysis_sample1$EXPOSURE_4YR)
dy <- as.vector(analysis_sample1$k_rank)

p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

# The histogram show that a non-trivial fraction of units are untreated whist there is no real
# pattern to the rest, bar the bunching at 4 years.
# binscatter plot of the change in the outcome over time with respect to the dose
binnedout <- binscatter(data=analysis_sample1, x="EXPOSURE_4YR", y="k_rank")
binnedout

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients

# using the cont_did function provided above to estimate the ATT etc
#Plot and as functions of the dose and provide estimates of ATT etc

cont_res <- cont_did(dy, dose)

cont_res$att.overall
cont_res$acrt.overall

plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

dL <- min(dose[dose>0])
dU <- max(dose)
# density of the dose
dose_grid <- seq(dL, dU, length.out=100)
frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid)))+
  ylab("Density weights") +
  xlab("Dose") +
  ylim(c(0,3)) + 
  labs(title="Density of dose")
frq_weights_plot

twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

plot_df <- cbind.data.frame(twfe_weights, dose_grid)

twfe_weights_plot <- ggplot(data=plot_df,
                            mapping=aes(x = dose_grid,
                                        y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid),
         max(dose_grid)))+
  ylab("TWFE weights") +
  xlab("Dose") +
  geom_vline(xintercept = mean(dose),
             colour="black",
             linewidth = 0.5,
             linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights")

twfe_weights_plot


# year treated 3 ----------------------------------------------------------

analysis_sample1 <- analysis_sample[analysis_sample$EXPOSURE_4YR <=3, ]
# plotting histogram
dose <- as.vector(analysis_sample1$EXPOSURE_4YR)
dy <- as.vector(analysis_sample1$k_rank)

p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

# The histogram show that a non-trivial fraction of units are untreated whist there is no real
# pattern to the rest, bar the bunching at 4 years.
# binscatter plot of the change in the outcome over time with respect to the dose
binnedout <- binscatter(data=analysis_sample1, x="EXPOSURE_4YR", y="k_rank")
binnedout

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients

# using the cont_did function provided above to estimate the ATT etc
#Plot and as functions of the dose and provide estimates of ATT etc

cont_res <- cont_did(dy, dose)

cont_res$att.overall
cont_res$acrt.overall

plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

dL <- min(dose[dose>0])
dU <- max(dose)
# density of the dose
dose_grid <- seq(dL, dU, length.out=100)
frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid)))+
  ylab("Density weights") +
  xlab("Dose") +
  ylim(c(0,3)) + 
  labs(title="Density of dose")
frq_weights_plot

twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

plot_df <- cbind.data.frame(twfe_weights, dose_grid)

twfe_weights_plot <- ggplot(data=plot_df,
                            mapping=aes(x = dose_grid,
                                        y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid),
         max(dose_grid)))+
  ylab("TWFE weights") +
  xlab("Dose") +
  geom_vline(xintercept = mean(dose),
             colour="black",
             linewidth = 0.5,
             linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights")

twfe_weights_plot

# years treated 4 full sample ----------------------------------------------------------

analysis_sample1 <- analysis_sample[analysis_sample$EXPOSURE_4YR <=4, ]
# plotting histogram
dose <- as.vector(analysis_sample1$EXPOSURE_4YR)
dy <- as.vector(analysis_sample1$k_rank)

p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

# The histogram show that a non-trivial fraction of units are untreated whist there is no real
# pattern to the rest, bar the bunching at 4 years.
# binscatter plot of the change in the outcome over time with respect to the dose
binnedout <- binscatter(data=analysis_sample1, x="EXPOSURE_4YR", y="k_rank")
binnedout

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients

# using the cont_did function provided above to estimate the ATT etc
#Plot and as functions of the dose and provide estimates of ATT etc

cont_res <- cont_did(dy, dose)

cont_res$att.overall
cont_res$acrt.overall

plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

dL <- min(dose[dose>0])
dU <- max(dose)
# density of the dose
dose_grid <- seq(dL, dU, length.out=100)
frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid)))+
  ylab("Density weights") +
  xlab("Dose") +
  ylim(c(0,3)) + 
  labs(title="Density of dose")
frq_weights_plot

twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

plot_df <- cbind.data.frame(twfe_weights, dose_grid)

twfe_weights_plot <- ggplot(data=plot_df,
                            mapping=aes(x = dose_grid,
                                        y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid),
         max(dose_grid)))+
  ylab("TWFE weights") +
  xlab("Dose") +
  geom_vline(xintercept = mean(dose),
             colour="black",
             linewidth = 0.5,
             linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights")

twfe_weights_plot




#### figures ----------------------------------------------------------------



## FIGURE 1 (from paper) ------------------------------------------
# figure works, table doesnt
rm(list=ls())
# Load necessary libraries
library(tidyverse)
library(lubridate)

rm(list=ls())
df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta")

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
#ggsave("fb_rollout.pdf", width = 11, height = 8.5, dpi = 300)


# Stagegred intro plot ------------------------------------------

# Create a unique identifier for each day and college combination
dfdf1 <- df1 %>%
  group_by(date, UNITID) %>%
  summarise()

# Create a sequence of dates for plotting
date_seq <- seq(from = min(dfdf1$date, na.rm = TRUE), to = max(dfdf1$date, na.rm = TRUE), by = "day")

# Calculate the cumulative number of colleges with FB access for each date
cumulative_df <- data.frame(date = date_seq)
cumulative_df$fb_access <- sapply(cumulative_df$date, function(d) {
  sum(dfdf1$date <= d, na.rm = TRUE)
})

# Normalizing fb_access to fraction by dividing by the total number of unique colleges
total_colleges <- length(unique(dfdf1$UNITID))
cumulative_df$fb_access <- cumulative_df$fb_access / total_colleges

# Plotting
ggplot(cumulative_df, aes(x = date, y = fb_access)) +
  geom_line() +
  labs(title = "Staggered Facebook Rollout Across Colleges",
       x = "Date",
       y = "Fraction of Colleges with FB Access") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format())



# DiD plots? --------------
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
dfdf <- dfdf2 %>% 
  mutate(
    year_treated = case_when(
      AY_FALL <= 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB <= 2005 ~ 2005,
      TRUE ~ 0  # Use NA_real_ for numeric NA
    )
  )

harvard <- dfdf %>%  select(FBName, AY_FALL, DateJoinedFB, year_treated,  EXPOSED, EXPOSURE_4YR, UNITID) %>% filter(UNITID == 166027) 
harvard


ggplot(dfdf, aes(x = AY_FALL, y = k_rank, color = as.factor(year_treated))) +
  geom_point() +
  theme_minimal()

# 1.5 binary treatment (2004) ------
dfdf1 = dfdf

# dfdf1$first.treat <- as.numeric(ifelse(dfdf1$EXPOSED > 0 & dfdf1$year_joinedFB == 2004 & dfdf1$year_treated == 2004 & dfdf1$AY_FALL <2004 , 1, 0))

dfdf1 <- dfdf1 %>%
  group_by(UNITID) %>%
  mutate(first.treat = ifelse(any(year_treated == 2004), 1, 0)) %>%
  ungroup()

dfdf1 %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat)
dfdf1 %>% filter(UNITID == 100706) %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat)
dfdf1 %>% filter(UNITID == 243744) %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat)

dfdf2 <- dfdf1 #%>% filter(AY_FALL <= 2000) 


average_k_rank <- dfdf2 %>%
  group_by(AY_FALL, first.treat) %>% summarise(average_k_rank = mean(k_rank, na.rm = TRUE))

ggplot(average_k_rank, aes(x = AY_FALL, y = average_k_rank, color = as.factor(first.treat))) +
  geom_line() + # Using geom_line to connect average points over AY_FALL
  geom_point()  + 
  # Optionally add points to highlight the actual averages
  labs(title = "Average K_Rank by Cohort and Initial Rollout",
       x = "Cohort",
       y = "Average K_Rank",
       color = "Uni Treated in 2004") +
  theme_minimal() +
  geom_vline(xintercept = 2000, slope = 0, linetype = "dashed")

# Calculate the average k_rank for each AY_FALL and year_treated
average_k_rank <- dfdf %>%
  group_by(AY_FALL, year_treated) %>%
  summarise(average_k_rank = mean(k_rank, na.rm = TRUE)) %>%
  ungroup() # Ensure the data is not grouped for plotting

# Plotting the averages
ggplot(average_k_rank, aes(x = AY_FALL, y = average_k_rank, color = as.factor(year_treated))) +
  geom_line() + # Using geom_line to connect average points over AY_FALL
  geom_point() + # Optionally add points to highlight the actual averages
  labs(title = "Average K_Rank by Cohort and Year Treated",
       x = "Cohort",
       y = "Average K_Rank",
       color = "Year Treated") +
  theme_minimal()
dfdf1$year_joinedFB <-  ifelse(is.na(dfdf1$year_joinedFB), 0, dfdf1$year_joinedFB)

dfdf1 <- dfdf1 %>%
  mutate(post = case_when(
    AY_FALL >= 2000 ~ 1, 
    TRUE ~ 0
  ))

dfdf1$year_joinedFB <- (as.numeric(dfdf2$year_joinedFB))
dfdf1$year_joinedFB <- ifelse(is.na(dfdf2$year_joinedFB), 0,dfdf2$year_joinedFB)
dfdf1$init.treat <- ifelse(dfdf2$year_joinedFB == 2004 & dfdf2$first.treat == 2004, 1,0)

dfdf1 %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post)
dfdf1 %>% filter(UNITID == 100706) %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post)
dfdf1 %>% filter(UNITID == 243744) %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post)

dfdf2 <- dfdf2 %>% select(UNITID, FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post, 
                          k_rank, EXPOSED, EXPOSURE_4YR, year_joinedFB)
# year = cohort. state = UNITID
# year_treated = year that cohort had access to fb
# k_rank rank of student in national cohort
head(df1)


average_k_rank <- dfdf2 %>%
  group_by(AY_FALL, init.treat) %>%
  summarise(average_k_rank = mean(k_rank, na.rm = TRUE)) %>%
  ungroup() # Ensure the data is not grouped for plotting

ggplot(average_k_rank, aes(x = AY_FALL, y = average_k_rank)) +
  geom_line(aes(color = as.factor(init.treat))) + # Use color to differentiate 'post' status
  labs(title = "Average K_Rank by Cohort, Post, and Init.Treat Status",
       x = "Cohort",
       y = "Average K_Rank",
       color = "Treat Status",
       shape = "Init.Treat Status") +
  theme_minimal() +
  geom_vline(xintercept = 2000, linetype="dashed", color = "red") # Example vertical line for a significant year

dfdf2 %<>% filter(year_joinedFB == 2004 | year_joinedFB == 0)

dfdf2 <- dfdf2 %>%
  mutate(post = case_when(
    AY_FALL >= 2000 ~ 1, 
    TRUE ~ 0
  ))

dfdf2$year_joinedFB <- ifelse(is.na(dfdf2$year_joinedFB), 0,dfdf2$year_joinedFB)
dfdf2$init.treat <- ifelse(dfdf2$year_joinedFB == 2004 & dfdf2$first.treat == 2004, 1,0)

dfdf2 %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post)
dfdf2 %>% filter(UNITID == 101435) %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post)
dfdf2 %>% filter(UNITID == 243744) %>% select(FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post)

dfdf2 <- dfdf2 %>% select(UNITID, FBName, AY_FALL, DateJoinedFB, year_treated, first.treat, init.treat, post, 
                          k_rank, EXPOSED, EXPOSURE_4YR, year_joinedFB)
# year = cohort. state = UNITID
# year_treated = year that cohort had access to fb
# k_rank rank of student in national cohort
head(df1)

average_k_rank <- dfdf2 %>%
  group_by(AY_FALL, init.treat) %>%
  summarise(average_k_rank = mean(k_rank, na.rm = TRUE)) %>%
  ungroup() # Ensure the data is not grouped for plotting

ggplot(average_k_rank, aes(x = AY_FALL, y = average_k_rank)) +
  geom_line(aes(color = as.factor(init.treat))) + # Use color to differentiate 'post' status
  labs(title = "Average K_Rank by Cohort, Post, and Init.Treat Status",
       x = "Cohort",
       y = "Average K_Rank",
       color = "Treat Status",
       shape = "Init.Treat Status") +
  theme_minimal() +
  geom_vline(xintercept = 2000, linetype="dashed", color = "red") # Example vertical line for a significant year
