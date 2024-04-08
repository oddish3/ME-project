rm(list=ls())

library(haven)
library(tidyverse)
library(fixest)
library(fastDummies)
library(lmtest)
library(magrittr)

analysis_sample <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>% 
  filter(twfe_sample == 1 #& late_adopter == 0
         ) %>%  filter(AY_FALL <=2005)



 # %>% filter(AY_FALL <=2003)
 # %>% filter(AY_FALL <=2002)
 # %>% filter(AY_FALL <=2001)


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
# plotting histogram
dose <- as.vector(analysis_sample$EXPOSURE_4YR)
dy <- as.vector(analysis_sample$k_rank)

p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

# The histogram show that a non-trivial fraction of units are untreated whist there is no real
# pattern to the rest, bar the bunching at 4 years.
library(binscatteR)
# binscatter plot of the change in the outcome over time with respect to the dose
binnedout <- binscatter(data=analysis_sample, x="EXPOSURE_4YR", y="k_rank")
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


# devtools::install_github("bcallaway11/qte")
# devtools::install_github("bcallaway11/pte")
# devtools::install_github("bcallaway11/ife")
# devtools::install_github("bcallaway11/BMisc")
  
# library(qte) # for change-in-changes
# library(pte) # for lagged outcomes
# library(ife) # for interactive fixed effects
# library(BMisc)
# library(dplyr)

head(analysis_sample)

# ## changes in changes
# set.seed(1234)
# cic_res <- cic2(yname="income",
#                 tname="year",
#                 idname="id",
#                 gname="group",
#                 data=analysis_sample,
#                 cl=4, # inference uses bootstrap, so using 4 cores here for parallel computing
#                 anticipation=1)
# # show group-time average treatment effects
# cic_res$attgt_results
# 
# 
# ## lagged outcomes
# 
# set.seed(1234)
# lo_res <- pte_default(yname="income",
#                       tname="year",
#                       idname="id",
#                       gname="group",
#                       lagged_outcome_cov = TRUE, # this includes lagged outcome
#                       data=analysis_sample,
#                       anticipation=1)
# 
# # show group-time average treatment effects
# lo_res$att_gt
# 
# ## interactive fixed effects
# 
# set.seed(1234)
# ife_res <- staggered_ife2(yname="income",
#                           tname="year",
#                           idname="id",
#                           gname="group",
#                           data=analysis_sample,
#                           anticipation=0,
#                           nife=1)
# # show group-time average treatment effects
# ife_res$att_gt  






















