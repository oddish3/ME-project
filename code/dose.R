rm(list=ls())

# Load necessary libraries
library(ggplot2)
library(readxl)
library(truncnorm)
#devtools::install_github("shommazumder/binscatteR")

library(ggplot2)
library(binscatteR)
library(np)


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








