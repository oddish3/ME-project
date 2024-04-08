# simulating data
rm(list=ls())

set.seed(0) # Ensure reproducibility
n <- 1000 # Number of observations

# Simulate data
data <- data.frame(
  id = rep(1:100, each = 10), # 100 entities, 10 time periods each
  time = rep(1:10, times = 100), # 10 time periods
  D = runif(n, min = 0, max = 1) # Continuous treatment variable with values between 0 and 100# + rnorm(n, mean = 0, sd = 5) # Outcome variable with a known linear relationship to D
  )

# Simulate an outcome variable with a known linear relationship to D and random noise
beta <- 0.5 # Known treatment effect parameter
data$outcome <- 10 + beta * data$D + rnorm(n, mean = 0, sd = 5) # Intercept of 10, noise with sd of 5

data$outcome <- 10 + 0.5 * data$D + rnorm(n, mean = 0, sd = 5) # Intercept of 10, noise with sd of 5

data$time <- as.factor(data$time)
data$id <- as.factor(data$id)
data$outcome <- as.numeric(data$outcome) # Ensure this is numeric
data$D <- as.numeric(data$D) # Ensure the treatment variable is numeric


# Visualize the relationship between D and outcome
# plot(data$D, data$outcome, xlab = "Treatment Intensity (D)", ylab = "Outcome", main = "Simulated Data")
# summary(data$D)
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

### fix
calculate_continuous_twfe_derivative <- function(data, outcome_var = "outcome", treatment_var = "D", time_var = "time", id_var = "id", granularity = 10) {
  
  # Define lower and upper bounds for treatment levels
  dL <- min(data[[treatment_var]][data[[treatment_var]] > 0])
  dU <- max(data[[treatment_var]])
  
  # Create a grid of treatment levels from dL to dU
  dose_grid <- seq(dL, dU, length.out = granularity)
  
  # Placeholder for results
  results <- data.frame(type = character(), weight = numeric(), avg_est = numeric(), stringsAsFactors = FALSE)
  
  # Initialize placeholders for previous estimate and level
  prev_estimate <- NA
  prev_level <- NA
  
  # Iterate over dose_grid to calculate weights and estimates
  for(current_level in dose_grid) {
    # Calculate weight using the custom function across the dose grid
    current_weight <- cont_twfe_weights(current_level, data[[treatment_var]])
    
    # Estimate the effect for the segment using a fixed effects model
    current_formula <- reformulate(c(paste0("I(", treatment_var, " >= ", current_level, ")"), paste0(time_var, " + ", id_var)), response = outcome_var)
    est_model <- fixest::feols(fml = current_formula, data = data)
    current_estimate <- coef(est_model)[2]
    
    # Store the initial estimate directly if no previous estimate is available
    if (is.na(prev_estimate)) {
      results <- rbind(results, data.frame(type = NA, weight = current_weight, avg_est = current_estimate))
    } else {
      # Calculate and store the change in effect directly if previous estimate is available
      change_in_effect <- current_estimate - prev_estimate
      # Construct a simplified type description, if needed
      simplified_type_description <- paste("Change from", prev_level, "to", current_level)
      results <- rbind(results, data.frame(type = simplified_type_description, weight = current_weight, avg_est = change_in_effect))
    }
    
    # Update previous values after the condition check
    prev_estimate <- current_estimate
    prev_level <- current_level
  }
  
  
  # Rescale weights, if necessary
  results$weight <- results$weight / sum(results$weight)
  
  return(results)
}

results <- calculate_continuous_twfe_derivative(data)

# View the results
print(results)




### ----- 
# Function to generate DiD data with two periods and continuous treatment
generate_did_data <- function(N, ATT, ACRT) {
  # Time periods
  periods <- c("t-1", "t")
  
  # Generate a binary treatment indicator
  treatment <- rbinom(N, 1, 0.5)
  
  # Continuous treatment variable for treated units in post-treatment period
  medicare_share_1983[treatment == 1] <- rtruncnorm(sum(treatment), a = 0, b = 1, mean = 0.5, sd = 0.16)
  
  # Define the quadratic relationship coefficients
  a <- -1
  b <- 1
  c <- 0.1
  d_capital_labor_ratio <- a * medicare_share_1983^2 + b * medicare_share_1983 + c
  
  # Potential outcomes without treatment in both periods
  Y0_t_minus_1 <- rnorm(N, mean = 10, sd = 2)  # Pre-treatment
  Y0_t <- rnorm(N, mean = 10, sd = 2)          # Post-treatment
  
  # Potential outcomes with treatment in the post-treatment period
  Y1_t <- Y0_t + ATT + ACRT * medicare_share_1983
  
  # Observed outcomes
  Y_t <- ifelse(treated == 1, Y1_t, Y0_t)  # Post-treatment outcome depends on treatment status
  
  # Combine the data into a data frame
  data_t <- data.frame(id = 1:N, period = periods[2], treated = treated, treatment_intensity = treatment_intensity, Y = Y_t)
  
  # Bind the two periods together
  data <- rbind(data_t_minus_1, data_t)
  
  return(data)
}

# # Example usage with known ATT and ACRT
# N <- 1000    # Number of observations
# ATT <- 0.5   # Assumed known average treatment effect on the treated
# ACRT <- 0.3  # Assumed known average causal response to treatment
# data <- generate_did_data(N, ATT, ACRT)

# Inspect the first few rows of the generated data
head(data)



