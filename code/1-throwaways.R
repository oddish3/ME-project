# Create the formula string dynamically
formula_str <- paste("k_rank ~ EXPOSED + k_married +", paste(dummy_vars, collapse = " + "), "| UNITID + AY_FALL")

# Convert the string to a formula
formula <- as.formula(formula_str)


# ============================
#  regressions 
# ============================
result <- feols(formula, data = analysis_sample, vcov = ~UNITID) # ${eqopp_demos} ${ipeds_demos}) havent been included
summary(result)


  
# seperate replication

# seperate function for DiD estimation

library(fixest)
did_estout <- function(data, seed = 9) {
  set.seed(seed)

  # Assuming eqopp_demos and ipeds_demos are character vectors containing variable names
  controls <- paste(c("k_married", _simpletiershock, eqopp_demos, ipeds_demos), collapse = " + ")

  formula_str <- paste("k_rank ~ EXPOSED +", controls, "| UNITID + AY_FALL")

  # Filtering data within the function call
  data_filtered <- data[data$AY_FALL <= 2001, ]

  # Using the formula with as.formula to convert string to formula
  est <- feols(as.formula(formula_str), data = data_filtered, vcov = ~UNITID, cluster = "UNITID")

  return(list(N_effect_0 = summary(est, robust = TRUE)$N,
              didmgt_estimates = coef(est),
              didmgt_variances = vcov(est)))
}
did_estout <- function(data, seed = 9) {
  set.seed(seed)
  est <- feols(k_rank ~ EXPOSED | UNITID + AY_FALL | k_married + _simpletiershock* + eqopp_demos + ipeds_demos, data = data[data$AY_FALL <= 2001, ], vcov = ~UNITID, cluster = "UNITID")
  list(N_effect_0 = summary(est, robust = TRUE)$N,
       didmgt_estimates = coef(est),
       didmgt_variances = vcov(est))
}
#store and format results
results <- did_estout(data = analysis_sample)
b_did <- as.data.frame(results$didmgt_estimates, row.names = "y1")
colnames(b_did) <- "EXPOSED"
V_did <- as.data.frame(results$didmgt_variances, row.names = "EXPOSED")
colnames(V_did) <- "EXPOSED"
print(V_did)
print(b_did)
#post results and use in later analysis
# Assume the function returns a list of results
N <- results$N_effect_0


#models
library(fixest)
library(dplyr)
library(broom)

# Placeholder for storing models, similar to eststo in Stata
models <- list()

# Model 1: Equivalent to 'reghdfe' with condition and fixed effects
m1 <- feols(k_rank ~ EXPOSED + k_married + eqopp_demos + ipeds_demos | UNITID + simpletiershock | 0 | UNITID, 
            data = filter(df, late_adopter == 0))
models$m1 <- tidy(m1)

# Model 2: Without the late_adopter condition
m2 <- feols(k_rank ~ EXPOSED + k_married + eqopp_demos + ipeds_demos | UNITID + simpletiershock | 0 | UNITID, df)
models$m2 <- tidy(m2)

# Model 3: Assuming 'did_estout' is translated into a custom R function
# Since 'did_estout' specifics are not provided, this part of the code would need to be adjusted based on its implementation
m3 <- did_estout_R(df)  # Placeholder for the translated 'did_estout' function
models$m3 <- tidy(m3)

# Adding additional information similar to 'estadd' in Stata
# This would require a custom approach in R, as there's no direct equivalent to 'estadd'
# Assuming 'add_model_info' is a function you define to append information to model summaries
models$m1 <- add_model_info(models$m1, uniFE = "Yes", yearFE = "Yes", democontrols = "Yes")
models$m2 <- add_model_info(models$m2, uniFE = "Yes", yearFE = "Yes", democontrols = "Yes")
models$m3 <- add_model_info(models$m3, uniFE = "Yes", yearFE = "Yes", democontrols = "Yes")

# 'label var' equivalent in R
# In R, variable labels can be added via factor levels or using the 'labelled' package for more detailed data management
# For display purposes, it might be simpler to rename the variable directly or adjust plot labels and table captions manually

# Run the second regression without the late_adopter condition
m2 <- lfe::felm(formula = k_rank ~ EXPOSED + k_married + unlist(eqopp_demos) + unlist(ipeds_demos) |
                  UNITID + simpletiershock |
                  0 | 0,
                data = your_data,
                cluster = ~UNITID)

# Run the did_estout function, assuming it's properly translated and defined in R
m3 <- did_estout(your_data)

# Add scalar indicators to the stored results to simulate 'estadd' from Stata
attributes(m1)$uniFE <- "Yes"
attributes(m2)$uniFE <- "Yes"
attributes(m3)$uniFE <- "Yes"

attributes(m1)$yearFE <- "Yes"
attributes(m2)$yearFE <- "Yes"
attributes(m3)$yearFE <- "Yes"

attributes(m1)$democontrols <- "Yes"
attributes(m2)$democontrols <- "Yes"
attributes(m3)$democontrols <- "Yes"

# Relabel the EXPOSED variable in your_data to "Exposed to Facebook"
names(your_data)[names(your_data) == "EXPOSED"] <- "Exposed to Facebook"




