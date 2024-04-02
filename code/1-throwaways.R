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