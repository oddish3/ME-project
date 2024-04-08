simulate_combined_data <- function() {
  num_states <- 2
  num_periods <- 7
  
  treated_states <- c(2)
  never_treated_states <- c(1)
  
  # Initialize states and times
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Initialize policy to 0, then assign probabilistically to states in the last period
  policy <- rep(0, length(states))
  policy[states == 2 & times >= 3] <- 1  # Policy activates in state 2, last period
  
  # Initialize dose with zeros, then generate for policy-active states in the last period
  dose <- rep(0, length(states))
  dose[policy == 1] <- rnorm(sum(policy), mean = 0.5, sd = 0.2)  # Assuming normal distribution for dose
  
  # Simulate outcomes with base level plus effect of dose
  intercepts <- c(10, 20)  # Base outcome for each state
  common_slope <- 2
  outcome <- intercepts[states] + common_slope * times + dose * 3
  
  # Calculate time since policy and time first treat
  time_since_policy <- ifelse(policy == 1, 1, 0)  # Simplified for 2 periods
  time_first_treat <- rep(NA, length(states))
  time_first_treat[policy == 1] <- 2  # Assuming treatment starts in period 2
  
  # Create the DataFrame
  generated_data <- data.frame(
    state = states,
    time = times,
    policy = policy,
    dose = dose,
    outcome = outcome,
    time_since_policy = time_since_policy,
    time_first_treat = time_first_treat
  )
  generated_data$ever_trt <- ifelse(generated_data$state %in% treated_states, "treated", "never-treated")
  
  
  return(generated_data)
}

s8 = simulate_combined_data()

ggplot(s8, aes(x = time, y = outcome)) + 
  geom_line(aes(col = as.factor(state))) + 
  geom_point(aes(fill = ever_trt, pch = ever_trt)) + labs(title = "Scenario 5 (Staggered timing)")





