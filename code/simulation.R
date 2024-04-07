## constant TREATMENT EFFECT

rm(list=ls())
simulate_data <- function() {
  set.seed(0)
  num_states <- 4
  num_periods <- 20
  
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Define which states are treated (e.g., states 3 and 4)
  treated_states <- c(3, 4)
  never_treated_states <- c(1, 2)
  
  # Assign random, staggered starting points for the policy only for treated states
  policy_starts <- c(rep(0, length(never_treated_states)), 
                     sample(round(1:(num_periods)/2), length(treated_states), replace=TRUE))
  policy <- rep(0, length(states))
  
  # Common growth rate for all states (slope)
  common_growth_rate <- 2
  
  # Base outcome level for each state (intercept)
  base_outcome_levels <- c(4, 7, 5, 11)  # For example, base levels for states 1 to 4
  
  # Calculate the outcome with the common slope and different intercepts
  outcome <- base_outcome_levels[states] + common_growth_rate * times
  
  # Apply policy effects only to treated states
  for(i in treated_states){
    policy_index <- which(states == i & times >= policy_starts[i])
    policy[policy_index] <- 1
    outcome[policy_index] <- outcome[policy_index] + 3
  }
  
  outcome <- round(outcome)
  time_since_policy <- ifelse(!is.na(policy_starts[states]), pmax(0, times - policy_starts[states]), NA)
  time_first_treat <- policy_starts[states]
  
  time_as_date <- rep(seq.Date(from=as.Date("2015-01-01"), by="month", length.out=num_periods), times=num_states)
  time_first_trt_date <- rep(NA, length(states))
  nonzero_policy_indices <- which(policy > 0 & !is.na(policy_starts[states]))
  time_first_trt_date[nonzero_policy_indices] <- as.Date("2015-01-01") + months(policy_starts[states[nonzero_policy_indices]] - 1)
  
  # Create the DataFrame with 'ever_trt' variable
  generated_data <- data.frame(
    state = states,
    time = times,
    policy = policy,
    outcome = outcome,
    time_since_policy = time_since_policy,
    time_first_treat = time_first_treat,
    time_as_date = time_as_date,
    time_first_trt_date = time_first_trt_date
  )
  
  generated_data$time_first_trt_date <- as.Date(generated_data$time_first_trt_date)
  
  # Assign 'ever_trt' status
  generated_data$ever_trt <- ifelse(generated_data$state %in% treated_states, "treated", "never-treated")
  
  return(generated_data)
}


s5 <- simulate_data()


s5 %<>% mutate(ever_trt = case_when(state %in% c("3", "4") ~ "treated",
                                    state %in% c("1", "2") ~ "never-treated"))
s5$state = as.factor(s5$state)
ggplot(s5, aes(x = time, y = outcome)) + 
  geom_line(aes(col = state)) + 
  geom_point(aes(fill = state, pch = ever_trt)) + labs(title = "Scenario 5 (Staggered timing)")


s5_mod <- lm(outcome ~  policy + state + factor(time), data = s5)
tidy(s5_mod)

s5 %<>% mutate(state_n = as.numeric(as.character(state)),
               policy_n = as.numeric(as.character(policy)))
#bacon decomp says no problems
#only comparing treated vs untreated and the average effect estimate is 2
s5_bacon <- bacon(outcome ~ policy_n,
                  data = s5,
                  id_var = "state_n",
                  time_var = "time")

# hetrogenos

simulate_data <- function() {
  set.seed(0)
  num_states <- 4
  num_periods <- 20
  
  # Define which states are treated (e.g., states 3 and 4)
  treated_states <- c(3, 4)
  
  # Initialize states and times
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Set base outcome for each state to create an offset at the beginning
  state_outcome_offsets <- c(10, 20, 30, 40)
  common_growth_rate <- 2
  
  # Assign policy start times for treated states only
  policy_starts <- c(NA, NA, 6, 12) # Assigning fixed policy start times for simplicity
  treatment_effect_growth_rate <- 1 # Treatment effect increase per time period after policy start
  
  # Initialize the outcome variable with different offsets for each state
  outcome <- state_outcome_offsets[states] + common_growth_rate * times
  
  # Apply the treatment effect to treated states
  for (i in seq_along(states)) {
    if (states[i] %in% treated_states && !is.na(policy_starts[states[i]])) {
      # Calculate the number of periods since the treatment started
      periods_since_treatment <- times[i] - policy_starts[states[i]]
      if (periods_since_treatment >= 0) {
        # Increase outcome by the treatment effect, which is linear over time
        outcome[i] <- outcome[i] + periods_since_treatment * treatment_effect_growth_rate
      }
    }
  }
  
  # Round outcome to the nearest whole number
  outcome <- round(outcome)
  
  # Create the DataFrame with 'ever_trt' variable
  generated_data <- data.frame(
    state = factor(states),
    time = times,
    outcome = outcome,
    ever_trt = ifelse(states %in% treated_states, "treated", "untreated")
  )
  
  return(generated_data)
}

# Generate the data
s6 <- simulate_data()


s6 %<>% mutate(ever_trt = case_when(state %in% c("3", "4") ~ "treated",
                                    state %in% c("1", "2") ~ "never-treated"))
s6$state = as.factor(s6$state)
ggplot(s6, aes(x = time, y = outcome)) + 
  geom_line(aes(col = state)) + 
  geom_point(aes(fill = state, pch = ever_trt)) + labs(title = "Scenario 5 (Staggered timing)")


s6_mod <- lm(outcome ~  policy + state + factor(time), data = s6)
tidy(s6_mod)

s6 %<>% mutate(state_n = as.numeric(as.character(state)),
               policy_n = as.numeric(as.character(policy)))
#bacon decomp says no problems
#only comparing treated vs untreated and the average effect estimate is 2
s6_bacon <- bacon(outcome ~ policy_n,
                  data = s6,
                  id_var = "state_n",
                  time_var = "time")

simulate_data_dh <- function() {
  set.seed(0)
  num_states <- 4
  num_periods <- 20
  
  # Define which states are treated
  treated_states <- c(3, 4)
  
  # Initialize states and times
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Set base outcome for each state and a common growth rate for untreated states
  state_outcome_offsets <- c(10, 20, 30, 40)
  common_growth_rate <- 2
  
  # Assign staggered policy start times for treated states only
  policy_starts <- c(NA, NA, 6, 12) # Staggered start times for simplicity
  
  # Different treatment effect growth rates for each treated state to create heterogeneity
  treatment_effect_growth_rates <- c(0, 0, 0.5, 1.0) # No effect for untreated states
  
  # Initialize the outcome variable with different offsets for each state
  outcome <- state_outcome_offsets[states] + common_growth_rate * times
  
  # Apply the dynamic and heterogeneous treatment effect to treated states
  for (i in seq_along(states)) {
    if (states[i] %in% treated_states && !is.na(policy_starts[states[i]])) {
      # Calculate the number of periods since the treatment started
      periods_since_treatment <- times[i] - policy_starts[states[i]]
      if (periods_since_treatment >= 0) {
        # Increase outcome by the treatment effect, which is linear and heterogeneous over time
        outcome[i] <- outcome[i] + periods_since_treatment * treatment_effect_growth_rates[states[i]]
      }
    }
  }
  
  # Round outcome to the nearest whole number
  outcome <- round(outcome)
  
  # Create the DataFrame with 'ever_trt' variable indicating treatment status
  generated_data <- data.frame(
    state = factor(states),
    time = times,
    outcome = outcome,
    ever_trt = ifelse(states %in% treated_states, "treated", "untreated")
  )
  
  return(generated_data)
}

# Generate the data
s8 <- simulate_data_dh()
s8 %<>% mutate(ever_trt = case_when(state %in% c("3", "4") ~ "treated",
                                    state %in% c("1", "2") ~ "never-treated"))
s8$state = as.factor(s8$state)
ggplot(s8, aes(x = time, y = outcome)) + 
  geom_line(aes(col = state)) + 
  geom_point(aes(fill = state, pch = ever_trt)) + labs(title = "Scenario 5 (Staggered timing)")


s8_mod <- lm(outcome ~  policy + state + factor(time), data = s8)
tidy(s8_mod)

s8 %<>% mutate(state_n = as.numeric(as.character(state)),
               policy_n = as.numeric(as.character(policy)))
#bacon decomp says no problems
#only comparing treated vs untreated and the average effect estimate is 2
s8_bacon <- bacon(outcome ~ policy_n,
                  data = s8,
                  id_var = "state_n",
                  time_var = "time")
