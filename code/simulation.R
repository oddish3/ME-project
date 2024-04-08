## constant TREATMENT EFFECT staggered timing sim -----
library(bacondecomp)
library(ggplot2)
library(dplyr)
library(broom)
library(lubridate)
library(magrittr)

rm(list=ls())

simulate_data <- function() {
  set.seed(0)
  num_states <- 4
  num_periods <- 20
  
  # Define which states are treated (e.g., states 3 and 4)
  treated_states <- c(3, 4)
  never_treated_states <- c(1, 2)
  
  # Initialize states and times
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Assign random, staggered starting points for the policy only for treated states
  policy_starts <- c(0, 0, 6, 9)
  policy <- rep(0, length(states))
  
  # Common growth rate for all states (slope)
  common_growth_rate <- 2
  
  # Base outcome level for each state (intercept)
  base_outcome_levels <- c(4, 7, 9, 11)  # For example, base levels for states 1 to 4
  
  # Calculate the outcome with the common slope and different intercepts
  outcome <- base_outcome_levels[states] + common_growth_rate * times
  
  # Apply policy effects only to treated states
  for(i in treated_states){
    policy_index <- which(states == i & times >= policy_starts[i])
    policy[policy_index] <- 1
    outcome[policy_index] <- outcome[policy_index] + 4
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

# hetrogenos treatment effect staggered timing sim ----

simulate_data <- function() {
  set.seed(0)
  num_states <- 4
  num_periods <- 20
  
  # Define which states are treated (e.g., states 3 and 4)
  treated_states <- c(3, 4)
  never_treated_states <- c(1, 2)
  
  # Initialize states and times
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Assign staggered starting points for the policy only for treated states
  policy_starts <- c(0, 0, 6, 9)
  policy <- rep(0, length(states))
  
  # Set base outcome for each state to create an offset at the beginning
  intercepts <- c(10, 15, 29, 30)
  common_slope <- 2  # Common slope before treatment
  
  # Calculate the outcome with the common slope and different intercepts
  outcome <- intercepts[states] + common_slope * times
  
  # Additional slope after treatment starts
  additional_slope <- 2  # This makes the slope steeper by 1 unit after treatment
  
  # Apply policy effects only to treated states
  for(i in treated_states){
    # Find indexes after policy starts for treated states
    policy_index <- which(states == i & times >= policy_starts[i])
    policy[policy_index] <- 1
    # Calculate the number of periods since the policy started
    periods_since_policy <- times[policy_index] - policy_starts[i]
    
    # Increase the slope for the outcome after the policy starts
    outcome[policy_index] <- outcome[policy_index] + additional_slope * periods_since_policy
  }
  
  # Rounding the outcome to simulate reported data
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

s6 <- simulate_data()

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
s6_bacon


# heterogenous and dynamic treatment effect staggered timing sim ----
rm(list=ls())
# Generate the data
simulate_data <- function() {
  set.seed(0)
  num_states <- 4
  num_periods <- 20
  
  # Define which states are treated (e.g., states 3 and 4)
  treated_states <- c(3, 4)
  never_treated_states <- c(1, 2)
  
  # Initialize states and times
  states <- rep(1:num_states, each=num_periods)
  times <- rep(1:num_periods, times=num_states)
  
  # Assign staggered starting points for the policy only for treated states
  policy_starts <- c(0, 0, 6, 9)  # NAs for never treated states
  policy <- rep(0, length(states))
  
  # Set base outcome for each state to create an offset at the beginning
  intercepts <- c(10, 15, 29, 30)
  common_slope <- 2  # Common slope before treatment
  
  # Define additional slopes for each treated state
  additional_slopes <- c(0, 0, 1, 2)  # No additional slope for never treated states
  
  # Calculate the outcome with the common slope and different intercepts
  outcome <- intercepts[states] + common_slope * times
  
  # Apply different policy effects to each treated state
  for(i in seq_along(treated_states)) {
    state <- treated_states[i]
    additional_slope <- additional_slopes[state]
    policy_index <- which(states == state & times >= policy_starts[state])
    policy[policy_index] <- 1
    # Calculate the number of periods since the policy started
    periods_since_policy <- times[policy_index] - policy_starts[state]
    
    # Increase the slope for the outcome after the policy starts
    outcome[policy_index] <- outcome[policy_index] + additional_slope * periods_since_policy
  }
  
  # Rounding the outcome to simulate reported data
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

s8 <- simulate_data()

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
s8_bacon
