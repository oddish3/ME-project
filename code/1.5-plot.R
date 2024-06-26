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



## TABLE 1 ------------------------------------------
library(dplyr)
library(readr)
library(knitr)

# Import data
cohort_graphdata <- read_csv("../original_study/labour-market/data/output/cohort_graphdata.csv")
analysis_sample <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta")

# Replace missing values and merge
analysis_sample <- analysis_sample %>%
  mutate(sat_math = if_else(mi_sat == 1, NA_real_, sat_math)) %>%
  left_join(cohort_graphdata, by = "UNITID") # Adjust join type and by clause as necessary

# Create variables
analysis_sample <- analysis_sample %>%
  mutate(all = 1,
         fb100 = FBIndex <= 100,
         other_fb_schools = late_adopter == 0 & !fb100,
         fb_schools = late_adopter == 0,
         no_fb_schools = late_adopter == 1) %>%
  mutate(across(c(k_married, par_rank, k_rank, k_emprate, female, hispanic, asian, black, frac_users), ~ .x * 100))
analysis_sample$fem

library(dplyr)

# Assuming 'analysis_sample' is your DataFrame prepared in Step 1
# Calculate summary statistics for specified groups

# Group by 'fb_schools' and calculate summary statistics
summary_stats_fb_schools <- analysis_sample %>%
  filter(fb_schools == 1) %>%
  summarise(
    Mean_Earnings = mean(k_mean, na.rm = TRUE),
    Mean_SAT_Math = mean(sat_math, na.rm = TRUE),
    Percentage_Married = mean(k_married, na.rm = TRUE),
    SD_Earnings = sd(k_mean, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  )

# Repeat the process for other groups as necessary, e.g., fb100
summary_stats_fb100 <- analysis_sample %>%
  filter(fb100 == 1) %>%
  summarise(
    Mean_Earnings = mean(k_mean, na.rm = TRUE),
    Mean_SAT_Math = mean(sat_math, na.rm = TRUE),
    Percentage_Married = mean(k_married, na.rm = TRUE),
    SD_Earnings = sd(k_mean, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  )

# Summary for other_fb_schools
summary_stats_other_fb_schools <- analysis_sample %>%
  filter(other_fb_schools == 1) %>%
  summarise(
    Mean_Earnings = mean(k_mean, na.rm = TRUE),
    Mean_SAT_Math = mean(sat_math, na.rm = TRUE),
    Percentage_Married = mean(k_married, na.rm = TRUE),
    SD_Earnings = sd(k_mean, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  )

# Summary for all schools
summary_stats_all_schools <- analysis_sample %>%
  summarise(
    Mean_Earnings = mean(k_mean, na.rm = TRUE),
    Mean_SAT_Math = mean(sat_math, na.rm = TRUE),
    Percentage_Married = mean(k_married, na.rm = TRUE),
    SD_Earnings = sd(k_mean, na.rm = TRUE),
    Count = n(),
    .groups = 'drop'
  )

# Combine summary statistics into one data frame for comparison
combined_summary_stats <- bind_rows(
  fb_schools = summary_stats_fb_schools,
  fb100 = summary_stats_fb100,
  other_fb_schools = summary_stats_other_fb_schools,
  all_schools = summary_stats_all_schools,
  .id = "Group"
)

# View the combined summary statistics
print(combined_summary_stats)

summary(analysis_sample$EXPOSURE_4YR)
head(analysis_sample$EXPOSURE_4YR)
