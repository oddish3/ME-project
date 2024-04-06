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
tiers <- unique(df1$barrons)
for(t in tiers) {
  # For each date, calculate the fraction of schools that have joined FB
  tier_df <- data.frame(date = date_seq)
  tier_df$fb_access <- sapply(tier_df$date, function(d) {
    mean(df1$date <= d & df1$barrons == t, na.rm = TRUE)
  })
  tier_df$tier = t
  tier_data[[as.character(t)]] <- tier_df
}

# Combine all tiers into one data frame
combined_df <- bind_rows(tier_data)

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
 df1 <- df1 %>%
   group_by(date, UNITID) %>%
   summarise()
 
 # Create a sequence of dates for plotting
 date_seq <- seq(from = min(df1$date, na.rm = TRUE), to = max(df1$date, na.rm = TRUE), by = "day")
 
 # Calculate the cumulative number of colleges with FB access for each date
 cumulative_df <- data.frame(date = date_seq)
 cumulative_df$fb_access <- sapply(cumulative_df$date, function(d) {
   sum(df1$date <= d, na.rm = TRUE)
 })
 
 # Normalizing fb_access to fraction by dividing by the total number of unique colleges
 total_colleges <- length(unique(df1$UNITID))
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
df2 = df %>%  select(simpletier, DateJoinedFB, FBName, super_opeid, barrons, UNITID, late_adopter, EXPOSED, EXPOSURE_4YR, k_rank, AY_FALL)

df2$DateJoinedFB <- as.Date(df2$DateJoinedFB, format = "%m/%d/%Y")
df2$year_joinedFB <- format(df2$DateJoinedFB, "%Y")


dfdf <- df2 %>% 
  mutate(
    year_treated = case_when(
      AY_FALL <= 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB == 2005 ~ 2005,
      TRUE ~ NA  # Default case if neither condition is met
    )
  )

 df2 <- df %>% 
   select(simpletier, DateJoinedFB, FBName, super_opeid, barrons, UNITID, late_adopter, EXPOSED, EXPOSURE_4YR, k_rank, AY_FALL)
 
 # Convert DateJoinedFB to Date format and create a year_joinedFB variable
 df2$DateJoinedFB <- as.Date(df2$DateJoinedFB, format = "%m/%d/%Y")
 df2$year_joinedFB <- format(df2$DateJoinedFB, "%Y")
 min(df2$DateJoinedFB, na.rm = TRUE)
 max(df2$DateJoinedFB, na.rm = TRUE)
 
 # Create year_treated variable
 dfdf <- df2 %>% 
   mutate(
     year_treated = case_when(
       AY_FALL <= 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
       AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB <= 2005 ~ 2005,
       TRUE ~ 2006  # Use NA_real_ for numeric NA
     )
   )
 
 harvard <- dfdf %>%  select(FBName, AY_FALL, DateJoinedFB, year_treated,  EXPOSED, EXPOSURE_4YR, UNITID) %>% filter(UNITID == 166027) 
 harvard
 

 ggplot(dfdf, aes(x = AY_FALL, y = k_rank, color = as.factor(year_treated))) +
   geom_point() +
   theme_minimal()
 
 
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
