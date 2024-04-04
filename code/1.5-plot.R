## FIGURE 1 ------------------------------------------
# figure works, table doesnt

# Load necessary libraries
library(tidyverse)
library(lubridate)

rm(list=ls())
df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta")

# Keep the necessary columns
df <- df %>% select(simpletier, DateJoinedFB, FBName, super_opeid, barrons, UNITID)

# Handle duplicates and convert the date format
df <- df %>% distinct() %>% mutate(date = mdy(DateJoinedFB))

# Create a sequence of dates
date_seq <- seq(from = min(df$date, na.rm = TRUE), to = max(df$date, na.rm = TRUE), by = "day")

# Initialize a list to store data frames for each tier
tier_data <- list()

# Loop over each tier to calculate fraction of schools with FB access
tiers <- unique(df$barrons)
for(t in tiers) {
  # For each date, calculate the fraction of schools that have joined FB
  tier_df <- data.frame(date = date_seq)
  tier_df$fb_access <- sapply(tier_df$date, function(d) {
    mean(df$date <= d & df$barrons == t, na.rm = TRUE)
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
