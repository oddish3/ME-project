rm(list=ls())
setwd("~/Documents/R_folder/MSc/ME/ME-project/original_study/labour-market")


library(dplyr)
library(readr)
library(lubridate)
library(haven)


# Load FB dates
fbdates <- read_csv('data/input/FB_introduction_dates_augmented.csv') %>%
  rename(FBIndex = id, FBName = `FB Name`) %>%
  filter(Ambiguous == 0) %>%
  select(UNITID, FBIndex, FBName, DateJoinedFB) %>%
  filter(!is.na(UNITID))

fbdates$UNITID <- as.integer(fbdates$UNITID)
fbschools <- unique(fbdates$UNITID)

# Load institutional characteristics
ic <- read_dta('data/output/ic.dta', col_select = c(unitid, year, control, iclevel, opeid)) %>%
  mutate(OPEID6 = substr(trimws(opeid), 1, 6),
         OPEID6 = as.integer(OPEID6),
         year = as.integer(year)) %>%
  filter(OPEID6 != "", !is.na(OPEID6)) %>%
  filter(year >= 1998 & year <= 2009)

# Limit to four-year schools
# Change Wright State to consistently be a four-year so it doesn't drop out between years
# ic$iclevel <- as.character(ic$iclevel) # IMPORTANT - MAY BE A BREAKING CHANGE DUE TO TRANSLATION INTO R
# ic$control <- as.character(ic$control) # IMPORTANT - MAY BE A BREAKING CHANGE DUE TO TRANSLATION INTO R
ic$iclevel[ic$unitid == 206613] <- 1

ic <- ic %>%
  filter(control != -3, iclevel == 1) %>%
  rename(OPEID = opeid, opeid = OPEID6, UNITID = unitid, AY_FALL = year) # changed by inspecting value

# Import super opeid xwalk from MRC, and the Barrons selectivity ranking
opeid_xwalk <- read_csv('data/input/mrc_table11.csv') %>%
  select(super_opeid, opeid)

barronsdf <- read_csv('data/input/mrc_table10.csv') %>%
  select(super_opeid, barrons)

opeid_xwalk <- left_join(opeid_xwalk, barronsdf, by = "super_opeid")

# Merge in the super-opeid xwalk to IC
ic <- inner_join(ic, opeid_xwalk, by = "opeid") %>%
  filter(super_opeid > 0) %>%
  select(-iclevel, -control)

# Identify the schools actually in the sample
super_opeid_fb_set <- unique(ic$super_opeid[ic$UNITID %in% fbschools])
ic$late_adopter <- as.integer(!(ic$super_opeid %in% super_opeid_fb_set))
ic$barrons <- as.integer(ic$barrons)

# Merge IC into fbdates
newdf_insamp <- inner_join(ic, fbdates, by = "UNITID")
newdf_outsamp <- filter(ic, late_adopter == TRUE)
newdf <- bind_rows(newdf_insamp, newdf_outsamp)

newdf$OPEID6 <- as.numeric(substr(newdf$OPEID, 1, 6))

# Construct access variable
enter_time <- paste0("7/1/", newdf$AY_FALL)
exit_time <- paste0("6/30/", newdf$AY_FALL + 4)
#


# Convert 'exit_time' and 'newdf$DateJoinedFB' to Date objects
exit_time_date <- as.Date(exit_time, format = "%m/%d/%Y")
newdf$DateJoinedFB_date <- parse_date_time(newdf$DateJoinedFB, orders = c("ymd", "mdy"))

#saving to df
newdf$exit_time_date <- exit_time_date
newdf$enter_time <- enter_time

# Calculate exposure time
exposure_time <- as.numeric(difftime(exit_time_date, newdf$DateJoinedFB_date, units = "days"))

exposure_time[exposure_time < 0] <- 0
# Handle NA values for exposure_time, assuming NA dates are treated as 0 exposure
exposure_time[is.na(exposure_time)] <- 0

newdf$exposure_time <- exposure_time




# Assuming 'newdf' and 'exposure_time' have been defined in your R environment already
newdf$exposure_time <- exposure_time
newdf$EXPOSURE_4YR <- exposure_time / 365
newdf$EXPOSURE_4YR[newdf$EXPOSURE_4YR > 4] <- 4
newdf$EXPOSED <- as.integer(exposure_time > (30 / 365))

# Load the earnings measures
birthearn <- read_csv('data/input/mrc_table3.csv')
robustearn <- read_csv('data/input/mrc_table5.csv')

# Get the relevant columns we need
earncols <- c('super_opeid','cohort','count','tier','multi','region','state','cz','cfips','czname',
              'female','par_mean','par_median','par_rank',
              'k_married','k_rank','k_mean','k_median','k_median_nozero','k_0inc',
              'k_q1','k_q2','k_q3','k_q4','k_q5','k_top10pc','k_top5pc','k_top1pc',
              'par_top10pc','par_top5pc','par_top1pc','par_toppt1pc',
              'par_q1','par_q2','par_q3','par_q4','par_q5')

# Add columns that start with specific prefixes
birthearn_cols_with_prefixes <- names(birthearn)[grepl('k_rank_cond_parq|k_married_cond_parq', names(birthearn))]
earncols <- c(earncols, birthearn_cols_with_prefixes)

robustcols <- names(robustearn)[grepl('married|k_rank|k_0inc|k_mean|par_', names(robustearn))]
robustearn <- robustearn %>%
  select(super_opeid, cohort, all_of(robustcols))

earncols <- unique(c(earncols, robustcols))

# Merge in earnings data
birthearn <- left_join(birthearn, robustearn, by = c('super_opeid', 'cohort'))
birthearn <- birthearn %>%
  select(all_of(earncols)) %>%
  right_join(opeid_xwalk, by = 'super_opeid') # Using right_join to replicate 'outer' join effect in pandas



# Assuming 'birthearn' dataframe is already loaded

# Initialize columns
birthearn$k_rank_m1 <- 0
birthearn$k_rank_m2 <- 0

# Calculate differences
krank_80_90 <- birthearn$k_q5 - birthearn$k_top10pc
krank_90_95 <- birthearn$k_top10pc - birthearn$k_top5pc
krank_95_99 <- birthearn$k_top5pc - birthearn$k_top1pc
krank_1_20 <- birthearn$k_q1 - birthearn$k_0inc

# Define percentiles and kranks
pctiles <- c(0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.99, 1)
kranks <- list(krank_1_20, birthearn$k_q2, birthearn$k_q3, birthearn$k_q4, 
               krank_80_90, krank_90_95, krank_95_99, birthearn$k_top1pc)

p_l <- birthearn$k_0inc
kcdf <- rep(0, nrow(birthearn))

for (i in seq_along(kranks)) {
  p <- pctiles[i]
  condmean <- (p + p_l) / 2
  condm2 <- (p^2 + p * p_l + p_l^2) / 3
  birthearn$k_rank_m1 <- birthearn$k_rank_m1 + kranks[[i]] * condmean
  birthearn$k_rank_m2 <- birthearn$k_rank_m2 + kranks[[i]] * condm2
  p_l <- p
}

# Calculate variance
birthearn$k_rank_var <- birthearn$k_rank_m2 - (birthearn$k_rank_m1)^2

# Descriptive statistics for new columns and k_rank
summary(select(birthearn, k_rank_m1, k_rank_m2, k_rank_var, k_rank))

# Renaming and dropping columns
birthearn <- birthearn %>%
  rename(OPEID6 = opeid) %>%
  select(-c(super_opeid, barrons))

# Adding AY_FALL
birthearn$AY_FALL <- birthearn$cohort + 18

# Assuming 'newdf' is already loaded and assuming 'beadf' is loaded from 'data/input/bea_regions.csv'
beadf <- read_csv('data/input/bea_regions.csv')

# Merging dataframes
newdf <- left_join(newdf, birthearn, by = c("OPEID6", "AY_FALL"))
newdf <- left_join(newdf, beadf, by = "state")


# Merge in demographics
demopanel <- read_csv('data/output/demos.csv') %>%
  mutate(across(c(men, women, white, black, hispanic, asian, unknown, alien, nativeamerican), ~ .x / total)) %>%
  rename(UNITID = unitid, AY_FALL = year)

newdf <- left_join(newdf, demopanel, by = c("UNITID", "AY_FALL"))

# Merge in the CIP2 data
completionsdf2 <- read_csv('data/output/cip2_completions.csv')
degree_types <- read_csv('data/input/CIP2_degreetypes.csv')

# Create a dictionary (list in R) for degree types
degree_dict <- split(degree_types$CIPCode2000, degree_types$type)

# Sum across CIP codes for each degree type
for(d in names(degree_dict)) {
  cipcols <- paste0("CIP_", degree_dict[[d]])
  cipcols <- cipcols[cipcols %in% names(completionsdf2)]
  completionsdf2[[paste0("major_", d)]] <- rowSums(select(completionsdf2, all_of(cipcols)), na.rm = TRUE)
}

# Sum across all major columns and compare against the total completions
# (This step replicates the describe and mean calculations in Python. Adjust based on specific needs.)
major_cols <- paste0("major_", names(degree_dict))
summary(rowSums(select(completionsdf2, all_of(major_cols)), na.rm = TRUE))

cip_range_cols <- paste0("CIP_", 0:54)
cip_range_cols <- cip_range_cols[cip_range_cols %in% names(completionsdf2)]
mean(rowSums(select(completionsdf2, all_of(cip_range_cols)), na.rm = TRUE) != 1)

completionsdf2 <- completionsdf2 %>%
  mutate(AY_FALL = YEAR - 4) %>%
  rename(COMPLETIONYEAR = YEAR)

newdf <- left_join(newdf, completionsdf2, by = c("UNITID", "AY_FALL"))
newdf <- filter(newdf, AY_FALL <= 2005)

# Save it
write_csv(newdf, '/home/oddish3/Documents/R_folder/MSc/ME/ME-project/data/processed/cohort_earn_panel.csv')

