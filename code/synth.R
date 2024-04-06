# rm(list=ls())
library(magrittr)
library(dplyr)
library(augsynth) #devtools::install_github("ebenmichael/augsynth")

df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta")
df$DateJoinedFB <- as.Date(df$DateJoinedFB, format = "%m/%d/%Y")
df$year_joinedFB <- format(df$DateJoinedFB, "%Y")
df %<>% 
  mutate(
    year_treated = case_when(
      AY_FALL <= 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB == 2005 ~ 2005,
      TRUE ~ 0 # Default case if neither condition is met
    )
  )
df %>% select(UNITID, FBName, AY_FALL, DateJoinedFB, year_treated, k_rank, EXPOSED, EXPOSURE_4YR) %>% sample_n(8)


# Keep the necessary columns
df1 <- df %>% select(AY_FALL, UNITID, year_treated, k_rank, EXPOSED, EXPOSURE_4YR)
# year = cohort. state = UNITID
# year_treated = year that cohort had access to fb
# k_rank rank of student in national cohort
head(df1)


# we need to include a treatment status column that indicates which college cohort is treated in a given year


df1$AY_FALL <- as.numeric(df1$AY_FALL)
df1$year_treated <- as.numeric(df1$year_treated)
df1$UNITID <- as.numeric(df1$UNITID)
df1$k_rank <- as.numeric(df1$k_rank)

max(df1$year_treated, na.rm = TRUE)

# partially pooled synthetic controls

ppool_syn <-  multisynth(k_rank ~ year_treated, UNITID, AY_FALL, df1)
# df1$year_treated

#single synth

r1 <- multisynth(k_rank ~ EXPOSURE_4YR, UNITID, AY_FALL, df1)
