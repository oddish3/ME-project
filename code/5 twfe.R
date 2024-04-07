#set up ----

rm(list=ls())

library(fixest)
library(haven)
library(magrittr)
library(dplyr)
library(broom)
library(ggplot2)
library(binscatteR)
library(did)
library(etwfe)
library(np)



df <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>% 
  filter(twfe_sample == 1 & late_adopter == 0)


df$k_rank <- df$k_rank * 100
#excluding a specific observation 
df <- filter(df, UNITID != 184694)
# create + recode simplebarrons variable
df$simplebarrons <- df$barrons
df$simplebarrons <- as.numeric(as.character(df$simplebarrons))
df$simplebarrons <- dplyr::recode(df$simplebarrons,
                                               `0` = 1, `1` = 1,
                                               `2` = 2, `3` = 2,
                                               `4` = 3,
                                               `5` = 4,
                                               `999` = 9)

# group data and generate identifiers
df$simpletiershock <- as.integer(interaction(df$simplebarrons, df$AY_FALL, drop = TRUE))
df <- fastDummies::dummy_cols(df, select_columns = "simpletiershock")

df$DateJoinedFB <- as.Date(df$DateJoinedFB, format = "%m/%d/%Y")
df$year_joinedFB <- format(df$DateJoinedFB, "%Y")
df %<>% 
  mutate(
    year_treated = case_when(
      AY_FALL < 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB <= 2005 ~ 2005,
      TRUE ~ 0  # Use NA_real_ for numeric NA
    )
  )

df %>% filter(FBName == "Harvard" | UNITID == 100706 | UNITID == 243744 ) %>% select(UNITID, FBName, AY_FALL, DateJoinedFB, year_treated, k_rank, EXPOSED, 
                                              EXPOSURE_4YR)

df1 <- df 
head(df1)

### 2 x 2 x 2 DiD ----- for 2004 vs 0 cases --------- 2 states by 2 time points
#' all from @https://rpubs.com/corinne-riddell/guide-to-did-estimators

# scenario 1
# Step 1: Create a dummy variable for the treatment group

df1$Treat <- ifelse(df1$year_joinedFB == 2004, 1, 0)
sum(df1$Treat == 1)

# STEP 2 : create a dummy variable for post-treatment period
# facebook rolled out in 2004, so 2000 cohort is the first post-treatment period
df1$post <- ifelse(df1$AY_FALL >= 2000, 1, 0)

# df_post <- df %>%
#   filter(EXPOSED > 0) %>%
#   group_by(UNITID) %>%
#   summarise(year_post = min(AY_FALL))
# df1 <- left_join(df, df_post, by = "UNITID")
# 
# dfdf2 <- dfdf2 %>%
#   mutate(post = case_when(
#     AY_FALL >= 2000 ~ 1, 
#     TRUE ~ 0
#   ))
# 
# df1$post <- ifelse(df1$year_joinedFB == 2004, 1, 0)
# df1$post <- ifelse(df1$AY_FALL >= df1$year_post, 1, 0)

df2 <- df1 %>% filter(AY_FALL < 2001) %>% filter(AY_FALL >= 1999) %>% filter(FBName == "Harvard" | UNITID == 100706)
# df2 %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated,Treat)
# df2 %>% filter(UNITID == 100706) %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, Treat)
# df2 %>% filter(UNITID == 243744) %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, Treat)

sum(df2$Treat == 1)
sum(df2$post == 1)
sum(df2$Treat * df2$post == 1)
df2$FBNAME1 = as.factor(df2$FBName)

ggplot(df2, aes(x = AY_FALL, y = k_rank)) + 
  geom_line(aes(group = FBNAME1, col = FBNAME1)) +
  geom_point(aes(fill = FBNAME1, pch = FBNAME1), size = 4) +
  geom_text(aes(label = sprintf("%.2f", k_rank)), vjust = -0.5, hjust = 0.5)

# # Pre-treatment difference of 15
# 
# Post-treatment difference of 9
# 
# Causal effect of policy = 9-15 = -6 or equivalently,
# 
# uni 1 difference of -0.5
# 
# uni 2 difference of 5
# 
# Causal effect of policy = -0.5-5 = -6 under the assumptions

# Two-way fixed effects (TWFE) regression model ----
# 
# # Under the canonical TWFE design, we can estimate the policy effect by including a fixed effect (FE) for state,
# # a FE for time, and an indicator for the policy change. The indicator should be 1 for when the treated states are 
# # in the post-treatment period, and 0 otherwise
df2$policy = df2$Treat * df2$post

r1 <- lm(k_rank ~ UNITID + AY_FALL + policy, data = df2)
summary(r1)

# scenario 2 - 3 states, heterogeneous treatment effects

# 2 x 2 x 3 DiD - including trinity - last treated
df3 <- df1 %>% filter(FBName == "Harvard" | FBName == "Simmons" | FBName == " Alabama Huntsville") %>% filter(AY_FALL < 2001) %>% filter(AY_FALL >= 1999)
df3$FBNAME1 = as.factor(df3$FBName)

ggplot(df3, aes(x = AY_FALL, y = k_rank)) + 
  geom_line(aes(group = FBNAME1, col = FBNAME1)) +
  geom_point(aes(fill = FBNAME1, pch = FBNAME1), size = 4) +
  geom_text(aes(label = sprintf("%.2f", k_rank)), vjust = -0.5, hjust = 0.5)

# The following can be read from the labelled plot for Scenario 2a:
#   
# Post-Pre Difference for never-treated state 1: 67 - 61 = 6
# 
# Post-Pre Difference for treated state 2: 63 - 68 = -5
# 
# Post-Pre Difference for treated state 3: 76.9 - 76.3 = -0.5
# 
# Causal effect of policy = -11 for state 2 vs 1 and -6.5 for state 3 vs 1 for an ATT of -8.75

# equivalent TWFE Regression model
df3$policy = df3$Treat * df3$post
r2 <- lm(k_rank ~ UNITID + AY_FALL + policy, data = df3)
summary(r2)

# When there are multiple treatment groups and two time points, where >1 of the groups becomes treated, you can calculate
# the DID estimate by hand or using TWFE regression. If treatment effects are heterogeneous across states, then the
# estimated effect is the average ATT across the states.

# scenario 3 Two states, multiple time points, staggered treatment timing

df4 <- df1

df_late <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") %>% 
  filter(twfe_sample == 1) 

df_late %>%
  filter(late_adopter == 1) %>%
  select(UNITID, FBName, AY_FALL, DateJoinedFB, k_rank, EXPOSED, EXPOSURE_4YR) %>%
  arrange(desc(AY_FALL))


df_late5 = df_late  %>% filter(FBName == "Harvard" | FBName == "Simmons" | FBName == " Alabama Huntsville" | UNITID == 100830) %>% 
  filter(AY_FALL < 2002) %>% filter(AY_FALL >= 1999) %>% 
  select(UNITID, FBName, AY_FALL, DateJoinedFB, k_rank, EXPOSED, EXPOSURE_4YR)

## 
df_late5$UNITID = as.factor(df_late5$UNITID)
df_late5$FBNAME1 = as.factor(df_late5$FBName)
df_late5 %<>% mutate(ever_trt = case_when(FBName == "Harvard" | FBName == "Simmons"  ~ "treated in 2004", 
                                    FBName == " Alabama Huntsville" ~ "treated in 2005",
                                    TRUE ~ "never treated"))

ggplot(df_late5, aes(x = AY_FALL, y = k_rank)) + 
  geom_line(aes(col = FBNAME1)) + 
  geom_point(aes(fill = FBNAME1, pch = ever_trt)) + labs(title = "Scenario 5 (Staggered timing)")


# now defining bunch of variables 

# # equivalent TWFE Regression model
# df3$policy = df3$Treat * df3$post
# r2 <- lm(k_rank ~ UNITID + AY_FALL + policy, data = df3)
# summary(r2)

df_late5$DateJoinedFB <- as.Date(df_late5$DateJoinedFB, format = "%m/%d/%Y")
df_late5$year_joinedFB <- format(df_late5$DateJoinedFB, "%Y")
df_late5 %<>% 
  mutate(
    year_treated = case_when(
      AY_FALL < 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB <= 2005 ~ 2005,
      TRUE ~ 0  # Use NA_real_ for numeric NA
    )
  )
df_late5$year_joinedFB <- ifelse(is.na(df_late5$year_joinedFB), 0, df_late5$year_joinedFB)
df_late5$Treat4 <- ifelse(df_late5$year_joinedFB == 2004, 1, 0)
df_late5$Treat5 <- ifelse(df_late5$year_joinedFB == 2005, 1, 0)


# STEP 2 : create a dummy variable for post-treatment period
# facebook rolled out in 2004, so 2000 cohort is the first post-treatment period
df_late5$post <- ifelse(df_late5$AY_FALL >= 2000, 1, 0)
df_late5$policy = ifelse(df_late5$Treat4 == 1 & df_late5$post == 1, 1, 
                         ifelse(df_late5$Treat5 == 1 & df_late5$post == 1, 1, 0))
df_late5$policy  

# Two-way fixed effects (TWFE) regression model ----
s5_mod <- lm(k_rank ~  policy + UNITID + factor(AY_FALL), data = df_late5)
tidy(s5_mod)

df_late5 %<>% mutate(UNITID_n = as.numeric(as.character(UNITID)),
               policy_n = as.numeric(as.character(policy)))

s5_bacon <- bacon(k_rank ~ policy_n,
                    data = df_late5,
                  id_var = "UNITID_n",
                  time_var = "AY_FALL")

#### trying to get bacon decomposition on staggered - not working ----

# df4 <- df1
# 
# df_late <- read_dta("../original_study/labour-market/data/output/analysis_sample.dta") 
# 
# df_late %>%
#   select(UNITID, FBName, AY_FALL, DateJoinedFB, k_rank, EXPOSED, EXPOSURE_4YR) %>%
#   arrange(desc(AY_FALL))
# 
# 
# df_late5 = df_late  %>% filter(FBName == "Harvard" | FBName == "Simmons" | FBName == " Alabama Huntsville" | UNITID == 100937) %>% 
#   # filter(AY_FALL < 2002) %>% 
#   select(UNITID, FBName, AY_FALL, DateJoinedFB, k_rank, EXPOSED, EXPOSURE_4YR)
# 
# ## 
# df_late5$UNITID = as.factor(df_late5$UNITID)
# df_late5$FBNAME1 = as.factor(df_late5$FBName)
# df_late5 %<>% mutate(ever_trt = case_when(FBName == "Harvard" | FBName == "Simmons"  ~ "treated in 2004", 
#                                           FBName == " Alabama Huntsville" ~ "treated in 2005",
#                                           TRUE ~ "never treated"))
# 
# ggplot(df_late5, aes(x = AY_FALL, y = k_rank)) + 
#   geom_line(aes(col = FBNAME1)) + 
#   geom_point(aes(fill = FBNAME1, pch = ever_trt)) + labs(title = "Scenario 5 (Staggered timing)")
# 
# 
# # now defining bunch of variables 
# 
# # # equivalent TWFE Regression model
# # df3$policy = df3$Treat * df3$post
# # r2 <- lm(k_rank ~ UNITID + AY_FALL + policy, data = df3)
# # summary(r2)
# 
# df_late5$DateJoinedFB <- as.Date(df_late5$DateJoinedFB, format = "%m/%d/%Y")
# df_late5$year_joinedFB <- format(df_late5$DateJoinedFB, "%Y")
# df_late5 %<>% 
#   mutate(
#     year_treated = case_when(
#       AY_FALL < 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
#       AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB <= 2005 ~ 2005,
#       TRUE ~ 0  # Use NA_real_ for numeric NA
#     )
#   )
# df_late5$year_joinedFB <- ifelse(is.na(df_late5$year_joinedFB), 0, df_late5$year_joinedFB)
# df_late5$Treat4 <- ifelse(df_late5$year_joinedFB == 2004, 1, 0)
# df_late5$Treat5 <- ifelse(df_late5$year_joinedFB == 2005, 1, 0)
# 
# # df1$first.treat <- ifelse(df1$EXPOSED > 0 & df1$year_joinedFB == 2004 & df1$year_treated == 2004 , df1$year_joinedFB, 0)
# 
# # STEP 2 : create a dummy variable for post-treatment period
# # facebook rolled out in 2004, so 2000 cohort is the first post-treatment period
# df_late5$post <- ifelse(df_late5$AY_FALL >= 2000, 1, 0)
# df_late5$policy = ifelse(df_late5$Treat4 == 1 & df_late5$post == 1, 1, 
#                          ifelse(df_late5$Treat5 == 1 & df_late5$post == 1, 1, 0))
# df_late5$policy  
# 
# # Two-way fixed effects (TWFE) regression model ----
# s5_mod <- lm(k_rank ~  policy + UNITID + factor(AY_FALL), data = df_late5)
# tidy(s5_mod)
# 
# df_late5 %<>% mutate(UNITID_n = as.numeric(as.character(UNITID)),
#                      policy_n = as.numeric(as.character(policy)))
# 
# s5_bacon <- bacon(k_rank ~ policy_n,
#                   data = df_late5,
#                   id_var = "UNITID_n",
#                   time_var = "AY_FALL")







# Run the TWFE regression of binary treatment case for   (df1) many x 2 x 2 ---- 
# Under the canonical TWFE design, we can estimate the policy effect by including a fixed effect (FE) for state, 
# a FE for time, and an indicator for the policy change. The indicator should be 1 for when the treated states are 
# in the post-treatment period, and 0 otherwise. The effect estimate is the regression coefficient on the policy variable.

sum(df1$Treat == 1)
sum(df1$post == 1)
sum(df1$Treat * df1$post == 1)

policy = df1$Treat * df1$post

s1_mod <- lm(k_rank ~  policy + UNITID + AY_FALL  , data = df1)
summary(s1_mod)




# Run the TWFE regression for staggered treatment timing (df3) using the year_treated variable ---- 
df3 <- df1 %>% filter(AY_FALL < 2001) %>% filter(AY_FALL >= 1999)
df2 %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated,Treat)
df2 %>% filter(UNITID == 100706) %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, Treat)
df2 %>% filter(UNITID == 243744) %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, Treat)

sum(df2$Treat == 1)
sum(df2$post == 1)
sum(df2$Treat * df2$post == 1)



df1$first.treat <- ifelse(df1$EXPOSED > 0 & df1$year_joinedFB == 2004 & df1$year_treated == 2004 , df1$year_joinedFB, 0)
# Run the TWFE regression
# R0 <- feols(k_rank ~ Treat +factor(UNITID) + factor(AY_FALL), data = df1)
r1 <- feols(k_rank ~ first.treat | UNITID + AY_FALL, data = df1) # r1 <- feols(k_rank ~ Treat + post + Treat * post | UNITID + AY_FALL, data = df1)
summary(r1) #feols clusters by UNITID here by default


mod =
  etwfe(
    fml  =  k_rank ~ EXPOSED + simpletiershock, # outcome ~ controls
    tvar = AY_FALL,        # time variable
    gvar = first.treat, # group variable
    data = df1,       # dataset
    vcov = ~UNITID  # vcov adjustment (here: clustered)
  )





# R0 <- feols(k_rank ~ Treat +factor(UNITID) + factor(AY_FALL), data = df1)
r1 <- feols(k_rank ~ first.treat | UNITID + AY_FALL, data = df1) # r1 <- feols(k_rank ~ Treat + post + Treat * post | UNITID + AY_FALL, data = df1)
summary(r1) #feols clusters by UNITID here by default



mod =
  etwfe(
    fml  =  k_rank ~ EXPOSED + simpletiershock, # outcome ~ controls
    tvar = AY_FALL,        # time variable
    gvar = first.treat, # group variable
    data = df1,       # dataset
    vcov = ~UNITID  # vcov adjustment (here: clustered)
  )

# M1 <- feols(k_rank ~ D | UNITID + simpletiershock, data = analysis_sample, vcov = "cluster")
# print(M1)

mod
emfx(mod) # ATT

mod_es = emfx(mod, type = "calendar") # ATT by period
mod_es

mod_es = emfx(mod, type = "group") # ATT by group
mod_es

# heterogenous treatment effects
hmod =
  etwfe(
    fml  =  k_rank ~ EXPOSED + simpletiershock, # outcome ~ controls
    tvar = AY_FALL,        # time variable
    gvar = first.treat, # group variable
    data = df1,       # dataset
    vcov = ~UNITID,  # vcov adjustment (here: clustered)
    xvar = barrons # treatment variable
  )

emfx(hmod)
emfx(hmod, hypothesis = "b1 = b2")
emfx(hmod, hypothesis = "b1 = b2", type = "calendar")


## staggered DiD
df1$Treat <- ifelse(df1$EXPOSED > 0, 1, 0)
df1$stag.treat <- ifelse(df1$EXPOSED > 0 & df1$year_joinedFB == 2004 & df1$year_treated == 2004 , df1$year_joinedFB, 
                         ifelse(df1$EXPOSED > 0 & df1$year_joinedFB == 2005 & df1$year_treated == 2005, df1$year_joinedFB, 
                                ifelse(df1$EXPOSED > 0 & df1$AY_FALL == 2005, 2005, 0)))

# create a dummy variable for post-treatment period
df1$post <- ifelse(df1$year_joinedFB == 2004, 1, 0)

df1 %>% filter(FBName == "Harvard") %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, stag.treat)
df1 %>% filter(UNITID == 100706) %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, stag.treat)
df1 %>% filter(UNITID == 243744) %>% select(FBName, AY_FALL, DateJoinedFB,  post, Treat, year_treated, stag.treat)

sum(df1$stag.treat == 2004 | 2005)
sum(df1$post == 1)
sum(df1$Treat * df1$post == 1)

r1 <- lm(k_rank ~ stag.treat * post, data = df1)
summary(r1)


# Run the TWFE regression
# R0 <- feols(k_rank ~ Treat +factor(UNITID) + factor(AY_FALL), data = df1)
r1 <- feols(k_rank ~ stag.treat | UNITID + AY_FALL, data = df1) # r1 <- feols(k_rank ~ Treat + post + Treat * post | UNITID + AY_FALL, data = df1)
summary(r1) #feols clusters by UNITID here by default





# mod =
#   etwfe(
#     fml  =  k_rank ~ EXPOSED + simpletiershock, # outcome ~ controls
#     tvar = AY_FALL,        # time variable
#     gvar = stag.treat, # group variable
#     data = df1,       # dataset
#     vcov = ~UNITID  # vcov adjustment (here: clustered)
#   )
# 
# # M1 <- feols(k_rank ~ D | UNITID + simpletiershock, data = analysis_sample, vcov = "cluster")
# # print(M1)
# 
# mod
# emfx(mod) # ATT
# 
# mod_es = emfx(mod, type = "calendar") # ATT by period
# mod_es
# 
# mod_es = emfx(mod, type = "group") # ATT by group
# mod_es
# 
# # heterogenous treatment effects
# hmod =
#   etwfe(
#     fml  =  k_rank ~ EXPOSED + simpletiershock, # outcome ~ controls
#     tvar = AY_FALL,        # time variable
#     gvar = stag.treat, # group variable
#     data = df1,       # dataset
#     vcov = ~UNITID,  # vcov adjustment (here: clustered)
#     xvar = barrons # treatment variable
#   )
# 
# emfx(hmod)
# emfx(hmod, hypothesis = "b1 = b2")
# emfx(hmod, hypothesis = "b1 = b2", type = "calendar")





# continuous DiD -----
#model <- feols(Y ~ D * Post | i + t, data = df)
#  Y_it (outcome variable), θ_t (time fixed effects), η_i (unit fixed effects), D_i 
#(treatment intensity or dose for unit i), and Post_t (dummy variable for post-treatment period).


# M1 <- feols(k_rank ~ EXPOSED * post | UNITID + simpletiershock, data = df1, vcov = "cluster")
# summary(M1)

# computes weights from TWFE regression ------------------------------------------

#' @param l a particular value of the treatment for which to compute weights
#' @param D an nx1 vector containing doses for all units
cont_twfe_weights <- function(l, D) {
  wt <- ( ( mean(D[D>=l]) - mean(D) ) * mean(1*(D>=l)) ) / var(D)
  wt
}

#' nonparametric estimates of att(d|d) and acrt(d|d)
#' @param dy the change in the outcome over time
#' @param dose the amount of the treatment
#' @return list( 
#'            local_effects - data frame containing the dose and estimates of 
#'              att(dose) and acrt(dose)
#'            att.overall - an estimate of the overall att
#'            acrt.overall - an estimate of the overall acrt
#'          )
cont_did <- function(dy, dose) {
  # choose bandwidth
  bw <- np::npregbw(formula=dy ~ dose,
                    regtype="ll",
                    bws=1.06,
                    bwscaling=TRUE,
                    bandwidth.compute=FALSE)
  # estimate att and acrt nonparametrically
  out <- np::npreg(bws=bw, gradients=TRUE, exdat=dose)
  
  # order from smallest to largest dose and drop untreated
  this_order <- order(dose)
  dose <- dose[this_order]
  dy <- dy[this_order]
  att.d <- out$mean[this_order]
  acrt.d <- out$grad[,1][this_order]
  att.d <- att.d[dose>0]
  acrt.d <- acrt.d[dose>0]
  att.overall <- mean(att.d)
  acrt.overall <- mean(acrt.d)
  
  return(list(local_effects=data.frame(dose=dose[dose>0],
                                       att.d=att.d,
                                       acrt.d=acrt.d),
              att.overall=att.overall,
              acrt.overall=acrt.overall))
}
# plotting histogram

### k_rank_diff calc
df1 <- df1 %>%
  arrange(AY_FALL)

# Step 2: Calculate the change in k_rank from one AY_FALL to the next
df1 <- df1 %>%
  group_by(UNITID) %>% # Assuming 'id' is a grouping variable you might have. If not, adjust accordingly.
  mutate(k_rank_diff = k_rank - lag(k_rank)) %>%
  ungroup() # Remove the grouping


df1$dose <- as.vector(df1$EXPOSURE_4YR)
dose <- as.vector(df1$EXPOSURE_4YR)
df1$dy <- as.vector(df1$k_rank_diff)
dy <- as.vector(df1$k_rank_diff)
p <- ggplot(data.frame(dose=dose), aes(x=dose)) + 
  geom_histogram()
p

# The histogram show that a non-trivial fraction of units are untreated whist there is no real
# pattern to the rest, bar the bunching at 4 years.
library(binscatteR)
# binscatter plot of the change in the outcome over time with respect to the dose
binnedout <- binscatter(data=df1, x="EXPOSURE_4YR", y="k_rank")
binnedout

twfe <- lm(dy ~ dose)
summary(twfe)$coefficients

twfe <- feols(dy ~ dose | UNITID + simpletiershock, data = df1, vcov = "cluster")
summary(twfe)$coefficients

twfe <- feols(dy ~ dose * post | UNITID + simpletiershock, data = df1, vcov = "cluster")
summary(twfe)$coefficients

twfe <- lm(k_rank_diff ~ EXPOSED, data = df1)
summary(twfe)$coefficients

twfe <- feols(k_rank_diff ~ EXPOSED | UNITID + simpletiershock, data = df1, vcov = "cluster")
summary(twfe)$coefficients

twfe <- feols(k_rank_diff ~ EXPOSED * post | UNITID + simpletiershock, data = df1, vcov = "cluster")
summary(twfe)$coefficients


# using the cont_did function provided above to estimate the ATT etc
#Plot and as functions of the dose and provide estimates of ATT etc

cont_res <- cont_did(dy, dose)

cont_res$att.overall
cont_res$acrt.overall

plot_df <- cont_res$local_effects

colnames(plot_df) <- c("dose", "att", "acrt")
ggplot(plot_df, aes(x=dose, att)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()

ggplot(plot_df, aes(x=dose, acrt)) +
  geom_hline(yintercept=0, color="red", linetype="dashed") +
  geom_line() +
  theme_bw()


dL <- min(dose[dose>0])
dU <- max(dose)

# density of the dose
dose_grid <- seq(dL, dU, length.out=100)
frq_weights_plot <- ggplot(data.frame(dose=dose[dose>0]), aes(x=dose)) +
  geom_density(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid), max(dose_grid)))+
  ylab("Density weights") +
  xlab("Dose") +
  ylim(c(0,3)) + 
  labs(title="Density of dose")
frq_weights_plot


twfe_weights <- sapply(dose_grid, cont_twfe_weights, D=dose)

plot_df <- cbind.data.frame(twfe_weights, dose_grid)

twfe_weights_plot <- ggplot(data=plot_df,
                            mapping=aes(x = dose_grid,
                                        y = twfe_weights)) +
  geom_line(colour = "darkblue", linewidth = 1.2) +
  xlim(c(min(dose_grid),
         max(dose_grid)))+
  ylab("TWFE weights") +
  xlab("Dose") +
  geom_vline(xintercept = mean(dose),
             colour="black",
             linewidth = 0.5,
             linetype = "dotted") +
  ylim(c(0,3)) +
  labs(title="TWFE weights")

twfe_weights_plot
















# # synthetic control ------
# library(Synth)
# 
# # Prepare the data
# # Assuming your data is in a dataframe called 'df'
# # 'outcome' is the variable you want to analyze
# # 'time' is the time variable
# # 'unit' is the unit identifier (e.g., state, country, etc.)
# # 'treated' is a binary variable indicating the treated units
# 
# # Run the synthetic control
# 
# dataprep(foo = df1, predictors = c(")
# 
# sc_result <- synth(
#   data = dfq,
#   outcome.variable = "k_rank",
#   unit.variable = "UNITID",
#   time.variable = "AY_FALL",
#   treatment.identifier = "Treat"
# )
# 
# # Summarize the results
# summary(sc_result)












