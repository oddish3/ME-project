rm(list=ls())
library(magrittr)
library(dplyr)
library(augsynth) #devtools::install_github("ebenmichael/augsynth")

df <- read_dta("../original_study/labour-market/data/output/df1.dta") %>% 
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
      AY_FALL <= 2004 & EXPOSED > 0 & year_joinedFB <= 2004  ~ 2004,
      AY_FALL <= 2005 & EXPOSED > 0 & year_joinedFB == 2005 ~ 2005,
      TRUE ~ 0 # Default case if neither condition is met
    )
  )
df %>% select(UNITID, FBName, AY_FALL, DateJoinedFB, year_treated, k_rank, EXPOSED, EXPOSURE_4YR) %>% sample_n(8)


df_post <- df %>%
  filter(EXPOSED > 0) %>%
  group_by(UNITID) %>%
  summarise(year_post = min(AY_FALL))
df1 <- left_join(df, df_post, by = "UNITID")


df1 <- df1 %>% select(UNITID, FBName, AY_FALL, DateJoinedFB, year_treated, year_post, k_rank, EXPOSED, EXPOSURE_4YR, simpletiershock)
# year = cohort. state = UNITID
# year_treated = year that cohort had access to fb
# k_rank rank of student in national cohort
head(df1)

df1$post <- ifelse(df1$AY_FALL >= df1$year_post, 1, 0)



# continuous DiD
#model <- feols(Y ~ D * Post | i + t, data = df)
#  Y_it (outcome variable), θ_t (time fixed effects), η_i (unit fixed effects), D_i 
#(treatment intensity or dose for unit i), and Post_t (dummy variable for post-treatment period).


M1 <- feols(k_rank ~ EXPOSED * post | UNITID + simpletiershock, data = df1, vcov = "cluster")
summary(M1)

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













###
unique_combinations <- df1 %>%
  distinct(post, treatment, .keep_all = TRUE)
unique_combinations
count_combinations <- df1 %>%
  group_by(post, treatment) %>%
  summarise(Count = n())
count_combinations
cross_tab <- table(df1$post, df1$treatment)
cross_tab
combination_check <- df1 %>%
  group_by(post, treatment) %>%
  summarise(Count = n()) %>%
  filter(Count == 0)
combination_check