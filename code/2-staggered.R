# ------------------------------------------
#           Staggered DiD Script 
# ------------------------------------------

source("~/Documents/R_folder/MSc/ME/ME-project/code/1.prelim-DiD.R")

head <- analysis_sample %>% select(all_of(Y), all_of(T), all_of(D), all_of(G))
head(head)
  

min(analysis_sample$cohort)
max(analysis_sample$cohort)

min(analysis_sample$cohort[analysis_sample$EXPOSED == 1])
max(analysis_sample$cohort[analysis_sample$EXPOSED == 1])

min(analysis_sample$AY_FALL)
max(analysis_sample$AY_FALL)

min(analysis_sample$AY_FALL[analysis_sample$EXPOSED == 1])
max(analysis_sample$AY_FALL[analysis_sample$EXPOSED == 1])
