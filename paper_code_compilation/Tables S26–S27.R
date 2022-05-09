library(tidyverse)
library(data.table)
library(lfe)
library(stargazer, quietly=T)

# Get data on profile start date of LinkedIn members ----------
regdate_mid <- fread("registrationDate_2015mid.csv")

setkey(edge2015, "mid")
setkey(regdate_mid, "memberId")
regdate_mid[edge2015] -> edge2015

names(edge2015)

# Table S26 ------------
# Quartiles of profile creation dates in the 2015 data 
edge2015$registrationDate %>% quantile(probs=seq(0,1,0.25),type=1,na.rm=T)
# 0%
# 2003-05-05
# 25%
# 2008-12-07
# 50%
# 2011-07-28
# 75%
# 2013-08-28
# 100%
# 2015-05-26

# Create quartile bins of profile creation date in 2015 data 
edge2015[, regDateQuartile := ntile(registrationDate, 4)]

# Edge level IV regression of job transmission on mutual connections metric 
ivspec_2015mf <- function(dat, n_f_threshold=10) {
    dat %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 
         (n_f_common + I(n_f_common > n_f_threshold) ~ source_variant) | 
         memberId,
       data = .) 
}

# Heterogeneity of 2015 edge-level results by quartiles ---------
# of profile creation date
reglist <- list()
for (i in 1:4) {
    edge2015[regDateQuartile == i] %>% ivspec_2015mf() -> reglist[[i]]
}
# Table S27 
stargazer(reglist, type='text')

# Mutual connections threshold at 8
reglist <- list()
for (i in 1:4) {
    edge2015[regDateQuartile == i] %>% ivspec_2015mf(n_f_threshold=8) -> reglist[[i]]
}
stargazer(reglist, type='text')

# Mutual connections threshold at 6
reglist <- list()
for (i in 1:4) {
    edge2015[regDateQuartile == i] %>% ivspec_2015mf(n_f_threshold=6) -> reglist[[i]]
}
stargazer(reglist, type='text')
# Results are largely robust to exact specification of mutual connections 
# threshold, as noted in Section 5.4 of the SI. 

