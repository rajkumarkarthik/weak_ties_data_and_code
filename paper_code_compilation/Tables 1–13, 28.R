library(tidyverse)
library(data.table)
library(stargazer)
library(xtable)
library(lfe)
library(coefplot)
library(gridExtra)

# Summary of 2015 dataset -------------
# Table 1
stargazer(edge2015)  

# Table 2
edge2015 %>% group_by(mid) %>% summarize(source_variant = source_variant[1]) %>% 
  group_by(source_variant) %>% tally() %>% mutate(prob = n / sum(n)) %>% 
  select(-n) %>% xtable(digits = 3) %>% print(include.rownames = FALSE)

# Summary of 2019 dataset ---------------
# Tables 3, 4 
stargazer(node2019)

# Fig 1B
node2019 %>% select(contains("exp")) %>% lapply(sum) %>% unlist()

# Table for Fig 1C aka Table 5 ------------
states <- fread("state_avgs.csv")
states %>% mutate(num_obs = formatC(num_obs, format = "d", big.mark = ","),
                  mean_diversity = 1 - mean_clus) %>% 
  select(state, num_obs, mean_degree, mean_diversity) %>% 
  xtable(digits = 3) %>% print(include.rownames = FALSE)

# OLS regs for 2015 collapsed to node level data -----------
# Table 6: First stage for 2015 collapsed
edge2015015 %>% 
  mutate(weak_tie = n_f_common <= 1, 
         medium_tie = (n_f_common > 1 & n_f_common <= 6),
         strong_tie = n_f_common > 6) %>% 
  group_by(mid) %>% 
  summarize(job_trans = sum(job_trans), variant_0 = variant_0[1],
            variant_1 = variant_1[1], variant_2 = variant_2[1],
            variant_3 = variant_3[1], variant_4 = variant_4[1],
            variant_5 = variant_5[1], variant_6 = variant_6[1],
            n_weak_tie = sum(weak_tie), n_medium_tie = sum(medium_tie), 
            n_strong_tie = sum(strong_tie)) ->
  node2015
# 1 and 6 are terciles of the mutual connections variable 

# First stage for weakest ties 
node2015 %>% felm(n_weak_tie ~ variant_0 + variant_2 + variant_3 + variant_4 + 
                    variant_5 + variant_6 | 0 | 0 | mid, data = .) -> 
  firststage_node2015_weakest
node2015 %>% 
  felm(n_medium_tie ~ variant_0 + variant_2 + variant_3 + variant_4 + 
         variant_5 + variant_6 | 0 | 0 | mid, data = .) -> 
  firststage_node2015_medium
node2015 %>% felm(n_strong_tie ~ variant_0 + variant_2 + variant_3 + variant_4 + 
                    variant_5 + variant_6 | 0 | 0 | mid, data = .) -> 
  firststage_node2015_strongest

# Table 6 
# Regression table with robust standard errors 
stargazer(firststage_node2015_weakest, firststage_node2015_medium, 
          firststage_node2015_strongest, type='text')

# OLS regs for Table 7 ----------
# 2015 edge level first stage 
# Column 1: Mutual connections outcome 
edge2015 %>% 
  felm(n_f_common ~ variant_0 + variant_2 + variant_3 + variant_4 + variant_5 + 
         variant_6 + own_connection_count +
         friend_connection_count + both_male + both_female + same_school + 
         f_years_older + same_region + same_placeCode | 0 | 0 | 
         mid,
       data = .) -> edge_firststage_mf
# variant_1 dropped explicitly for multicollinearity 

# Column 2: interaction intensity outcome
edge2015 %>% 
  felm(interaction_intensity ~ variant_0 + variant_2 + variant_3 + variant_4 + 
         variant_5 + variant_6 + own_connection_count +
         friend_connection_count + both_male + both_female + same_school + 
         f_years_older + same_region + same_placeCode | 0 | 0 | 
         mid,
       data = .) -> edge_firststage_ii
# variant_1 dropped explicitly for multicollinearity 

# Table 8---------
# 2015 node level relationship between tie strength and job transmission------
node2015 %>% felm(job_trans ~ n_weak_tie + n_medium_tie + n_strong_tie | 0 | 0 |
                    mid, data = .) -> node2015_ols

node2015 %>% 
  felm(job_trans ~ 1 | 0 | 
         (n_weak_tie + n_medium_tie + n_strong_tie ~ variant_0 + 
            variant_2 + variant_3 + variant_4 + variant_5 + variant_6) | mid,
       data = .) -> node2015_iv

# Table 8 
stargazer(node2015_ols, node2015_iv)

# Fig 3 tables ------------
# Mutual connections metric 
# IV regs with standard errors clustered at the sender member level 
edge2015 %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | (n_f_common ~ source_variant) | 
         mid,
       data = .) -> iv_mf_0

edge2015 %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 
         (n_f_common + I(n_f_common > 10) ~ source_variant) | 
         mid,
       data = .) -> iv_mf_1

# OLS regs 
edge2015 %>% 
  felm(job_trans ~ n_f_common + 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 0 | 
         mid,
       data = .) -> ols_mf_0

edge2015 %>% 
  felm(job_trans ~ n_f_common + I(n_f_common > 10) + 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 0 | 
         mid,
       data = .) -> ols_mf_1

# Table 9 
stargazer(ols_mf_0, ols_mf_1, iv_mf_0, iv_mf_1)

# IV specifications where standard errors are clustered at the individual
# observation level 
edge2015 %>% mutate(rowid = row_number()) %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | (n_f_common ~ source_variant) | 
         rowid,
       data = .) -> iv_mf_2

edge2015 %>% mutate(rowid = row_number()) %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 
         (n_f_common + I(n_f_common > 10) ~ source_variant) | 
         rowid,
       data = .) -> iv_mf_3

# Table 28 
stargazer(iv_mf_0, iv_mf_1, iv_mf_2, iv_mf_3)

# Interaction intensity metric 
# IV regs 
edge2015 %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 
         (messages_exchanged ~ source_variant) | 
         mid,
       data = .) -> iv_ii_0

edge2015 %>% 
  felm(job_trans ~ 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 
         (messages_exchanged + I(messages_exchanged > 0.5) ~ 
            source_variant) | 
         mid,
       data = .) -> iv_ii_1

# OLS regs 
edge2015 %>% 
  felm(job_trans ~ messages_exchanged + 
         own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 0 | 
         mid,
       data = .) -> ols_ii_0

edge2015 %>% 
  felm(job_trans ~ messages_exchanged + 
         I(messages_exchanged > 0.5) + own_connection_count +
         friend_connection_count +
         both_male + both_female + same_school + 
         f_years_older + same_region +
         same_placeCode | 0 | 0 | 
         mid,
       data = .) -> ols_ii_1 

# Table 10 
stargazer(ols_ii_0, ols_ii_1, iv_ii_0, iv_ii_1)

node2019 %>% mutate(all_ties = n_weak_pretreat + n_strong_pretreat) %>% 
  relocate(all_ties, .before = n_weak_pretreat) -> node2019

# Table 11 -----------
# 2019 node level first stage replication -----------
node2019 %>% 
  felm(all_ties ~ exp_variant | 0 | 0 | id, data = .) -> 
  firststage19_allties
# Control variant is automatically dropped 
node2019 %>% 
  felm(n_weak_pretreat ~ exp_variant | 0 | 0 | id, data = .) -> 
  firststage19_weakties
node2019 %>% 
  felm(n_strong_pretreat ~ exp_variant | 0 | 0 | id, data = .) -> 
  firststage19_strongties
# Table 11 
stargazer(firststage19_allties, firststage19_weakties, 
          firststage19_strongties)

# Table for 2019 job applies result --------
# With pre-treatment tie strength as endogenous variable -------------
node2019 %>%
  felm(
    job_applies ~ 1 | 0 |
      (
        n_weak_pretreat + n_strong_pretreat ~ exp_variant
      ) | id,
    data = .
  ) -> iv_node

node2019 %>% 
  felm(job_applies ~ n_weak_pretreat + n_strong_pretreat | 0 | 0 | id,
     data = .) -> ols_node

# Table 12 
stargazer(ols_node, iv_node)

# With post-treatment tie strength as endogenous variable -------------
node2019 %>% 
  felm(job_applies ~ n_weak_diff + n_strong_diff | 0 | 0 | id,
     data = .) -> ols_node2

node2019 %>%
  felm(
    job_applies ~ 1 | 0 |
      (
        n_weak_diff + n_strong_diff ~ exp_variant
      ) | id,
    data = .
  ) -> iv_node2

# Table 13 
stargazer(ols_node2, iv_node2)


