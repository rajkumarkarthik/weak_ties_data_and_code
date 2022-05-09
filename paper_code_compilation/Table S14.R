# Proportion of different industries in our sample ------------
node2019 %>% group_by(naics_group) %>% 
  summarize(naics_name = naics_name[1], N = n()) -> indcounts

indcounts %>% filter(complete.cases(.)) %>% 
  mutate(indpct = N / sum(N) * 100) %>% 
  rename(naics_code = naics_group,
         industry = naics_name,
         sample_pct = indpct) %>% select(-N) -> indpct

# Intent-to-treat results --------------
# 2019 data 
options(pillar.sigfig = 4)
node2019 %>% group_by(exp_variant) %>% 
  summarize(avg_job_applies = mean(job_applies),
            se_job_applies = round(sd(job_applies) / sqrt(n()), 3)) 
# exp_variant    avg_job_applies
# Control                 4.39
# A                       4.39
# B                       4.41
# C                       4.44
# D                       4.44
# E                       4.46
# F                       4.41
# G                       4.38





