library(tidyverse)
library(data.table)
library(xtable)

# Get engagement metrics data 
engagement <- fread("engagement.csv")  

# Show metrics: messages, posts, likes and shares
# Including standard error 
engagement %>% 
  group_by(expt_id, metricName, variant) %>% select(lift, std_err, pval) %>% 
  unique() %>% 
  pivot_wider(names_from = metricName, values_from = c("lift", "std_err", "pval")) %>% 
  arrange(expt_id) %>% ungroup() %>% select(-expt_id) %>% 
  mutate(variant = LETTERS[1:7]) %>% 
  xtable(digits = 3) %>% print(include.rownames = FALSE)


