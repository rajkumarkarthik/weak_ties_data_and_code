library(tidyverse)
library(data.table)
library(lfe)
library(gridExtra)
library(coefplot)
library(AER)
library(stargazer)

# Get 2015 edge level data ------------
# Tercile cutoffs for collapsed regression
cutoffs <- quantile(edge2015$n_f_common, probs = c(1/3, 2/3))

# Collapse 2015 edge level data to node level, aggregating tie counts 
# at the level of tie strength terciles 
edge2015 %>%
  mutate(weak_tie = n_f_common <= cutoffs[1],
              medium_tie = (n_f_common > cutoffs[1] & n_f_common <= cutoffs[2]),
              strong_tie = n_f_common > cutoffs[2]) %>%
  group_by(mid) %>%
  summarize(job_trans = sum(job_trans), source_variant = source_variant[1],
            Weak = sum(weak_tie), Medium = sum(medium_tie),
            Strong = sum(strong_tie),
            own_connection_count = own_connection_count[1]) ->
  node2015

# 2015 node level IV regression of job transmission on tie counts by strength 
node2015 %>% felm(job_trans ~ 1 | 0 | 
                  (Weak + Medium + Strong ~ source_variant) | mid,
                data = .) -> ivmod
# Clustering at mid level gives robust SEs  

# 2015 node level OLS regression of job transmission on tie counts by strength 
node2015 %>% felm(job_trans ~ Weak + Medium + Strong | 0 | 0 | mid,
              data = .) -> lmod 

# Generate ggplot default theme colors (uniformly around the color wheel)
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
ggcols <- gg_color_hue(2)

theme_update(plot.title = element_text(hjust = 0.5))
coefplot(lmod, outerCI = 0, horizontal = TRUE, intercept = FALSE,
         color = ggcols[1], title = "OLS") -> ols1
coefplot(ivmod, outerCI = 0, horizontal = TRUE, intercept = FALSE,
         color = ggcols[2], title = "IV") -> iv1
grid.arrange(grobs=list(ols1, iv1), nrow=1, 
             top = "Node-level job transmission regression")

# Table for these regressions -------
# Table S6 of SI: first-stage of 2015 node level data 
node2015 %>% 
  felm(Weak ~ source_variant | 0 | 0 | mid, data = .) -> 
  firststage_weak
node2015 %>% 
  lm(Medium ~ source_variant | 0 | 0 | mid, data = .) -> 
  firststage_medium
node2015 %>% 
  lm(Strong ~ source_variant | 0 | 0 | mid, data = .) -> 
  firststage_strong
stargazer(firststage_weak, firststage_medium, firststage_strong)

# Table S8 of SI: OLS and IV regressions of job transmission on tie counts 
# by strength in 2015 node level data 
stargazer(lmod, ivmod)

