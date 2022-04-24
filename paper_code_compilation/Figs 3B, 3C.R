library(data.table)
library(tidyverse)
library(lfe)
library(modelr)
library(gridExtra)
set.seed(42)

# Get the 2015 edge level data --------
edge2015 %>% names()

# Figure 3B: Job transmission by mutual friends ---------------
# Generate ggplot default theme colors 
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

edge2015 %>% sample_frac(size=0.1) -> edge20151

# Get fitted values of mutual connections from endogenous variables and 
# control variables 
edge20151 %>% add_predictions(lm(n_f_common ~ source_variant + 
                                own_connection_count +
                                friend_connection_count +
                                both_male + both_female + same_school + 
                                f_years_older + same_region +
                                same_placeCode, 
                              data = edge2015),
                           var = "n_f_common_fitted") -> edge20151

theme_update(plot.title = element_text(hjust = 0.5))  # Centers ggplot titles 

# Binscatter of iv fitted values for mutual connections and job transmission 
edge20151 %>% 
  ggplot(aes(x=n_f_common_fitted, y=job_trans)) + 
  stat_summary_bin(fun.y = 'mean', bins = 20, geom = 'point') +
  xlim(0, 35) +
  stat_summary_bin(fun.data = mean_se, bins= 20, col = ggplotColours(2)[2]) +
  labs(x = 'Number of mutual friends', y = 'Probability of job transmission') + 
  theme(axis.title.y=element_blank()) -> piv1

# Binscatter of raw mutual connections variable and job transmission 
edge20151 %>% 
  ggplot(aes(x=n_f_common, y=job_trans)) + 
  stat_summary_bin(fun.y = 'mean', bins = 20, geom = 'point') +
  xlim(0, 35) +
  stat_summary_bin(fun.data = mean_se, bins= 20, col = ggplotColours(2)[1]) +
  labs(x = 'Number of mutual friends', y = 'Probability of job transmission') -> pols1

grid.arrange(pols1, piv1, nrow=1, 
             top="Edge-level relationship between structural tie strength and job transmission")

# Figure 3C: Job trasmission by messaging intensity -----------------
edge20151 %>% add_predictions(lm(messages_exchanged ~ source_variant + 
                                 own_connection_count +
                                 friend_connection_count +
                                 both_male + both_female + same_school + 
                                 f_years_older + same_region +
                                 same_placeCode, 
                               data = edge2015),
                            var = "messages_exchanged_fitted") -> edge20151
# messages_exchanged is a normalized variable 

# Binscatter of iv fitted values for interaction intensity and job transmission 
edge20151 %>% 
  ggplot(aes(x=messages_exchanged_fitted, y=job_trans)) + 
  xlim(0, 1) + 
  stat_summary_bin(fun.y = 'mean', bins = 20, geom = 'point') +
  stat_summary_bin(fun.data = mean_se, bins= 20, col=ggplotColours(2)[2]) +
  labs(x = 'Interaction intensity', y = 'Probability of job transmission') + 
  theme(axis.title.y=element_blank()) -> piv2

# Binscatter of raw interaction intensity variable and job transmission 
edge20151 %>% 
  ggplot(aes(x=messages_exchanged, y=job_trans)) + 
  xlim(0, 1) + 
  stat_summary_bin(fun.y = 'mean', bins = 20, geom = 'point') +
  stat_summary_bin(fun.data = mean_se, bins= 20, col=ggplotColours(2)[1]) +
  labs(x = 'Interaction intensity', y = 'Probability of job transmission') -> pols2

grid.arrange(pols2, piv2, nrow=1, 
             top="Edge-level relationship between interaction intensity and job transmission")



