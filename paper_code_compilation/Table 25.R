bind_all_csv_into_one <- function(filename) {
  constituent_filenames <- list.files(filename, pattern = "*.csv", full.names = TRUE)
  rbindlist(lapply(constituent_filenames, fread))
}

# Get invites received data ----------
df_receives <- bind_all_csv_into_one("df_receives_2019AprMayexp_all.csv")
names(df_receives) <- c("id", "invite_receives")

# Merge with 2019 node data 
setkey(node2019, "id")
setkey(df_receives, "id")
df_receives[node2019] -> node2019

node2019[, invite_receives := replace_na(invite_receives, 0)]

# Generate standardized differences from control ------------
# Obtain control mean and variance 
node2019 %>% filter(exp_variant == 'Control') %>% 
  pull(invite_receives) %>% mean() -> exp_control_mean
node2019 %>%  filter(exp_variant == 'Control') %>% 
  pull(invite_receives) %>% var() -> exp_control_var

# Table 25: standardized differences 
node2019 %>% group_by(exp_variant) %>% 
  summarize(invites_received_avg = mean(invite_receives),
            invites_received_var = var(invite_receives)) %>% 
  mutate(standardized_diff = (invites_received_avg - exp_control_mean) / 
           sqrt((invites_received_var + exp_control_var) / 2)) 





