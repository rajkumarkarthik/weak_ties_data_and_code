library(tidyverse)
library(data.table)
library(lfe)
library(stargazer)
library(xtable)
library(colorspace)
library(gridExtra)

# Merge in industry codes -----------
setkey(indids, "id")  # industry IDs for each LinkedIn member
setkey(node2019, "id")  # 2019 node level data 
indids[node2019] -> node2019
rm(indids)
setkey(node2019, "industryId")
setkey(naics, "industry_id")
naics[node2019] -> node2019  # Industry features at the NAICS 2d level 
setkey(node2019, "NAICS_2017_2D")
indhet %>% filter(t == 2019) -> indhet2019  # Industry rankings as on 2019
setDT(indhet2019, key = "naics_group")
indhet2019[node2019] -> node2019

# The principal IV regression for the 2019 node level data 
main_spec <- function(dat) {
  dat %>% 
    felm(
      job_applies ~ 1 | 0 |
        (n_weak_pretreat + n_strong_pretreat ~ exp_variant) | id,
      data = .
    ) 
}
# Clustering standard errors at the id level (individual observation) 
# provides robust standard errors 

# Quantiles of industry ranks ------------
qtle <- function(vec) {
  quantile(vec, na.rm=T)
}
node2019 %>% select(contains("_rank")) %>% sapply(qtle) ->
  qtles

# Get two regressions based on the top 25% of ranks and the bottom 75% of ranks 
# for a particular industry rank 
get_two_regressions <- function(rankvar) {
  models <- list()
  df_merged_filtered %>% filter(!!as.symbol(rankvar) <= qtles[4, rankvar]) %>% 
    main_spec() -> models[[1]]
  df_merged_filtered %>% filter(!!as.symbol(rankvar) > qtles[4, rankvar]) %>% 
    main_spec() -> models[[2]]
  models
}

# Plot heterogeneity results as a point range. Each plot provides weak and 
# strong tie coefficients for the observations in the top 25% and bottom 75% 
# of a particular industry ranking 
pointrange_binary_heterogeneity <- 
  function(mod_l, mod_r, plot_title, print_legend = FALSE,
           xlabel = "") {
    rbind(get_coefs_and_stderrors(mod_l),
          get_coefs_and_stderrors(mod_r)) %>% as.data.frame() %>% 
      rename(Coefficient = Estimate, SE = `Std. Error`) %>% 
      mutate(Type = factor(c("Weak", "Strong", "Weak", "Strong"), 
                           levels = c("Weak", "Strong")),
             Rank = c("Top", "Top", "Bottom", "Bottom")) %>% 
      mutate(Rank = factor(Rank, levels = c("Top", "Bottom"))) ->
      resdf
    
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length = n + 1)
      hcl(h = hues, l = 65, c = 100)[1:n]
    }
    
    errcols <- gg_color_hue(2)
    resdf %>% mutate(errColors = case_when(Type == "Strong" ~ errcols[1],
                                           Type == "Weak" ~ errcols[2])) ->
      resdf
    
    theme_update(plot.title = element_text(hjust = 0.5))
    # From https://stackoverflow.com/questions/10330314/pointrange-plot-with-boxplot-type-grouping 
    resdf %>% 
      ggplot(aes(x = Rank, y = Coefficient, group = Type, color = errColors)) + 
      theme_classic() + theme(plot.title = element_text(hjust = 0.5)) + 
      geom_pointrange(aes(ymin = Coefficient - SE, ymax = Coefficient + SE),
                      position = position_dodge(width = 0.4)) + 
      geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed") + 
      scale_color_manual(values = errcols[2:1]) + 
      ggtitle(plot_title) + ylab("Job applies per tie") -> plt
    
    if (!print_legend) {
      plt <- plt + theme(legend.position="none")
    } 
    
    if (xlabel != "") {
      plt <- plt + xlab(xlabel)
    }
    
    plt
  }

# Plot for main spec -----------
node2019 %>% main_spec() -> mod0
mod0 %>% get_coefs_and_stderrors() %>% as.data.frame() %>% 
  rename(Coefficient = Estimate, SE = `Std. Error`) %>% 
  mutate(Type = factor(c("Weak", "Strong"), levels = c("Weak", "Strong")),
         errColors = errcols[2:1]) -> res0df
res0df %>% 
  ggplot(aes(x = Type, y = Coefficient, color = Type)) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_pointrange(aes(ymin = Coefficient - SE, ymax = Coefficient + SE),
                  position = position_dodge(width = 0.4)) + 
  geom_hline(aes(yintercept = 0), color = "grey", linetype = "dashed") + 
  scale_color_manual(values = errcols[2:1]) + 
  ggtitle("Full sample") + xlab("Tie type") + 
  ylab("Job applies per tie") -> p0
rm(mod0)

# Heterogeneity results ---------------
# WFH result 
wfh_regs <- get_two_regressions("wfh_shares_unweighted")
pointrange_binary_heterogeneity(wfh_regs[[1]], wfh_regs[[2]], 
                             "Heterogeneity by ability to\nwork from home", 
                             print_legend = F,
                             xlabel = "Industries by ability to work from home") ->
  p_wfh

# IT 
it_regs <- get_two_regressions("it1_sc_rank")
pointrange_binary_heterogeneity(it_regs[[1]], it_regs[[2]], 
                             "Heterogeneity by\ninformation technology", 
                             print_legend = F,
                             xlabel = "Industries by IT intensity") -> p_it

# Suitability for machine learning
sml_regs <- get_two_regressions("sml_rank")
pointrange_binary_heterogeneity(sml_regs[[1]], sml_regs[[2]], 
                             "Heterogeneity by suitability for\nmachine learning", 
                             print_legend = F,
                             xlabel = "Industries by suitability for ML") ->
  p_sml

# AI score
ai_regs <- get_two_regressions("ai_score_rank")
pointrange_binary_heterogeneity(ai_regs[[1]], ai_regs[[2]], 
                             "Heterogeneity by suitability for\nartificial intelligence", 
                             print_legend = F,
                             xlabel = "Industries by suitability for AI") ->
  p_ai

# Software score
software_regs <- get_two_regressions("software_score_rank")
pointrange_binary_heterogeneity(software_regs[[1]], software_regs[[2]], 
                             "Heterogeneity by software use", 
                             print_legend = F,
                             xlabel = "Industries by software use") ->
  p_software

# Robot score
robot_regs <- get_two_regressions("robot_score_rank")
pointrange_binary_heterogeneity(robot_regs[[1]], robot_regs[[2]], 
                             "Heterogeneity by robotization", 
                             print_legend = F,
                             xlabel = "Industries by robotization") ->
  p_robot

# Put all plots together ---------
# Function to extract the legend from a ggplot 
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
bleg <- g_legend(p0)

# Complete Figure 4 
grid.arrange(grobs = list(ggplot() + theme_classic(), 
                          p0+ theme(legend.position="none"), bleg, 
                          p_it, p_software, p_sml, p_ai, p_robot, p_wfh),
             heights = c(2, 1.5, 1.5), widths = c(1, 1, 1),
             layout_matrix = rbind(1:3, 4:6, 7:9))

# Tables for these figures ------------
# Make table with industry ranks ----------
naics %>% select(NAICS_2017_2D, wfh_shares_unweighted) %>% 
  unique() %>% arrange(desc(wfh_shares_unweighted)) %>% 
  mutate(wfh_rank_old = row_number()) %>% 
  right_join(indhet2019, by = c("NAICS_2017_2D" = "naics_group")) -> ind_ranks

ind_ranks %>% select(NAICS_2017_2D, naics_name, any_of(c("wfh_rank_old", varnames))) %>% 
  select(-task_manual_rank) %>% arrange(NAICS_2017_2D) -> ind_ranks

# Table 15 
ind_ranks[, 1:5] %>% xtable(digits = 0) %>% print(include.rownames = FALSE)
# Table 16 
ind_ranks[, c(1:2, 6:8)] %>% xtable(digits = 0) %>% 
  print(include.rownames = FALSE)


# Make regression tables for heterogeneity 
# Table 17 
stargazer(wfh_regs[[1]], wfh_regs[[2]], it_regs[[1]], it_regs[[2]])
# Table 18 
stargazer(sml_regs[[1]], sml_regs[[2]], ai_regs[[1]], ai_regs[[2]])
# Table 19 
stargazer(software_regs[[1]], software_regs[[2]],
          robot_regs[[1]], robot_regs[[2]])





