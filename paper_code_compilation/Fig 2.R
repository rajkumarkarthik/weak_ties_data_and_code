library(data.table)
library(tidyverse)
library(colorspace)
library(coefplot)
library(gridExtra)

# geojob is data on geolocation for LinkedIn members 
# Subset data to make it manageable 
geojob[id %% 1e3 == 0] -> geojob1

# Plot 2A -----------
theme_update(plot.title = element_text(hjust = 0.5))  # Center plot title 
# Plot degree distribution
geojob1 %>% ggplot(aes(x=numberOfConnections, group=job_seeker, fill=job_seeker)) + 
  scale_x_log10() + xlab("Degree") + ggtitle("Network size distribution") +
  geom_density(adjust=1.5, alpha=0.4) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8, 0.8)) + ylab("Density") -> p1

# Plot 2B -------------
# Density of clustering coefficient for different bins of degree 
geojob1 %>% mutate(diversity = 1 - localClusteringCoefficient,
                   degquantile = cut_number(numberOfConnections, 4)) ->
  geojob1
levels(geojob1$degquantile) <- paste("Degree quartile", 1:4)
geojob1 %>% 
  ggplot(aes(x=diversity, group=job_seeker, fill=job_seeker)) + 
  scale_x_continuous(trans='exp') + 
  xlab("Diversity") + ggtitle("Network diversity distribution by degree quartile") +
  geom_density(adjust=1.5, alpha=0.4) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "none") + 
  facet_wrap(~degquantile) + 
  labs(fill="Job seeker status") + ylab("Density") -> p2 

# Plot 2C -----------------
tiestr <- fread("tie_strength_pretreatment_2019FebthruApr_1e2.csv")
tiestr %>% sample_frac(size=0.01) -> tiestr1
# tiestr1 is 0.01% of the total number of ties created during the 
# experimental period. We sample this small number for tractability 
# when plotting

# Generate ggplot default theme colors (uniformly around the color wheel)
ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}

median_original <- 0.0023988005997001498  
# Computed on Spark on the entire dataset (1e4 x larger than tiestr1)

# Plot tie strength distribution 
tiestr1 %>% 
  ggplot(aes(x=tie_strength)) + 
  scale_x_log10() + 
  xlab("Tie strength") + ggtitle("Tie strength distribution") +
  ylab("Density") +
  geom_density(adjust=2, alpha=0.4, fill=ggplotColours(3)[2]) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = median_original, linetype="dashed") + 
  geom_text(aes(x=median_original, label="\nStrong ties", y=0.2), colour="red", 
            angle=90, text=element_text(size=11), data=data.frame()) +
  geom_text(aes(x=median_original, label="Weak ties\n", y=0.2), colour="blue", 
            angle=90, text=element_text(size=11), data=data.frame()) -> p3

rm(geojob)

# Plot 2D ----------------
# Plot first stage of the effect of 2019 wave experiments on number of 
# new ties created 
# Get 2019 experiment data ---------
setDT(node2019, key="id")

# First stage regressions of 2019 node level data 
node2019 %>% lm(I(n_weak_pretreat + n_strong_pretreat) ~ exp_variant, 
                          data = .) -> redform0
node2019 %>% lm(n_weak_pretreat ~ exp_variant, data = .) -> redform1
node2019 %>% lm(n_strong_pretreat ~ exp_variant, data = .) -> redform2

ggcols <- ggplotColours(3)
redform0 %>% coefplot(intercept=FALSE, xlab="Average new ties created", 
                      ylab="Treatment variant", title="All ties",
                      outerCI = 0, color = ggcols[2]) + 
  theme_classic() + theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_discrete(labels = LETTERS[1:7]) -> p4
redform1 %>% coefplot(intercept=FALSE, xlab="Average new ties created", 
                      ylab="Treatment variant", title="Weak ties",
                      outerCI = 0, color = ggcols[2]) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                          axis.text.y=element_blank(), 
                          axis.title.y=element_blank()) +
  scale_y_discrete(breaks=NULL) -> p5
redform2 %>% coefplot(intercept=FALSE, xlab="Average new ties created",
                      ylab="Treatment variant", title="Strong ties",
                      outerCI = 0, color = ggcols[2]) +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5),
                          axis.text.y=element_blank(), 
                          axis.title.y=element_blank()) +
  scale_y_discrete(breaks=NULL) -> p6
grid.arrange(p4, p5, p6, nrow=1, top = "Effect of experiments on network",
             widths = c(1, 1, 1)) -> pbottom


grid.arrange(grobs = list(p1, p2, p3, pbottom), 
             layout_matrix = rbind(1:3, rep(4, 3)))



