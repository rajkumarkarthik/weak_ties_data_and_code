library(data.table)
library(tidyverse)
library(usmap)
library(colorspace)

# Get 2019 node data ---------
setDT(node2019, key="id")

# Get data linking zip code and state
ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
temp <- tempfile()
download.file(ZipCodeSourceFile , temp)
ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
unlink(temp)
names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
                    "AdminName1", "AdminCode1", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")

ZipCodes %>% select(zipCode = zip, stateCode = AdminCode1) -> ZipCodes

# geojob is data on geolocation for each LinkedIn member 
geojob[countryCode == 'us'] -> geojob_us
geojob_us[, postalCode := as.numeric(postalCode)]
setkey(geojob_us, "postalCode")
setDT(ZipCodes, key="zipCode")
ZipCodes[geojob_us] -> geojob_us
geojob_us[id %in% node2019$id] -> geojob_us

# Get state averages data of degree and clustering coefficient ------------
geojob_us[, .(mean_degree = mean(numberOfConnections), 
              mean_clus = mean(localClusteringCoefficient),
              num_obs = .N),
                            by = .(stateCode)] -> state_avgs

state_avgs %>% filter(!is.na(stateCode) & stateCode != "") -> state_avgs

# Get state coordinates. Data from https://gist.github.com/sjengle/5315515 
statesGeo <- fread("us-capitals.csv")
statesGeo %>% select(state=abbrev, latitude, longitude) -> statesGeo
setDT(state_avgs, key="stateCode")
setDT(statesGeo, key="state")
statesGeo[state_avgs] -> state_avgs

# Convert latlong to Alberts Equal Area projection ---------
state_avgs %>% select(longitude, latitude) %>% usmap_transform() -> 
  state_avgs_trans

# Figure 1C ------------
# First, average network degree by US state 
# Color palette from https://www.r-bloggers.com/tol-color-schemes/ 
state_avgs %>% select(state, mean_degree) %>% 
  filter(!is.na(state) & state != "" & state != "DC") -> state_avgs_deg

plot_usmap(data = state_avgs_deg, values="mean_degree", color="black") + 
  scale_fill_stepsn(colors=rev(darken(inlmisc::GetColors(5, "discrete rainbow"), amount=0.2)), 
                    name="Average degree") + 
  theme(legend.position = "right") + 
  geom_point(data=arrange(state_avgs_trans, longitude), 
             aes(x=longitude.1, y=latitude.1, size=arrange(state_avgs, longitude)$num_obs)) +
  scale_size_continuous(labels = scales::comma) + 
  labs(size="Experiment units")

# Next, average local clustering coef by US state 
# We look at "network diversity," which is 1 - local clustering coefficient
state_avgs %>% select(state, mean_clus) %>% 
  mutate(mean_divers = 1 - mean_clus) %>% 
  select(-mean_clus) %>% 
  filter(!is.na(state) & state != "") -> state_avgs_clus

plot_usmap(data = state_avgs_clus, values="mean_divers", color="black") + 
  scale_fill_stepsn(colors=rev(darken(inlmisc::GetColors(5, "discrete rainbow"), amount=0.2)), 
                    name="Avg diversity") + 
  theme(legend.position = "right") + 
  geom_point(data=arrange(state_avgs_trans, longitude), 
             aes(x=longitude.1, y=latitude.1, size=arrange(state_avgs, longitude)$num_obs)) +
  scale_size_continuous(labels = scales::comma) + 
  labs(size="Experiment units")

