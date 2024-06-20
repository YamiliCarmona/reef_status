# Load libraries
library(dplyr)
library(janitor)
library(readr)

# Read and clean the functional groups data
func_groups <- read.csv("data/cluster_to_create_traits.csv") %>% 
  janitor::clean_names() 

# Read and clean the ltem data
# ltem <- readRDS("data/ltem_historic_updated_modified_2024-04-23.RDS") %>% 
#   janitor::clean_names() %>% 
#   filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))

ltem <- readRDS("data/fish_datagr_prod-by-species-all-sites.RDS")%>%
  janitor::clean_names()%>% 
  filter(!(reef == "SAN_JOSE_ANIMAS_PINACULOS" & year == 2014 & species == "Hoplopagrus guentherii"))

# Merge the datasets
merged_data <- left_join(ltem, func_groups, by = "species")

data <- merged_data %>%
  select(-trophic_level.y, -max_size_tl.y, -diet.y) %>%
  rename(trophic_level = trophic_level.x, max_size_tl = max_size_tl.x, diet = diet.x)

str(data)

# Save the result
# saveRDS(data, "data/fish_datagr_prod-by-species-herbivoros.RDS")
saveRDS(data, "data/fish_datagr_prod-by-species-all-sites-integrada.RDS")



