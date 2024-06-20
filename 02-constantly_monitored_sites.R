# Load libraries --------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data

# ltem  <- readRDS("data/fish_datagr_prod-by-species-all-sites.RDS") |> 
#   janitor::clean_names()


ltem  <- readRDS( "data/fish_datagr_prod-by-species-all-sites-integrada.RDS")
  

regions <- c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")

# Filter by regions of interest
filtered_ltem <- ltem %>%
  filter(year >= 2010) %>% 
  filter(region %in% regions) #%>%
  # filter(reef %in% c("CANTIL_MEDIO", "CASITAS", "ISLOTE_CABO_PULMO", "LOBERA_CABO_PULMO", "MORROS_CABO_PULMO", #"SAN_JOSE_ANIMAS_NORTE", 
  #                    "BURROS", "SAN_DIEGO_ABNEGADO", "REFUGIO_MORENA", "PUNTA_BOTELLA", "ISLOTE_AGUA_VERDE",
  #                    "ESPIRITU_SANTO_PARTIDA_NORESTE", "ESPIRITU_SANTO_ISLOTES_NORTE", "ESPIRITU_SANTO_BALLENA", "ESPIRITU_SANTO_PAILEBOTE", "ESPIRITU_SANTO_PUNTA_LOBOS",
  #                    "CARMEN_ABISMO", "CORONADO_LAJAS", "CORONADO_MONO", "DANZANTE_BIZNAGA", "MONSERRAT_PUNTA_SURESTE"
  # ))
# filter(family == "Scaridae")
# filter(family %in% c("Carcharhinidae", "Ginglymostomatidae", "Sphyrnidae"))  



site_summary <- filtered_ltem %>%
  group_by(region, reef, depth2) %>%
  summarise(total_years = n_distinct(year), .groups = 'drop')

threshold_years <- 6

# Filter sites with consistent data based on threshold_years
consistent_sites <- site_summary %>%
  filter(total_years >= threshold_years)

# Join consistent sites back to filtered_ltem
filtered_ltem <- filtered_ltem %>%
  inner_join(consistent_sites, by = c("region", "reef", "depth2"))

# Select relevant columns
site_characteristics <- filtered_ltem %>%
  select(year, region, reef, depth, depth2, total_years) %>%
  distinct()

# Join with consistent sites to get final data
final_data <- filtered_ltem %>%
  inner_join(consistent_sites, by = c("region", "reef", "depth2"))

# Check the number of sites by region, depth, and protection
site_counts <- final_data %>%
  group_by(region, year, depth2) %>%
  summarise(num_sites = n_distinct(reef), .groups = 'drop')

print(site_counts)

# Count the number of sites by year and region
site_counts_by_year_region <- final_data %>%
  group_by(region, year) %>%
  summarise(num_sites = n_distinct(reef), .groups = 'drop')

print(site_counts_by_year_region)

final_data <- final_data %>%
  rename(total_years = total_years.x) %>%
  select(-total_years.y)|> 
  filter(habitat %in% c("BLOQUES", "PARED"))


# Print the structure of final_data
str(final_data)

sites <- final_data |> 
  distinct(region, reef, depth2, total_years)
  
# Save the file with saveRDS
saveRDS(final_data, "data/standardized_ltem_sites.RDS")
