# Load libraries --------------------------------------------------------

library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(ggplot2)     # For data visualization
library(vegan)       # For ecological community analysis

# Load data ------------------------------------------------------------

# sppdata <- readRDS("data/fish_datagr_prod-by-species-all-sites.RDS") |> 
#   janitor::clean_names()

# ltem <- readRDS("data/standardized_ltem_sites.RDS") 

sppdata <- readRDS("data/fish_datagr_prod-by-species-all-sites-integrada.RDS")

# Summarize for each transect -----------------------------------------

data_prod_brut <- sppdata |> 
  # Sum for each transect
  group_by(year, region, reef, depth2, transect, diet) %>%
  mutate(
    biom = sum(biom) / area,    # Biomass (kg ha^−1)
    prod = sum(prod) / area,    # Production (g d^−1 ha^−1)
    productivity = (prod / biom) * 100) %>%
  ungroup() |> 
  # Mean for each site
  group_by(year, region, reef, transect, diet) %>%
  mutate(
    biom = mean(biom),
    prod = mean(prod),
    productivity = mean(productivity)) %>% 
  ungroup() %>%
  # Transforming data
  mutate(
    log10ProdB = productivity,          # % per day
    log10Biom = log10(biom + 1),        # (g m -2)
    log10Prod = log10(prod + 1),        # (g m-2 d-1)
    latitude = as.numeric(as.character(latitude)),
    longitude = as.numeric(as.character(longitude)))

# Print summary statistics ---------------------------------------------

print(paste("Minimum biomass:", min(data_prod_brut$biom)))
print(paste("Maximum biomass:", max(data_prod_brut$biom)))
print(paste("Minimum production:", min(data_prod_brut$prod)))
print(paste("Maximum production:", max(data_prod_brut$prod)))
print(paste("Minimum turnover:", min(data_prod_brut$productivity)))
print(paste("Maximum turnover:", max(data_prod_brut$productivity)))

# Data management -------------------------------------------------------

biom75 = quantile(data_prod_brut$log10Biom, 0.95)
biom25 = quantile(data_prod_brut$log10Biom, 0.25)
prod75 = quantile(data_prod_brut$log10ProdB, 0.75)
prod25 = quantile(data_prod_brut$log10ProdB, 0.25)
max_biom = max(data_prod_brut$log10Biom)

# Diving data into 3 classes for each biomass/productivity relationship

management = data_prod_brut %>% 
  mutate(class = ifelse(log10Biom < biom25 & log10ProdB < prod25, "deadzone",
                        ifelse(log10ProdB > prod75, "partial",
                               ifelse(log10Biom > biom75 & log10ProdB < prod75, "pristine", "transition")))) %>%
  mutate(class = as.factor(class))

range(management$productivity)
range(management$biom)
range(management$prod)

unique(management$class)


# Clean data ------------------------------------------------------------

fish <- management |> 
  filter(label == "PEC") |> 
  filter(year >= 2010) |> 
  filter(region %in% c("Loreto", "Corredor", "La Paz", "Cabo Pulmo")) |> 
  # filter(family == "Scaridae")
  filter(family %in% c("Serranidae", "Lutjanidae", "Scaridae")) |>
  # filter(family %in% c("Carcharhinidae", "Caranx", "Sphyrnidae")) |> 
  # filter(genus %in% c("Mycteroperca", "Ginglymostomatidae", "Epinephelus", "Hoplopagrus", "Lutjanus", "Scarus", "Caulolatilus", "Paranthias" )) |> 
  # filter(species %in% c("Mycteroperca rosacea", "Mycteroperca jordani",
  #                       "Caranx caballus", "Caranx caninus", "Caranx sexfasciatus",
  #                       "Epinephelus acanthistius", "Epinephelus itajara",
  #                       "Hoplopagrus guntheri", "Mycteroperca xenarcha",
  #                       "Lutjanus argentiventris", "Lutjanus novemfasciatus",
  #                       "Scarus ghobban", "Scarus compressus", "Scarus perico",
  #                       "Caulolatilus princeps", "Lutjanus guttatus",
  #                       "Caulolatilus affinis", "Paranthias colonus")) |> 
  mutate(region = factor(region, levels = c("Loreto", "Corredor", "La Paz", "Cabo Pulmo"))) |> 
  mutate(
    a_ord = as.numeric(a),
    b_pen = as.numeric(b),
    quantity = as.numeric(quantity),
    size = as.numeric(size),
    area = as.numeric(area),
    month = as.numeric(month),
    biomass = (quantity * a_ord * (size^b_pen)) / (area * 100) # Formula to calculate biomass (ton/ha)
  ) |> 
  mutate(biomass = as.numeric(biomass))

str(fish)

unique(fish$species)

# Process community data -----------------------------------------------

community_data <- fish |> 
  group_by(year, mpa, protection_level, region, reef, habitat, transect, depth2, diet, family) |>
  summarise(
    abundance = sum(quantity, na.rm = TRUE),
    richness = n_distinct(species),
    biomass = sum(biomass, na.rm = TRUE),
    prod = mean(prod), 
    productivity = mean(productivity),
    size = mean(size, na.rm = TRUE)) |> 
  group_by(year, region, reef, transect, family) |>
  summarise(
    abundance = mean(abundance),
    richness = sum(richness),
    avg_biomass = mean(biomass, na.rm = TRUE),
    prod = mean(prod), 
    productivity = mean(productivity),
    size = mean(size))

unique(community_data$reef)

