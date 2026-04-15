# 26MAY2025
# SIENA BLIER
# DEPLOYED ARU COORDINATES


# 0.  Load Libraries ------------------------------------------------------

library(sf)
library(dplyr)
library(tidyverse)

# Load csv and only keep needed columns
deployed_arus <- read.csv("csv_files/aru_coordinates.csv") %>% 
  as.data.frame() %>%
  select(Site, Coordinates) 

# Separate coordinates into Lat and Long columns + remove sites without coordinates
deployed_arus <- separate(deployed_arus, Coordinates, sep =",", into=c("Lat", "Long")) %>% 
  na.omit()

# Reorder columns for sf package 
deployed_arus <- select(deployed_arus, Site, Long, Lat) 

# Make shapefile of coords
deployed_arus_sf <- st_as_sf(deployed_arus, coords = c("Long", "Lat")) %>% st_set_crs(4326)
str(deployed_arus_sf)
st_crs(deployed_arus_sf)
st_crs(soverdi_shp)

st_write(deployed_arus_sf, "Shapefiles/deployed_arus.shp", append = FALSE, delete_layer = TRUE)


# 2.  Look at IRBV data ---------------------------------------------------

irbv_data <- read.csv("csv_files/arbres_gp_2023_2024.csv")
str(irbv_data)
unique(irbv_data$id_microforet) 
irbv_micros_data <- irbv_data %>%
  filter(type == "Microforet") 

unique(irbv_micros_data$id_microforet)


# 3. Filter Soverdi data to sites of interest -------------------------------------------------

unique(deployed_arus$Site)

list_sov_sites <- c("Campus Loyola", "Université de Montréal", "UdeM", "Hôpital en santé mentale Rivière-des-Prairies", 
                    "5555 Salaberry", "Carrefour", 
                    "Institut universitaire en Santé Mentale de Montréal et CHSLD Jeanne Le Ber",
                    "Cégep Marie-Victorin", "Institut universitaire en santé mentale Douglas",
                    "Hôpital Maisonneuve-Rosemont")
#^ Removed Boise Steinberg

sov_data_filtered <- soverdi_data %>% filter(site %in% list_sov_sites)

unique(sov_data_filtered$site) # Looks good - should have 11 items


# 4. Summarize Soverdi data -----------------------------------------------


# Function to get mode (most frequent value)
get_mode <- function(x) {
  ux <- unique(na.omit(x))       # remove NAs if any
  ux <- as.character(ux)  # coerce to character if needed
  ux[which.max(tabulate(match(x, ux)))]
}

aru_site_summ <- sov_data_filtered %>% 
  group_by(site) %>% 
  summarise(mean_dhs = mean(dhs,na.rm = TRUE),
            mean_hauteur = mean(hauteur, na.rm = TRUE),
            count = n(),
            year = median(annee, na.rm=TRUE),
            sd_height = sd(hauteur, na.rm=TRUE),
            sd_dhs = sd(dhs, na.rm=TRUE),
            mode_reseau = get_mode(reseau),
            mode_type = get_mode(typeevenement),
            borough = get_mode(entiteadministrative))

glimpse(aru_site_summ)
aru_site_summ %>% summary()

#sov_data_filtered$measure_dbh <- sov_data_filtered$hauteur >= 200

species_counts <- sov_data_filtered %>% 
  group_by(genre,nomespece) %>%
  count() %>%
  as.data.frame()


# 5. Species lists --------------------------------------------------------

# Get list of species from LOYO-1
loyo1_trees <- read.csv("loyo1_trees.csv")


loyo1_spp <- loyo1_trees %>%
  group_by(genre,nomespece) %>%
  count() %>%
  as.data.frame() %>%
  write.csv("csv_files/loyo1_spp.csv")

tree_spp_counts <- species_counts[order(species_counts$n, decreasing = TRUE),]
rm(species_counts)

# List of all Sov species
sov_spp <- sov_data_filtered %>% 
  group_by(genre,nomespece) %>%
  count() %>%
  as.data.frame()

sov_spp$fullname <- paste(sov_spp$genre,sov_spp$nomespece)

sov_spp$fullname %>% write.csv("csv_files/sov_spp.csv")

# List of City Trees

city_spp <- read.csv("city_trees.csv") %>% 
  group_by(genus,species) %>%
  count() %>%
  as.data.frame()

city_spp$fullname <- paste(city_spp$genus,city_spp$species)

biglist_spp <- c(sov_spp$fullname, city_spp$fullname)
filtered_biglist <- unique(biglist_spp) %>% as.data.frame()
colnames(filtered_biglist) <- "Value"
filtered_biglist$Species1 <- filtered_biglist$Value
filtered_biglist %>% write.csv("csv_files/biglist_spp.csv", row.names = FALSE)
read.csv("csv_files/biglist_spp.csv")  
  
# List of species at UdeM site
udem_trees <- read.csv("csv-files/scrap-paper.csv")


udem_spp <- udem_trees %>%
  group_by(genre,nomespece) %>%
  count() %>%
  arrange(desc(n)) %>%
  write.csv("csv-files/udem_spp.csv")

tree_spp_counts <- species_counts[order(species_counts$n, decreasing = TRUE),]
rm(species_counts)
  