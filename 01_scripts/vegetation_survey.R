# Jeremy Clouthier April 23, 2026
# Note - dead trees are counted
# Cleaning up the vegetation survey data
mtl_trees <- read.csv("00_rawdata/B_data-collection/mtl_trees.csv")
nondoc_trees <- read.csv("00_rawdata/B_data-collection/nondoc_trees.csv")
soverdi_trees <- read.csv("00_rawdata/B_data-collection/soverdi_trees.csv")

# Check for duplicate entries
soverdi_fid <- read.csv("00_rawdata/B_data-collection/soverdi_trees.csv")
unique(soverdi_fid$fid) %>% length()
nrow(soverdi_fid)

nondoc_fid <- read.csv("00_rawdata/B_data-collection/nondoc_trees.csv")
unique(nondoc_fid$fid) %>% length()
nrow(nondoc_fid)

# Extract the columns of interest - Site name, species, dbh, etc...
library(tidyverse)
soverdi_trees <- soverdi_trees %>% 
  select(Site_1, dbh_2025, genre, nomespece, height2025, Comments, 
         Measured_DBH, Measured_height, Present)

nondoc_trees <- nondoc_trees %>% 
  select(Site, dbh_2025, height2025, species_clean, Species, Source, Comments, 
         Measured_DBH, Measured_height)

# Rename columns to have consistent names across all layers
soverdi_trees <- soverdi_trees %>% rename(DBH = dbh_2025)
soverdi_trees <- soverdi_trees %>% rename(Site = Site_1)
soverdi_trees <- soverdi_trees %>% rename(Genus = genre)
soverdi_trees <- soverdi_trees %>% rename(Species = nomespece)
soverdi_trees <- soverdi_trees %>% rename(Height = height2025)

nondoc_trees <- nondoc_trees %>% rename(DBH = dbh_2025)
nondoc_trees <- nondoc_trees %>% rename(Species_1 = species_clean)
nondoc_trees <- nondoc_trees %>% rename(Species_2 = Species)
nondoc_trees <- nondoc_trees %>% rename(Height = height2025)



# Start removing entries with blank data or NA
# DBH column - Sum entries with multi-stems, remove blank entries
soverdi_trees <- soverdi_trees %>%
  filter(DBH != "") %>%
  rowwise() %>%
  mutate(
    DBH = sum(as.numeric(trimws(unlist(strsplit(DBH, ",")))), na.rm = TRUE)
  ) %>%
  ungroup()

nondoc_trees <- nondoc_trees %>%
  filter(DBH != "") %>%
  rowwise() %>%
  mutate(
    DBH = sum(
      as.numeric(trimws(unlist(strsplit(DBH, ",")))),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Merge soverdi trees genus and species column into single species column
library(stringr)

soverdi_trees <- soverdi_trees %>%
  mutate(
    Species = str_c(Genus, Species, sep = " ")
  )
# Remove left over genus column
soverdi_trees <- soverdi_trees %>%
  select(-Genus)

# Comments column - Managing count entries - ex: row has one species with x5
count_trees <- nondoc_trees %>%
  filter(str_detect(Comments, "[xX]\\s*\\d+"))

# Clean Up the Species Column - Species entries are scattered across columns
# Remove Leave Comment in Species_2 column, change any empties to NA, and
# change Other to NA
unique(nondoc_trees$Comments)

nondoc_trees <- nondoc_trees %>%
  mutate(
    Species_1 = na_if(Species_1, ""),
    Species_1 = na_if(Species_1, "Other"),
    Species_2 = na_if(Species_2, ""),
    Species_2 = na_if(Species_2, "Leave Comment"),
    Species_2 = na_if(Species_2, "Leave comment")
  )

# Merge columns Species_2 to Species_1
nondoc_trees <- nondoc_trees %>%
  mutate(
    Species_1 = coalesce(Species_1, Species_2)
  )

# Create master list of species to keep from unique function
# Use list to flag species in Comments, then transfer to Species_1
species_list <- unique(nondoc_trees$Species_1[!is.na(nondoc_trees$Species_1)])

nondoc_trees$Species_1 <- ifelse(
  is.na(nondoc_trees$Species_1),
  str_extract(nondoc_trees$Comments, str_c(species_list, collapse = "|")),
  nondoc_trees$Species_1
)
# Extract any potential binomial names - This will pull random two word phrases also
nondoc_trees$species_guess <- str_extract(
  nondoc_trees$Comments,
  "\\b[A-Z][a-z]+\\s[a-z]+\\b")
# Check the extraction and see the highest frequency 
sort(table(nondoc_trees$species_guess), decreasing = TRUE)

# Add the unique extracted names to a list and filter out any NA
extracted_list <- unique(nondoc_trees$species_guess)
extracted_list <- extracted_list[!is.na(extracted_list)]

# Compare the extracted species and phrases to master list 
new_species <- setdiff(extracted_list, species_list)
# Inspect the list of species
sort(new_species)

# Manually add the species that show up in Comments but not in Species_1
inspected_new_species <- c(
  "Aesculus hippocastanum","Betula pendula","Betula pubescens","Crataegus sp", 
  "Crataegus submollis", "Euonymus alatus", "Frangula alnus", "Hydrangea paniculata",
  "Lonicera spp", "Malus huphensis", "Picea sp", "Populus canadensis", "Salix euxina", 
  "Rhamnus cathartica", "Taxus cuspidata", "Thuya occidentalis", "Tilia europea") 

# Fix any typos in the inspected list
inspected_new_species[inspected_new_species == "Thuya occidentalis"] <- "Thuja occidentalis"

# Add unique new species to master list
master_species <- unique(c(species_list, inspected_new_species))

# Use master list to extract species from Comments
pattern <- str_c(master_species, collapse = "|")

nondoc_trees$species_from_comments <- str_extract(nondoc_trees$Comments, pattern)

# Use fuzzy matching to catch any missing typos in Comments
install.packages("stringdist")
library(stringdist)

nondoc_trees$species_from_comments <- sapply(nondoc_trees$Comments, function(x) {
  dist <- stringdist::stringdist(x, master_species)
  if (min(dist) <= 2) {
    master_species[which.min(dist)]
  } else {
    NA
  }
})

# Fill in the blanks in Species_1
nondoc_trees$Species_1 <- ifelse(
  is.na(nondoc_trees$Species_1),
  nondoc_trees$species_from_comments,
  nondoc_trees$Species_1
)

# Check what is still missing/failed
nondoc_trees$Comments[is.na(nondoc_trees$Species_1)]

# Fix obvious species but bad match due to small typo
nondoc_trees$Comments[nondoc_trees$Comments == "Common buckthorn "] <- "Rhamnus cathartica"
nondoc_trees$Comments[nondoc_trees$Comments == "Hydrangea paniculatum "] <- "Hydrangea paniculata"


# Fix entries that have frequency by adding the corresponding rows
# Create a column with frequency counts
library(stringr)
nondoc_trees$count <- as.numeric(str_extract(nondoc_trees$Comments, "(?<=x)\\d+"))
# Replace any NA with 1 observation
nondoc_trees$count[is.na(nondoc_trees$count)] <- 1
# Duplicate rows based on count
nondoctrees_expanded <- nondoc_trees %>%
  uncount(weights = count)
# Ensure master list is still good
pattern <- str_c(master_species, collapse = "|")
# Create species from comments column to pull from
nondoctrees_expanded$species_from_comments <- str_extract(
  nondoctrees_expanded$Comments,
  pattern)
# Apply species names to Species_1 pulling from species_comments column
nondoctrees_expanded$Species_1 <- ifelse(
  is.na(nondoctrees_expanded$Species_1),
  nondoctrees_expanded$species_from_comments,
  nondoctrees_expanded$Species_1)
# Rename Species_1 to Species for consistency across layers
nondoctrees_expanded <- nondoctrees_expanded %>% rename(Species = Species_1)

### Jeremy Clouthier End of Day Debrief April 23, 2026 ###
# nondoctrees_expanded is now cleaned and is the nondoc trees master list
# soverdi_trees has genus species columns - maybe merge them somehow for consistency
# mtl_trees remains untouched

## Jeremy Clouthier April 24, 2026 ##
# Select the desired columns from mtl_trees
library(tidyverse)
mtl_trees <- mtl_trees %>% 
  select(Site_1, dbh_2025, fullname, height2025, Comments, 
         Measured_DBH, Measured_height)

# Rename column names to be consistent with other layers
mtl_trees <- mtl_trees %>% rename(DBH = dbh_2025)
mtl_trees <- mtl_trees %>% rename(Species = fullname)
mtl_trees <- mtl_trees %>% rename(Height = height2025)
mtl_trees <- mtl_trees %>% rename(Site = Site_1)

# Remove blank entries for DBH column and sum multi-stem entries
mtl_trees <- mtl_trees %>%
  filter(DBH != "") %>%
  rowwise() %>%
  mutate(
    DBH = sum(
      as.numeric(trimws(unlist(strsplit(DBH, ",")))),
      na.rm = TRUE
    )
  ) %>%
  ungroup()

# Fix inaccurate identifications that were pointed out in comments
mtl_trees[174, 3] <- "Acer freemanii" # Was Acer rubrum
mtl_trees[195, 3] <- "Juglans nigra" # Was Gymnocladus dioicus
mtl_trees[214, 3] <- "Populus spp" # Was Populas deltoides
mtl_trees[259, 3] <- "Acer platanoides" # Was Acer saccharinum


## Combine all tree layers into one data frame ##
all_trees <- bind_rows(
  mtl_trees %>% mutate(layer = "mtl"),
  soverdi_trees %>% mutate(layer = "soverdi"),
  nondoctrees_expanded %>% mutate(layer = "nondoctrees")
) %>%
  select(Site, Species, layer, DBH, Height)

# Remove any entries with empty values for sites
all_trees <- all_trees %>%
  filter(!is.na(Site) & Site != "")

# Mean DBH per site
mean_dbh_site <- all_trees %>%
  group_by(Site) %>%
  summarise(mean_dbh = mean(DBH, na.rm = TRUE))
# Plotting mean dbh per site
library(ggplot2)

ggplot(mean_dbh_site, aes(x = Site, y = mean_dbh)) +
  geom_col() +
  labs(
    title = "Mean DBH per Site",
    x = "Site",
    y = "Mean DBH" 
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Mean DBH per site AND layer
mean_dbh_site_layer <- all_trees %>%
  group_by(Site, layer) %>%
  summarise(mean_dbh = mean(dbh, na.rm = TRUE))
# Plotting mean dbh per site based on layer
ggplot(mean_dbh_site_layer, aes(x = Site, y = mean_dbh, fill = layer)) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean DBH per Site by Layer",
    x = "Site",
    y = "Mean DBH"
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Most frequent species per site
species_counts <- all_trees %>%
  group_by(Site, Species) %>%
  summarise(n = n(), .groups = "drop")

most_freq_species <- species_counts %>%
  group_by(Site) %>%
  slice_max(order_by = n, n = 1, with_ties = TRUE)

# Calculating basal area by converting DBH in cm to meters
all_trees <- all_trees %>%
  mutate(
    dbh_m = DBH / 100,
    basal_area = pi * (dbh_m / 2)^2
  )
# Total Basal area per site
basal_area_site <- all_trees %>%
  group_by(Site) %>%
  summarise(
    total_ba = sum(basal_area, na.rm = TRUE),
    mean_ba = mean(basal_area, na.rm = TRUE),
    n = n()
  )

# Total Basal area per site AND layer
basal_area_site_layer <- all_trees %>%
  group_by(Site, layer) %>%
  summarise(
    total_ba = sum(basal_area, na.rm = TRUE),
    .groups = "drop"
  )

# Relative Dominance 
rel_dom_species <- all_trees %>%
  group_by(Site, Species) %>%
  summarise(
    species_ba = sum(basal_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Site) %>%
  mutate(
    total_ba = sum(species_ba),
    rel_dominance = species_ba / total_ba
  )
# Change to percentage
rel_dom_species <- rel_dom_species %>%
  mutate(rel_dominance_pct = rel_dominance * 100)

# Global top 10 species by relative dominance
top10_species <- all_trees %>%
  group_by(Species) %>%
  summarise(total_ba = sum(basal_area, na.rm = TRUE)) %>%
  arrange(desc(total_ba)) %>%
  slice_head(n = 10) %>%
  pull(Species)

rel_dom_top10 <- rel_dom_species %>%
  filter(Species %in% top10_species)

rel_dom_top10 <- rel_dom_species %>%
  mutate(
    Species = ifelse(Species %in% top10_species, Species, "Other")
  ) %>%
  group_by(Site, Species) %>%
  summarise(rel_dominance = sum(rel_dominance), .groups = "drop")

# Top 10 relative dominance per site
rel_dom_top_site <- rel_dom_species %>%
  group_by(Site) %>%
  slice_max(order_by = rel_dominance, n = 10)

# Plotting global relative dominance
library(ggplot2)

ggplot(rel_dom_top10, aes(x = Site, y = rel_dominance, fill = Species)) +
  geom_col() +
  labs(
    title = "Relative Dominance (Top 10 Species)",
    y = "Relative Dominance"
  ) + scale_fill_discrete(
    breaks = c(top10_species, "Other")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting top 10 relative dominance per site
library(ggplot2)

ggplot(rel_dom_top_site, aes(x = Site, y = rel_dominance, fill = Species)) +
  geom_col() +
  labs(
    title = "Relative Dominance (Top 10 Species)",
    y = "Relative Dominance"
  ) + scale_fill_discrete(
    breaks = c(top10_species, "Other")
  ) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Jeremy Clouthier End of Day Debrief April 24, 2026 ###
# Finished cleaning up each layer data frame and merged into all_trees
# Analyzed mean dbh per site and by layer
# Analyzed relative dominance per site and by layer