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

### Jeremy Clouthier End of Day Debrief April 23, 2026 ###
# nondoctrees_expanded is now cleaned and is the nondoc trees master list
# soverdi_trees has genus species columns - maybe merge them somehow for consistency
# mtl_trees remains untouched