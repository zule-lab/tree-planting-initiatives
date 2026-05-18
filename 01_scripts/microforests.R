# Jeremy Clouthier

aru_data <- read.csv("00_rawdata/B_data-collection/aru-details_final.csv")
tags <-read.csv("02_outdata/B_Species-lists/tags_filtered.csv")
names(tags)[names(tags) == "location"] <- "Site"
names(tags)[names(tags) == "species_common_name"] <- "Species"

# Filter for micro forest sites
library(dplyr)
micro_forest_data <- aru_data %>% 
  filter(Planting.Type == "Microforest")

# Remove uncessary column
micro_forest_data$Site.. <- NULL

# Rename columns so they match with richness data, for later
names(micro_forest_data)[names(micro_forest_data) == "Site"] <- "Full_name"
names(micro_forest_data)[names(micro_forest_data) == "ARU.ID"] <- "Site"

# Save micro forest data as csv
library(readr)
write.csv(micro_forest_data, "microforest_data.csv")

# Load in microforest data 
library(readr)

mf_data <- read_csv("02_outdata/microforest_data.csv")

# Add species richness for each site
richness_allsites <- read.csv("02_outdata/SR-by-site.csv")

# Rename columns so they match with micro forest data
richness_allsites$X <- NULL
names(richness_allsites)[names(richness_allsites) == "site.name"] <- "Site"
names(richness_allsites)[names(richness_allsites) == "SR"] <- "Richness"

# Join richness data to matching site
mf_richness <- micro_forest_data %>%
  left_join(richness_allsites %>% select(Site, Richness),
            by = "Site")

# Remove Parc Pelican rows since ARU was stolen for one site
mf_richness <- na.omit(mf_richness)

#Rename Columns to prepare for t.test 
names(mf_richness)[names(mf_richness) == "ARU.location"] <- "Treatment"
# Rename In -> Planted, Out -> Control
mf_richness$Treatment[mf_richness$Treatment == "In"] <- "Planted"
mf_richness$Treatment[mf_richness$Treatment == "Out"] <- "Control"

# Create Site_Base column to perform paired t.test
mf_richness$Site_base <- sub("-[12]$", "", mf_richness$Site)

# Assign richness per site based on treatment (Planted vs. control)
library(dplyr)
treatment_data <- mf_richness %>%
  group_by(Site_base, Treatment) %>%
  summarise(Richness = mean(Richness), .groups = "drop")

# Pivot to wide format to have richness for each treatment by site
library(tidyr)

treatment_richness <- treatment_data %>%
  pivot_wider(
    names_from = Treatment,
    values_from = Richness
  )

# Run t.test on paired richness data
t.test(treatment_richness$Planted,
       treatment_richness$Control,
       paired = TRUE)

# Summary statistics for microforest sites, mean, median, min/max SR
# Done on all sites, -1 and -2

# Using mf_data which has a column for treatment = planted/control
mf_summary <- mf_richness %>%
  group_by(Treatment) %>%
  summarise(
    mean_richness   = mean(Richness, na.rm = TRUE),
    median_richness = median(Richness, na.rm = TRUE),
    min_richness    = min(Richness, na.rm = TRUE),
    max_richness    = max(Richness, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Determining which species occur in planted sites, but not in control sites
# Planted -> -1, Control -> -2
# Create list of unique species split into planted and control

microforest_sites <- c("VANI", "WILF", "ZOTI", "MARQ")

microforest_tags <- tags %>%
  mutate(Site_base = sub("-[12]$", "", Site))

microforest_tags <- microforest_tags %>%
  filter(Site_base %in% microforest_sites)

microforest_tags <- microforest_tags %>%
  select(Site, Species) %>%
  distinct()

planted <- microforest_tags %>%
  filter(grepl("-1$", Site))

control <- microforest_tags %>%
  filter(grepl("-2$", Site))

mf_bird_diff <- setdiff(planted$Species, control$Species)

# Filter to micro forest sites with unique recorded species, no duplicates
mf_species_diff <- tags %>%
  mutate(Site_base = sub("-[12]$", "", Site)) %>%
  filter(Site_base %in% microforest_sites) %>%
  select(Site, Site_base, Species) %>%
  distinct()

# Separate into planted and control
planted <- mf_species_diff %>%
  filter(grepl("-1$", Site))

control <- mf_species_diff %>%
  filter(grepl("-2$", Site))

# Anti-join for site-specific difference. 
# Interpretation: Column 1 site -> Planted; Column 2 -> Species unique to planted site
library(dplyr)

site_differences <- planted %>%
  anti_join(control, by = c("Site_base", "Species"))

site_differences <- site_differences %>%
  select(Site_base, Species)

# Top 10 Bird species across all sites, with added site occurrence
library(dplyr)
names(mf_data)[names(mf_data) == "species_common_name"] <- "Species"

combined <- mf_data %>%
  group_by(Species) %>%
  summarise(
    total_observations = n(),
    sites_present = n_distinct(Site),
    .groups = "drop"
  ) %>%
  arrange(desc(total_observations))


# Take Two: Microforest Data ----------------------------------------------

filtered_tags <- read.csv("02_outdata/B_species-lists/tags_filtered.csv")
microforest.location <- c("MARQ-1", "MARQ-2", "PELI-2", "WILF-1", "WILF-2", "ZOTI-1", "ZOTI-2", "VANI-1", "VANI-2")
microforest.tags <- filtered_tags %>% filter(location %in% microforest.location)

# Get SR of each microforest site (planted and control)
empty_matrix <- matrix(nrow=length(microforest.location), ncol=2)

for(i in 1:length(microforest.location)) {
  
  site.name <- microforest.location[[i]]
  number.spp <-microforest.tags %>% filter(location == site.name) %>% 
    distinct(species_code) %>% nrow()
  empty_matrix[i,1] <- site.name
  empty_matrix[i, 2] <- number.spp
  
}

# Rename empty data frame to sr.df and add column names
microforest.df <- as.data.frame(empty_matrix)

colnames(microforest.df) <- c("site.name", "SR")


library(tidyr)
microforest.df2 <- microforest.df %>% tidyr::separate(site.name, c("Park", "Treatment.number"), sep = "-", remove = FALSE)
microforest.df2$Treatment.name <- with(microforest.df2, ifelse(Treatment.number == "1", "Planted", "Control"))
microforest.df2$SR <- as.numeric(microforest.df2$SR)
microforest.df2

# Calculate summary statistics --------------------------------------------

microforest.df2.control <- microforest.df2 %>% filter(Treatment.name == "Control")
microforest.df2.control
mean(microforest.df2.control$SR)

# Most Common Species -----------------------------------------------------

microforest.tags %>% distinct(location, species_code)

unique.species <- unique(microforest.tags$species_code)

sites.per.spp <- matrix(nrow=length(unique.species), ncol=2)

for(i in 1:length(unique.species)) {
  species.name <- unique.species[[i]]
  species.tags <-microforest.tags %>% filter(species_code == species.name) 
  n.locations <- species.tags %>% distinct(location) %>% nrow()
  sites.per.spp[i, 1] <- species.name
  sites.per.spp[i, 2] <- n.locations
  
}

sites.per.spp.df <- as.data.frame(sites.per.spp)


colnames(sites.per.spp.df) <- c("Species", "n.sites")

sites.per.spp.df$n.sites.fixed<- as.numeric(sites.per.spp.df$n.sites)
str(sites.per.spp.df)
sites.per.spp.df.sorted <- sites.per.spp.df[order(-sites.per.spp.df$n.sites.fixed),]
sites.per.spp.df.sorted


# Most common species in planted sites only -------------------------------

microforest.planted.sites <- c("MARQ-1", "WILF-1", "ZOTI-1",  "VANI-1")
microforest.planted.tags <- filtered_tags %>% filter(location %in% microforest.planted.sites)

mf.unique.species <- unique(microforest.planted.tags$species_code)

mf.sites.per.spp <- matrix(nrow=length(mf.unique.species), ncol=2)

for(i in 1:length(mf.unique.species)) {
  species.name <- mf.unique.species[[i]]
  species.tags <-microforest.planted.tags %>% filter(species_code == species.name) 
  n.locations <- species.tags %>% distinct(location) %>% nrow()
  mf.sites.per.spp[i, 1] <- species.name
  mf.sites.per.spp[i, 2] <- n.locations
  
}

mf.sites.per.spp.df <- as.data.frame(mf.sites.per.spp)


colnames(mf.sites.per.spp.df) <- c("Species", "n.sites")
mf.sites.per.spp.df

mf.sites.per.spp.df$n.sites.fixed<- as.numeric(mf.sites.per.spp.df$n.sites)
str(mf.sites.per.spp.df)
mf.sites.per.spp.df.sorted <- mf.sites.per.spp.df[order(-mf.sites.per.spp.df$n.sites.fixed),]
mf.sites.per.spp.df.sorted
