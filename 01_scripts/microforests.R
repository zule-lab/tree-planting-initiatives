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

# Top 10 Bird species across all sites
library(dplyr)

top_species <- microforest_tags %>%
  count(Species, sort = TRUE)
