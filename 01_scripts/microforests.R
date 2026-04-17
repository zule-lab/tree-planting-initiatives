# Jeremy Clouthier

aru_data <- read.csv("00_rawdata/B_data-collection/aru-details_final.csv")
tags <-read.csv("02_outdata/tags_filtered.csv")

# Filter for micro forest sites
library(dplyr)
micro_forest_data <- aru_data %>% 
  filter(Planting.Type == "Microforest")

library(readr)
write.csv(micro_forest_data, "microforest_data.csv")

# Load in microforest data 
library(readr)
mf_data <- read_csv("02_outdata/microforest_data.csv")

# Assign richness per site based on treatment (Planted vs. control)
library(dplyr)
richness_data <- mf_data %>%
  group_by(Site_base, Treatment) %>%
  summarise(Richness = mean(Richness), .groups = "drop")

# Pivot to wide format to have richness for each treatment by site
library(tidyr)

richness_site <- richness_data %>%
  pivot_wider(
    names_from = Treatment,
    values_from = Richness
  )

# Run t.test on paired richness data
t.test(richness_site1$Planted,
       richness_site1$Control,
       paired = TRUE)

# Summary statistics for microforest sites, mean, median, min/max SR
# Done on all sites, -1 and -2

# Using mf_data which has a column for treatment = planted/control
mf_summary <- mf_data %>%
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

# Filter to micro forest sites with unique recorded species, no duplicates
mf_bird_diff <- tags %>%
  mutate(Site_base = sub("-[12]$", "", Site)) %>%
  filter(Site_base %in% mf_speciesdiff) %>%
  select(Site, Site_base, Species) %>%
  distinct()

# Separate into planted and control
planted <- mf_bird_diff %>%
  filter(grepl("-1$", Site))

control <- mf_bird_diff %>%
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

top_species <- mf_difference %>%
  count(Species, sort = TRUE)
