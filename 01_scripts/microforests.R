# Jeremy Clouthier

# Load in microforest data 
library(readr)
mf_data <- read_csv("00_rawdata/microforest_data.csv")

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