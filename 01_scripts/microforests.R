# Jeremy Clouthier

aru_data <- read.csv("00_rawdata/ARU_deployment_data.csv")
tags <-read.csv("02_outdata/tags_filtered.csv")

# Filter for micro forest sites

writecsv
# Load in microforest data 
library(readr)
#mf_data <- read_csv("00_rawdata/microforest_data.csv")

#Reorganizing the microforest filtering
filtered_tags <- read.csv("02_outdata/B_species-lists/tags_filtered.csv")
microforest.location <- c("MARQ-1", "MARQ-2", "PELI-2", "WILF-1", "WILF-2", "ZOTI-1", "ZOTI-2", "VANI-1", "VANI-2")
microforest.tags <- filtered_tags %>% filter(location %in% microforest.location)


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
