# 11MAR2026
# Processing Tag Report from Wildtrax
# Script by: Siena Blier


# 0. Set-up ---------------------------------------------------------------


# Load data
tags <- read.csv("00_rawdata/B_data-collection/aru-report.csv")

# Load libraries

library(dplyr)


# 1. Explore & Clean Data -------------------------------------------------


# View data
str(tags)

# keep only variables of interest

tags_reduced <- tags %>% select(location, recording_date_time, species_code, 
                                species_common_name, tag_rating, tag_is_verified)

# Make sure all tags are verified
tags_reduced %>% select(tag_is_verified) %>% unique() # All good!

# Remove that column
tags_reduced <- tags_reduced %>% select(-tag_is_verified)

# Look at data structure
str(tags_reduced)

# Check possible tag values
tags_reduced %>% select(tag_rating) %>% table(useNA = "always")

# Remove all false positives and ambiguous REVI/PHVI detections
tags_filtered <- tags_reduced %>% filter(tag_rating == 5 | is.na(tag_rating))
#tags_ambiguous <- tags_reduced %>% filter(tag_rating == 3)
#tags_filtered <- tags_reduced %>% filter(tag_rating != 1 | is.na(tag_rating))

write.csv(tags_filtered, "02_outdata/B_species-lists/tags_filtered.csv")

# Check all tag ratings are either 5 or 3 or NA
tags_filtered %>% select(tag_rating) %>% table(useNA = "always") # Nice!

# How many species did we find?
tags_filtered %>% distinct(species_code)

# Make csv of all names for translations
tags_filtered %>% distinct(species_common_name) %>% as.data.frame() %>% 
  write.csv("02_outdata/B_species-lists/all-species.csv")

# Import translations
spp_transl <- read.csv("00_rawdata/B_data-collection/spp-names_translations.csv")
spp_transl <-spp_transl %>% select(-scientific)
colnames(spp_transl)
colnames(spp_transl) <- c("species_common_name", "FR_common")
colnames(spp_transl)


# 2. List of species per site ---------------------------------------------

# What sites do we have?

sites <- unique(tags_filtered$location) # where is HOMR-2??
tags_filtered %>% select(location) %>% table(useNA = "always")

# tags_filtered %>% distinct(location, species_code)

# Example for DOUG-1

names(tags_filtered)
test.doug <- tags_filtered %>% filter(location == "DOUG-1") %>% 
  distinct(species_common_name) 
test.doug

test.doug.fr <- tags_filtered %>% filter(location == "DOUG-1") %>% 
  distinct(species_common_name)%>% as.data.frame() %>% 
  left_join(spp_transl) 
test.doug.fr # Nice!

#test.doug %>% write.csv("02_outdata/species-list_doug-1.csv")

# For loop
for(site in sites) {
  
  site.df <- tags_filtered %>% filter(location == site) %>% 
    distinct(species_common_name) %>% as.data.frame() %>% 
    left_join(spp_transl) 
  
  site.df %>% write.csv(paste0("02_outdata/B_species-lists/species-list_", site, ".csv"))
}

# Check one

doug.1 <- read.csv("02_outdata/B_species-lists/species-list_DOUG-1.csv")
mavi.1 <- read.csv("02_outdata/B_species-lists/species-list_MAVI-1.csv")

# Recap of all Soverdi sites
sov_sites <- c("LOYO-1", "UDEM-1", "DOUG-1", "HRDP-1", "HOMR-1", "SOEU-1", "JEAN-1",
          "MAVI-1", "VANI-1")

sov_allspp <- tags_filtered %>% filter(location %in% sov_sites) %>% 
  distinct(species_common_name) %>% as.data.frame() %>% 
  left_join(spp_transl)

sov_allspp %>% write.csv("05_sharing-is-caring/sommaire-especes.csv")

# Site details for soverdi
aru_details <- read.csv("00_rawdata/B_data-collection/aru-details_final.csv")
str(aru_details)

aru_info <- aru_details %>% select(ARU.ID, Site, Coordinates, First.recording.date, 
                       Last.recording.date) 
# Clean up
aru_info[aru_info == "Formerly UDEM-1, now UDEM-2"] <- "UDEM-2"
aru_info[aru_info == "Formerly UDEM-2, now UDEM-1"] <- "UDEM-1"
aru_info[aru_info == "Formerly HRDP-2, now HRDP-1"] <- "HRDP-1"
aru_info[aru_info == "Formerly HRDP-1, now HRDP-2"] <- "UDEM-2"

# Check
aru_info$ARU.ID

write.csv(aru_info, "05_sharing-is-caring/details-sites.csv")


# 3. Play with data with Jeremy -------------------------------------------

# Ex: Get SR for one site
tags_filtered %>% filter(location == "DOUG-1") %>% distinct(species_code) %>% nrow()

# For loop to get SR for all sites
empty_matrix <- matrix(, nrow=length(sites), ncol=2)

for(i in 1:length(sites)) {
  
  site.name <- sites[[i]]
  number.spp <-tags_filtered %>% filter(location == site.name) %>% 
    distinct(species_code) %>% nrow()
  empty_matrix[i,1] <- site.name
  empty_matrix[i, 2] <- number.spp
  
}

# Add column names

sr.df <- as.data.frame(empty_matrix)
colnames(sr.df) <- c("site.name", "SR")
sr.df

write.csv(sr.df, "02_outdata/SR-by-site.csv") # Save df

# Most common species

### JEREMY CAN ADD HIS CODE HERE ###
