## Quick code for Siena's tree planting sites ## 


library(tidyverse)
library(dplyr)


Mtl_planting_ARU.df <- read.csv("00_rawdata/B_data-collection/aru-report.csv")

# filter to make sure all were verified
Mtl_planting_ARU.df <- subset(Mtl_planting_ARU.df, tag_is_verified == 't')

# replaced NAs (old system) with 5 rating (new system) for true positives

Mtl_planting_ARU.df <- Mtl_planting_ARU.df %>%
  mutate(tag_rating = replace_na(tag_rating, 5))

# select only true positives

Mtl_planting_ARU.df <- subset(Mtl_planting_ARU.df, tag_rating == 5)

# merge locations information for a joint ID for summarizing

Mtl_planting_ARU.df$location2 <- substr(Mtl_planting_ARU.df$location,1,4)

# get species list per location

summary_counts_long <- Mtl_planting_ARU.df %>%
  group_by(location2, species_common_name) %>%
  summarise(Count = n(), .groups = 'drop') 


list_of_tables <- split(summary_counts_long, summary_counts_long$location2)

setwd("C:/Users/FreiB/Documents/Analysis_Others/Siena_Blier")

# Export using lapply
lapply(names(list_of_tables), function(x) {
  write.csv(list_of_tables[[x]], file = paste0(x, ".csv"), row.names = FALSE)
})

