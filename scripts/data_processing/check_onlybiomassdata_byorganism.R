####################################
# Code to check the methods for studies with biomass-only data, by organism type
####################################

library(tidyverse)
library(dplyr)
library(ggplot2)

# # INPUT FILES # #
biotime.raw <- read.csv(here::here('../BioTIMEQuery_24_06_2021.csv'))
metadata <- read.csv(here::here('raw_data/BioTIMEMetadata_24_06_2021.csv'))

############################### Check the organisms that have only biomass data of type Weight

# Filter for entries with only biomass data (either abundance is NA or explicitly 0) and biomass type 'weight'
biomass_only_weight <- biotime.raw %>%
  filter((sum.allrawdata.BIOMASS > 0 & sum.allrawdata.ABUNDANCE == 0) |
           (is.na(sum.allrawdata.ABUNDANCE) & !is.na(sum.allrawdata.BIOMASS))) %>%
  left_join(metadata %>% select(STUDY_ID, REALM, ORGANISMS, BIOMASS_TYPE), by = "STUDY_ID") %>%
  filter(BIOMASS_TYPE == "Weight")

# Count occurrences of each organism type within each realm
biomass_count <- biomass_only_weight %>%
  group_by(REALM, ORGANISMS) %>%
  summarize(count = n(), .groups = "drop")

# Create separate plots for terrestrial, marine, and freshwater
plot_terrestrial <- biomass_count %>%
  filter(REALM == "Terrestrial") %>%
  ggplot(aes(x = ORGANISMS, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Organism Type", y = "Count", title = "Terrestrial - Biomass Only (Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_marine <- biomass_count %>%
  filter(REALM == "Marine") %>%
  ggplot(aes(x = ORGANISMS, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Organism Type", y = "Count", title = "Marine - Biomass Only (Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_freshwater <- biomass_count %>%
  filter(REALM == "Freshwater") %>%
  ggplot(aes(x = ORGANISMS, y = count)) +
  geom_bar(stat = "identity") +
  labs(x = "Organism Type", y = "Count", title = "Freshwater - Biomass Only (Weight)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



############################### # Example to examine ORGANISMS == "Fish"

# Filter for studies with only biomass data (no abundance) where BIOMASS_TYPE is 'weight' and ORGANISMS is 'fish'
fish_biomass_only_studies <- biotime.raw %>%
  filter((sum.allrawdata.BIOMASS > 0 & sum.allrawdata.ABUNDANCE == 0) |
           (is.na(sum.allrawdata.ABUNDANCE) & !is.na(sum.allrawdata.BIOMASS))) %>%
  left_join(metadata %>% select(STUDY_ID, REALM, ORGANISMS, BIOMASS_TYPE), by = "STUDY_ID") %>%
  filter(BIOMASS_TYPE == "Weight", ORGANISMS == "fish") %>%
  distinct(STUDY_ID)

# View the result
fish_biomass_only_studies

# check methods of study 148 - see whether the biomass measure is appropiate or not
metadata[which(metadata$STUDY_ID == "507"),"METHODS"]


# Studies checked for both "Fish" and "fish":

# 99, 148, 290 -> NO METHODS (NON-VALID)
# 289 -> weight of species per sampling area (VALID)
# 467 -> abundance biomass by sampling station (VALID)
# 507 -> weight of individuals of the same species per haul (VALID)


