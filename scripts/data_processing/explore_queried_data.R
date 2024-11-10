# # LIBRARIES # #
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggplot2)


rm(list=ls()) 


# # INPUT FILES # #
biotime.raw <- read.csv(here::here('../BioTIMEQuery_24_06_2021.csv'))
metadata <- read.csv(here::here('raw_data/BioTIMEMetadata_24_06_2021.csv')) #biotime metadata for 10 km pairs to use in lit review (explore_biotime.R)




### What taxa have Cover or Volume as biomass measure?

# Group by CLASS and BIOMASS_TYPE to count occurrences
class_biomass_data <- metadata %>%
  # Filter out rows where BIOMASS_TYPE is NA
  dplyr::filter(!is.na(BIOMASS_TYPE)) %>%
  # Group by ORGANISMS and BIOMASS_TYPE
  dplyr::group_by(ORGANISMS, BIOMASS_TYPE) %>%
  # Summarize by counting the occurrences
  dplyr::summarize(count = n(), .groups = 'drop')


plot_biomass <- ggplot(class_biomass_data, aes(x = ORGANISMS, y = count, fill = BIOMASS_TYPE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "ORGANISMS", y = "Count", title = "Distribution of Biomass Types (Cover and Volume) Across Classes") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(plot_biomass)


#################### what weight means? checking it for fish

fish_biomass_studies <- metadata %>%
  # Filter for studies where ORGANISMS is "fish" and BIOMASS_TYPE is not NA
  dplyr::filter(ORGANISMS == "Fish" & !is.na(BIOMASS_TYPE)) %>%
  # Select relevant columns
  dplyr::select(STUDY_ID, BIOMASS_TYPE, ORGANISMS)

# Step 2: Filter biotime.raw to retain only rows from fish biomass studies
# and rows that have only biomass data (no abundance data)
filtered_biotime_raw <- biotime.raw %>%
  # Join with the fish_biomass_studies to keep only relevant studies
  dplyr::inner_join(fish_biomass_studies, by = "STUDY_ID") %>%
  # Filter to retain rows with biomass data but no abundance data
  dplyr::filter(sum.allrawdata.BIOMASS > 0 & sum.allrawdata.ABUNDANCE == 0)

# View the filtered data
filtered_biotime_raw

#[1]  99, 148, 290 -> NO METHODS 
# 289 -> weight of species per sampling area
# 467 -> abundance biomass by sampling station
# 507 -> weight of individuals of the same species per haul



##################### Biomass and abundance data

# Check if rows have both non-zero ABUNDANCE and BIOMASS
biotime.raw$has_abundance <- biotime.raw$sum.allrawdata.ABUNDANCE > 0
biotime.raw$has_biomass <- biotime.raw$sum.allrawdata.BIOMASS > 0

# Create a summary table showing which rows have ABUNDANCE, BIOMASS, or both
summary_table <- table(
  has_abundance = biotime.raw$has_abundance,
  has_biomass = biotime.raw$has_biomass
)

# Print the summary table
print(summary_table)

# Optionally, you can extract rows with both ABUNDANCE and BIOMASS for further investigation
both_abundance_biomass <- biotime.raw[biotime.raw$has_abundance & biotime.raw$has_biomass, ]


############ ABUNDANCE / BIOMASS DATA

# Step 1: Merge biotime.raw with metadata to add the REALM information
biotime_with_realm <- biotime.raw %>%
  inner_join(metadata %>% select(STUDY_ID, REALM), by = "STUDY_ID")

# Step 2: Check if rows have both non-zero ABUNDANCE and BIOMASS
biotime_with_realm$has_abundance <- biotime_with_realm$sum.allrawdata.ABUNDANCE > 0
biotime_with_realm$has_biomass <- biotime_with_realm$sum.allrawdata.BIOMASS > 0

# Step 3: Create a new column to classify rows based on the presence of abundance and biomass
biotime_with_realm$class <- with(biotime_with_realm, 
                                 ifelse(has_abundance & has_biomass, "Both ABUNDANCE & BIOMASS", 
                                        ifelse(has_abundance, "Only ABUNDANCE", 
                                               ifelse(has_biomass, "Only BIOMASS", "Neither"))))

# Step 4: Filter data into terrestrial and marine realms
terrestrial_data <- biotime_with_realm %>% filter(REALM == "Terrestrial")
marine_data <- biotime_with_realm %>% filter(REALM == "Marine")
freshwater_data <- biotime_with_realm %>% filter(REALM == "Freshwater")

# Step 5: Create plots for terrestrial and marine realms

# For Terrestrial Realm
terrestrial_summary <- as.data.frame(table(terrestrial_data$class))
abund_biomass_terrestrial <- ggplot(terrestrial_summary, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Category", y = "Number of Rows", fill = "Category", 
       title = "Terrestrial") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# For Marine Realm
marine_summary <- as.data.frame(table(marine_data$class))
abund_biomass_marine <- ggplot(marine_summary, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Category", y = "Number of Rows", fill = "Category", 
       title = "Marine") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# For Freshwater Realm
freshwater_summary <- as.data.frame(table(freshwater_data$class))
abund_biomass_freshwater <- ggplot(freshwater_summary, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Category", y = "Number of Rows", fill = "Category", 
       title = "Freshwater") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

ggarrange(abund_biomass_terrestrial,
          abund_biomass_marine,
          abund_biomass_freshwater,
          common.legend = TRUE,
          
          ncol = 3,
          nrow = 1
)

########### What organisms are in only-abundance data?


# Step 1: Merge biotime.raw with metadata to add the REALM and ORGANISMS information
biotime_with_organisms <- biotime.raw %>%
  inner_join(metadata %>% select(STUDY_ID, REALM, ORGANISMS), by = "STUDY_ID")

# Step 2: Filter rows where there is only biomass and no abundance
biomass_only_data <- biotime_with_organisms %>%
  filter(sum.allrawdata.BIOMASS > 0 & sum.allrawdata.ABUNDANCE == 0)

# Step 3: Separate the data into terrestrial, marine, and freshwater realms
terrestrial_biomass_only <- biomass_only_data %>% filter(REALM == "Terrestrial")
marine_biomass_only <- biomass_only_data %>% filter(REALM == "Marine")
freshwater_biomass_only <- biomass_only_data %>% filter(REALM == "Freshwater")

# Step 4: Create summary tables of organism types for all three realms

# For Terrestrial Realm
terrestrial_summary <- as.data.frame(table(terrestrial_biomass_only$ORGANISMS))

# For Marine Realm
marine_summary <- as.data.frame(table(marine_biomass_only$ORGANISMS))

# For Freshwater Realm
freshwater_summary <- as.data.frame(table(freshwater_biomass_only$ORGANISMS))

# Step 5: Create bar plots to visualize the types of organisms for rows with only biomass

# Plot for Terrestrial Realm
ggplot(terrestrial_summary, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Organism Type", y = "Number of Rows", fill = "Organism Type", 
       title = "Distribution of Organism Types (Terrestrial, Only Biomass)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for Marine Realm
ggplot(marine_summary, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Organism Type", y = "Number of Rows", fill = "Organism Type", 
       title = "Distribution of Organism Types (Marine, Only Biomass)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for Freshwater Realm
ggplot(freshwater_summary, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "Organism Type", y = "Number of Rows", fill = "Organism Type", 
       title = "Distribution of Organism Types (Freshwater, Only Biomass)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



############################### SUBSET DATA

# Step 1: Filter biotime.raw to retain only rows with abundance data and no biomass data
abundance_only_data <- biotime.raw %>%
  dplyr::filter(sum.allrawdata.ABUNDANCE > 0 & sum.allrawdata.BIOMASS == 0)

# Step 2: Select the first 5 unique studies
first_5_studies <- abundance_only_data %>%
  dplyr::distinct(STUDY_ID) %>%
  dplyr::slice(1:5)  # Select the first 5 studies

# Step 3: Subset the biotime.raw dataset based on those 5 studies
subset_abundance_only <- abundance_only_data %>%
  dplyr::filter(STUDY_ID %in% first_5_studies$STUDY_ID)

#write.csv(subset_abundance_only, here::here("AF_pilot/processed_data/subset_abundance_only.csv"))




############################### check spatial resultions

ggplot(metadata, aes(x = AREA_SQ_KM, y = NUMBER_OF_SAMPLES)) +
  geom_point() +
  theme_classic() +
  labs(x = "Area (sq km)", y = "Number of Samples", title = "Relationship between Area and Number of Samples") +
  scale_x_log10() +  # Log-scale the x-axis if needed to handle wide range in area
  scale_y_log10()    # Log-scale the y-axis if needed to handle wide range in number of samples
