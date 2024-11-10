


# Step 1: Handle potential false 0s in abundance and biomass
# Replace 0s with NAs if the study didn't record how abundance or biomass was measured
biotime.raw$sum.allrawdata.ABUNDANCE[which(
  biotime.raw$STUDY_ID %in% 
    metadata$STUDY_ID[which(is.na(metadata$ABUNDANCE_TYPE))])] <- NA

biotime.raw$sum.allrawdata.BIOMASS[which(
  biotime.raw$STUDY_ID %in% 
    metadata$STUDY_ID[which(is.na(metadata$BIOMASS_TYPE))])] <- NA

# Step 2: Handle missing plot information by replacing NAs in PLOT with a placeholder
biotime.raw$PLOT[which(is.na(biotime.raw$PLOT))] <- "Was_NA"

# Step 3: Create a UNIQUE_ID to aggregate data per study, species, and year across all plots
biotime.raw$UNIQUE_ID <- paste(biotime.raw$STUDY_ID, biotime.raw$SPECIES, biotime.raw$YEAR, sep = "~")

# Step 4: Aggregate both abundance and biomass data by study, species, and year across all plots
# We aggregate abundance and biomass together and handle NAs automatically
biotime.agg <- aggregate(cbind(sum.allrawdata.ABUNDANCE, sum.allrawdata.BIOMASS) ~ UNIQUE_ID, 
                         data = biotime.raw, 
                         FUN = function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE))

# Step 5: Add identifying columns (STUDY_ID, SPECIES, YEAR) back into the aggregated dataset
biotime.agg$STUDY_ID <- biotime.raw$STUDY_ID[match(biotime.agg$UNIQUE_ID, biotime.raw$UNIQUE_ID)]
biotime.agg$SPECIES <- biotime.raw$SPECIES[match(biotime.agg$UNIQUE_ID, biotime.raw$UNIQUE_ID)]
biotime.agg$YEAR <- biotime.raw$YEAR[match(biotime.agg$UNIQUE_ID, biotime.raw$UNIQUE_ID)]

# Step 6: Calculate the sample size (number of plots) that went into calculating the mean for both abundance and biomass
ss_abun <- as.data.frame(table(biotime.raw$UNIQUE_ID[-which(is.na(biotime.raw$sum.allrawdata.ABUNDANCE))]))
ss_bio <- as.data.frame(table(biotime.raw$UNIQUE_ID[-which(is.na(biotime.raw$sum.allrawdata.BIOMASS))]))

# Merge sample size information into the aggregated dataset
biotime.agg$SAMPLE_SIZE_ABUNDANCE <- ss_abun$Freq[match(biotime.agg$UNIQUE_ID, ss_abun$Var1)]
biotime.agg$SAMPLE_SIZE_BIOMASS <- ss_bio$Freq[match(biotime.agg$UNIQUE_ID, ss_bio$Var1)]

# Step 7: Remove any zero means for abundance or biomass if necessary
biotime.agg <- biotime.agg %>%
  dplyr::filter(!(sum.allrawdata.ABUNDANCE == 0 & !is.na(sum.allrawdata.ABUNDANCE))) %>%
  dplyr::filter(!(sum.allrawdata.BIOMASS == 0 & !is.na(sum.allrawdata.BIOMASS)))

# Step 8: Create a new variable for study and species
biotime.agg$STUDY_SPECIES <- paste(biotime.agg$STUDY_ID, biotime.agg$SPECIES, sep = "~")

# View the final aggregated dataset
head(biotime.agg)
