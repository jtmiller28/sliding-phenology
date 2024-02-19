### Data Cleaning for Phenology data that matches some of Vaughn Shirey's work in case of overlap
library(data.table)
library(dplyr)
library(lubridate)
# library(CoordinateCleaner) # currently has install issues on server

### Read In Data ##############################################################################################################################################################################################################################################################################################################################
# Bring in the inat annotated records by phenobase & CCH2 annotated herbarium data
inat_obs <- fread("./sliding-phenology/data/raw/phenobase-observations-dwca/observations.csv")
# Bring in the cch2 data that has been joined with annotations (see supplementary script for this)
cch2_data <- fread("./sliding-phenology/data/raw/cch2-aligned-region-data.csv")
###############################################################################################################################################################################################################################################################################################################################################
## iNat cleaning ##############################################################################################################################################################################################################################################################################################################################
# Select for fields of interest
inat_obs <- inat_obs[, .(id, occurrenceID, datasetName, informationWithheld, recordedBy, recordedByID, identifiedBy, identifiedByID, captive, eventDate, eventTime, verbatimEventDate, verbatimLocality, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, identificationID, scientificName, taxonRank, reproductiveCondition)]
unique(inat_obs$captive) # Filter cultivated
unique(inat_obs$informationWithheld)
wild_string <- "wild"
inat_w_obs <- inat_obs[captive == wild_string]
inat_data <- inat_w_obs
###############################################################################################################################################################################################################################################################################################################################################
## CCH2 cleaning ##############################################################################################################################################################################################################################################################################################################################
# Select for fields of interest
cch2_data <- cch2_data[, .(id, informationWithheld, recordedBy, identifiedBy, eventDate, verbatimEventDate, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, scientificName, taxonRank, reproductive, unopenFlower, openFlower, fruit)]
# Remove data that has no associated eventDate
cch2_data <- cch2_data[!is.na(eventDate)]
# Remove data without geocoords 
cch2_data <- cch2_data[!is.na(decimalLatitude)]
cch2_data <- cch2_data[!is.na(decimalLongitude)]
###############################################################################################################################################################################################################################################################################################################################################
## Combine inaturalist and cch2 data ##########################################################################################################################################################################################################################################################################################################
# Create a new label field to note where the data is coming from
inat_data <- inat_data[, aggregator := "inat"]
cch2_data <- cch2_data[, aggregatpr := "cch2"]
# Assure correct date class 
#cch2_data$eventDate <- as.POSIXct(cch2_data$eventDate, format = "%Y-%m%d") Remove?
# Bind data, fill in NAs for non-conforming fields between the datasets
comb_data <- dplyr::bind_rows(inat_data, cch2_data)
# Assure appropriate Date information for datasets by parsing out year month and day and creating doy
# Do the same thing for the bees
comb_data$eventDate <- as.POSIXct(comb_data$eventDate, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
comb_data <- comb_data[, c("year", "month", "day", "doy") := list(year(eventDate), month(eventDate), day(eventDate), yday(eventDate))] # Shortcut
###############################################################################################################################################################################################################################################################################################################################################

unique(inat_obs$reproductiveCondition) # We'll create a flowering and full dataset (obs effort)
non_repoductive_vec <- c("flower budding", "no evidence of flowering", "fruiting", "fruiting|flower budding", "fruiting|no evidence of flowering", "flowering|no evidence of flowering") # Not sure what the last one means, ask Rob. 
# Remove records that do not contain any coordinates
inat_obs <- inat_obs[!is.na(decimalLongitude)]
inat_obs <- inat_obs[!is.na(decimalLatitude)]
# Remove records with unneccessarily large uncertainty values 
inat_obs <- inat_obs[coordinateUncertaintyInMeters <= 1000] # 1km will be our threshold (subject to change)
# Create a wild only dataset to remove the cultivated observations
inat_w_obs <- inat_obs[captive == wild_string]
unique(inat_obs$datasetName) # We can create datasets that are research grade or not by filtering for "iNaturalist research-grade observations"
research_grade_string <- "iNaturalist research-grade observations"
# Create a research grade dataset
inat_rg <- inat_w_obs[datasetName == research_grade_string]
# Create a flowering only dataset
inat_rg_flowering <- inat_rg[!reproductiveCondition %in% non_repoductive_vec]

## write out these initial datasets
fwrite(inat_w_obs, "./sliding-phenology/data/processed/inat-wild-obs.csv")
fwrite(inat_rg, "./sliding-phenology/data/processed/inat-wild-rg.csv")
fwrite(inat_rg_flowering, "./sliding-phenology/data/processed/inat-wild-rg-flowering.csv")

### CCH2 annotated herbarium data
# Bring in the cch2 data that has been joined with annotations (see supplementary script for this)
cch2_data <- fread("./sliding-phenology/data/raw/cch2-aligned-region-data.csv")
# Select fields of interest
cch2_data <- cch2_data[, .(id, informationWithheld, recordedBy, identifiedBy, eventDate, verbatimEventDate, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, scientificName, taxonRank, reproductive, unopenFlower, openFlower, fruit)]
# Remove data that has no associated eventDate
cch2_data <- cch2_data[!is.na(eventDate)]
# Remove data without geocoords 
cch2_data <- cch2_data[!is.na(decimalLatitude)]
cch2_data <- cch2_data[!is.na(decimalLongitude)]
# Create a subset of openFlower data
openFlower_data <- cch2_data[openFlower == TRUE]
# Create a subset of the rest of the data
otherRepro_data <- cch2_data[openFlower == FALSE | is.na(openFlower)]



### Coordinate Cleaning (Save for when we can~)





