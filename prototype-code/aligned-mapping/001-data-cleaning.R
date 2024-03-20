### Data Cleaning for Phenology data that matches some of Vaughn Shirey's work in case of overlap
library(data.table)
library(dplyr)
library(lubridate)
library(bdc)
library(tidyverse)
library(rgnparser)
library(magrittr)
library(stringr)
### Read In Data ##############################################################################################################################################################################################################################################################################################################################
# Bring in the inat annotated records by phenobase & CCH2 annotated herbarium data
inat_obs <- fread("sliding-phenology/data/raw/phenobase-observations-dwca/observations.csv")
# Bring in the cch2 data that has been joined with annotations (see supplementary script for this)
cch2_data <- fread("sliding-phenology/data/raw/cch2-aligned-region-data.csv")
###############################################################################################################################################################################################################################################################################################################################################
## iNat cleaning ##############################################################################################################################################################################################################################################################################################################################
# Select for fields of interest
inat_obs <- inat_obs[, .(id, occurrenceID, datasetName, informationWithheld, recordedBy, recordedByID, identifiedBy, identifiedByID, captive, eventDate, eventTime, verbatimEventDate, verbatimLocality, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, identificationID, scientificName, taxonRank, reproductiveCondition)]
unique(inat_obs$captive) # Filter cultivated
unique(inat_obs$informationWithheld)
wild_string <- "wild"
inat_w_obs <- inat_obs[captive == wild_string]
# Remove records that do not contain any coordinates
inat_w_obs <- inat_w_obs[!is.na(decimalLongitude)]
inat_w_obs <- inat_w_obs[!is.na(decimalLatitude)]
inat_data <- inat_w_obs # align variable naming conventions
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
cch2_data <- cch2_data[, aggregator := "cch2"]
cch2_data <- cch2_data[, reproductiveCondition := "DNE in cch2"]
cch2_data <- cch2_data[, datasetName := "DNE in cch2"]
cch2_data <- cch2_data %>% 
  mutate(openFlower = case_when(
    openFlower == TRUE ~ "present",
    openFlower == FALSE ~ "absent", 
    is.na(openFlower) ~ "unknown"
  )) %>%
  mutate(unopenFlower = case_when(
    unopenFlower == TRUE ~ "present",
    unopenFlower == FALSE ~ "absent", 
    is.na(unopenFlower) ~ "unknown"
  )) %>%
  mutate(reproductive = case_when(
    reproductive == TRUE ~ "present",
    reproductive == FALSE ~ "absent", 
    is.na(reproductive) ~ "unknown"
  ))
inat_data <- inat_data[, c("reproductive", "unopenFlower", "openFlower") := "DNE in inat"]
# Assure correct date class 
cch2_data$eventDate <- as.POSIXct(cch2_data$eventDate, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
inat_data$eventDate <- as.POSIXct(inat_data$eventDate, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
# Bind data, fill in NAs for non-conforming fields between the datasets
comb_data <- dplyr::bind_rows(inat_data, cch2_data)

# Assure appropriate Date information for datasets by parsing out year month and day and creating doy
comb_data <- comb_data[, c("year", "month", "day", "doy") := list(year(eventDate), month(eventDate), day(eventDate), yday(eventDate))] # Shortcut
###############################################################################################################################################################################################################################################################################################################################################
## Taxonomic Harmonization ####################################################################################################################################################################################################################################################################################################################
comb_data_og <- comb_data # create a copy
comb_cols <- names(comb_data) # extract cols names
parse_names <- bdc_clean_names(comb_data$scientificName)
parse_names <- parse_names %>%  
  dplyr::select(.uncer_terms, names_clean)
comb_data <- dplyr::bind_cols(comb_data, parse_names)
query_names <- bdc_query_names_taxadb(
  sci_name            = comb_data$names_clean,
  replace_synonyms    = TRUE, # replace synonyms by accepted names?
  suggest_names       = TRUE, # try to found a candidate name for misspelled names?
  suggestion_distance = 0.9, # distance between the searched and suggested names
  db                  = "gbif", # taxonomic database
  rank_name           = "Plantae", # a taxonomic rank
  rank                = "kingdom", # name of the taxonomic rank
  parallel            = FALSE, # should parallel processing be used?
  ncores              = 6, # number of cores to be used in the parallelization process
  export_accepted     = FALSE # save names linked to multiple accepted names
)
comb_data <- comb_data %>%
  dplyr::rename(verbatim_scientificName = scientificName) %>%
  dplyr::select(-names_clean) %>%
  dplyr::bind_cols(., query_names)

report <- bdc_create_report(data = comb_data,
                    database_id = "database_id",
                    workflow_step = "taxonomy",
                    save_report = FALSE)
report
saveRDS(report, file = "../data/processed/taxonomic-harmonization-report.rds")

comb_data <- comb_data %>% 
  filter(!is.na(scientificName)) %>% 
  select(id, occurrenceID, datasetName, recordedBy, captive, eventDate, verbatimLocality, decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters, verbatim_scientificName, taxonRank...19, aggregator, reproductiveCondition, openFlower, year, month, day, doy, scientificName, taxonRank...37, taxonomicStatus, acceptedNameUsageID, kingdom, phylum, class, order, family, genus, specificEpithet, infraspecificEpithet, scientificNameAuthorship, vernacularName)

comb_data <- comb_data %>% 
  rename(verbatimTaxonRank = taxonRank...19, harmonizedTaxonRank = taxonRank...37) # one step to make the data a bit more clear
###############################################################################################################################################################################################################################################################################################################################################
## Create datasets according to research grade status and phenology ###########################################################################################################################################################################################################################################################################
# Remove records with unneccessarily large uncertainty values 
comb_data <- comb_data[coordinateUncertaintyInMeters <= 1000] # 1km will be our threshold (subject to change)
# Create a research grade dataset
unique(comb_data$datasetName) # We can create datasets that are research grade or not by filtering for "iNaturalist research-grade observations"
research_grade_string <- "iNaturalist research-grade observations"
comb_data_rg <- comb_data[datasetName == research_grade_string | datasetName == "DNE in cch2"]
# Create a flowering only dataset
unique(inat_data$reproductiveCondition) # We'll create a flowering and full dataset (obs effort)
inat_reproductive_vec <- c("flowering", "flowering|flower budding", "flowering|fruiting", "flowering|fruiting|flower budding", "flowering|no evidence of flowering")
comb_data_rg_flowering <- comb_data_rg[reproductiveCondition %in% inat_reproductive_vec | reproductiveCondition == "DNE in cch2"] # keep cch2 records as these are not filled out here
comb_data_rg_flowering <- comb_data_rg_flowering[openFlower == "present" | openFlower == "DNE in inat"]
## Further Select fields of interest
## write out these initial datasets
fwrite(comb_data, "../data/processed/combined_annotated_phen_data.csv")
fwrite(comb_data_rg, "../data/processed/combined_annotated_rg_phen_data.csv")
fwrite(comb_data_rg_flowering, "../data/processed/combined_annotated_rg_flowering_phen_data.csv")
