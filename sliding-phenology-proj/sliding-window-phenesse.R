### Rscript for phenesse runs on the sliding window analysis
setwd("/blue/soltis/millerjared/temp-sliding-phen/")
options(repos = c(CRAN = "https://cran.rstudio.com"))
# Specify the list of packages
packages_to_install <- c("dplyr", "ggplot2", "phenesse", "data.table", "doParallel", "foreach")

# Check and install packages if not already installed
for (pkg in packages_to_install) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

#Load Libraries
library(dplyr)
library(ggplot2)
library(phenesse)
library(data.table)
library(doParallel)
library(foreach)
print("library load success")

# Set up Dir ##########################################################################################################################
args = commandArgs(trailingOnly=TRUE)
plot_dir <- args[1] # plot dir is "/blue/soltis/millerjared/temp-sliding-phen/outputs/phenesse-species-plots-par/"
table_dir <- args[2] # table dir is "/blue/soltis/millerjared/temp-sliding-phen/outputs/phenesse-species-tables-par/"

#######################################################################################################################################
# Assign Parallel Processing Set-up
cores = 10 # Number of cores to be used
cl <- makeCluster(cores[1]) # Creates copy of R to communicate over sockets
registerDoParallel(cl) # Register the Parallel Process
#######################################################################################################################################
# Load Data
hbg <- fread("/blue/soltis/millerjared/temp-sliding-phen/data/processed/high-coverage-bee-data.csv")
hpg <- fread("/blue/soltis/millerjared/temp-sliding-phen/data/processed/high-coverage-plant-data.csv")
print("loaded data successfully")

#### Create dataset according to geographic bins
# bee taxa
grid_holder_b <- list()
gid_b_v <- unique(hbg$GID)
for(i in 1:length(unique(hbg$GID))){
  grid_holder_b[[i]] <- hbg[GID == gid_b_v[[i]],]
}
# plant taxa
grid_holder_p <- list()
gid_p_v <- unique(hpg$GID)
for(i in 1:length(unique(hbg$GID))){
  grid_holder_p[[i]] <- hpg[GID == gid_p_v[[i]],]
}
print("successfully geo-binned data")


#### Create sliding time bins of the datasets
# Function that uses a for-loop to create the decadal subsets
decadal_subsetter <- function(dataset, start_year, end_year, bin_size){
  decadal_subsets <- list() # empty storage list
  for(i in start_year:(end_year - bin_size)){
    end <- i + bin_size # Our end condition 
    subset_data <- dataset[year >= i & year <= end, ] # Subsets the data into bins on a decadal sliding scale
    decadal_subsets[[paste(i, end, sep = "-")]] <- subset_data # Label
  }
  return(decadal_subsets)
}

# Set arguments
start_year <- 1950
end_year <- 2023
bin_size <- 10
nested_list_holder_b <- list()
for(i in 1:length(grid_holder_b)){
  decadal_subsets <- decadal_subsetter(dataset = grid_holder_b[[i]], # Take the ith grid and do the decadal subsetting  
                                       start_year = start_year, 
                                       end_year = end_year, 
                                       bin_size = bin_size)
  nested_list_holder_b[[paste(i, "grid", sep = "-")]] <- decadal_subsets
}

nested_list_holder_p <- list()
for(i in 1:length(grid_holder_p)){
  decadal_subsets <- decadal_subsetter(dataset = grid_holder_p[[i]], # Take the ith grid and do the decadal subsetting  
                                       start_year = start_year, 
                                       end_year = end_year, 
                                       bin_size = bin_size)
  nested_list_holder_p[[paste(i, "grid", sep = "-")]] <- decadal_subsets
}


### Phenesse #######################################################################################################################
foreach(w = 1:length(nested_list_holder_p), .packages = c("data.table", "phenesse", "doParallel")) %dopar% {
  spatial_ID <- names(nested_list_holder_p[w]) # replace with w
  spatial_bin <- nested_list_holder_p[[w]] # replace with w 
  foreach(j = 1:length(spatial_bin)) %dopar% {
    named_temporal_bin <- names(spatial_bin[j]) # replace with j
    spatial_temporal_ID <- paste0(spatial_ID, "_", named_temporal_bin)
    temporal_spatial_bin <- spatial_bin[[j]] # replace with j
    unique_names_in_bin_v <- unique(temporal_spatial_bin$scientificName) # extract a vector of uniq scientific names from bin
    foreach(i = 1:length(unique(temporal_spatial_bin$scientificName))) %dopar% {
      named_subset <- temporal_spatial_bin[scientificName %in% unique_names_in_bin_v[i]] # replace with i
      if(nrow(named_subset) <= 5 | length(unique(named_subset$doy)) <= 3){
        named_phenometric_summary <- data.table(taxa = "plant", scientificName = unique(named_subset$scientificName), GID = unique(named_subset$GID), number_of_records = nrow(named_subset), fiftieth_percentile = NA, fiftieth_var = NA, upper_ci = NA, lower_ci = NA)
      }else{
        # Using phenesse
        named_subset <- named_subset[doy >= 0 & doy <= 365]
        fiftieth_est <- tryCatch(phenesse::weib_percentile_ci(observations = named_subset$doy, percentile = 0.5, iterations = 10, bootstraps = 100, parallelize = "multicore", ncpus = 4),
                                 error = function(e) NA)
        
        if(!is.na(fiftieth_est[[1]])){
          # Start plot saving
          name <- gsub(",", "", unique(named_subset$scientificName)) # Fix some file naming shennanigans
          name <- gsub(" ", "-", name) # ""
          png(paste0(plot_dir,"plant_", name, "_", spatial_temporal_ID, ".png"), width = 1000, height = 600)
          hist(named_subset$doy, xlim = c(0,365), main = paste0(unique(named_subset$scientificName), " histogram for time bin ", spatial_temporal_ID), xlab = "day of year")
          abline(v = fiftieth_est[1], col = "red")
          
          upper_ci <- fiftieth_est[[3]]
          lower_ci <- fiftieth_est[[2]]
          
          abline(v = upper_ci, lty= 'dashed', col = "blue")
          abline(v = lower_ci, lty = 'dashed', col = "blue")
          # Add a legend
          legend("topright", legend = c("50th Percentile", "Confidence Interval"),
                 col = c("red", "blue"), lty = c(1, 2), lwd = c(1, 2), cex = 0.9)
          dev.off()
          
          
          # End plot saving ##################################################################################################
          
          named_phenometric_summary <- data.table(taxa = "plant", scientificName = unique(named_subset$scientificName), GID = unique(named_subset$GID),
                                                  number_of_records = nrow(named_subset))
          named_phenometric_summary <- named_phenometric_summary[, fiftieth_percentile := fiftieth_est[[1]]]
          named_phenometric_summary <- named_phenometric_summary[, upper_ci := fiftieth_est[[3]]]
          named_phenometric_summary <- named_phenometric_summary[, lower_ci := fiftieth_est[[2]]]
          
          fwrite(named_phenometric_summary, paste0(table_dir, "plant_", name, "_", spatial_temporal_ID, ".csv"))
        } # End of if statement (issue with the NA)
        else{
          named_phenometric_summary <- data.table(taxa = "plant", scientificName = unique(named_subset$scientificName), GID = unique(named_subset$GID), number_of_records = nrow(named_subset), fiftieth_percentile = NA, fiftieth_var = NA, upper_ci = NA, lower_ci = NA)
          
          # End of else statement
          name <- gsub(",", "", unique(named_subset$scientificName)) # Fix some file naming shennanigans
          name <- gsub(" ", "-", name) # ""
          fwrite(named_phenometric_summary, paste0(table_dir, "plant_", name, "_", spatial_temporal_ID, ".csv"))
          
        } # End of else
      } # End of else statement
      name <- gsub(",", "", unique(named_subset$scientificName)) # Fix some file naming shennanigans
      name <- gsub(" ", "-", name) # ""
      fwrite(named_phenometric_summary, paste0(table_dir, "plant_", name, "_", spatial_temporal_ID, ".csv"))
    } # end of i loop
  } # end of j loop
} # end of w loop



