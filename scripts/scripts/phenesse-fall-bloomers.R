### Rscript for phenesse runs on the sliding window analysis
setwd("/blue/soltis/millerjared/sliding-phenology")
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
plot_dir <- args[1] # plot dir is "/blue/soltis/millerjared/sliding-phenology/outputs/phenesse-species-plots-fall/"
table_dir <- args[2] # table dir is "/blue/soltis/millerjared/sliding-phenology/outputs/phenesse-species-tables-fall/"

#######################################################################################################################################
# Assign Parallel Processing Set-up
cores = 10 # Number of cores to be used
cl <- makeCluster(cores[1]) # Creates copy of R to communicate over sockets
registerDoParallel(cl) # Register the Parallel Process
#######################################################################################################################################
# Load Data
dt_long <- fread("/blue/soltis/millerjared/sliding-phenology/data/processed/fallish-plants.csv")
print("loaded data successfully")

#### Create dataset according to geographic bins
# Reorganize into nested lists for the loops  
# Geographic Binning 
gid_p_v <- unique(dt_long$GID) # create a vector for later
grid_holder_p <- split(dt_long, dt_long$GID) # split into unique GID lists

# Temporal Subsetting 
time_bins <- list()
for(i in 1:length(grid_holder_p)){
  time_bins[[i]] <- split(grid_holder_p[[i]], grid_holder_p[[i]]$time_bin)
}

# Sp level kde simus 
nested_list_holder_p <- time_bins # rename the var 

print("successfully nested the spatial/temporal elements of the data")

### Phenesse #######################################################################################################################
foreach(w = 1:length(nested_list_holder_p), .packages = c("data.table", "phenesse", "doParallel")) %dopar% {
  spatial_ID <- names(nested_list_holder_p[w]) # replace with w
  spatial_bin <- nested_list_holder_p[[w]] # replace with w 
  foreach(j = 1:length(spatial_bin)) %dopar% {
    named_temporal_bin <- names(spatial_bin[j]) # replace with j
    spatial_temporal_ID <- paste0(spatial_ID, "_", named_temporal_bin)
    temporal_spatial_bin <- spatial_bin[[j]] # replace with j
    unique_names_in_bin_v <- unique(temporal_spatial_bin$speciesName) # extract a vector of uniq scientific names from bin
    foreach(i = 1:length(unique(temporal_spatial_bin$speciesName))) %dopar% {
      named_subset <- temporal_spatial_bin[speciesName %in% unique_names_in_bin_v[i]] # replace with i
      if(nrow(named_subset) <= 5 | length(unique(named_subset$doy)) <= 3){
        named_phenometric_summary <- data.table(taxa = "plant", speciesName = unique(named_subset$speciesName), GID = unique(named_subset$GID), number_of_records = nrow(named_subset), fiftieth_percentile = NA, fiftieth_var = NA, upper_ci = NA, lower_ci = NA)
      }else{
        # Using phenesse
        named_subset <- named_subset[doy >= 0 & doy <= 365]
        fiftieth_est <- tryCatch(phenesse::weib_percentile_ci(observations = named_subset$doy, percentile = 0.5, iterations = 10, bootstraps = 100, parallelize = "multicore", ncpus = 4),
                                 error = function(e) NA)
        
        if(!is.na(fiftieth_est[[1]])){
          # Start plot saving
          name <- gsub(",", "", unique(named_subset$speciesName)) # Fix some file naming shennanigans
          name <- gsub(" ", "-", name) # ""
          png(paste0(plot_dir,"plant_", name, "_", spatial_temporal_ID, ".png"), width = 1000, height = 600)
          hist(named_subset$doy, xlim = c(0,365), main = paste0(unique(named_subset$speciesName), " histogram for time bin ", spatial_temporal_ID), xlab = "day of year")
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
          
          named_phenometric_summary <- data.table(taxa = "plant", speciesName = unique(named_subset$speciesName), GID = unique(named_subset$GID),
                                                  number_of_records = nrow(named_subset))
          named_phenometric_summary <- named_phenometric_summary[, fiftieth_percentile := fiftieth_est[[1]]]
          named_phenometric_summary <- named_phenometric_summary[, upper_ci := fiftieth_est[[3]]]
          named_phenometric_summary <- named_phenometric_summary[, lower_ci := fiftieth_est[[2]]]
          
          fwrite(named_phenometric_summary, paste0(table_dir, "plant_", name, "_", spatial_temporal_ID, ".csv"))
        } # End of if statement (issue with the NA)
        else{
          named_phenometric_summary <- data.table(taxa = "plant", speciesName = unique(named_subset$speciesName), GID = unique(named_subset$GID), number_of_records = nrow(named_subset), fiftieth_percentile = NA, fiftieth_var = NA, upper_ci = NA, lower_ci = NA)
          
          # End of else statement
          name <- gsub(",", "", unique(named_subset$speciesName)) # Fix some file naming shennanigans
          name <- gsub(" ", "-", name) # ""
          fwrite(named_phenometric_summary, paste0(table_dir, "plant_", name, "_", spatial_temporal_ID, ".csv"))
          
        } # End of else
      } # End of else statement
      name <- gsub(",", "", unique(named_subset$speciesName)) # Fix some file naming shennanigans
      name <- gsub(" ", "-", name) # ""
      fwrite(named_phenometric_summary, paste0(table_dir, "plant_", name, "_", spatial_temporal_ID, ".csv"))
    } # end of i loop
  } # end of j loop
} # end of w loop