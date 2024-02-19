### For our CCH2 data we need to limit the scope of each pull in accordance with their API pull limit (<1,000,000)
library(sf)
library(tidyverse)
library(lwgeom) # a package that has st_split...
library(data.table)

## Load in maps of each state to acquire extents, which then will be used to pull from the site. 
basemap_wgs <- sf::st_read("../data/raw/shapefiles/10m_cultural/10m_cultural/ne_10m_admin_1_states_provinces.shp") %>%
  dplyr::filter(name %in% c("California", "Nevada", "Arizona", "New Mexico", "Utah",
                            "Colorado", "Texas", "Oklahoma", "Sonora", "Chihuahua",
                            "Coahuila", "Nuevo León", "Tamaulipas", "Baja California", "Baja California Sur",
                            "Sinaloa", "Durango", "Zacatecas", "San Luis Potosí"))

extent <- st_bbox(basemap_wgs)

# Calculate the upper quadrant center of the extent
center_x <- (extent["xmin"] + extent["xmax"]) / 2
center_y <- (extent["ymin"] + extent["ymax"]) / 1.75 # splits california into easier to handle 

# Create two points horizontally aligned with the center
point1 <- st_point(c(extent["xmin"], center_y))
point2 <- st_point(c(extent["xmax"], center_y))

# Combine the points to create a line of intersection
intersection_line <- st_cast(st_combine(st_sfc(list(point1, point2), crs = st_crs(basemap_wgs))), "LINESTRING")
intersection_line_sf <- st_as_sf(intersection_line, crs = st_crs(basemap_wgs))
split_shape <- st_split(basemap_wgs, intersection_line)

# Shapefiles are a pain to split it turns out. Alternative, we're just going to calc max extents below and above the midpoint of the intersection line
st_bbox(basemap_wgs)
upper_box <- c(extent[1], point1[2], extent[3], extent[4]) # manually enter in the midpoint lat as the ymin
lower_box <- c(extent[1], extent[2], extent[3], point1[2]) # manually enter in the midpoint lat as the ymax
# Plot for clarity 
ggplot() +
  geom_sf(split_shape, mapping = aes(), color = "steelblue")

### Data was aquired using the upper_box and lower_box extents as the bounding pull on Tracheophyta with geocoords in CCH2
# Now we'll append phenology to each of these datasets and combine them 
upper_specimens <- read_csv("/home/jt-miller/Gurlab/sliding-phenology/data/raw/cch2_upper_box_2024-02-13_080734_DwC-A/occurrences.csv")
lower_specimens <- read_csv("/home/jt-miller/Gurlab/sliding-phenology/data/raw/cch2_lower_box_2024-02-13_085927_DwC-A/occurrences.csv")

#Load phenological data
pheno_upper <- read_csv("/home/jt-miller/Gurlab/sliding-phenology/data/raw/cch2_upper_box_2024-02-13_080734_DwC-A/measurementOrFact.csv", col_types = cols(
  coreid = col_double(),
  measurementType = col_character(),
  measurementTypeID	= col_character(),
  measurementValue = col_character(),
  measurementValueID	= col_character(),
  measurementUnit = col_character(),
  measurementDeterminedDate = col_character(),
  measurementDeterminedBy = col_character()
))
pheno_lower <- read_csv("/home/jt-miller/Gurlab/sliding-phenology/data/raw/cch2_lower_box_2024-02-13_085927_DwC-A/measurementOrFact.csv", col_types = cols(
  coreid = col_double(),
  measurementType = col_character(),
  measurementTypeID	= col_character(),
  measurementValue = col_character(),
  measurementValueID	= col_character(),
  measurementUnit = col_character(),
  measurementDeterminedDate = col_character(),
  measurementDeterminedBy = col_character()
))
#Convert the one-score-per-line phenological data into a dataframe containing five columns: id, reproductive (TRUE/FALSE), unopen flowers present (TRUE/FALSE), flowers present (TRUE/FALSE), fruit present (TRUE/FALSE)
pheno_upper_col <- matrix(ncol=5,nrow=length(unique(pheno_upper$coreid)))
pheno_lower_col <- matrix(ncol=5,nrow=length(unique(pheno_lower$coreid)))

colnames(pheno_upper_col)=c("id","reproductive","unopenFlower","openFlower","fruit")
colnames(pheno_lower_col)=c("id","reproductive","unopenFlower","openFlower","fruit")

pheno_upper_col <- as.data.frame(pheno_upper_col)
pheno_lower_col <- as.data.frame(pheno_lower_col)

pheno_upper_col$id <-unique(pheno_upper$coreid)
pheno_lower_col$id <-unique(pheno_lower$coreid)

for(i in 1:dim(pheno_upper)[1]){
  thisMatch <- match(pheno_upper$coreid[i],pheno_upper_col$id)
  if (pheno_upper$measurementValue[i]=="sterile"){
    pheno_upper_col$reproductive[thisMatch]=FALSE
  }else if(pheno_upper$measurementValue[i]=="reproductive"){
    pheno_upper_col$reproductive[thisMatch]=TRUE
  }else if(pheno_upper$measurementType[i]=="Unopen Flower" & pheno_upper$measurementType[i]=="present"){
    pheno_upper_col$unopenFlower[thisMatch]=TRUE
  }else if(pheno_upper$measurementType[i]=="Unopen Flower" & pheno_upper$measurementValue[i]=="absent"){
    pheno_upper_col$unopenFlower[thisMatch]=FALSE
  }else if(pheno_upper$measurementType[i]=="Open Flower" & pheno_upper$measurementValue[i]=="present"){
    pheno_upper_col$openFlower[thisMatch]=TRUE
  }else if(pheno_upper$measurementType[i]=="Open Flower" & pheno_upper$measurementValue[i]=="absent"){
    pheno_upper_col$openFlower[thisMatch]=FALSE
  }else if(pheno_upper$measurementType[i]=="Fruit" & pheno_upper$measurementValue[i]=="present"){
    pheno_upper_col$fruit[thisMatch]=TRUE
  }else if(pheno_upper$measurementType[i]=="Fruit" & pheno_upper$measurementValue[i]=="absent"){
    pheno_upper_col$fruit[thisMatch]=FALSE
  }
}

for(i in 1:dim(pheno_lower)[1]){
  thisMatch <- match(pheno_lower$coreid[i],pheno_lower_col$id)
  if (pheno_lower$measurementValue[i]=="sterile"){
    pheno_lower_col$reproductive[thisMatch]=FALSE
  }else if(pheno_lower$measurementValue[i]=="reproductive"){
    pheno_lower_col$reproductive[thisMatch]=TRUE
  }else if(pheno_lower$measurementType[i]=="Unopen Flower" & pheno_lower$measurementType[i]=="present"){
    pheno_lower_col$unopenFlower[thisMatch]=TRUE
  }else if(pheno_lower$measurementType[i]=="Unopen Flower" & pheno_lower$measurementValue[i]=="absent"){
    pheno_lower_col$unopenFlower[thisMatch]=FALSE
  }else if(pheno_lower$measurementType[i]=="Open Flower" & pheno_lower$measurementValue[i]=="present"){
    pheno_lower_col$openFlower[thisMatch]=TRUE
  }else if(pheno_lower$measurementType[i]=="Open Flower" & pheno_lower$measurementValue[i]=="absent"){
    pheno_lower_col$openFlower[thisMatch]=FALSE
  }else if(pheno_lower$measurementType[i]=="Fruit" & pheno_lower$measurementValue[i]=="present"){
    pheno_lower_col$fruit[thisMatch]=TRUE
  }else if(pheno_lower$measurementType[i]=="Fruit" & pheno_lower$measurementValue[i]=="absent"){
    pheno_lower_col$fruit[thisMatch]=FALSE
  }
}

#Attach phenological data to specimen data
spec_pheno_upper <- left_join(upper_specimens, pheno_upper_col, by="id")
spec_pheno_lower <- left_join(lower_specimens, pheno_lower_col, by="id")

# Merge data
spec_pheno_full <- rbind(spec_pheno_lower, spec_pheno_upper)

# Filter for duplicates
spec_pheno_d <- spec_pheno_full %>% 
  distinct(catalogNumber, scientificName,decimalLatitude, decimalLongitude, year, month, day, reproductive, unopenFlower, openFlower, fruit, .keep_all = TRUE )

# write out the dataset
fwrite(spec_pheno_d, "/home/jt-miller/Gurlab/sliding-phenology/data/raw/cch2-aligned-region-data.csv")

