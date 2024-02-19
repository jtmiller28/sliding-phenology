### Geographic Subsetting with accordance to Vaughn Shirey's maps 
library(data.table)
library(tidyverse)
library(sf)

## Read in the data
# inat data
inat_w_obs <- fread("/home/jt-miller/Gurlab/sliding-phenology/data/processed/inat-wild-obs.csv")
inat_rg <- fread("/home/jt-miller/Gurlab/sliding-phenology/data/processed/inat-wild-rg.csv")
inat_rg_flowering <- fread("/home/jt-miller/Gurlab/sliding-phenology/data/processed/inat-wild-rg-flowering.csv")

##### BASEMAP AND ENVIRONMENTAL DATA ##### (Vaughn's Delimitation)
#### Basemap ####
PROJ_CRS <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
sf::sf_use_s2(FALSE) # turning of the autotuned proj 
# Load a basemap of states/provinces for the study region
basemap_wgs <- sf::st_read("../data/raw/shapefiles/10m_cultural/10m_cultural/ne_10m_admin_1_states_provinces.shp") %>%
  dplyr::filter(name %in% c("California", "Nevada", "Arizona", "New Mexico", "Utah",
                            "Colorado", "Texas", "Oklahoma", "Sonora", "Chihuahua",
                            "Coahuila", "Nuevo León", "Tamaulipas", "Baja California", "Baja California Sur",
                            "Sinaloa", "Durango", "Zacatecas", "San Luis Potosí"))
basemap <- sf::st_transform(basemap_wgs, PROJ_CRS)

#### WWF Biomes ####
wwf_biomes <- sf::st_read("../data/raw/shapefiles/WWF/official/wwf_terr_ecos.shp") %>%
  sf::st_crop(basemap_wgs) %>%
  sf::st_intersection(basemap_wgs) %>%
  dplyr::filter(BIOME %in% c(12, 13)) %>%
  sf::st_union() %>%
  sf::st_make_valid() %>%
  sf::st_sf()

wwf_biomes <- sf::st_transform(wwf_biomes, PROJ_CRS)

#### Spatial Hexes ####
# Generate spatial hexes for binning the data at a variety of scales.
hex_100km <- sf::st_make_grid(basemap,
                              cellsize = 100*1000, #  100km hex cells
                              square = FALSE,
                              flat_topped = FALSE) %>%
  sf::st_as_sf() %>%
  dplyr::mutate(hex100_id = row_number()) %>%
  sf::st_intersection(wwf_biomes) %>%
  dplyr::mutate(area = sf::st_area(.))
saveRDS(hex_100km, "../data/processed/hex_100km.RDS")

# plot for clarity 
ggplot() +
  geom_sf(basemap, mapping = aes()) +
  geom_sf(wwf_biomes, mapping = aes(alpha = 0.5, fill = "orange")) + 
  geom_sf(hex_100km, mapping = aes(alpha = 0.3)) +
  theme_bw() +
  guides(fill = FALSE, alpha = FALSE)



## Make the data spatial 
inat_w_obs_sf <- st_as_sf(inat_w_obs, 
                              coords = c("decimalLongitude", "decimalLatitude"),
                              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                              remove = FALSE)
inat_rg_sf <- st_as_sf(inat_rg, 
                          coords = c("decimalLongitude", "decimalLatitude"),
                          crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                          remove = FALSE)
inat_rg_flowering_sf <- st_as_sf(inat_rg_flowering, 
                       coords = c("decimalLongitude", "decimalLatitude"),
                       crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                       remove = FALSE)
## Transform the projection to equal area to match our basemap
inat_w_obs_sf <- sf::st_transform(inat_w_obs_sf, crs = PROJ_CRS)
inat_rg_sf <- sf::st_transform(inat_rg_sf, crs = PROJ_CRS)
inat_rg_flowering_sf <- sf::st_transform(inat_rg_flowering_sf, crs = PROJ_CRS)

## Spatial Joins with hexcells
inat_w_obs_hexed100 <- sf::st_join(inat_w_obs_sf, hex_100km)
inat_rg_hexed100 <- sf::st_join(inat_rg_sf, hex_100km)
inat_rg_flowering_hexed100 <- sf::st_join(inat_rg_flowering_sf, hex_100km)

## Remove records without an associated hexcell
inat_w_obs_hexed100 <- inat_w_obs_hexed100 %>% 
  dplyr::filter(!is.na(hex100_id))
inat_rg_hexed100 <- inat_rg_hexed100 %>% 
  dplyr::filter(!is.na(hex100_id))
inat_rg_flowering_hexed100 <- inat_rg_flowering_hexed100 %>% 
  dplyr::filter(!is.na(hex100_id))

## Save file as an r obj
saveRDS(inat_w_obs_hexed100, file = "../data/processed/inat_w_obs_hexed100.rds")
saveRDS(inat_rg_hexed100, file = "../data/processed/inat_rg_hexed100.rds")
saveRDS(inat_rg_flowering_hexed100, file = "../data/processed/inat_rg_flowering_hexed100.rds")

## Save as a csv
inat_w_obs_hexed100 <- inat_w_obs_hexed100 %>% 
  st_drop_geometry()
inat_rg_hexed100 <- inat_rg_hexed100 %>% 
  st_drop_geometry()
inat_rg_flowering_hexed100 <- inat_rg_flowering_hexed100 %>% 
  st_drop_geometry()

fwrite(inat_w_obs_hexed100, file = "../data/processed/inat_w_obs_hexed100.csv")
fwrite(inat_rg_hexed100, file = "../data/processed/inat_rg_hexed100.csv")
fwrite(inat_rg_flowering_hexed100, file = "../data/processed/inat_rg_flowering_hexed100.csv")




