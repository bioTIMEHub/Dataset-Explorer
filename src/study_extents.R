
# Generate hexagon grid polygons for BioTIME studies
# Author: Cher Chow
# Updated: 27 Apr 2021

require(tidyverse)
require(sf)


set.seed(24)
setwd('src/') # select the app_data.csv file in the Shiny source folder. sets wd to the src folder
# intended to be run under Dataset-Explorer R Project file

# assumes the BioTIME v2 public query with metadata is read in as BT2
# BT2 <- readRDS('/Users/cherchow/Dropbox/towards BioTIME v2/FINALFILESv2/pubQMeta.rds')
dt_meta <- BT2 %>% distinct(STUDY_ID, CENT_LAT, CENT_LONG, YEAR, TITLE, BIOME_MAP, CLIMATE, REALM, TAXA) %>% 
  group_by_at(vars(-YEAR)) %>% summarise(Duration = max(YEAR) - min(YEAR) + 1)
dt_coords <- BT2 %>% distinct(STUDY_ID, LATITUDE, LONGITUDE)
dt_contacts <- read_csv('allContactsPlusNew.csv')
dt_meta <- left_join(dt_meta, dt_contacts[2:4], by = "STUDY_ID")

summary(dt_coords)
summary(dt_meta)

type <- dt_coords %>% group_by(STUDY_ID) %>% 
  summarise(sites_n = n()) %>% ungroup
dt_meta <- left_join(dt_meta, type, by="STUDY_ID")

# write_csv(dt_meta, 'dt_meta.csv') # save a copy of the file

# only multilocation studies need to have their coords processed

# create a multipoint sf object with dataset coordinates (from rawdata tables)
dt_coords <- dt_coords %>% filter(STUDY_ID %in% dt_meta$STUDY_ID[dt_meta$sites_n > 1]) %>% 
  st_as_sf(., coords=c('LONGITUDE', 'LATITUDE'), crs = 4326) # convert lat lon columns to point geometry

# write_csv(dt_coords, 'dt_shiny2_coords.csv')

# plot to check
library(rnaturalearth)
proj = '+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +R=6371008.7714 +units=m +no_defs +type=crs'
world <- ne_countries(scale = 'medium', returnclass = 'sf') %>% st_make_valid()
ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data=dt_coords, color='blue', shape = 21, size = 2) +
  guides(fill = 'none') + 
  coord_sf(crs = proj) +
  theme_minimal()

# make a hexagon grid to overlay the area that covers all our studies
# redefine projection for the target 
proj = "+proj=natearth +lon_0=0 +x_0=0 +y_0=0 +R=6371008.7714 +units=m +no_defs +type=crs"

hex_grid <- st_combine(dt_coords) %>% st_transform(., crs = proj) %>% 
  st_make_grid(., cellsize = 193600, square=F, what='polygons')

ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data=hex_grid, color="coral", fill='transparent') +
  guides(fill = 'none') + 
  coord_sf(crs = proj) +
  theme_minimal()
  
# generate a vector that tells me which hexagon grid corresponds with each study coordinate point
# operate in the target projection
dt_coords <- st_transform(dt_coords, proj)
sum(st_is_valid(dt_coords) + 0) == nrow(dt_coords)
sum(st_is_valid(hex_grid) + 0)
study_hex <- st_intersects(dt_coords, hex_grid)

# each item in this list is a coordinate and the value = hex index it intersects with
study_hex <- rapply(study_hex, function(x) x[1], how = "unlist") # just select the first hex cell if a point intersects with multiple
dt_coords$hexcell <- study_hex # add information from overlap analysis to dataframe
# hexcell corresponds to the cell number that contains that coordinate point

dt_coords <- dt_coords %>% distinct(STUDY_ID, hexcell) # keep only distinct hexagon cell records
# each row = 1 hex cell per study

# check if there are any NAs
dt_coords %>% filter(is.na(hexcell)|hexcell == ''|hexcell == 0)

nHex <- dt_coords %>% group_by(STUDY_ID) %>% count() %>% arrange(n, STUDY_ID)
# create a dataframe where n = number of hex cells per study
# vector that helps index multiple cell studies from single cell studies
dt_meta$hex <- 0
dt_meta$hex[dt_meta$STUDY_ID %in% nHex$STUDY_ID[nHex$n > 1]] <- 1

# make an empty dataframe to populate
extents <- data.frame(STUDY_ID = dt_meta$STUDY_ID[dt_meta$hex == 1], geometry = '')

# for each study, create a joined polygon of all the hexcells it covers
for (i in 1:nrow(extents)) {
  extents$geometry[i] <- hex_grid[c(dt_coords$hexcell[which(dt_coords$STUDY_ID == extents$STUDY_ID[i])])] %>% st_combine
}

extents <- st_as_sf(extents)
st_crs(extents) <- proj # make sure sf knows the CRS for the polygons

ggplot() +
  geom_sf(data = world, aes(group = admin), color='grey40', fill='grey95') +
  geom_sf(data=extents, aes(fill=STUDY_ID), alpha=0.5) +
  coord_sf(crs=proj)

# one sf tibble for studies that have hex geometry
extents <- left_join(extents, dt_meta, by="STUDY_ID") %>% select(!c(hex, sites_n))
# another for single point studies
dt_points <- dt_meta %>% ungroup %>% filter(hex == 0) %>% st_as_sf(coords = c('CENT_LONG', 'CENT_LAT'), crs = 4326) %>% 
  select(!c(hex, sites_n)) %>% st_transform(., proj)

dt_points$wkt <- st_as_text(dt_points$geometry)
st_drop_geometry(dt_points) %>% write_csv(., 'dt_points.csv')

extents %>% mutate(wkt = st_as_text(geometry)) %>% select(!c(geometry, starts_with('CENT'))) %>% 
  write_csv(., 'dt_hex.csv')
