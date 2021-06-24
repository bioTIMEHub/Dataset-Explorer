
# Generate hexagon grid polygons for BioTIME studies
# Author: Cher Chow
# Updated: 27 Apr 2021

require(tidyverse)
require(rworldmap)
require(sf)
require(maptools)
require(utils)

setwd('src/') # select the app_data.csv file in the Shiny source folder. sets wd to the src folder
# intended to be run under Dataset-Explorer R Project file

# import study coordinates
study_coords <- read.csv('app_coords.csv', header=T, blank.lines.skip=T)
study_coords[c(117618,121261,169392),1] <- 169 # fix STUDY_ID bug

wgs84 <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' # BioTIME default
# web mercator in km
merckm <- '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs'

# create a multipoint sf object with dataset coordinates (from rawdata tables)
studies_pointsonly <- study_coords %>% dplyr::select(LONGITUDE, LATITUDE) %>% 
  st_as_sf(., coords=c('LONGITUDE', 'LATITUDE'), crs=wgs84) %>% 
  st_transform(., crs=merckm)

# calculate a convex hull that covers all studies
hulls <- st_convex_hull(st_union(studies_pointsonly))

# Plot to check
# ggplot() +
#   geom_polygon(data=map_data('world'), aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#   geom_sf(data=hulls, fill='lightblue', color='blue', alpha=0.2) +
#   geom_sf(data=studies_pointsonly, color='blue', shape=21, size=0.2) +
#   coord_sf(crs=merckm) +
#   theme_minimal()

# make a hexagon grid to overlay the area that covers all our studies
# remember units are in km! 
hex_grid <- st_make_grid(hulls, cellsize=300, square=F, what='polygons')
hex_grid <- st_cast(hex_grid, to='MULTIPOLYGON') # turn single polygon into one polygon per cell

# ggplot() +
#     geom_polygon(data=map_data('world'), aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#     geom_sf(data=studies_pointsonly, color='blue', alpha=0.5) +
#     coord_sf(crs=merckm) +
#     geom_sf(data=hex_grid, color="orange", fill='transparent')

# generate a vector that tells me which hexagon grid corresponds with each study coordinate point
#study_coords_hex <- sp::over(studies_pointsonly, hex_grid, returnList=F)
study_hex <- st_intersects(studies_pointsonly, hex_grid)
# add that to the study coordinates data frame to ref back to STUDY_ID
study_hex <- rapply(study_hex, function(x) x[1], how = "unlist") # just select the first hex cell if a point intersects with multiple
study_coords$hexcell <- study_hex # add information from overlap analysis to dataframe
# hexcell corresponds to the cell number that contains that coordinate point
study_coords <- study_coords %>% distinct(STUDY_ID, hexcell) # keep only distinct hexagon cell records
# each row = 1 hex cell per study
# check if there are any NAs
study_coords %>% filter(is.na(hexcell)|hexcell == ''|hexcell == 0)

nHex <- study_coords %>% group_by(STUDY_ID) %>% count() # create a dataframe where n = number of hex cells per study

# vector that helps index multiple cell studies from single cell studies
mult.cell.studies <- nHex %>% filter(n > 1) %>% pull(STUDY_ID)
sing.cell.studies <- nHex %>% filter(n == 1) %>% pull(STUDY_ID)
length(mult.cell.studies) + length(sing.cell.studies) == n_distinct(study_coords$STUDY_ID) # does this separation add up?
# no studies left behind!

hex_grid_sf <- hex_grid %>% st_transform(., crs=wgs84) %>%
  st_wrap_dateline(., options='WRAPDATELINE=YES') %>%
  st_transform(., crs=merckm) # in case something goes haywire at dateline

# make an empty dataframe to populate
extents <- data.frame(STUDY_ID=mult.cell.studies, geometry=rep('', length(mult.cell.studies)))

# for each study, create a joined polygon of all the hexcells it covers
for (i in 1:length(mult.cell.studies)) {
  extents$geometry[i] <- hex_grid[c(study_coords$hexcell[which(study_coords$STUDY_ID == mult.cell.studies[i])])] %>% 
  st_union(., by_feature=F)
}

extents <- st_as_sf(extents) %>% st_cast(., 'MULTIPOLYGON')
st_crs(extents) <- merckm # make sure sf knows the CRS for the polygons

ggplot() +
  geom_polygon(data=map_data('world'), aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
  geom_sf(data=extents, aes(fill=STUDY_ID), alpha=0.5) +
  coord_sf(crs=merckm)

BT_datasets <- read.csv('app_data.csv', header=T, blank.lines.skip=T)
extents <- left_join(extents, BT_datasets, by='STUDY_ID')
save(extents, file='large_extent_studies.RData')
save(sing.cell.studies, file='single_cell_studies.RData')
