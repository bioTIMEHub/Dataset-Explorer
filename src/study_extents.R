
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

# only points version... in case merges get messy
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

# hexagonal grid generating function from https://strimas.com/post/hexagonal-grids/ 
make_grid <- function(x, cell_diameter, cell_area, clip = FALSE) {
  if (missing(cell_diameter)) {
    if (missing(cell_area)) {
      stop("Must provide cell_diameter or cell_area")
    } else {
      cell_diameter <- sqrt(2 * cell_area / sqrt(3))
    }
  }
  ext <- as(raster::extent(x) + cell_diameter, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate array of hexagon centers
  g <- spsample(ext, type = "hexagonal", cellsize = cell_diameter, 
                offset = c(0.5, 0.5))
  # convert center points to hexagons
  g <- HexPoints2SpatialPolygons(g, dx = cell_diameter)
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

# make a hexagon grid to overlay the area that covers all our studies
# remember units are in km! 
#hex_grid <- make_grid(hulls, cell_area = 300^2, clip = FALSE)
hex_grid <- st_make_grid(hulls, cellsize=300, square=F, what='polygons')
hex_grid <- st_cast(hex_grid, to='MULTIPOLYGON') # turn single polygon into one polygon per cell

# ggplot() +
#     geom_polygon(data=map_data('world'), aes(x=long, y=lat, group=group), color='grey', fill='transparent') +
#     geom_sf(data=studies_pointsonly, color='blue', alpha=0.5) +
#     coord_sf(crs=merckm) +
#     geom_sf(data=hex_grid, color="orange", fill='transparent')

# for each hexagon cell that overlays our study points, match the attributes from that study point to that cell
# your R will be incapacitated for a while
# study_hex <- sp::over(study_points, hex_grid, returnList = T)

# filter out the hexagon cells that don't overlap with any points
# study_hex_filter <- study_hex %>% discard(function(x) nrow(x) == 0)
# rm(study_hex)

# generate a vector that tells me which hexagon grid corresponds with each study coordinate point
#study_coords_hex <- sp::over(studies_pointsonly, hex_grid, returnList=F)
study_coords_hex <- st_intersects(studies_pointsonly %>% st_cast(., to='MULTIPOINT'), hex_grid, sparse=F)
# add that to the study coordinates data frame to ref back to STUDY_ID
study_coords$hexcell <- sapply(unlist(study_coords_hex, recursive=FALSE, use.names=FALSE), "[", 1)
study_coords <- study_coords %>% distinct(STUDY_ID, hexcell) # keep only distinct hexagon cell records
study_coords <- study_coords %>% filter(!is.na(hexcell))

mult.cell.studies <- study_coords %>% arrange(STUDY_ID) %>% filter(duplicated(STUDY_ID)) %>% distinct(STUDY_ID) %>% dplyr::select(STUDY_ID) %>% as_vector()
sing.cell.studies <- study_coords %>% arrange(STUDY_ID) %>% filter(!STUDY_ID %in% mult.cell.studies) %>% distinct(STUDY_ID) %>% dplyr::select(STUDY_ID) %>% as_vector()

# hex_grid_sf <- hex_grid %>% spTransform(., CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
#   st_as_sf(.) %>% st_wrap_dateline(., options='WRAPDATELINE=YES') %>%
#   st_transform(., crs="+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")

extents <- data.frame(STUDY_ID=mult.cell.studies, geometry=rep('', length(mult.cell.studies)))

for (i in 1:length(mult.cell.studies)) {
  extents$geometry[i] <- hex_grid[c(study_coords$hexcell[which(study_coords$STUDY_ID == mult.cell.studies[i])]),1] %>% 
  st_union(., by_feature=F)
}

extents <- left_join(extents, BT_datasets, by='STUDY_ID')
save(extents, file='large_extent_studies.RData')
save(sing.cell.studies, file='single_cell_studies.RData')
