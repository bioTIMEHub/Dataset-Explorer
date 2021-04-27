
# Generate hexagon grid polygons for BioTIME studies
# Author: Cher Chow
# Updated: 27 Apr 2021

require(tidyverse)
require(maps)
require(raster)
require(sp)
require(sf)
require(maptools)
require(utils)

setwd(file.choose() %>% dirname()) # select the app_data.csv file in the Shiny source folder. sets wd to the src folder

# import study metadata
BT_datasets <- read.csv('app_data.csv', header=T, blank.lines.skip=T)
BT_datasets$TAXA <- as.factor(BT_datasets$TAXA)
BT_datasets$REALM <- as.factor(BT_datasets$REALM)
BT_datasets$BIOME_MAP <- as.factor(BT_datasets$BIOME_MAP)
BT_datasets$CLIMATE <- as.factor(BT_datasets$CLIMATE)
BT_datasets$X <- NULL

BT_datasets <- BT_datasets %>% distinct(STUDY_ID, .keep_all = T)

# import study coordinates
study_coords <- read.csv('app_coords.csv', header=T, blank.lines.skip=T)
study_coords[c(117618,121261,169392),1] <- 169 # fix STUDY_ID bug

# spatial points data (without coordinates)
study.data <- left_join(study_coords, BT_datasets, by="STUDY_ID")
study.data <- study.data %>% dplyr::select(!c(CENT_LAT, CENT_LONG, LATITUDE, LONGITUDE))
# create a spatial points dataframe
study_points <- SpatialPointsDataFrame(cbind(study_coords$LONGITUDE, study_coords$LATITUDE), data=study.data, proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))

# only points version... in case merges get messy
studies_pointsonly <- SpatialPoints(cbind(study_coords$LONGITUDE,study_coords$LATITUDE), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>% 
  sp::spTransform(., CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))

# calculate a convex hull that covers all studies
hulls <- SpatialPoints(cbind(study_coords$LONGITUDE,study_coords$LATITUDE), proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
# leaflet uses Google Mercator projection, so we'll have to generate our grid for it
hulls <- sp::spTransform(hulls, CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")) %>% 
  rgeos::gConvexHull()

# make sure we match the projection for our points with the hexagon grid
study_points <- sp::spTransform(study_points, CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"))

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
hex_grid <- make_grid(hulls, cell_area = 450^2, clip = FALSE)

# for each hexagon cell that overlays our study points, match the attributes from that study point to that cell
# your R will be incapacitated for a while
# study_hex <- sp::over(study_points, hex_grid, returnList = T)

# filter out the hexagon cells that don't overlap with any points
# study_hex_filter <- study_hex %>% discard(function(x) nrow(x) == 0)
# rm(study_hex)

# generate a vector that tells me which hexagon grid corresponds with each study coordinate point
study_coords_hex <- sp::over(studies_pointsonly, hex_grid, returnList=F)
# add that to the study coordinates data frame to ref back to STUDY_ID
study_coords$hexcell <- study_coords_hex
study_coords <- study_coords %>% distinct(STUDY_ID, hexcell) %>% filter(!STUDY_ID == 2001) # keep only distinct hexagon cell records
study_coords <- study_coords %>% filter(!is.na(hexcell))

mult.cell.studies <- study_coords %>% arrange(STUDY_ID) %>% filter(duplicated(STUDY_ID)) %>% distinct(STUDY_ID) %>% dplyr::select(STUDY_ID) %>% as_vector()
sing.cell.studies <- study_coords %>% arrange(STUDY_ID) %>% filter(!duplicated(STUDY_ID)) %>% distinct(STUDY_ID) %>% dplyr::select(STUDY_ID) %>% as_vector()

hex_grid_sf <- hex_grid %>% spTransform(., CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')) %>%
  st_as_sf(.) %>% st_wrap_dateline(., options='WRAPDATELINE=YES') %>%
  st_transform(., crs="+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs")
extents <- data.frame(STUDY_ID=mult.cell.studies, geometry=rep('', length(mult.cell.studies)))
for (i in 1:length(mult.cell.studies)) {
  extents$geometry[i] <- hex_grid_sf[c(study_coords$hexcell[which(study_coords$STUDY_ID == mult.cell.studies[i])]),1] %>% 
  st_union(., by_feature=F)
}

extents <- left_join(extents, BT_datasets, by='STUDY_ID')
saveRDS(extents, file='large_extent_studies.rds')
saveRDS(sing.cell.studies, file='single_cell_studies.rds')
save.image(file='study_extents.RData')
