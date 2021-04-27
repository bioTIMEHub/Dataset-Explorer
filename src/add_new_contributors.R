
#######################################################

# Adding new contributors to Shiny working data
# Author: Cher chow
# Updated: April 26th, 2021

rm(list=ls(all.name=T)) # clear environment

require(dplyr) # data wrangling
require(readxl) # reading excel sheets

filenames <- list.files(path = '/Users/cher/Dropbox/towards BioTIME v2/NewStudies', pattern = '.xlsx$', full.names = T)
# having filenames base all of our extraction keeps everything in the same order


# Extract fields from Excel files -----------------------------------------

raw <- as.list(rep('', length(filenames))) # the raw data fields to calculate some summary statistics
site <- as.list(rep('', length(filenames)))
DS <- as.list(rep('', length(filenames))) 
contacts <- as.list(rep('', length(filenames))) 

for (i in 1:length(filenames)) {
  site[[i]] <- read_excel(filenames[i], col_names = T, na = '', range = 'metaDataSite!B1:M2')
  DS[[i]] <- read_excel(filenames[i], col_names = T, na = '', range = 'metaDataDS!B1:D2')
  contacts[[i]] <- read_excel(filenames[i], col_names = T, na = '', range = 'metaDataContacts!B1:C2')
  raw[[i]] <- read_excel(filenames[i], sheet=1, col_names = T, na = '', range = cell_cols('C:M'))
  }

# collapse the lists into dataframes
contacts <- do.call(rbind.data.frame, contacts) %>% select(CONTACT_1 = ContactName1, CONTACT_2 = ContactName2)
site <- do.call(rbind.data.frame, site) %>% 
  select(REALM=Realm, CENT_LAT=CentralLatitudeSite, CENT_LONG=CentralLongitudeSite, BIOME_MAP=BiomeMap, CLIMATE = Climate)
DS <- do.call(rbind.data.frame, DS) %>% select(TAXA=Taxa, TITLE = Title)


# Calculate summary statistics for working data --------------------------------------------

stats <- data.frame(START_YEAR='', END_YEAR='', NUMBER_OF_SPECIES='')

# calculate summary statistics from each rawdata table
for (i in 1:length(filenames)) {
  stats[i,1] <- min(raw[[i]][['Year']]) %>% as.numeric()
  stats[i,2] <- max(raw[[i]][['Year']]) %>% as.numeric()
  stats[i,3] <- raw[[i]] %>% as_tibble() %>% select(Family, Genus, Species) %>% unique() %>% nrow() %>% as.numeric()
}
View(stats)
# rm(raw) # once visual check shows it's okay, delete the rawdata tables
str(stats)
stats <- stats %>% mutate(across(.cols = everything(), as.numeric)) # make sure things are numeric
stats <- stats %>% mutate(DURATION = END_YEAR - START_YEAR + 1) # +1 for year inclusive

new_studies <- bind_cols(site, DS, contacts, stats)
View(new_studies)
new_studies$STUDY_ID <- paste0('2000', 1:length(filenames)) %>% as.numeric()
# assign some dummy study IDs. 2.number to distinguish them from current public studies
new_studies$CONTACT_2 <- str_replace_all(new_studies$CONTACT_2, '0', '')
write.csv(new_studies, file='/Volumes/Cherbet/BioTIME/Dataset-Explorer/src/new_studies.csv', col.names=T, na='') # export the final dataframe

# Merge
working_data <- read.csv(file.choose(), header=T) # read in working_data.csv to merge
# working_data$DURATION <- working_data$DURATION + 1
full_join(working_data, new_studies) %>% write.csv(., file='/Volumes/Cherbet/BioTIME/Dataset-Explorer/src/app_data.csv', col.names=T, row.names=F, na='')

# Study_coords file -------------------------------------------------------

new_coords <- lapply(raw, function(x) x %>% distinct(Latitude, Longitude) %>% as_tibble())
for (i in 1:length(filenames)) {
  new_coords[[i]]['STUDY_ID'] <- rep(paste0('2000', i), dim(new_coords[[i]])[1]) %>% as.numeric()
} # put in Study ID identifiers
new_coords <- do.call(rbind.data.frame, new_coords)

new_coords <- new_coords %>% as_tibble() %>% dplyr::select(STUDY_ID, LATITUDE=Latitude, LONGITUDE=Longitude)
# in case we accidentally use comma for decimal points in curation
new_coords$LATITUDE <- str_replace_all(new_coords$LATITUDE, ',', '.')
new_coords$LONGITUDE <- str_replace_all(new_coords$LONGITUDE, ',', '.')
write.csv(new_coords, file = '/Volumes/Cherbet/BioTIME/Dataset-Explorer/src/new_coords.csv', col.names=T, na='') # export the coordinates
new_coords <- new_coords %>% mutate(across(where(is.character), as.numeric))

# Merge
coords <- read.csv(file.choose(), header=T) # read in study coords to merge
full_join(coords, new_coords) %>% write.csv(., file='/Volumes/Cherbet/BioTIME/Dataset-Explorer/src/app_coords.csv', col.names=T, row.names=F, na='')

