
#######################################################

# Adding new contributors to Shiny working data
# Author: Cher chow
# Updated: April 26th, 2021

rm(list=ls(all.name=T)) # clear environment

require(dpyr) # data wrangling
require(readxl) # reading excel sheets

filenames <- list.files(path = '/Users/cher/Dropbox/towards BioTIME v2/NewStudies', pattern = '.xlsx$', full.names = T)
# having filenames base all of our extraction keeps everything in the same order

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

stats <- data.frame(START_YEAR='', END_YEAR='', NUMBER_OF_SPECIES='')

# calculate summary statistics from each rawdata table
for (i in 1:length(filenames)) {
  stats[i,1] <- min(raw[[i]][['Year']]) %>% as.numeric()
  stats[i,2] <- max(raw[[i]][['Year']]) %>% as.numeric()
  stats[i,3] <- raw[[i]] %>% as_tibble() %>% select(Family, Genus, Species) %>% unique() %>% nrow() %>% as.numeric()
}
View(stats)
rm(raw) # once visual check shows it's okay, delete the rawdata tables
str(stats) # make sure things are numeric
stats <- stats %>% mutate(DURATION = END_YEAR - START_YEAR + 1) # +1 for year inclusive

new_studies <- bind_cols(site, DS, contacts, stats)
View(new_studies)
new_studies$STUDY_ID <- paste0('U', 1:length(filenames)) # assign some dummy study IDs. U for unpublished.


