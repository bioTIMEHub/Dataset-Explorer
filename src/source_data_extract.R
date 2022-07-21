
# Generate BioTIME working data source file for app
# Updated 27 Apr 2021
# Author: Cher Chow

# This script must be run from within the server's RStudio to extract data from the SQL database

# Set up BioTIME database connection
Connection <- dbConnect(RMariaDB::MariaDB(), dbname="##redacted##", username="##redacted##", password="##redacted##", host="##redacted##")


# Metadata ----------------------------------------------------------------

# get dataset coordinates
studies <- dbGetQuery(Connection, "SELECT STUDY_ID, CENT_LAT, CENT_LONG, TAXA, TITLE, START_YEAR, END_YEAR, NUMBER_OF_SPECIES from datasets")
contributors <- dbGetQuery(Connection, "SELECT STUDY_ID, CONTACT_1, CONTACT_2 from contacts")

BT_datasets <- full_join(studies, contributors, by="STUDY_ID") # link dataset info with contributors

# create a column for time-series durationb
BT_datasets <- datasets %>% mutate(DURATION = END_YEAR - START_YEAR + 1) # year inclusive

# export
# Public version only, doesn't include new or non-public datasets
write.csv(BT_datasets, '/Volumes/Cherbet/BioTIME/Dataset-Explorer/src/working_data.csv', col.names = T, na = '', row.names = F)

# Spatial coordinate data -------------------------------------------------

# create a spatial data table
# Not centroids! All plot coordinates within datasets
study_coords <- dbGetQuery(Connection, 'select distinct STUDY_ID, LATITUDE, LONGITUDE from allrawdata')

# export
# Public version only, doesn't include new or non-public datasets
write.csv(study_coords, '/Volumes/Cherbet/BioTIME/Dataset-Explorer/src/study_coords.csv', col.names = T, na = '', row.names = F)
