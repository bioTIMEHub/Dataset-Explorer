
# Dataset Explorer for BioTIME
### An interactive Shiny app to visually show the time-series datasets in the BioTIME database  
**Author** Cher Chow  
**Contact** fycc1 at st-andrews.ac.uk or through GitHub.  

## Requirements
This app is not yet published or hosted on a server, so this will only run if you open this from RStudio and run it locally from your computer.
* RStudio and R (preferably version 4.0+)
* This app requires these packages to run: `shiny`, `shinyjs`, `tidyverse`, `plotly`, `sf`, `leaflet`, and `leaflet.providers`.
* Source files that calculate dataset extents require additional packages but these are all listed in the `.R` files themselves.

## Features
### shinyMap
The shinyMap app is a version with only the dataset map (intended for the main website page). It has a control panel for filtering datasets by categories like biome, taxa, realm, and duration. The map itself is interactive. Each dataset is represented by either a circle marker or groups of hex cells. Clicking on these will pop-up a tooltip with more details about the dataset.

### shinyTrends
Work in progress! This will be a full page data explorer that also incorporates dataset filtering and showing diversity trends from our 2014 (Dornelas et al.) and 2019 (Blowes et al.) Science papers. 

## Running the app locally
1. Clone a local copy of the repository.
2. In the subfolder of the app you want to run (either `shinyMap` or `shinyTrends`), open `app.R` in RStudio.
3. Click the Run button that shows up in the upper-right corner of RStudio.
4. This should run out of the box smoothly except that it'll prompt a pop-up in RStudio to set the working directory. Choose any file within the src folder.