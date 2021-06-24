
# BioTIME App
# Description: Explores BioTIME datasets by mapping the global coverage of data sets, attributing contributors,
# and shows trends from Science 2014 paper.
# Author: Cher Chow
# Updated: 26 Apr 2021

require(shiny)
require(shinyjs)
require(tidyverse)
require(leaflet)
require(sf)


# Data set up -------------------------------------------------------------

# SET WORKING DIRECTORY FIRST :)
# select the app_data.csv file. this sets the working directory to the src folder
# setwd('/Volumes/Cherbet/BioTIME/Dataset-Explorer/src')
# in server, working directory for server side defaults to the app folder
set.seed(24)

# BioTIME color functions
source('./src/scale_gg_biotime.R')

# load data
BT_datasets <- read.csv('./src/app_data.csv', header=T) # table for dataset metadata
load('./src/large_extent_studies.RData') # load the hex cell study extents
load('./src/single_cell_studies.RData') # load vector listing studies that are too small to plot extents

# Housekeeping after importing
BT_datasets$TAXA <- str_to_title(BT_datasets$TAXA) # fix mismatched title case for levels
BT_datasets$TAXA <- as.factor(BT_datasets$TAXA)
BT_datasets$REALM <- as.factor(BT_datasets$REALM)
BT_datasets$BIOME_MAP <- as.factor(BT_datasets$BIOME_MAP)
BT_datasets$CLIMATE <- as.factor(BT_datasets$CLIMATE)

# transform the spatial data to fit the original BioTIME WGS datum
extents <- st_as_sf(extents) %>% st_set_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs") %>% 
  st_transform(., 4326)

# User Interface (Frontend) ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    # Style import
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # things on UI/client side are located in www folder
    # all assets that have to be accessible for frontend like css or images have to be in the www folder
  ),
  useShinyjs(),
  
  div(id='mapwrap',
    fluidRow(id='count',
        div(class='stat', h1(textOutput('studies')), h4('studies')),
        div(class='stat', h1(textOutput('years')), h4('years')),
        div(class='stat', h1(textOutput('contributors')), h4('contributors')),
        div(class='stat', h1(textOutput('taxa')), h4('taxa')),
        div(class='stat', h1(textOutput('biome')), h4('biomes'))
    ),
  
    # Dataset map
    fluidRow(id='maprow',
             leafletOutput("StudyMap", height='100%'), #output map
             
             absolutePanel(id='control', draggable=T, # control panel
                           h2('Filter datasets'),
                           tags$div(class="accordion",
                                    tags$input(id='tog_realm', type='checkbox', class='accordion-toggle', name='toggle', checked=NA),
                                    tags$label(`for`='tog_realm', 'Realm'),
                                    tags$section(checkboxGroupInput('Realm', label=NULL,
                                                                    choiceNames=levels(BT_datasets$REALM),
                                                                    selected=levels(BT_datasets$REALM),
                                                                    choiceValues=levels(BT_datasets$REALM)))),
                           
                           tags$div(class="accordion",
                                    tags$input(id='tog_taxa', type='checkbox', class='accordion-toggle', name='toggle'),
                                    tags$label(`for`='tog_taxa', 'Taxa'),
                                    tags$section(checkboxGroupInput('Taxa', label=NULL,
                                                                    choiceNames=levels(BT_datasets$TAXA),
                                                                    selected=levels(BT_datasets$TAXA),
                                                                    choiceValues=levels(BT_datasets$TAXA)))),
                           
                           tags$div(class="accordion",
                                    tags$input(id='tog_climate', type='checkbox', class='accordion-toggle', name='toggle'),
                                    tags$label(`for`='tog_climate', 'Climate'),
                                    tags$section(checkboxGroupInput('Climate', label=NULL,
                                                                    choiceNames=levels(BT_datasets$CLIMATE),
                                                                    selected=levels(BT_datasets$CLIMATE),
                                                                    choiceValues=levels(BT_datasets$CLIMATE)))),
                           
                           tags$div(class="accordion",
                                    tags$input(id='tog_dur', type='checkbox', class='accordion-toggle', name='toggle', checked=NA),
                                    tags$label(`for`='tog_dur', 'Duration (yrs)'),
                                    tags$section(sliderInput('Duration', label=NULL,
                                                             min=2, max=130, value=c(2, 130),
                                                             round = T, ticks = F, animate = FALSE))),
                           actionButton('reset', 'Reset')
             ) # end control panel
    ) # end row
  ) # end wrapper
) # end UI


# Server -----------------------------------------------------------

# define all the under the hood computations that have to be done to show the map
server <- function(input, output) {
  
  # reset button
  observeEvent(input$reset, {
    reset('control')
  })
  

# Dataset map -------------------------------------------------------------
  
  # dataframe with information but not split (tallies)
  datasets <- reactive({
    BT_datasets %>% filter(DURATION >= input$Duration[1] & DURATION <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  studies <- reactive({ # make a working dataframe for single cell studies based on the filter options from the input
    BT_datasets %>% filter(STUDY_ID %in% sing.cell.studies) %>% 
      filter(DURATION >= input$Duration[1] & DURATION <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  # make a working dataframe for large extent (hex cell) studies based on the filter options from the input
  large.studies <- reactive({
    extents %>% filter(DURATION >= input$Duration[1] & DURATION <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  # create a palette for the leaflet map to use
  pal <- colorBin(palette=intPalette(biotime_palettes[['gradient']][2:5])(5),
                  domain=2:130,
                  bins=c(2,10,25,50,100,130))
  
  # generate a reactive dataset summary statistic counter
  output$studies <- renderText({datasets() %>% filter(STUDY_ID < 600) %>% pull(STUDY_ID) %>% n_distinct()})
  output$contributors <- renderText({c(datasets() %>% pull(CONTACT_1), datasets() %>% pull(CONTACT_2)) %>% n_distinct()})
  output$taxa <- renderText({datasets() %>% pull(TAXA) %>% n_distinct()})
  output$biome <- renderText({datasets() %>% pull(BIOME_MAP) %>% n_distinct()})
  output$years <- renderText({datasets() %>% pull(DURATION) %>% max()})
  
  # url for custom BioTIME color basemap tiles
  BioTIMEtile <- 'https://api.mapbox.com/styles/v1/biotime/ckpz9jzdk2u7117lm6fngw29j/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYmlvdGltZSIsImEiOiJja3B6OWhzaDkxbnloMndwOXhtYnM3cGtoIn0.MMtE79hfFUUDQxXOQHmqxg'
  
  # draw the map
  output$StudyMap <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom=1.3, worldCopyJump=F)) %>%
      setView(lng=0, lat=0, zoom=1.25) %>% 
      # addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addTiles(urlTemplate=BioTIMEtile) %>% 
      addPolygons(data=large.studies(), # add the large extent studies by hex polygons we generated
                  fillOpacity=0.5, fillColor=~pal(DURATION), weight=1.2, color='#155f4933',
                  highlightOptions = highlightOptions(fillOpacity=0.9,fillColor='#cf7941', bringToFront = F),
                  popup = ~paste0("<h5>", TITLE,"</h5>",
                                  '<h6>', CONTACT_1, ifelse(CONTACT_2 != '', ', ', ''), CONTACT_2,' et al. </h6>',
                                  "<strong>Duration: </strong>",START_YEAR," to ",END_YEAR,"<br/>",
                                  "<strong>Taxa: </strong>",TAXA, "<br/>",
                                  "<strong>Biome: </strong>", BIOME_MAP)) %>% 
      addCircleMarkers(data=studies(), ~CENT_LONG, ~CENT_LAT, radius=~DURATION/15+6, 
                       opacity=1, fillOpacity=1, fillColor=~pal(DURATION), weight=2, color='#155f49',
                       clusterOptions = markerClusterOptions(clickable=T, riseOnHover=T, freezeAtZoom = 5,
                                                             # specify custom cluster thresholds with a javascript function
                                                             iconCreateFunction=JS("function (cluster) {    
                                        var childCount = cluster.getChildCount(); 
                                        var c = ' marker-cluster-';  
                                        if (childCount > 30) {  
                                          c += 'large';  
                                        } else if (childCount > 10) {  
                                          c += 'medium';
                                        } else { 
                                          c += 'small';  
                                        }    
                                        return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
                                    
                                      }")),
                       popup = ~paste0("<h5>", TITLE,"</h5>",
                                       '<h6>', CONTACT_1, ifelse(CONTACT_2 != '', ', ', ''), CONTACT_2,' et al. </h6>',
                                       "<strong>Duration: </strong>",START_YEAR," to ",END_YEAR,"<br/>",
                                       "<strong>Taxa: </strong>",TAXA, "<br/>",
                                       "<strong>Biome: </strong>", BIOME_MAP)) %>%
      addLegend(data=bind_rows(studies(), large.studies() %>% as_tibble()), position='bottomright', title='Duration', pal=pal, opacity=1,
                values=~DURATION)
  })
}  

# Run the application 
shinyApp(ui = ui, server = server)
