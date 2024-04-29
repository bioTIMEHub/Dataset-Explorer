
# BioTIME App
# Description: Explores BioTIME datasets by mapping the global coverage of data sets, attributing contributors,
# and shows trends from Science 2014 paper.
# Author: Cher Chow

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
# all spatial data needs to be in WGS84 because leaflet is stupid
dt_points <- read_csv('src/dt_points_4326.csv')
dt_hex <- read_csv('src/dt_hex_3857.csv') %>% 
  st_as_sf(., wkt = 'wkt', crs = 3857) %>% st_transform(., 4326) %>% st_wrap_dateline
meta <- bind_rows(dt_points, dt_hex) %>% st_drop_geometry %>% select(!c(wkt, geometry)) %>% distinct %>% arrange(STUDY_ID)
meta %>% filter(is.na(TITLE))

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
               div(class='stat', h1(textOutput('biome')), h4('biomes')),
               div(class="warning", 'We recommend viewing our map on larger devices for the best experience')
      ),
      
      # Dataset map
      fluidRow(id='maprow',
               leafletOutput("StudyMap"), #output map
               
               absolutePanel(id='control', draggable=T, # control panel
                             h2('Filter datasets'),
                             tags$div(class="accordion",
                                      tags$input(id='tog_realm', type='checkbox', class='accordion-toggle', name='toggle'),
                                      tags$label(`for`='tog_realm', 'Realm'),
                                      tags$section(actionButton("selectallR", label="Select/Deselect all", class='selectall'),
                                                   checkboxGroupInput('Realm', label=NULL,
                                                                      choiceNames=sort(unique(meta$REALM)),
                                                                      selected=sort(unique(meta$REALM)),
                                                                      choiceValues=sort(unique(meta$REALM)))
                                                   )),
                             
                             tags$div(class="accordion",
                                      tags$input(id='tog_taxa', type='checkbox', class='accordion-toggle', name='toggle'),
                                      tags$label(`for`='tog_taxa', 'Taxa'),
                                      tags$section(actionButton("selectallT", label="Select/Deselect all", class='selectall'),
                                                   checkboxGroupInput('Taxa', label=NULL,
                                                                      choiceNames=sort(unique(meta$TAXA)),
                                                                      selected=sort(unique(meta$TAXA)),
                                                                      choiceValues=sort(unique(meta$TAXA))))),
                             
                             tags$div(class="accordion",
                                      tags$input(id='tog_climate', type='checkbox', class='accordion-toggle', name='toggle'),
                                      tags$label(`for`='tog_climate', 'Climate'),
                                      tags$section(actionButton("selectallC", label="Select/Deselect all", class='selectall'),
                                                   checkboxGroupInput('Climate', label=NULL,
                                                                      choiceNames=sort(unique(meta$CLIMATE)),
                                                                      selected=sort(unique(meta$CLIMATE)),
                                                                      choiceValues=sort(unique(meta$CLIMATE))))),
                             
                             tags$div(class="accordion",
                                      tags$input(id='tog_dur', type='checkbox', class='accordion-toggle', name='toggle'),
                                      tags$label(`for`='tog_dur', 'Duration (yrs)'),
                                      tags$section(sliderInput('Duration', label=NULL,
                                                               min=2, max=130, value=c(2, 130),
                                                               round = T, ticks = F, animate = FALSE))),
                             actionButton('reset', 'Reset'),
                             
                             # manual marker legend
                             tags$div(class='studies-legend')
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
  
  # select/deselect all buttons
  
  observe({
    if (input$selectallR > 0) {
      if (input$selectallR %% 2 == 0){
        updateCheckboxGroupInput(inputId="Realm",
                                 choices = sort(unique(meta$REALM)),
                                 selected = sort(unique(meta$REALM)))
        }
      else {
        updateCheckboxGroupInput(inputId="Realm",
                                 choices = sort(unique(meta$REALM)),
                                 selected = c())
      }
    }
  })
  
  observe({
    if (input$selectallT > 0) {
      if (input$selectallT %% 2 == 0){
        updateCheckboxGroupInput(inputId="Taxa",
                                 choices = sort(unique(meta$TAXA)),
                                 selected = sort(unique(meta$TAXA)))
      } else {
        updateCheckboxGroupInput(inputId="Taxa",
                                 choices = sort(unique(meta$TAXA)),
                                 selected = c())
      }}
  })
  
  observe({
    if (input$selectallC > 0) {
      if (input$selectallC %% 2 == 0){
        updateCheckboxGroupInput(inputId="Climate",
                                 choices = sort(unique(meta$CLIMATE)),
                                 selected = sort(unique(meta$CLIMATE)))
      } else {
        updateCheckboxGroupInput(inputId="Climate",
                                 choices = sort(unique(meta$CLIMATE)),
                                 selected = c())
      }}
  })
  
  
  # Dataset map -------------------------------------------------------------
  
  # define some static parameters
  # create a palette for the leaflet map to use
  pal <- colorBin(palette=intPalette(biotime_palettes[['gradient']][3:6])(5),
                  domain=2:130,
                  bins=c(2,10,25,50,100,130))
  
  # spatial data with the filters from UI applied
  points <- reactive({
    dt_points %>% filter(Duration >= input$Duration[1] & Duration <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })

  hex <- reactive({
    dt_hex %>% filter(Duration >= input$Duration[1] & Duration <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  # generate a reactive dataset summary statistic counter
  # metadata
  metadata <- reactive({
    meta %>% filter(Duration >= input$Duration[1] & Duration <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  output$studies <- renderText({metadata() %>% pull(STUDY_ID) %>% n_distinct()})
  output$contributors <- renderText({c(metadata() %>% pull(CONTACT_1), metadata() %>% pull(CONTACT_2)) %>% n_distinct()})
  output$taxa <- renderText({metadata() %>% pull(TAXA) %>% n_distinct()})
  output$biome <- renderText({metadata() %>% pull(BIOME_MAP) %>% n_distinct()})
  output$years <- renderText({metadata() %>% pull(Duration) %>% max()})
  
  # draw the map
  output$StudyMap <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 1.5, maxZoom = 7, worldCopyJump = T)) %>%
      
      # randomize the view each load
      setView(lng = runif(n=1, min = -90, max=90), lat = runif(n=1, min = -60, max=60), zoom = 3) %>%
      
      addEasyButton(easyButton( # global view button
        icon="fa-globe", title="Zoom out to global view",
        onClick=JS("function(btn, map){ map.setView(new L.LatLng(0, 0), 2); }"))) %>%

      # manual legend
      addControl(position='bottomright', html = "
                 <ul id='legend'>
                    <li><div id='legend-study' class='symbol'></div> Study </li>
                    <li><div id='legend-hex' class='symbol'></div> Large-extent study</li>
                    <li><div id='legend-cluster' class='symbol'>3</div> Study cluster</li>
                </ul>
                 ") %>%
      addLegend(data=metadata(), position='bottomright', title='Duration', pal=pal, opacity=1,
                values=~Duration) %>% 

      addTiles(urlTemplate = 'https://api.mapbox.com/styles/v1/biotime/ckpz9jzdk2u7117lm6fngw29j/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYmlvdGltZSIsImEiOiJja3B6OWhzaDkxbnloMndwOXhtYnM3cGtoIn0.MMtE79hfFUUDQxXOQHmqxg') %>% 
      
      # large extent studies by hex polygons we generated
      addPolygons(data=hex(),
                  fillOpacity=0.5, fillColor=~pal(Duration), weight=1.4, color='#155f4966',
                  highlightOptions = highlightOptions(fillOpacity=0.9,fillColor='#253A45', bringToFront = F),
                  popup = ~paste0("<h5>", TITLE,"</h5>",
                                  '<h6>', CONTACT_1, ifelse(is.na(CONTACT_2) == F, paste0(', ', CONTACT_2), ''),' et al. </h6>',
                                  "<strong>Taxa: </strong>",TAXA, "<br/>",
                                  "<strong>Biome: </strong>", BIOME_MAP,"<br/>",
                                  "<a class='button' target='_parent' href='http://biotime.st-andrews.ac.uk/selectStudy.php?study=", STUDY_ID, "'>View full database record</a>")) %>%

      # public single location studies
      addCircleMarkers(data=points(), ~LON, ~LAT, radius=8,
                       opacity=1, fillOpacity=1, fillColor=~pal(Duration), weight=2, color='#155f49',
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 4, spiderfyOnMaxZoom = F, # specify custom cluster thresholds with a javascript function
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
                                       '<h6>', CONTACT_1, ifelse(is.na(CONTACT_2) == F, paste0(', ', CONTACT_2), ''),' et al. </h6>',
                                       "<strong>Taxa: </strong>",TAXA, "<br/>",
                                       "<strong>Biome: </strong>", BIOME_MAP, "<br/>",
                                       "<a class='button' target='_parent' href='http://biotime.st-andrews.ac.uk/selectStudy.php?study=", STUDY_ID, "'>View full database record</a>"))
  })
} # end server  

# Run the application 
shinyApp(ui = ui, server = server)
