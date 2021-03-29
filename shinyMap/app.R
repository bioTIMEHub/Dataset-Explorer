
# BioTIME App
# Description: Explores BioTIME datasets by mapping the global coverage of data sets, attributing contributors,
# and shows trends from Science 2014 paper.
# Author: Cher Chow
# Updated: 29 Mar 2021

require(utils) # for choose directory
require(shiny)
require(shinyjs)
#require(RMariaDB)
#require(DBI)
require(tidyverse)
require(plotly)
require(leaflet)
require(leaflet.providers)


# Data set up -------------------------------------------------------------

# if running outside of BioTIME server
# SET WORKING DIRECTORY FIRST :) choose the directory where all the source data files are held (should be src)
setwd(choose.dir())
# BioTIME color functions
source('scale_gg_biotime.R')
BT_datasets <- read.csv('working_data.csv', header=T) # table for dataset metadata
study.extents <- readRDS('large_extent_studies.rds')
study.extents <- st_as_sf(study.extents) %>% st_set_crs("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs") %>% 
  st_transform(., 4326)
# study_coords <- read.csv('study_coords.csv', header=T) # table of dataset coordinates (can be > 1)

## if running for real on BioTIME server, uncomment these lines
# Set up BioTIME database connection
# Connection <- dbConnect(RMariaDB::MariaDB(), dbname="biotimedb", username="btteam", password="20Biotime17", host="127.0.0.1")
# get dataset coordinates
# studies <- dbGetQuery(Connection, "SELECT STUDY_ID, CENT_LAT, CENT_LONG, TAXA, TITLE, START_YEAR, END_YEAR, NUMBER_OF_SPECIES from datasets")
# contributors <- dbGetQuery(Connection, "SELECT STUDY_ID, CONTACT_1, CONTACT_2 from contacts")
# BT_datasets <- full_join(studies, contributors, by="STUDY_ID")
# link dataset info with contributors
# BT_datasets <- datasets %>% mutate(DURATION=END_YEAR-START_YEAR)
# create a column for time-series duration
# study_coords <- dbGetQuery(Connection, 'select distinct STUDY_ID, LATITUDE, LONGITUDE from allrawdata')

BT_datasets$DURATION <- BT_datasets$DURATION+1 # year inclusive
BT_datasets$TAXA <- as.factor(BT_datasets$TAXA)
BT_datasets$REALM <- as.factor(BT_datasets$REALM)
BT_datasets$BIOME_MAP <- as.factor(BT_datasets$BIOME_MAP)
BT_datasets$CLIMATE <- as.factor(BT_datasets$CLIMATE)

# get diversity indices data for trends
a_div <- read.csv('alpha_s1.csv', header=T)
# S species richness; varS variance in species richness among samples;
# N summed abundances for all species in pooled samples;
# varN variance among samples in summed abundances for all species;
# SsqrtN, species richness divided by the square root of total summed abundances;
# PIE probability of interspecific encounter; DomMc McNaughton dominance; 
# expShannon exponential of Shannon diversity (also known as the Hill Number 1D;
# Chao, Chao1 asymptotic species richness estimator; Chao2 bias corrected Chao1.

b_div <- read.csv('beta_s2.csv', header=T)
# Jaccard_B is the Jaccard similarity between the year’s pooled samples and the pooled sample of the first year in the time series (the time series baseline);
# Horn_B similar to Jaccard_B but using Morisita-Horn distance;
# Chao_B, as before but using Chao distance;
# Pearson_B as before but using Pearson correlation

a_div <- left_join(a_div, BT_datasets %>% select(ID=STUDY_ID, TAXA, REALM, CLIMATE), by='ID')
b_div <- left_join(b_div, BT_datasets %>% select(ID=STUDY_ID, TAXA, REALM, CLIMATE), by='ID')




# User Interface (Frontend) ---------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    # Style import
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
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
           )
  )),
  
  div(id='trendwrap',
  # Diversity trends
  fluidRow(id="trendrow",
           column(width=3, 
                  h2('Settings'),
                  tags$div(class="accordion",
                           tags$input(id='tog_metrics', type='checkbox', class='accordion-toggle', name='toggle', checked=NA),
                           tags$label(`for`='tog_metrics', 'Choose metrics'),
                           tags$section(selectInput('a_metric', 'Alpha metric', 
                                                    c("Species richness" = 'S',
                                                      "Total abundance" = "N",
                                                      'Hill number' = 'expShannon')),
                                        selectInput('b_metric', 'Beta metric', 
                                                    c("Jaccard dissimilarity" = 'Jaccard_B',
                                                      "Pearson correlation" = "Pearson_B")),
                                        radioButtons('color_by', 'Color trends by', 
                                                    c("Realm" = 'REALM',
                                                      "Taxa" = "TAXA",
                                                      "Climate" = 'CLIMATE'))
                           )
                  )
           ),
           
           column(width=9,
                  h1('Diversity trends'),
                  h3('Alpha diversity'),
                  plotlyOutput('alpha_div', width="90%", height='50vh'),
                  h3('Beta diversity'),
                  plotlyOutput('beta_div', width='90%', height='50vh')
           )
  ))
)


# Server -----------------------------------------------------------

# define all the under the hood computations that have to be done to show the map
server <- function(input, output) {
  
  # reset button
  observeEvent(input$reset, {
    reset('control')
  })
  

# Dataset map -------------------------------------------------------------
  
  sing.studies <- readRDS('single_cell_studies.rds') # load vector listing studies that are too small to plot extents
  BT_datasets <- BT_datasets %>% filter(STUDY_ID %in% sing.studies)
  studies <- reactive({ # make a working dataframe based on the filter options from the input
    BT_datasets %>% filter(DURATION >= input$Duration[1] & DURATION <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  large.studies <- reactive({
    study.extents %>% filter(DURATION >= input$Duration[1] & DURATION <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  # create a palette for the leaflet map to use
  pal <- colorBin(palette=biotime_cols(palette='gradient')(5),
                  domain=2:130,
                  bins=c(2,10,25,50,100,130), pretty=T)
  
  # generate a reactive dataset summary statistic counter
  output$studies <- renderText({studies() %>% distinct(STUDY_ID, .keep_all = T) %>%
      bind_rows(., large.studies() %>% as_tibble() %>% distinct(STUDY_ID, .keep_all = T)) %>% distinct(STUDY_ID) %>% nrow()})
  output$contributors <- renderText({
    bind_rows(studies(), large.studies() %>% as_tibble()) %>% distinct(CONTACT_1) %>% nrow() +
    bind_rows(studies(), large.studies() %>% as_tibble()) %>% distinct(CONTACT_2) %>% nrow()
  })
  output$taxa <- renderText({bind_rows(studies(), large.studies() %>% as_tibble()) %>% distinct(TAXA) %>% nrow() })
  output$biome <- renderText({bind_rows(studies(), large.studies() %>% as_tibble()) %>% distinct(BIOME_MAP) %>% nrow()})
  output$years <- renderText({bind_rows(studies(), large.studies() %>% as_tibble()) %>% select(DURATION) %>% max()})
  
  # draw the map
  output$StudyMap <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom=1.3, worldCopyJump=F)) %>%
      setView(lng=0, lat=0, zoom=1.25) %>% 
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(data=large.studies(), 
                  fillOpacity=0.5, fillColor=~pal(DURATION), weight=0.5, color='#155f4933',
                  highlightOptions = highlightOptions(fillOpacity=0.9,fillColor='#cf7941', bringToFront = F),
                  popup = ~paste0("<h5>", TITLE,"</h5>",
                                  '<h6>', CONTACT_1, ', ', CONTACT_2,'</h6>',
                                  "<strong>Duration: </strong>",START_YEAR," to ",END_YEAR,"<br/>",
                                  "<strong>Taxa: </strong>",TAXA, "<br/>",
                                  "<strong>Biome: </strong>", BIOME_MAP)) %>% 
      addCircleMarkers(data=studies(), ~CENT_LONG, ~CENT_LAT, radius=~DURATION/15+6, 
                       opacity=1, fillOpacity=1, fillColor=~pal(DURATION), weight=2, color='#155f49',
                       clusterOptions = markerClusterOptions(clickable=T, riseOnHover=T, freezeAtZoom = 4,
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
                                       '<h6>', CONTACT_1, ', ', CONTACT_2,'</h6>',
                                       "<strong>Duration: </strong>",START_YEAR," to ",END_YEAR,"<br/>",
                                       "<strong>Taxa: </strong>",TAXA, "<br/>",
                                       "<strong>Biome: </strong>", BIOME_MAP)) %>%
      addLegend(data=bind_rows(studies(), large.studies() %>% as_tibble()), position='bottomright', title='Duration', pal=pal, opacity=1,
                values=~DURATION)
  })
  

  # Diversity trends --------------------------------------------------------
  
  # make working dataframes based on the filter options from the input
  # alpha diversity data
  a_div_data <- reactive({ 
    a_div %>% filter(REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  # beta diversity data
  b_div_data <- reactive({
    b_div %>% filter(REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  output$alpha_div <- renderPlotly({
    # alpha diversity gg object
    alpha_div <- ggplot(a_div_data(), aes_string(x='Year', y=input$a_metric, group='ID')) +
      geom_line(aes_string(color=input$color_by), alpha=0.7) +
      labs(x='Year', y=names(input$a_metric)) +
      scale_color_biotime(palette='realms', discrete=T, name=names(input$color_by)) +
      theme_bw(base_size=14, base_family = 'Public Sans') + 
      theme(panel.grid=element_blank(), axis.ticks=element_line(size=0.3))
    ggplotly(alpha_div)
  })
  
  output$beta_div <- renderPlotly({
    beta_div <- ggplot(b_div_data(), aes_string(x='Year', y=input$b_metric, group='ID')) +
      geom_line(aes_string(color=input$color_by), alpha=0.7) +
      labs(x='Year', y=names(input$b_metric)) +
      scale_color_biotime(palette='realms', discrete=T, name=names(input$color_by)) +
      theme_bw(base_size=14, base_family = 'Public Sans') + theme(panel.grid=element_blank(), axis.ticks=element_line(size=0.3))
    ggplotly(beta_div)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
