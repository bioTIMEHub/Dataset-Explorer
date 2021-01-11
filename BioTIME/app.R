
# BioTIME App
# Description: Explores BioTIME datasets by mapping the global coverage of data sets, attributing contributors,
# and shows trends from Science 2014 paper.
# Author: Cher Chow
# Updated: 11 January 2021

require(shiny)
require(shinyjs)
require(RMariaDB)
require(DBI)
require(tidyverse)
require(plotly)
require(maps)
require(sp)
require(rgdal)
require(leaflet)
require(leaflet.providers)

# Data set up -------------------------------------------------------------

# if running outside of BioTIME server
# SET WORKING DIRECTORY FIRST :)
setwd("~/OneDrive - University of St Andrews/BioTIME/Shiny")
BT_datasets <- read.csv('working_data.csv', header=T)
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
  ),
  useShinyjs(),
  
  # Application title
  div(id='title', p("BioTIME Dataset Map")),
  tags$img(id='logo', src='https://i.postimg.cc/R0VdD7G9/Bio-Time-Logo-Transparent.png'),
  div(id='count',
      column(1, h1(textOutput('studies')), h4('studies'), align='center'),
      column(1, h1(textOutput('years')), h4('years'), align='center'),
      column(1, h1(textOutput('contributors')), h4('contributors'), align='center'),
      column(1, h1(textOutput('taxa')), h4('taxa'), align='center'),
      column(1, h1(textOutput('biome')), h4('biomes'), align='center')
  ),
  
  # Dataset map
  fluidRow(id='maprow',
           leafletOutput("StudyMap", height='85vh'), #output map
           
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
  ),
  
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
  )
)


# Server -----------------------------------------------------------

# BioTIME color functions

## BioTIME palettes set up
biotime_palettes <- list(
  'realms' = c("#155f49","#66c1d1","#d9d956","#cf7941"),
  'gradient' = c("#00483d","#127c8e","#31b9c2","#86db9c","#c0f176","#ffff67"),
  'cool' = c("#155f49","#67b6c4"),
  'warm' = c("#d9d956","#cf7941"))

intPalette <- function (colors, ...) {
  ramp <- colorRamp(colors, ...)
  function(n) {
    if (n > length(colors)) {x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L) 
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)}
    else colors[sort(sample(x=c(1:length(colors)), size=n, replace=F))]
  }
}

biotime_cols <- function(palette = "gradient", reverse = FALSE, ...) {
  pal <- biotime_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  intPalette(pal, ...) # interpolates palette colours
}

# Scale construction for ggplot use
# *USAGE NOTE: the defaults to both color and fill scales are the realm palette and for discrete data.
# Remember to change these arguments when plotting colors continuously. 

scale_color_biotime <- function(palette = "realms", discrete = TRUE, reverse = FALSE, ...) {
  pal <- biotime_cols(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("color", paste("biotime_", palette, sep=''), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

# UK English friendly :)
scale_colour_biotime <- function(palette = "realms", discrete = TRUE, reverse = FALSE, ...) {
  pal <- biotime_cols(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("colour", paste("biotime_", palette, sep=''), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_biotime <- function(palette = "realms", discrete = TRUE, reverse = FALSE, ...) {
  pal <- biotime_cols(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale("fill", paste("biotime_", palette, sep=''), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}



# define all the under the hood computations that have to be done to show the map
server <- function(input, output) {
  
  # reset button
  observeEvent(input$reset, {
    reset('control')
  })
  

# Dataset map -------------------------------------------------------------
  
  studies <- reactive({ # make a working dataframe based on the filter options from the input
    BT_datasets %>% filter(DURATION >= input$Duration[1] & DURATION <= input$Duration[2] & REALM %in% input$Realm & TAXA %in% input$Taxa & CLIMATE %in% input$Climate)
  })
  
  # create a palette for the leaflet map to use
  pal <- colorBin(palette=biotime_cols(palette='gradient')(8),
                  domain=0:max(BT_datasets$NUMBER_OF_SPECIES),
                  bins=c(0,50,100,250,500,1000,2000,5000,10000), pretty=T)
  
  # generate a reactive dataset summary statistic counter
  output$studies <- renderText({length(studies() %>% select(STUDY_ID) %>% as_vector() %>% unique())})
  output$contributors <- renderText({
    length(studies() %>% select(CONTACT_1) %>% as_vector() %>% unique()) + 
      length(studies() %>% select(CONTACT_2) %>% as_vector() %>% unique())
  })
  output$taxa <- renderText({length(studies() %>% select(TAXA) %>% as_vector() %>% unique())})
  output$biome <- renderText({length(studies() %>% select(BIOME_MAP) %>% as_vector() %>% unique())})
  output$years <- renderText({studies() %>% select(DURATION) %>% max()})
  
  # draw the map
  output$StudyMap <- renderLeaflet({
    studies() %>% 
      leaflet(options = leafletOptions(minZoom=1.3, worldCopyJump=F)) %>%
      setView(lng=0, lat=0, zoom=1.25) %>% 
      addProviderTiles(providers$CartoDB.VoyagerNoLabels) %>%
      addCircleMarkers(~CENT_LONG, ~CENT_LAT, radius=~DURATION/12+6,
                       fillOpacity=0.4, fillColor=~pal(NUMBER_OF_SPECIES), weight=1.1, color='#155f49',
                       popup = ~paste0("<h5>", TITLE,"</h5>",
                                       '<h6>', CONTACT_1, ', ', CONTACT_2,'</h6>',
                                       "<strong>Duration: </strong>",START_YEAR," to ",END_YEAR,"<br/>",
                                       "<strong>Taxa: </strong>",TAXA, "<br/>",
                                       "<strong>Biome: </strong>", BIOME_MAP)) %>% 
      addLegend(position='bottomright', title='Number of species', pal=pal, opacity=1,
                values=~NUMBER_OF_SPECIES)
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
