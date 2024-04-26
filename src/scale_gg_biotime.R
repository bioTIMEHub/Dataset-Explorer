#
# Function to implement BioTIME palettes as scales in ggplot
# Author: Cher Chow
# Updated: 20 November 2020

require(ggplot2)

## BioTIME palettes set up
biotime_palettes <- list(
  'realms4' = c("#155f49","#66c1d1","#d9d956","#d17538"),
  'realms6' = c("#155f49","#1ea19a","#61cde0","#EBEB58","#e9ae27","#d17538"),
  'heat' = c("#155f49","#1ea19a","#61cde0","#f7ffda","#ebeb58","#e9ae27","#d17538"),
  'gradient' = c("#002120","#00483d","#127c8e","#25b0ba","#72e390","#d6f054"),
  'cool' = c("#155f49","#1ea19a","#61cde0"),
  'warm' = c("#d9d956","#e9ae27","#d17538"))

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
