#' Plotting Aesthetics
#' 

### 0) Preamble ----
### >> a) Dependencies ----
library(tvthemes) #A:tla colour theme
library(here)
if(!require(ggpomological)){  # for colour scheme
  devtools::install_github("gadenbuie/ggpomological")
  library(ggpomological)
}
library(paletteer) 
library(ggplot2)

### 1) Colour scheme ----
# suggestion 1: draw from ggpomological theme: https://github.com/gadenbuie/ggpomological )
scales::show_col(ggpomological:::pomological_palette)

# example 1: dark (C) vs light (BB)
acj_c <- palettes_d$ggpomological$pomological_palette[1]
acj_bb <- palettes_d$ggpomological$pomological_palette[5]
tre_c <- palettes_d$ggpomological$pomological_palette[2]
tre_bb <- palettes_d$ggpomological$pomological_palette[7]
que_c <- palettes_d$ggpomological$pomological_palette[8]
que_bb <- palettes_d$ggpomological$pomological_palette[6]
# example 2: loud (C) vs. muted (BB)
acj_c <- palettes_d$ggpomological$pomological_palette[1]
acj_bb <- palettes_d$ggpomological$pomological_palette[9]
tre_c <- palettes_d$ggpomological$pomological_palette[3]
tre_bb <- palettes_d$ggpomological$pomological_palette[4]
que_c <- palettes_d$ggpomological$pomological_palette[8]
que_bb <- palettes_d$ggpomological$pomological_palette[6]

# suggestion 2: draw from Dresdencolor:::paired theme -> dark (C) vs light (BB)
scales::show_col(palettes_d$DresdenColor$paired)

acj_c <- palettes_d$DresdenColor$paired[11]
acj_bb <- palettes_d$DresdenColor$paired[12]
tre_c <- palettes_d$DresdenColor$paired[7]
tre_bb <- palettes_d$DresdenColor$paired[8]
que_c <- palettes_d$DresdenColor$paired[5]
que_bb <- palettes_d$DresdenColor$paired[6]


# theme_blue <- "#4088C7"
# theme_green <- "#34B362"
# theme_darkblue <- "#1D5799"
# theme_yellow <- "#FABC55"

#suggestion 3: using the Fire Nation palette from {tvthemes}
scales::show_col(avatar_pal(palette = "FireNation")(4))
#then use lighten() to get the paired shades e.g.
scales::show_col(
  colorspace::lighten(avatar_pal(palette = "FireNation")(4),
                      .5, space = "HLS")
)


##Colour palette

#specify the three colours for sites - CHANGE HERE
colours = c("#F2E22C", "#A37D5D", "#C20A08")

colours_site <- tribble(
  ~t, ~c,
  "ACJ", colours[1],
  "TRE", colours[2],
  "QUE", colours[3],
  "ACJ C", colours[1],
  "ACJ BB", colorspace::darken(colours[1], 0.4),
  "TRE C", colours[2],
  "TRE BB", colorspace::darken(colours[2], 0.4),
  "QUE C", colours[3],
  "QUE BB", colorspace::darken(colours[3], 0.4)
)


### 2) For facet labels ----

#To capitalise labels
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

#rename traits - plase this in facet labeller cal and will change automatically
traits_parsed <- c(
  plant_height_cm = "Height", 
  leaf_area_cm2 = "LA~(cm^2)", 
  sla_cm2_g = "SLA~(cm^2%.%g)",
  ldmc = "LDMC", 
  leaf_thickness_ave_mm = "Thickness~(mm)")

### 3) Figure theme ----



# End of script ----

