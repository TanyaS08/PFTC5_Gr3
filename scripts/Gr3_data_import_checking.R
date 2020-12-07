# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2020
#
# Group 3: Trait & taxonomic community response to fire and elevation
# Authors: Dagmar D. Egelkraut, Lucely Vilca Bustamante, Sonya Geange,
#          Korina Ocampo-Zuleta, Jonathan von Oppen, Jess Rickenback,
#          Tanya Strydom
# Contact: dagmar.egelkraut@uib.no


#' ------------------------------------------------------------------#
#' TO DO:
#' - Add species cover data for ACJ Control from 2019. Check???
#' - check ACJ&TRE recording year with Lucely, 2019?!? -> 2b i, l.
#' - functional group column check genera -> 2b iii, l.143
#' - + cover values as 0.5? -> 2b iii, l. 141
#' - Need to add control traits data from QUE from 2019
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
if(!require(skimr)){        # for quick overview of dataset
  install.packages("skimr")
  library(skimr)
}
library(tidyverse)
library(tidylog)
if(!require(stringr)){        # for string operations
  install.packages("stringr")
  library(stringr)
}
if(!require(ggpomological)){  # for colour scheme
  devtools::install_github("gadenbuie/ggpomological")
  library(ggpomological)
}
if(!require(ggpomological)){  # for colour scheme
  install.packages("ggridges")
  library(ggridges)
}
library(paletteer)            # for "palettes_d" function to display colour schemes
library(ggdist)
library(tvthemes) #A:tla colour theme
library(osfr) #for getting files off of osf
library(here) #uses wd as starting point for paths
library(ggridges) #for ridge plots

### >> b) Colour scheme ----
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
                      .3, space = "HLS")
)

### >> c) Functions ----
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

### >> d) Data from osf ----

osf_retrieve_node("gs8u6") %>%
  osf_ls_files() %>%
  filter(name %in% c("traits", "community")) %>%
  osf_download(
    path = here(path = "data/"),
    conflicts = "overwrite"
  )

### 1) Data import ----
# traits data - complete
traits_raw <- read.csv(file.path("data", "traits", "PFTC5_Peru_2020_LeafTraits_clean.csv"),
                       header = T,
                       sep = ",",
                       stringsAsFactors = TRUE)

#TODO
# community data
species_files <- paste(file.path("data", "community"), 
                       dir(file.path("data", "community"), 
                           pattern = ""), sep = "/")
species_raw <- map_df(species_files, read_csv)

### 2) Data cleaning ----

### >> a) Traits data ----
skim(traits_raw)

# clean data
traits <- traits_raw %>%
  #remove WAY sites - not needed
  filter(site != "WAY") %>%
  #convert plot ID & indiv no. to factor
  mutate_at(vars(plot_id, individual_nr), 
            as.factor)

# check again
skim(traits)


### >> b) Community data ----
species <- species_raw %>%
  #select only target site = TRE, QUE, ACJ
  filter(site %in% c("QUE", "ACJ") &
           treatment == "C" &
           year == 2019)
  
### 3) Summary graphs ----
# N species sampled for traits per site and treatment
traits %>% group_by(Site, Treatment) %>%
  summarise(n_taxa = n_distinct(Taxon)) %>%
  ggplot(aes(x = Site, y = n_taxa, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c(theme_red, theme_blue)) +
  scale_x_discrete(limits = c("ACJ", "TRE", "QUE")) +
  theme_bw() +
  labs(y = "total no. taxa")

#plant height density plot
traits %>% #group_by(Site) %>%
  ggplot(aes(Plant_Height_cm, fill = Site)) +
  geom_density(alpha = .9, kernel = "gaussian") +
  scale_fill_manual(values = c(acj_c,
                               tre_c,
                               que_c)) +
  theme_bw() +
  labs(y = "density")

#plant height density plot - split by treatment
##Need to set a colour scheme
traits %>%
  #combine both Site and treatment to one variable
  unite(Plot,
        c(Site, Treatment),
        sep = " ", remove = FALSE) %>%
  mutate(Plot = factor(Plot, levels = c("ACJ C",
                                        "ACJ BB",
                                        "TRE C",
                                        "TRE BB",
                                        #"QUE C",
                                        "QUE BB"))) %>%
  ggplot() +
  stat_density_ridges(aes(y = Site,
                          x = Plant_Height_cm,
                          fill = Plot,
                          color = Plot),
                      alpha = 0.7) +
  scale_fill_paletteer_d("DresdenColor::paired") +
  scale_colour_paletteer_d("DresdenColor::paired") +
  # scale_fill_manual(values = c(acj_c,
  #                              acj_bb,
  #                              tre_c,
  #                              tre_bb,
  #                              #que_c,
  #                              que_bb)) +
  #scale_fill_paletteer_d("nationalparkcolors::Everglades") +
  theme_bw() +
  labs(y = "density")


#plotting all traits using facets
##Need to set a colour scheme
#density_plots <-
traits %>%
  #combine both Site and treatment to one variable
  unite(
    #new variable name
    plot,
    #cols to combine
    c(site, treatment),
    sep = " ", remove = FALSE
  ) %>%
  select(
    plot, site, plant_height_cm, leaf_area_cm2, sla_cm2_g, ldmc,
    leaf_thickness_ave_mm
  ) %>%
  #pivot into long format for splitting into facets
  pivot_longer(cols = -c(plot, site),
               values_to = "Value",
               names_to = "Trait") %>%
  mutate(plot = factor(plot, levels = c("ACJ C",
                                        "ACJ BB",
                                        "TRE C",
                                        "TRE B",
                                        #"QUE C",
                                        "QUE BB")),
         # Rename traits for labels
         Trait = case_when(Trait == "plant_height_cm" ~ "Plant height (cm)",
                           Trait == "leaf_area_cm2" ~ "Wet mass (g)",
                           Trait == "sla_cm2_g" ~ "SLA (cm2/g)",
                           Trait == "ldmc" ~ "LDMC",
                           Trait == "leaf_thickness_ave_mm" ~ "Leaf Thickness (mm)")) %>%
  ggplot() +
  stat_density_ridges(aes(y = site,
                          x = Value,
                          fill = plot,
                          color = plot),
                      alpha = 0.5) +
  facet_wrap(vars(Trait),
             scales = "free") +
  scale_fill_paletteer_d("DresdenColor::paired") +
  scale_colour_paletteer_d("DresdenColor::paired") +
  theme_bw() +
  labs(y = "density",
       x = "trait value")

ggsave("/Users/tanyastrydom/Documents/Uni/PFTC/PFTC5/PFTC5_Gr3/output/DensityPlots.png",
       density_plots,
       height = 8.3, width = 15,
       units = "in", dpi = 600)

# End of script ----
