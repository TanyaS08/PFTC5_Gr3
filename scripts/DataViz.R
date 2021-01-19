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
#'
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tvthemes) #A:tla colour theme
library(here)
if(!require(ggpomological)){  # for colour scheme
  devtools::install_github("gadenbuie/ggpomological")
  library(ggpomological)
}
if(!require(ggridges)){  # for pretty plots
  install.packages("ggridges")
  library(ggridges)
}
library(paletteer)            # for "palettes_d" function to display colour schemes
library(ggdist)

### >> b) Call source script----

source(here(path = "scripts/Gr3_data_import_checking.R"))

### >> c) Colour scheme ----
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


### 1) Summary graphs ----

# Number of indivdiuals traits data were collected for
  #per individual per plot per site per treatment

NIndivids <-
ggplot(traits) +
  geom_histogram(aes(x = site,
                     fill = treatment),
                 stat ="count",
                 position = "dodge") +
  facet_wrap(vars(plot_id)) +
  labs(title = "TRAITS",
       y = "number of samples") +
  scale_fill_manual(values = c("#7E605E",
                               "#8AB573")) +
  theme_bw()

ggsave(here(path = "output/Number_indivs_per_plot.png"),
       NIndivids,
       height = 8.3, width = 10,
       units = "in", dpi = 600)

# Number of species
  #species per plot per site per treatment

NSpecies <-
ggplot(species) +
  geom_histogram(aes(x = site,
                     fill = treatment),
                 stat ="count",
                 position = "dodge") +
  facet_wrap(vars(plot_id)) +
  labs(title = "COMMUNITY",
       y = "number of species") +
  scale_fill_manual(values = c("#7E605E",
                               "#8AB573")) +
  theme_bw()

ggsave(here(path = "output/Number_species_per_plot.png"),
       NSpecies,
       height = 8.3, width = 10,
       units = "in", dpi = 600)

# N0. species sampled for traits per site and treatment

NTaxa <-
  traits %>% 
  group_by(site, treatment) %>%
  summarise(n_taxa = n_distinct(name_2020)) %>%
  ggplot(aes(x = site, 
             y = n_taxa, 
             fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("#7E605E",
                               "#8AB573")) +
  scale_x_discrete(limits = c("ACJ", "TRE", "QUE")) +
  theme_bw() +
  labs(y = "Total no. taxa",
       x = "Site")

ggsave(here(path = "output/number_of_taxa.png"),
       NTaxa,
       height = 8.3, width = 10,
       units = "in", dpi = 600)

### 2) Density Distribution of Traits ----

density_plots <-
  traits %>%
  ungroup() %>%
  filter(
    #remove what are potentially erroneous LDMC vals
    ldmc <= 1
  ) %>%
  #combine both Site and treatment to one variable
  unite(
    #new variable name
    plot,
    #cols to combine
    c(site, treatment),
    sep = " ", remove = FALSE
  ) %>%
  select(
    plot, site, plant_height_cm, sla_cm2_g, ldmc,
    leaf_thickness_ave_mm
  ) %>%
  #pivot into long format for splitting into facets
  pivot_longer(cols = -c(plot, site),
               values_to = "Value",
               names_to = "Trait") %>%
  #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
  mutate(site = case_when(site == "ACJ" ~ "3 468 m.a.s.l.",
                          site == "TRE" ~ "3 715 m.a.s.l.",
                          site == "QUE" ~ "3 888 m.a.s.l"),
         # Rename traits for labels
         Trait = case_when(Trait == "plant_height_cm" ~ "Plant~height~(cm)",
                           Trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                           Trait == "ldmc" ~ "LDMC",
                           Trait == "leaf_thickness_ave_mm" ~ "Leaf~Thickness~(mm)")) %>%
  ggplot() +
  stat_density_ridges(aes(y = site,
                          x = Value,
                          fill = plot,
                          color = plot),
                      alpha = 0.6) +
  facet_wrap(vars(Trait),
             scales = "free_x",
             labeller = label_parsed) +
  scale_fill_manual(name = "Plot",
                    values = c("#ecb100", 
                               "#FFDD76", 
                               "#a10000",
                               "#FF5151",
                               "#FF4500", 
                               "#FFA280")) +
  scale_colour_manual(name = "Plot",
                      values = c("#ecb100", 
                                 "#FFDD76", 
                                 "#a10000",
                                 "#FF5151",
                                 "#FF4500", 
                                 "#FFA280")) +
  theme_bw() +
  labs(y = "Density",
       x = "Trait Value") +
  theme(legend.position = "bottom")

ggsave(here(path = "output/traits_density_plots.png"),
       density_plots,
       height = 8.3, width = 15,
       units = "in", dpi = 600)


# End of script ----
