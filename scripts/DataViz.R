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
library(naniar)

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

### >> a) Number of indivdiuals (traits)----
# Number of indivdiuals traits data were collected for
#per individual per plot per site per treatment

NIndivids <-
  ggplot(traits) +
  geom_histogram(aes(x = site,
                     fill = treatment),
                 stat ="count",
                 position = "dodge") +
  facet_wrap(vars(plot_id)) +
  labs(title = "TRAITS not trimmed",
       y = "number of samples") +
  scale_fill_manual(values = c("#7E605E",
                               "#8AB573")) +
  theme_bw()

ggsave(here(path = "output/Number_indivs_per_plot.png"),
       NIndivids,
       height = 8.3, width = 10,
       units = "in", dpi = 600)


### >> b) Number of specied (community)----
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
  summarise(n_taxa = n_distinct(taxon)) %>%
  ggplot(aes(x = site, 
             y = n_taxa, 
             fill = treatment)) +
  geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
  scale_fill_manual(values = c("#7E605E",
                               "#8AB573")) +
  scale_x_discrete(limits = c("ACJ", "TRE", "QUE")) +
  theme_bw() +
  labs(y = "Total no. taxa",
       x = "Site",
       title = "Not Trimmed")

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
  filter(ldmc <= 1) %>%
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
  filter(Value >= 0) 

trait_density <-
density_plots %>%
  filter(Value != Inf) %>%
  ggplot() +
  geom_density_ridges(aes(y = site,
                          x = Value,
                          fill = plot,
                          colour = plot),
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
       trait_density,
       height = 8.3, width = 15,
       units = "in", dpi = 600)



# End of script ----

##Zone of Experimentation

### >> Appendix 1) The case of the missing data----

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
    leaf_thickness_ave_mm, functional_group, treatment 
  ) %>%
  ggplot() +
  stat_density_ridges(aes(y = site,
                          x = ldmc,
                          fill = functional_group,
                          colour = functional_group),
                      alpha = 0.6) +
  facet_wrap(vars(treatment),
             ncol = 1,
             labeller = label_parsed)  +
  theme_bw() +
  labs(y = "Density",
       x = "Trait Value") +
  theme(legend.position = "bottom")

### >> Appendix 2) The case of the missing data----

cowplot::save_plot(
  here(path = "output/missing_data.png"),
  cowplot::plot_grid(
    vis_miss(traits %>%
               filter(year == 2020) %>%
               select(-c(area_flag, dry_flag, wet_flag))) +
      labs(title = "2020 Traits data"), 
    vis_miss(traits %>%
               filter(year != 2020) %>%
               select(-c(area_flag, dry_flag, wet_flag))) +
      labs(title = "Not 2020 Traits data") +
      theme(axis.text.x = element_blank()),
    ncol = 1
  )
)

### >> Appendix 3) The trimmed (traits) menace ----


cowplot::save_plot(
  here(path = "output/trimmed_vs_untrimmed_traits.png"),
  cowplot::plot_grid(
    ggplot(traits) +
      geom_histogram(aes(x = site,
                         fill = treatment),
                     stat ="count",
                     position = "dodge") +
      #facet_wrap(vars(plot_id)) +
      labs(title = "traits NOT trimmed",
           y = "number of samples") +
      scale_fill_manual(values = c("#7E605E",
                                   "#8AB573")) +
      theme_bw() +
      ylim(0, 300),
    ggplot(traits_trimmed) +
      geom_histogram(aes(x = site,
                         fill = treatment),
                     stat ="count",
                     position = "dodge") +
      #facet_wrap(vars(plot_id)) +
      labs(title = "traits trimmed",
           y = "number of samples") +
      scale_fill_manual(values = c("#7E605E",
                                   "#8AB573")) +
      theme_bw() +
      ylim(0, 300),
    ncol = 1
  )
)


