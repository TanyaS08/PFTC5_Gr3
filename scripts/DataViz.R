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
library(here)
if(!require(ggridges)){  # for pretty plots
  install.packages("ggridges")
  library(ggridges)
}           # for "palettes_d" function to display colour schemes
library(ggdist)
library(naniar)

### >> b) Call source script----

source(here::here(path = "scripts/Gr3_data_import_checking.R"))

### 1) Summary graphs ----

### >> a) Number of indivdiuals (traits)----
# Number of indivdiuals traits data were collected for
#per individual per plot per site per treatment

ggplot(traits) +
  geom_histogram(aes(x = site,
                     fill = treatment),
                 stat ="count",
                 position = "dodge") +
  facet_wrap(vars(plot_id)) +
  labs(title = "TRAITS not trimmed",
       y = "number of samples") +
  theme_bw()

ggsave(here(path = "output/Number_indivs_per_plot.png"),
       height = 8.3, width = 10,
       units = "in", dpi = 300)


### >> b) Number of specied (community)----
# Number of species
#species per plot per site per treatment

ggplot(species) +
  geom_histogram(aes(x = site,
                     fill = treatment),
                 stat ="count",
                 position = "dodge") +
  facet_wrap(vars(year)) +
  labs(title = "COMMUNITY",
       y = "number of species") +
  scale_fill_manual(values = c("#7E605E",
                               "#8AB573")) +
  theme_bw()

ggsave(here(path = "output/Number_species_per_plot.png"),
       height = 8.3, width = 10,
       units = "in", dpi = 300)

# N0. species sampled for traits per site and treatment

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
       height = 8.3, width = 10,
       units = "in", dpi = 600)

### 2) Density Distribution of Traits ----

density_plots <-
  traits %>%
  ungroup() %>%
  #combine both Site and treatment to one variable
  unite(
    #new variable name
    plot,
    #cols to combine
    c(site, treatment),
    sep = " ", remove = FALSE
  ) %>%
  #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
  mutate(site = case_when(site == "ACJ" ~ "3 468 m.a.s.l.",
                          site == "TRE" ~ "3 715 m.a.s.l.",
                          site == "QUE" ~ "3 888 m.a.s.l"),
         # Rename traits for labels
         trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                           trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                           trait == "ldmc" ~ "LDMC",
                           trait == "leaf_thickness_mm" ~ "Leaf~Thickness~(mm)"))

density_plots %>%
  filter(value != Inf) %>%
  ggplot() +
  geom_density_ridges(aes(y = site,
                          x = value,
                          fill = plot,
                          colour = plot),
                      alpha = 0.6) +
  facet_wrap(vars(trait),
             scales = "free_x",
             labeller = label_parsed) +
  scale_fill_manual(name = "Plot",
                    values = colours_site$c,
                    breaks = colours_site$t) +
  scale_colour_manual(name = "Plot",
                      values = colours_site$c,
                      breaks = colours_site$t) +
  theme_bw() +
  labs(y = "Density",
       x = "Trait Value") +
  theme(legend.position = "bottom")

ggsave(here(path = "output/traits_density_plots.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 300)



# End of script ----

##Zone of Experimentation

### >> Appendix 1) The case of the missing data----

traits_raw %>%
  filter(is.na(functional_group))

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
  dplyr::select(
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


