#' Matching chamical traits form online databases as 
#' placeholder while waiting for chem traits

### Call source script----

source(here::here(path = "scripts/0_data_import.R"))

### Packages ----

library(BIEN)

`%notin%` <- Negate(`%in%`)

### Data Wrangling ----

traits = 
  traits %>%
  ungroup() %>%
  separate(taxon,
           into = c("genus", "species"),
           sep = "\\s", 2,
           remove = FALSE,
           # keeps cf. and species taxon together
           extra = "merge")

#get family list & BIEN records

#List of families if df - BIEN puls from famil downwards so we get spp, genus and fam
fam_list = 
  traits %>%
  ungroup() %>%
  distinct(family) %>%
  pull(family)

BIEN_traits_family = BIEN_trait_traitbyfamily(fam_list,
                                              c("leaf nitrogen content per leaf dry mass",
                                                "leaf phosphorus content per leaf dry mass", 
                                                "leaf carbon content per leaf nitrogen content",
                                                "leaf carbon content per leaf dry mass"))

anti_join(traits %>%
            ungroup() %>%
            distinct(family),
          BIEN_traits_family,
          by = c('family' = 'scrubbed_family'))

# "Eriocaulaceae" "Euphobiaceae" <- no matches in BIEN

#pulls species we have matches for
spp_matches =
  inner_join(traits %>%
               ungroup() %>%
               distinct(taxon),
             BIEN_traits_family,
             by = c('taxon' = 'scrubbed_species_binomial')) %>%
  select(-c(url_source,project_pi, project_pi_contact, access, id, unit, method)) %>%
  mutate(match_level = rep("species", nrow(.))) %>%
  #rename to make joining easier later
  rename(family = scrubbed_family,
         genus = scrubbed_genus)

#pulls genera we have matches for
genus_matches = 
  inner_join(traits %>%
               distinct(genus),
             BIEN_traits_family,
             by = c('genus' = 'scrubbed_genus')) %>%
  select(-c(url_source,project_pi, project_pi_contact, access, id, unit, method)) %>%
  mutate(match_level = rep("genus", nrow(.))) %>%
  #removes traits that we have at spp level
  anti_join(.,
            spp_matches,
            by = c("scrubbed_family" = "family", "trait_name")) %>%
  #rename to make joining easier later
  rename(family = scrubbed_family,
         taxon = scrubbed_species_binomial)

#pulls families we have matches for
family_matches = 
  inner_join(traits %>%
               ungroup() %>%
               distinct(family),
             BIEN_traits_family,
             by = c('family' = 'scrubbed_family')) %>%
  select(-c(url_source,project_pi, project_pi_contact, access, id, unit, method)) %>%
  mutate(match_level = rep("family", nrow(.))) %>%
  #removes traits that we have at spp & genus level
  anti_join(.,
            genus_matches,
            by = c("family", "trait_name")) %>%
  #rename to make joining easier later
  rename(taxon = scrubbed_species_binomial,
         genus = scrubbed_genus)


### Adding BIEN traits ----

traits_BIEN = 
  rbind(
    # add genus level traits
    left_join(traits %>%
                 ungroup() %>%
                 select(-c(trait, value)),
               genus_matches %>%
                 group_by(trait_name, genus) %>%
                 summarise(mean_trait = mean(as.numeric(trait_value))) %>%
                 ungroup()  %>%
                 pivot_wider(id_cols = -c(trait_name, mean_trait),
                             names_from = trait_name,
                             values_from = mean_trait,
                             values_fill = NA),
               by = 'genus'),
    # add genus level traits
    left_join(traits %>%
                 ungroup() %>%
                 select(-c(trait, value)),
               family_matches %>%
                 group_by(trait_name, family) %>%
                 summarise(mean_trait = mean(as.numeric(trait_value))) %>%
                 ungroup() %>%
                 pivot_wider(id_cols = -c(trait_name, mean_trait),
                             names_from = trait_name,
                             values_from = mean_trait,
                             values_fill = NA),
               by = 'family'),
    #add spp level traits
    left_join(traits %>%
                 ungroup() %>%
                 select(-c(trait, value)),
               spp_matches %>%
                 group_by(trait_name, taxon) %>%
                 summarise(mean_trait = mean(as.numeric(trait_value))) %>%
                 ungroup() %>%
                 pivot_wider(id_cols = -c(trait_name, mean_trait),
                             names_from = trait_name,
                             values_from = mean_trait,
                             values_fill = NA) %>%
                 mutate(`leaf carbon content per leaf nitrogen content` = NA),
               by = c('taxon'))
  ) %>%
  rename(c_n = `leaf carbon content per leaf nitrogen content`,
         nitrogen = `leaf nitrogen content per leaf dry mass`,
         phosphorus = `leaf phosphorus content per leaf dry mass`,
         carbon = `leaf carbon content per leaf dry mass`) %>%
  pivot_longer(cols = c(c_n, nitrogen, phosphorus, carbon),
               names_to = 'trait',
               values_to = 'value')

### Missing traits - list to send to TRY ----

# dir.create("data/processed")
# 
# missing_traits = 
#   traits_w_chem %>%
#   filter(is.na(value) & 
#            trait %in% c("carbon", "c_n", "nitrogen", "phosphorus")) %>%
#   ungroup() %>%
#   distinct(taxon, trait) %>%
#   mutate(trait = case_when(trait == "c_n" ~ "146",
#                            trait == "nitrogen" ~ "50",
#                            trait == "phosphorus" ~ "15",
#                            trait == "carbon" ~ "13")) %>%
#   rbind(traits_w_chem %>%
#           ungroup() %>%
#           distinct(taxon) %>%
#           mutate(trait = "151")) %>% #C:P
#   rbind(traits_w_chem %>%
#           ungroup() %>%
#           distinct(taxon) %>%
#           mutate(trait = "78")) %>% #N isotope
#   rbind(traits_w_chem %>%
#           ungroup() %>%
#           distinct(taxon) %>%
#           mutate(trait = "89")) #C isotope
# 
# write.csv(missing_traits,
#           "data/TRY/TRY_spp_list_with_id.csv")
# 
# missing_traits %>%
#   distinct(trait)
# 
# ### TRY get IDs ----
# 
# TRY_spp = read.delim("data/TRY/TRY_Species.txt")
# 
# left_join(missing_traits %>%
#             #rename to match with TRY col name
#             rename(AccSpeciesName = taxon),
#           TRY_spp) %>%
#   rename(TraitID = trait) %>%
#   filter(!is.na(AccSpeciesID)) %>%
#   distinct(AccSpeciesID) %>%
#   pull()

### Adding TRY data ----

TRY_data = 
  read.delim("data/TRY/TRY_Data.txt") %>%
  filter(!is.na(TraitID)) %>%
  select(SpeciesName, TraitName, StdValue) %>%
  rename(taxon = SpeciesName,
         trait = TraitName,
         value = StdValue) %>%
  mutate(trait = case_when(trait == "Leaf nitrogen (N) content per leaf area" ~ 
                             "nitrogen",
                           trait == "Leaf phosphorus (P) content per leaf dry mass" ~
                             "phosphorus",
                           trait == "Leaf carbon (C) content per leaf dry mass" ~
                             "carbon",
                           trait == "Leaf carbon/nitrogen (C/N) ratio" ~ 
                             "c_n",
                           trait == "Leaf carbon (C) isotope signature (delta 13C)" ~
                             "delta_13C"),
         taxon = case_when(taxon == "Lycopodium clavatum  L." ~
                             "Lycopodium clavatum",
                           taxon == "Lycopodium clavatum L." ~
                             "Lycopodium clavatum",
                           TRUE ~ taxon)) %>%
  separate(taxon,
           into = c("genus", "species"),
           sep = "\\s", 2,
           remove = FALSE,
           # keeps cf. and species taxon together
           extra = "merge")

# add to trait df


traits_TRY = 
  rbind(left_join(traits %>%
                     ungroup() %>%
                     select(-c(trait, value)),
                   TRY_data %>%
                     group_by(genus, trait)  %>%
                     summarise(mean_trait = mean(as.numeric(value))) %>%
                     ungroup() %>%
                     pivot_wider(id_cols = -c(trait, mean_trait),
                                 names_from = trait,
                                 values_from = mean_trait,
                                 values_fill = NA),
                   by = 'genus'),
        left_join(traits %>%
                     ungroup() %>%
                     select(-c(trait, value)),
                   TRY_data %>%
                     group_by(taxon, trait)  %>%
                     summarise(mean_trait = mean(as.numeric(value))) %>%
                     ungroup() %>%
                     pivot_wider(id_cols = -c(trait, mean_trait),
                                 names_from = trait,
                                 values_from = mean_trait,
                                 values_fill = NA),
                   by = 'taxon')) %>%
  # add C:P and denta_N15 as new variables with dummy values
  mutate(c_p = rnorm(n(),
                     13,
                     1),
         delta_15N = rnorm(n(),
                     -26,
                     1)) %>%
  pivot_longer(cols = c(c_n, nitrogen, phosphorus, carbon, delta_13C, c_p, delta_15N),
               names_to = 'trait',
               values_to = 'value')



### Renames and env. clean ----

traits = traits %>%
  rbind(.,
        traits_TRY,
        traits_BIEN)  %>%
  filter(!is.na(year)) %>%
  #'remove' any potential duplicates by averaging
  group_by(year, season, month, site, treatment, plot_id, individual_nr,
           id, functional_group, family, taxon, genus, species, burn_year, elevation,
           latitude, longitude, course, trait) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>%
  group_by(trait, site) %>%
  # repalce NAs with a sample from rnorm for traot mean
  mutate(value = coalesce(value,
                           rnorm(n(), 
                                 mean(value, na.rm = T),
                                 1)))

#remove unneeded dfs
rm(traits_BIEN, traits_TRY, genus_matches,
   family_matches, spp_matches, BIEN_traits_family,
   TRY_data, fam_list)

# save dataframe to data/processed

dir.create("data/processed")
write.csv(traits,
          "data/processed/LeafTraits_placeholder_chem.csv")

### Bonus plot ----

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
  filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "wet_mass_g")) %>%
  #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
  mutate(site = case_when(site == "ACJ" ~ "3 468 m.a.s.l.",
                          site == "TRE" ~ "3 715 m.a.s.l.",
                          site == "QUE" ~ "3 888 m.a.s.l"),
         # Rename traits for labels
         trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                           trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                           trait == "ldmc" ~ "LDMC",
                           trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)",
                           trait == "phosphorus" ~ "Phosphorus~content~(mg/g)",
                           trait == "nitrogen" ~ "Nitrogen~content~(mg/g)",
                           trait == "carbon" ~ "Carbon~content~(mg/g)",
                           trait == "c_n" ~ "Carbon~per~leaf~nitrogen",
                           trait == "delta_13C" ~ "delta~C[13]",
                           trait == "c_p" ~ "Carbon~per~leaf~phosphorous",
                           trait == "delta_15N" ~ "delta~N[15]"))

library(ggridges)

density_plots %>%
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
  labs(y = "Density",
       x = "Trait Value") +
  theme_classic() +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = colorspace::darken("#7E605E", 0.4)),
        plot.background = element_rect(fill = colorspace::darken("#7E605E", 0.4)),
        legend.background = element_rect(fill = colorspace::darken("#7E605E", 0.4)),
        text = element_text(colour = "grey96"),
        axis.text = element_text(colour = "grey96"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "grey96", size = rel(0.1)),
        strip.background = element_rect(fill = colorspace::darken("#7E605E", 0.6),
                                        colour = NA),
        strip.text = element_text(colour = "grey96"))

ggsave(here(path = "output/traits_density_plots_w_chem.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 300)
