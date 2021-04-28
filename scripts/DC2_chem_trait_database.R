#' Matching chamical traits form online databases as 
#' placeholder while waiting for chem traits

### Call source script----

source(here::here(path = "scripts/0_data_import.R"))

### Packages ----

library(BIEN)

`%notin%` <- Negate(`%in%`)

### Data Wrangling ----

#get family list & BIEN records

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

spp_matches =
  inner_join(traits %>%
               ungroup() %>%
               distinct(taxon),
             BIEN_traits_family,
             by = c('taxon' = 'scrubbed_species_binomial')) %>%
  select(-c(url_source,project_pi, project_pi_contact, access, id, unit, method)) %>%
  mutate(match_level = rep("species", nrow(.))) %>%
  rename(family = scrubbed_family,
         genus = scrubbed_genus)

genus_matches = 
  inner_join(traits %>%
               ungroup() %>%
               distinct(genus),
             BIEN_traits_family,
             by = c('genus' = 'scrubbed_genus')) %>%
  select(-c(url_source,project_pi, project_pi_contact, access, id, unit, method)) %>%
  mutate(match_level = rep("genus", nrow(.))) %>%
  #removes traits that we have at spp level
  anti_join(.,
            spp_matches,
            by = c("scrubbed_family" = "family", "trait_name")) %>%
  rename(family = scrubbed_family,
         taxon = scrubbed_species_binomial)

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
  rename(taxon = scrubbed_species_binomial,
         genus = scrubbed_genus)


### Adding chem traits to morpho traits ----

traits_wide = 
  traits %>%
  pivot_wider(id_cols = -c(trait,value),
              names_from = trait,
              values_from = value,
              values_fill = NA)

traits_w_chem = 
  rbind(
    # add genus level traits
    right_join(traits_wide,
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
    right_join(traits_wide,
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
    right_join(traits_wide,
               spp_matches %>%
                 group_by(trait_name, taxon) %>%
                 summarise(mean_trait = mean(as.numeric(trait_value))) %>%
                 ungroup() %>%
                 pivot_wider(id_cols = -c(trait_name, mean_trait),
                             names_from = trait_name,
                             values_from = mean_trait,
                             values_fill = NA),
               by = c('taxon'))
  ) %>%
  rename( c_n = `leaf carbon content per leaf nitrogen content`,
          nitrogen = `leaf nitrogen content per leaf dry mass`,
          phosphorus = `leaf phosphorus content per leaf dry mass`,
          carbon = `leaf carbon content per leaf dry mass`) %>%
  pivot_longer(cols = c(dry_mass_g, ldmc, leaf_area_cm2, leaf_thickness_mm, plant_height_cm,
                        sla_cm2_g, wet_mass_g, c_n, nitrogen, phosphorus, carbon),
               names_to = 'trait',
               values_to = 'value') %>%
  filter(!is.na(value))

traits = traits_w_chem

#remove unneeded dfs
rm(traits_wide, traits_w_chem, genus_matches,
   family_matches, spp_matches)