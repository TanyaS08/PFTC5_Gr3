#### Data analysis task 3 - Ordination ####

## Authors: Natalia Quinteros, Jonathan Henn, Ragnhild Gya

#### Call source script ####

source(here::here(path = "scripts/0_data_import.R"))

#### Load libraries ####
library(vegan)

#### Make wide dataset ####

species_wide <- species %>% 
  select(-family, -functional_group, -burn_year) %>% 
  pivot_wider(names_from = taxon, values_from = cover, values_fill = 0)


nmds <- metaMDS(species_wide[-c(1:10)])

nmds_out <- bind_cols(species_wide[c(1:10)], as.data.frame(nmds$points)) 


nmds_out %>% 
  ggplot(aes(x = MDS1, y = MDS2, color = treatment, shape = site)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~year)


nmds_out %>% filter(year == 2019) %>% 
  ggplot(aes(x = MDS1, y = MDS2, color = treatment, shape = month)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~site)
