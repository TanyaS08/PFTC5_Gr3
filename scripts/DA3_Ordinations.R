#### Data analysis task 3 - Ordination ####

## Authors: Natalia Quinteros, Jonathan Henn, Ragnhild Gya

#### Call source script ####

source(here::here(path = "scripts/0_data_import.R"))
source(here::here(path = "scripts/plotting_aesthetics.R"))

# Load new datasets from imputations
traits <- read.csv("./data/processed/traits_traitstrapped_raw.csv", header = TRUE)

# Community weighted means instead
traits_CWM <- read.csv("./data/processed/traits_traitstrapped_moments.csv", header = TRUE)
traits_CWM <- traits_CWM %>% select(-X) %>% filter(moment == "mean") 

#### Load libraries ####
library(vegan)
library(ggfortify)

#### Make wide dataset ####

species_wide <- species %>% 
  filter(season == "wet_season") %>% 
  select(-family, -functional_group, -burn_year) %>% 
  pivot_wider(names_from = taxon, values_from = cover, values_fill = 0)

#### NMDS analysis ####
nmds <- metaMDS(species_wide[-c(1:10)])

nmds_out <- bind_cols(species_wide[c(1:10)], as.data.frame(nmds$points)) 

nmds_out %>% 
  ggplot(aes(x = MDS1, y = MDS2, color = treatment, shape = site)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() 

nmds_out %>% 
  ggplot(aes(x = MDS1, y = MDS2, color = treatment, shape = site)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~year)

nmds_out %>%
  unite(
    plot, c(site, treatment),
    sep = " ", remove = FALSE
  ) %>% 
  mutate(site = factor(site, levels = c("ACJ", "TRE", "QUE"))) %>% 
  ggplot(aes(x = MDS1, y = MDS2, color = plot,
             fill = plot)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  stat_ellipse(
    alpha = 0.15, geom = "polygon"
  ) +
  geom_point() +
  facet_wrap(~site)  +
  scale_colour_manual(name = "Plot",
                      values = colours_site$c,
                      breaks = colours_site$t) +
  scale_fill_manual(name = "Plot",
                      values = colours_site$c,
                      breaks = colours_site$t) +
  theme_classic() +
  theme(legend.position = "bottom")

ggsave(here(path = "output/nmds_community.png"),
       height = 6, width = 10,
       units = "in", dpi = 300)

orditorp(nmds, display = "species", col = "red", air = 0.01)
orditorp(nmds, display = "sites", cex = 1.1, air = 0.01)

#### PCA traits analysis ####

# On individual level - not CWM

#### Make wide dataset ####

traits_wide <- traits %>% 
  #filter(season == "wet_season") %>% 
  select(-family, -functional_group) %>% 
  group_by(taxon, site, treatment, trait) %>%
  summarise(value = mean(value)) %>%
  pivot_wider(names_from = trait, values_from = value, values_fill = NA) %>%
  na.omit() %>% #filling with NAs so that the true 0 values remain in the dataset after the next filtering step
mutate(plant_height_cm = log(plant_height_cm), #log transform all traits that need log transformation
       sla_cm2_g = log(sla_cm2_g),
       leaf_area_cm2 = log(leaf_area_cm2),
       dry_mass_g = log(dry_mass_g),
       wet_mass_g = log(wet_mass_g))

#### PCA traits analysis ####
PCA_trait <- rda(traits_wide[c(4:10)])

biplot(PCA_trait, choices = c(1,2), type = c("text", "points"))

PCA_trait_env <- bind_cols(traits_wide, as.data.frame(PCA_trait$CA$u)) 
PCA_trait_axis <- as.data.frame(PCA_trait$CA$v)

#Looking for evidence that there is a difference in traits between wet and dry season - there is not strong evidence for that. We can go on and lump everything together in the dataset and analyse both seasons together.
# PCA_trait_env %>% 
#   mutate(site = factor(site, levels = c("ACJ", "TRE", "QUE")))+
#   ggplot(aes(x = PC1, y = PC2, color = season, shape = treatment)) +  
#   geom_hline(aes(yintercept = 0)) +
#   geom_vline(aes(xintercept = 0)) +
#   geom_point() +
#   stat_ellipse() +
#   facet_grid(treatment~site)

PCA_trait_env %>%
  unite(plot, c(site, treatment),
    sep = " ", remove = FALSE) %>% 
  mutate(site = factor(site, levels = c("ACJ", "TRE", "QUE"))) %>% 
  ggplot(aes(x = PC1, y = PC2, color = plot,
             fill = plot)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  stat_ellipse(
    alpha = 0.15, geom = "polygon"
  ) +
  geom_point() +
  facet_grid(~site)  +
  scale_colour_manual(name = "Plot",
                      values = colours_site$c,
                      breaks = colours_site$t) +
  scale_fill_manual(name = "Plot",
                    values = colours_site$c,
                    breaks = colours_site$t) +
  theme_classic() +
  theme(legend.position = "bottom")


ggsave(here(path = "output/nmds_trait.png"),
       height = 6, width = 10,
       units = "in", dpi = 300)