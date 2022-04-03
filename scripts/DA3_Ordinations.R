#### Data analysis task 3 - Ordination ####

## Authors: Natalia Quinteros, Jonathan Henn, Ragnhild Gya

#### Call source script ####

source(here::here(path = "scripts/0_data_import.R"))

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
  ggplot(aes(x = MDS1, y = MDS2, color = treatment)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~site)

orditorp(nmds, display = "species", col = "red", air = 0.01)
orditorp(nmds, display = "sites", cex = 1.1, air = 0.01)


#### PCA community analysis ####
# I think we should moe away from this after reading some literature. Seems like the PCA method does not deal with 0 very well, nor is it a good representation of non-linear trends. "PCA is extremely useful when we expect species to be linearly (or even monotonically) related to each other. Unfortunately, we rarely encounter such a situation in nature. It is much more likely that species have a unimodal species response curve" (from: https://ourcodingclub.github.io/tutorials/ordination/)

PCA <- rda(species_wide[-c(1:10)])

plot(PCA)
plot(PCA, display = c("sites", "species"), type = "points")
plot(PCA, display = "species", type = "points")

biplot(PCA, choices = c(1,2), type = c("text", "points"), xlim = c(-5,10))

PCA_env <- bind_cols(species_wide[c(1:10)], as.data.frame(PCA$x)) 

PCA_env %>% 
  ggplot(aes(x = PC1, y = PC2, color = treatment, shape = site)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() 

PCA_env %>% 
  ggplot(aes(x = PC1, y = PC2, color = treatment, shape = site)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~year)

PCA_env %>% 
  ggplot(aes(x = PC1, y = PC2, color = treatment)) +  
  geom_hline(aes(yintercept = 0)) +
  geom_vline(aes(xintercept = 0)) +
  geom_point() +
  stat_ellipse() +
  facet_wrap(~site)

autoplot(PCA, data = species_wide, colour = "site",
         loadings = TRUE, loadings.colour = 'black',
         loadings.label = TRUE, loadings.label.size = 3, variance_percentage = TRUE)

#### PCA traits analysis ####

#Need CWM mean traits from this. Will do later