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
#'A lot...
#'
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----

library(ade4)
library(RColorBrewer)
library(MASS)
library(plotrix)
library(ggforce)
library(reshape)
library(gganimate)

### >> b) Call source script----

#source(here(path = "scripts/Gr3_data_import_checking.R"))

### 1) Removing 'uncommon' spp. from traits dataset ----


blob_traits =
  traits_raw %>%
  dplyr::select(site, treatment, name_2020, functional_group, plant_height_cm, leaf_area_cm2, sla_cm2_g, ldmc, leaf_thickness_ave_mm) %>%
  na.omit() %>%
  filter(sla_cm2_g != Inf)


Blob_sp <- blob_traits[,3]
Blob_GF <- blob_traits[,4]
Site = blob_traits[,1]
Treatment = blob_traits[,2]
dat <- blob_traits[,-c(1:4)]
traits <- colnames(blob_traits)

blob_traits_list = 
  read.csv(file.path("data", "raw", "traits", "PFTC5_Peru_2020_LeafTraits_clean.csv"),
           header = T,
           sep = ",") %>%
  rbind(read.csv(file.path("data", "raw", "traits", "PunaProject_Peru_2019_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",")) %>%
  rbind(read.csv(file.path("data", "raw", "traits", "PFTC3_Peru_2018_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",")) %>%
  dplyr::select(site, treatment, name_2020, functional_group, plant_height_cm, leaf_area_cm2, sla_cm2_g, ldmc, leaf_thickness_ave_mm) %>%
  na.omit() %>%
  filter(sla_cm2_g != Inf) %>%
  filter(site %in% c("ACJ", "QUE", "TRE")) %>%
  group_by(site) %>%
  group_split()


xlim <- c(-6, 6)
ylim <- c(-6, 6)
lims <- c(c(-7, 7), c(-7, 7)) 
mult <- 5.5

plots <- vector(mode = "list", length = length(blob_traits_list))
arrows <- vector(mode = "list", length = length(blob_traits_list))
plot_data <- vector(mode = "list", length = length(blob_traits_list))
for (i in 1:length(blob_traits_list)) {
  
  dat = blob_traits_list[[i]][,-c(1:4)]
  Blob_GF = blob_traits_list[[i]][,4]
  Blob_treat = blob_traits_list[[i]][,2]
  
  
  titre <- c(plant_height_cm = "Height", leaf_area_cm2 = "LA~(cm^2)", sla_cm2_g = "SLA~(cm^2%.%g)",
             ldmc = "LDMC", leaf_thickness_ave_mm = "Thickness~(mm)")[colnames(dat)]
  
  PCA <- ade4::dudi.pca(dat, center = T, scale = T, scannf = F, nf = ncol(dat))
  means<- PCA$cent
  sds <- PCA$norm
  eigen <- round(PCA$eig / sum(PCA$eig) * 100, 1)
  
  signx <- ifelse(sign(sum(sign(PCA$c1[, "CS1"]))) < 0, -1 ,1)
  signy <- ifelse(sign(sum(sign(PCA$c1[, "CS2"]))) < 0, -1, 1)
  x = signx * PCA$li[, 'Axis1']
  y = signy * PCA$li[, 'Axis2']
  Axis1 <- signx * PCA$li[, 'Axis1']
  Axis2 <- signy * PCA$li[, 'Axis2']
  
  arrows[[i]] = tibble(
    Trait = titre, 
    xend = signx * PCA$co[, 'Comp1'] * mult,
    yend = signy * PCA$co[, 'Comp2'] * mult,
    x = rep(0, length(titre)),
    y = rep(0, length(titre)),
    Site = rep(blob_traits_list[[i]]$site[1], length(titre))
  )
  
  plot_data[[i]] = tibble(Axis1 = x, 
                          Axis2 = y,
                          FGroup = blob_traits_list[[i]][,4],
                          Treatment = Blob_treat,
                          Site = blob_traits_list[[i]][,1])
}

do.call(rbind.data.frame, arrows)
test = do.call(rbind.data.frame, plot_data)

test[,3] = test$FGroup$functional_group
test[,4] = test$Treatment$treatment
test[,5] = test$Site$site
test$Site <- factor(as.factor(test$Site),levels=c("ACJ","TRE","QUE"))

levels(test$Site)

arrows_plot = do.call(rbind.data.frame, arrows)
arrows_plot$Site = factor(as.factor(arrows_plot$Site),levels=c("ACJ","TRE","QUE"))

gif <-   
  ggplot(test,
         aes(x = Axis1,
             y = Axis2))  +
  stat_density_2d(geom = "polygon",
                  aes(fill = FGroup,
                      alpha = ..nlevel..),
                  contour_var = "ndensity",
                  breaks = c(0.5, 0.9)) +
  scale_alpha(range = c(0.3, 0.7)) +
  guides(alpha = FALSE) +
  geom_point(size = 0.1,
             alpha = 0.6) +
  geom_segment(data = arrows_plot,
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.3,"cm"))) +
  geom_text(data = arrows_plot,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  xlim(xlim) +
  ylim(ylim) +
  # Here comes the gganimate code
  transition_states(
    Site,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(title = 'Site: {closest_state}',
       x = "PC1",
       y = "PC2")

anim_save(here(path = "output/PCA_annimate_site_level.gif"),
          gif,
          height = 797, width = 1440,
          units = "px")

pca_site <- 
ggplot(test,
       aes(x = Axis1,
           y = Axis2))  +
  stat_density_2d(geom = "polygon",
                  aes(fill = FGroup,
                      alpha = ..nlevel..),
                  contour_var = "ndensity",
                  breaks = c(0.5, 0.9)) +
  scale_alpha(range = c(0.3, 0.7)) +
  guides(alpha = FALSE) +
  geom_point(size = 0.1,
             alpha = 0.6) +
  facet_wrap(vars(Site)) +
  geom_segment(data = arrows_plot,
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.3,"cm"))) +
  geom_text(data = arrows_plot,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  xlim(xlim) +
  ylim(ylim) +
  labs(x = "PC1",
       y = "PC2")

ggsave(here(path = "output/PCA_site.png"),
       pca_site,
       height = 8.3, width = 17,
       units = "in", dpi = 600)

### 2) All sites ----

blob_traits = 
  read.csv(file.path("data", "raw", "traits", "PFTC5_Peru_2020_LeafTraits_clean.csv"),
           header = T,
           sep = ",") %>%
  rbind(read.csv(file.path("data", "raw", "traits", "PunaProject_Peru_2019_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",")) %>%
  rbind(read.csv(file.path("data", "raw", "traits", "PFTC3_Peru_2018_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",")) %>%
  dplyr::select(site, treatment, name_2020, functional_group, plant_height_cm, leaf_area_cm2, sla_cm2_g, ldmc, leaf_thickness_ave_mm) %>%
  na.omit() %>%
  filter(sla_cm2_g != Inf) %>%
  filter(site %in% c("ACJ", "QUE", "TRE"))

dat = blob_traits[,-c(1:4)]
Blob_GF = blob_traits[,4]
Blob_treat = blob_traits[,2]


titre <- c(plant_height_cm = "Height", leaf_area_cm2 = "LA~(cm^2)", sla_cm2_g = "SLA~(cm^2%.%g)",
           ldmc = "LDMC", leaf_thickness_ave_mm = "Thickness~(mm)")[colnames(dat)]

PCA <- ade4::dudi.pca(dat, center = T, scale = T, scannf = F, nf = ncol(dat))
means<- PCA$cent
sds <- PCA$norm
eigen <- round(PCA$eig / sum(PCA$eig) * 100, 1)

signx <- ifelse(sign(sum(sign(PCA$c1[, "CS1"]))) < 0, -1 ,1)
signy <- ifelse(sign(sum(sign(PCA$c1[, "CS2"]))) < 0, -1, 1)
x = signx * PCA$li[, 'Axis1']
y = signy * PCA$li[, 'Axis2']
Axis1 <- signx * PCA$li[, 'Axis1']
Axis2 <- signy * PCA$li[, 'Axis2']

arrows_all = tibble(
  Trait = titre, 
  xend = signx * PCA$co[, 'Comp1'] * mult,
  yend = signy * PCA$co[, 'Comp2'] * mult,
  x = rep(0, length(titre)),
  y = rep(0, length(titre)))

plot_data_all = tibble(Axis1 = x, 
                       Axis2 = y,
                       FGroup = blob_traits[,4],
                       Treatment = Blob_treat,
                       Site = blob_traits[,1])

plot_data_all$Site <- factor(as.factor(plot_data_all$Site),levels=c("ACJ","TRE","QUE"))

gif_all <-   
  ggplot(plot_data_all,
         aes(x = Axis1,
             y = Axis2))  +
  stat_density_2d(geom = "polygon",
                  aes(fill = FGroup,
                      alpha = ..nlevel..),
                  contour_var = "ndensity",
                  breaks = c(0.5, 0.9)) +
  scale_alpha(range = c(0.3, 0.7)) +
  guides(alpha = FALSE) +
  geom_point(size = 0.1,
             alpha = 0.6)  +
  geom_segment(data = arrows_all,
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.3,"cm"))) +
  geom_text(data = arrows_all,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  xlim(xlim) +
  ylim(ylim) +
  # Here comes the gganimate code
  transition_states(
    Site,
    transition_length = 2,
    state_length = 1
  ) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out') +
  labs(title = 'Site: {closest_state}',
       x = "PC1",
       y = "PC2")  

anim_save(here(path = "output/PCA_annimate_global.gif"),
          gif_all,
          height = 797, width = 1440,
          units = "px")

pca_all <- 
ggplot(plot_data_all,
       aes(x = Axis1,
           y = Axis2))  +
  stat_density_2d(geom = "polygon",
                  aes(fill = FGroup,
                      alpha = ..nlevel..),
                  contour_var = "ndensity",
                  breaks = c(0.5, 0.9)) +
  scale_alpha(range = c(0.3, 0.7)) +
  guides(alpha = FALSE) +
  geom_point(size = 0.2,
             alpha = 0.6) +
  facet_wrap(vars(Site)) +
  geom_segment(data = arrows_all,
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.3,"cm"))) +
  geom_text(data = arrows_all,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  xlim(xlim) +
  ylim(ylim) +
  labs(x = "PC1",
       y = "PC2")


ggsave(here(path = "output/PCA_global.png"),
       pca_all,
       height = 8.3, width = 17,
       units = "in", dpi = 600)

