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
#' Extract % var explained by axis for PCA
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
library(tidyverse)

### >> b) Call source script----

#source(here(path = "scripts/Gr3_data_import_checking.R"))

### 1) Removing 'uncommon' spp. from traits dataset ----


blob_traits =
  traits %>%
  ungroup() %>%
  dplyr::select(site, treatment, taxon, functional_group, plant_height_cm, leaf_area_cm2, sla_cm2_g, ldmc, leaf_thickness_ave_mm) %>%
  na.omit() %>%
  filter(sla_cm2_g != Inf)


#this list is if we want to do traitspace at site level/burn level??
blob_traits_list = 
  traits %>%
  ungroup %>%
  dplyr::select(site, treatment, taxon, functional_group, plant_height_cm, leaf_area_cm2, sla_cm2_g, ldmc, leaf_thickness_ave_mm) %>%
  na.omit() %>%
  filter(sla_cm2_g != Inf) %>%
  filter(site %in% c("ACJ", "QUE", "TRE")) %>%
  group_by(site) %>%
  group_split()


xlim <- c(-6, 6)
ylim <- c(-6, 6)
lims <- c(c(-7, 7), c(-7, 7)) 
mult <- 5.5

### 2) "Global" traitspace ----

dat = blob_traits[,-c(1:4)]
Blob_GF = blob_traits[,4]
Blob_treat = blob_traits[,2]
Blob_site = blob_traits[,1]


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
                       FGroup = Blob_GF$functional_group,
                       Treatment = Blob_treat$treatment,
                       Site = Blob_site$site)

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

#pca_all <- 
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
  facet_grid(cols = vars(Site),
             rows = vars(Treatment)) +
  geom_segment(data = arrows_all,
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.2,"cm")),
               size = 0.2) +
  geom_text(data = arrows_all,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE, 
            size = 2) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  xlim(-5,5) +
  ylim(-5,5) +
  labs(x = "PC1",
       y = "PC2")


ggsave(here(path = "output/PCA_global.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 300)


# End of script ----


##There are some loops etc. here that might be useful...

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
               arrow = arrow(length=unit(0.2,"cm")),
               size = 0.2) +
  geom_text(data = arrows_plot,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE,
            size = 2) +
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
  facet_grid(cols = vars(Site),
             rows = vars(Treatment)) +
  geom_segment(data = arrows_plot,
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.2,"cm")),
               size = 0.2) +
  geom_text(data = arrows_plot,
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE,
            size = 2) +
  theme_classic() +
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 20)) +
  xlim(-5,5) +
  ylim(-5,5) +
  labs(x = "PC1",
       y = "PC2")


ggsave(here(path = "output/PCA_site.png"),
       height = 8.3, width = 15,
       units = "in", dpi = 300)

ggplot(test %>%
         filter(Site == "ACJ" &
                  Treatment == "BB"),
       aes(x = Axis1,
           y = Axis2))  +
  with_shade(
    stat_density_2d(geom = "polygon",
                    aes(#fill = FGroup,
                      alpha = ..nlevel..),
                    contour_var = "ndensity",
                    fill = "#65B4E6"
    ),
    height_map = ch_alpha('height_map'),
    azimuth = 150,
    height = 5,
    sigma = 0,
    ignore_background = FALSE
  ) +
  stat_density_2d(geom = "polygon",
                  aes(colour = FGroup),
                  contour_var = "ndensity",
                  fill = NA,
                  breaks = c(0.9),
                  size = 1
  ) +
  scale_alpha(range = c(0.3, 0.9)) +
  guides(alpha = FALSE,
         fill = FALSE) +
  ggblur::geom_point_blur(
    aes(colour = FGroup),
    size = 0.5,
    alpha = 0.6) +
  #facet_wrap(vars(Site)) +
  geom_segment(data = arrows_plot %>%
                 filter(Site == "ACJ" &
                          Treatment == "BB"),
               aes(xend = xend, 
                   yend = yend,
                   x = x,
                   y = y),
               arrow = arrow(length=unit(0.3,"cm")),
               color = "grey65") +
  geom_text(data = arrows_plot %>%
              filter(Site == "ACJ" &
                       Treatment == "BB"),
            aes(label = Trait,
                x = xend + 0.3,
                y = yend + 0.2),
            hjust = 0.1,
            parse = TRUE,
            color = "grey65") +
  theme_classic() +
  theme(axis.text = element_text(size = 9, color = "grey65"),
        axis.title = element_text(color = "grey65"),
        plot.background = element_rect(fill = "#141438"),
        panel.background = element_rect(fill = "#141438"),
        legend.background = element_rect(fill = "#141438"),
        legend.position = 'bottom',
        legend.key = element_blank(),
        legend.text = element_text(color = "grey65"),
        legend.title = element_text(color = "grey65"),
  ) +
  xlim(-4,4) +
  ylim(-4,4) +
  labs(x = "PC1",
       y = "PC2")

ggplot(volcano_long, aes(y, x)) +
  as_reference(
    geom_raster(aes(alpha = z), fill = 'black', interpolate = TRUE, show.legend = FALSE),
    id = 'height_map'
  ) +
  with_shade(
    geom_contour_filled(aes(z = z, fill = after_stat(level))),
    height_map = ch_alpha('height_map'),
    azimuth = 150,
    height = 5,
    sigma = 10
  ) +
  coord_fixed() +
  guides(fill = guide_coloursteps(barheight = 10))