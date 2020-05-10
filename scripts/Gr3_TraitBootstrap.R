# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2020
#
# Group 3: Trait & taxonomic community response to fire and elevation
# Authors: Dagmar D. Egelkraut, Lucely Vilca Bustamante, Sonya Geange,
#          Korina Ocampo-Zuleta, Jonathan von Oppen, Jess Rickenback, 
#          Tanya Strydom
# Contact: dagmar.egelkraut@uib.no


#' ------------------------------------------------------------------#
#'  DATA IMPORTING AND DATAFRAMES
#'  - Import and clean data using Gr3_data_import_checking.R
#'    Can also be found in PFTC5_Gr3 Repo at: 
#'    (https://github.com/TanyaS08/PFTC5_Gr3/blob/master/scripts/Gr3_data_import_checking.R)
#'  - 'species' df = community cover data
#'  - 'traits' df = traits data
#' ------------------------------------------------------------------#

#' ------------------------------------------------------------------#
#'   TO DO:
#'  - Need to update trait variables (when available) that are selected 
#'    when converting to long format -> ca. l. 50
#'  - TBD: hierarchy when imputing/bootstrapping
#'    for now using: Site > Treatment > PlotID
#'        - changing this requires modifying select and pivot_longer when 
#'          making the long dfs
#'  - TBD: number of reps and sample size for bootstrapping
#'  - Rank sites from high to low as opposed to alphabetical
#'    for plotting
#'  - if we do decide to plot outputs maybe set better colour scheme
#'    manually
#'  - need to update colour pallete of plots when all sites are in dataset
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
#install.packages("devtools")
if(!require(traitstrap)){ # for bootstrapping trait data 
  devtools::install_github("richardjtelford/traitstrap")
  library(traitstrap)
}
library(FactoMineR)
library(factoextra)

### 1) Bootstrap trait values using CWM ----

### >> a) traits df into long format ---- 
traits_long <-
  traits %>%
  #select relevant variables
  ##STILL NEED TO ADD ALL TRAIT VARIABLES
  select(.,
         Site,
         Treatment,
         Taxon,
         Plant_Height_cm,
         LeafArea_cm2,
         Leaf_Thickness_avg_mm,
         PlotID) %>%
  #pivot into long format
  
  pivot_longer(.,
  #columns not to be pivoted but are 'retained' i.e. not the traits
               cols = -c(Taxon,
                         Site,
                         Treatment,
                         PlotID),
  #traits column
               names_to  = "Trait",
  #traits value column
               values_to = "Value") %>%
  
  #remove NA values to avoid sampling
  na.omit()

### >> b) species df into long format ----
species_long <-
  species %>%
  #select relevant variables
  select(.,
         Site,
         Cover,
         Treatment,
         Taxon,
         PlotID) %>%
  
  #pivot into long format
  pivot_longer(.,
  #columns not to be pivoted but are 'retained' i.e. not the traits
               cols = -c(Taxon,
                         Site,
                         Treatment,
                         PlotID),
  #traits column
               names_to = "Attribute",
  #cover column
               values_to = "Cover") %>%
  
  # remove 'Attribute' column to keep df sleek
  select(.,
         -Attribute) %>%
  
  #reclassify vars
  mutate_at(.,
            vars(Taxon),
            as.character)
  

### >> c) Bootstrapping calculations ----
## We need to decide what out 'levels' or heirarchy are.
##For now using Site > Treatment > PlotID

trait_bootstrap <-
  
  #This imputes the  trait data 
  trait_impute(
    species_long,
    traits_long,
    scale_hierarchy = c("Site",
                        "Treatment",
                        "PlotID"),
    taxon_col = "Taxon",
    trait_col = "Trait",
    value_col = "Value",
    abundance_col = "Cover",
    keep_all = TRUE) %>%
  
  #This bootstraps the imputed data using CWM
  trait_np_bootstrap(.,
                     nrep = 1000, #TBD
                     sample_size = 200) #TBD


#Summary of different moments (\mu, lower CI and upper CI)
trait_bootstrap_summary <-
  SummariseBootMoments(trait_bootstrap)

### 2) Plotting Boot Moments ----

### >> a) Density plots ----
 ##Mean trait value
trait_bootstrap %>%
  #combine both Site and treatment to one variable
  unite(Plot,
        c(Treatment, PlotID),
        sep = " ", remove = FALSE) %>% 
  ggplot(aes(mean, fill = Plot)) +
  facet_wrap(vars(Trait, Site),
             scales = 'free') +
  geom_density(alpha = .5, kernel = "gaussian") +
  scale_fill_brewer(palette = "RdYlGn")  +
  theme_bw() +
  labs(y = "density")

## This code can be modified and tweaked for the other
 # moments if desired

### >> b) Whisker plots ----

##Mean trait value
trait_bootstrap_summary %>%
  #combine both Site and treatment to one variable
  unite(Plot,
        c(Treatment, PlotID),
        sep = " ", remove = FALSE) %>% 
  ggplot() +
  #split into panels by trait type and site
  facet_grid(rows = vars(Site),
             cols = vars(Trait),
             scales = 'free_x',) +
  geom_pointrange(aes(x = Plot,
                      y = Mean,
                      ymin = CIlow.mean,
                      ymax  = CIhigh.mean,
                      colour = Plot)) +
  coord_flip() +
  scale_colour_brewer(palette = "RdYlGn")  +
  theme_bw()

## This code can probably be modified and tweaked for the other
# moments if so desired

### 3) Mulivariate analysis of bootstrapped data ----

### >> a) Manipulate df onto wide format ----

  traits_wide  <-
  trait_bootstrap %>%
  #remove all moments except for mean
  select(-c(variance,
            skewness,
            kurtosis)) %>%
  #change into wide form i.e. each trait becomes a column
  pivot_wider(.,
              names_from = Trait,
              values_from = mean) %>%
  #need to create a unique ID for each entry - make row name
  mutate(row_ID = paste0(n, "_", Site, "_", Treatment, "_", PlotID)) %>%
  #set new column as row name
  column_to_rownames("row_ID")

### >> b) PCA ----

trait.pca <- PCA(
  #remove identification columns
  traits_wide[,-c(1:5)],
  graph = FALSE,
  scale = TRUE)

fviz_pca(trait.pca,
         label = "var",
         #create a grouping var consisting of treatment and site
         habillage = as.factor(paste0(traits_wide$Site,
                                      "_",
                                      traits_wide$Treatment)),
         addEllipses = TRUE,
         #using the selected colour scheme for consistency
         #NEED TO UPDATE COLOURS WHEN ALL SITES ARE INCLUDED
         palette = c(acj_bb,
                     que_bb,
                     tre_bb,
                     tre_c))

# End of script ----
