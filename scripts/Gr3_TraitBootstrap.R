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
#'    Can also be found in PFTC5_Gr Repo at: 
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
#'  - TBD: number of reps and sample size for
#'    bootstrapping
#'  - Rank sites from high to low as opposed to alphabetical
#'    for plotting
#' ------------------------------------------------------------------#

### 0) Preamble ----
### >> a) Dependencies ----
#install.packages("devtools")
if(!require(traitstrap)){ # for bootstrapping trait data 
  devtools::install_github("richardjtelford/traitstrap")
  library(traitstrap)
}
library(ggplot2) #for plotting

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
  #gather into long format
  gather(key = "Trait",
         value = "Value", 
         -c(Taxon,
            Site,
            Treatment,
            PlotID)) %>%
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
  #gather into long format
  gather(key = "Attribute",
         value = "Cover", 
         -c(Taxon,
            Site,
            Treatment,
            PlotID)) %>%
  # remove 'Attribute' column to keep df sleek
  select(.,
         -Attribute) %>%
  
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
    keep_all = TRUE
  ) %>%
  
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

## This code can be modified and tweaked for the other
# moments if desired

# End of script ----