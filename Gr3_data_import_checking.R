# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2019
#
# Group 3: Trait & taxonomic community response to fire and elevation
# Authors: Dagmar D. Egelkraut, Lucely Vilca Bustamante, Sonya Geange,
#          Korina Ocampo-Zuleta, Jonathan von Oppen, Jess Rickenback, 
#          Tanya Strydom
# Contact: dagmar.egelkraut@uib.no

### 0) Dependencies ----
if(!require(skimr)){        # for quick overview of dataset
  install.packages("skimr")
  library(skimr)
}
library(tidyverse)

### 2) Colour scheme ----
# ( from https://color.adobe.com/SRADDET-Sud-2-color-theme-14318632 )
theme_red <- "#F96654"
theme_blue <- "#4088C7"
theme_green <- "#34B362"
theme_darkblue <- "#1D5799"
theme_yellow <- "#FABC55"

### 1) Import data ----
fire_traits <- read.csv("PFTC5_Peru_2020_LeafTraits_cleaned_20-03-19.csv", 
                        header = T, 
                        sep = ",")
### 2) Inspect data ----
skim(fire_traits)

# LeafArea_cm2 is a factor, make numeric
fire_traits$LeafArea_cm2 <- as.numeric(fire_traits$LeafArea_cm2.x)

# create taxon column from Genus + species
fire_traits <- fire_traits %>% 
  mutate(Taxon = paste(Genus, " ", Species))
fire_traits$Taxon <- as.factor(fire_traits$Taxon)

# Average leaf thickness measurements
fire_traits <- fire_traits %>% 
  mutate(Leaf_Thickness_avg_mm = 
           (Leaf_Thickness_1_mm + Leaf_Thickness_2_mm + Leaf_Thickness_3_mm) / 3)

# QUE contains C samples, have to be BB
fire_traits <- fire_traits %>% 
  mutate_at(vars(Experiment, Site), as.character) %>% 
  mutate(.,
         Experiment = ifelse(Site == "QUE" & Experiment %in% c("C", "B"), "BB", Experiment),
  )

# check again
skim(fire_traits)

### 3) Clean dataset ----
# limit to traits project
fire_traits_compl <- fire_traits %>% 
  filter(Project == "T") %>% 
# exclude cases with NA in Experiment, just for ease
  drop_na(Experiment) %>% 
# also drop WAY samples, as not our focus
  filter(!Site == "WAY")

### 4) Summary graphs ----
# N species per site and treatment
fire_traits_compl %>% group_by(Site, Experiment) %>% 
  summarise(n_taxa = n_distinct(Taxon)) %>% 
  ggplot(aes(x = Site, y = n_taxa, fill = Experiment)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c(theme_red, theme_blue)) +
    scale_x_discrete(limits = c("ACJ", "TRE", "QUE")) +
    theme_bw() +
    labs(y = "total no. taxa")

# End of script ----