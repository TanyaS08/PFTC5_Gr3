# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2020
#
# Group 3: Trait & taxonomic community response to fire and elevation
# Authors: Dagmar D. Egelkraut, Lucely Vilca Bustamante, Sonya Geange,
#          Korina Ocampo-Zuleta, Jonathan von Oppen, Jess Rickenback, 
#          Tanya Strydom
# Contact: dagmar.egelkraut@uib.no

### 0) Preamble ----
### >> a) Dependencies ----
if(!require(skimr)){        # for quick overview of dataset
  install.packages("skimr")
  library(skimr)
}
library(tidyverse)

### >> b) Colour scheme ----
# ( from https://color.adobe.com/SRADDET-Sud-2-color-theme-14318632 )
theme_red <- "#F96654"
theme_blue <- "#4088C7"
theme_green <- "#34B362"
theme_darkblue <- "#1D5799"
theme_yellow <- "#FABC55"

### 1) Data import ----
# traits data - complete
traits_total <- read.csv(file.path("data", "data_raw", "PFTC5_Peru_2020_LeafTraits_cleaned_20-03-21.csv"), 
                         header = T, 
                         sep = ",")
# community data - part 1/2 (ACJ & TRE)
species_fire_acj_tre <- read.csv(file.path("data", "data_raw", "communitydata_TRE_ACJ_March2020.csv"),
                                 header = T,
                                 sep = ",")
# community data - part 2/2 (QUE)
species_fire_que <- read.csv(file.path("data", "data_raw", "communitydata_QUE_March2020.csv"),
                             header = T,
                             sep = ",")

### 2) Data cleaning ----
### >> a) Traits data ----
skim(traits_total)

# LeafArea_cm2 is a factor, make numeric
traits_total$LeafArea_cm2 <- as.numeric(traits_total$LeafArea_cm2)

# create taxon column from Genus + species
traits_total <- traits_total %>% 
  mutate(Taxon = paste(Genus, " ", Species))
traits_total$Taxon <- as.factor(traits_total$Taxon)

# Average leaf thickness measurements
traits_total <- traits_total %>% 
  mutate(Leaf_Thickness_avg_mm = 
           (Leaf_Thickness_1_mm + Leaf_Thickness_2_mm + Leaf_Thickness_3_mm) / 3)

# QUE contains C samples, have to be BB -> recode 
traits_total <- traits_total %>% 
  mutate_at(vars(Experiment, Site), as.character) %>% 
  mutate(.,
         Experiment = ifelse(Site == "QUE" & Experiment %in% c("C", "B"), "BB", Experiment)
         )

# check again
skim(traits_total)

# clean dataset:
traits_total_compl <- traits_total %>% 
# keep only samples from traits (T) project
  filter(Project == "T") %>% 
# exclude cases with NA in Experiment, just for ease
  drop_na(Experiment) %>% 
# also drop WAY samples, as not our focus
  filter(!Site == "WAY")

### >> b) Community data ----
# # compile graminoid taxa vector
# graminoid_taxa <- species_fire_acj_tre %>% 
#   select(name) %>% 
#   pull() %>% 
#   unique() %>% 
#   str_subset("Carex") # how to include more than one??

# basic cleaning (rename to lower case, drop empty records)
species_fire_acj_tre <- species_fire_acj_tre %>% 
  # rename columns to lower case
  rename_all(tolower) %>% 
  # drop all-NA "real_species_name" & "habit" columns
  select_if(function(x){!all(is.na(x))}) %>% 
  # rename "name" to "taxon"
  rename(taxon = name) %>% 
  # drop absent species
  filter(!cover == "") %>% 
  # remove dots after "cf"
  mutate_at(vars(taxon), ~str_remove(., "\\.")) %>% 
  # extract cfs into new column...
  mutate_at(vars(taxon), list(cf = ~str_extract(., "cf "))) %>% 
  # ...remove extra space from cf column...
  mutate_at(vars(cf), ~str_remove(., " ")) %>% 
  # ...and delete "cf " from taxon column
  mutate_at(vars(taxon), ~str_remove(., "cf ")) %>% 
  # split taxon into genus and species
  separate(taxon, into = c("genus", "species"), 
           sep = " ", 2, 
           remove = FALSE) # %>% 
  # create functional group column based on genus name
  ## TBC

species_fire_que <- species_fire_que %>% 
  # rename columns to lower case
  rename_all(tolower) %>% 
  # drop all-NA "real_species_name" & "habit" columns
  select_if(function(x){!all(is.na(x))}) %>% 
  # rename "name" to "taxon"
  rename(taxon = name) %>% 
  # drop absent species
  filter(!cover == "") %>% 
  # remove dots after "cf"
  mutate_at(vars(taxon), ~str_remove(., "\\.")) %>% 
  # extract cfs into new column...
  mutate_at(vars(taxon), list(cf = ~str_extract(., "cf "))) %>% 
  # ...remove extra space from cf column...
  mutate_at(vars(cf), ~str_remove(., " ")) %>% 
  # ...and delete "cf " from taxon column
  mutate_at(vars(taxon), ~str_remove(., "cf ")) %>% 
  # split taxon into genus and species
  separate(taxon, into = c("genus", "species"), 
           sep = " ", 2, 
           remove = FALSE) # %>% 
# create functional group column based on genus name
## TBC

mutate(., functional_group = ifelse(name %in% graminoid_taxa, 
                                      "graminoid", 
                                      "forb"))
# join both datasets


### 4) Summary graphs ----
# N species sampled for traits per site and treatment
traits_total_compl %>% group_by(Site, Experiment) %>% 
  summarise(n_taxa = n_distinct(Taxon)) %>% 
  ggplot(aes(x = Site, y = n_taxa, fill = Experiment)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    scale_fill_manual(values = c(theme_red, theme_blue)) +
    scale_x_discrete(limits = c("ACJ", "TRE", "QUE")) +
    theme_bw() +
    labs(y = "total no. taxa")

traits_total_compl %>% #group_by(Site) %>% 
  ggplot(aes(Plant_Height_cm, fill = Site)) +
  geom_density(alpha = .5, kernel = "gaussian") +
  scale_fill_manual(values = c(theme_darkblue, theme_green, theme_yellow)) +
  theme_bw() +
  labs(y = "density")


# End of script ----