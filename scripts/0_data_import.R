# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2020
#
# Group 3: Trait & taxonomic community response to fire and elevation


### 0) Preamble ----
### >> a) Dependencies ----
# if(!require(skimr)){        # for quick overview of dataset
#   install.packages("skimr")
#   library(skimr)
# }
library(tidyverse)
library(tidylog)
if(!require(stringr)){        # for string operations
  install.packages("stringr")
  library(stringr)
}
library(here) #uses working directory as starting point for paths
library(gsheet)
#devtools::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)


### >> b) Data from osf ----

dir.create("data")
dir.create("data/raw")


#Download community data from OSF
get_file(node = "gs8u6",
         file = "PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv",
         path = "data/raw",
         remote_path = "community")


#Download traits data from OSF
get_file(node = "gs8u6",
         file = "PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv",
         path = "data/raw",
         remote_path = "traits")

### 1) Data cleaning ----

### >> a) Traits data ----

# traits data - complete
traits_raw <- read.csv(file.path("data", "raw", "PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv"),
                       header = T,
                       sep = ",") %>%
  filter(site %in% c("ACJ", "QUE", "TRE") &
           year == 2020 |
           site %in% c("ACJ", "QUE", "TRE") &
           year %in% c(2019, 2018) &
           treatment %in% c("C", "NB")|
           site == "QUE" &
           treatment == "B" &
           year == 2019)

### >> b) Community data ----

species_raw <- read.csv(file.path("data", "raw", "PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv"),
                        header = T,
                        sep = ",") %>%
  filter(site %in% c("ACJ", "QUE", "TRE") &
           year == 2020 |
           site %in% c("ACJ", "QUE", "TRE") &
           year %in% c(2019, 2018) &
           treatment %in% c("C", "NB")|
           site == "QUE" &
           treatment == "B" &
           year == 2019)


#' This is where the 2020 Community data lives

### 2) Data filtering ----

### >> a) Traits data ----

# clean data
traits <- traits_raw %>%
  #Rename 2019 samples to C for QUE
  mutate( 
    treatment = case_when(site == "QUE" & 
                            year %in% c(2018, 2019) ~ "C",
                          TRUE ~ treatment)) %>%
  #remove November samples (multiple sampling from 2019 - Puna Project)
  filter(treatment != "B") %>%
  ##REMOVING DUPLICATES FOR INDIVIDUALS
  #group by each individual at each plot for each site & treatment
  group_by(site, treatment, plot_id, taxon, individual_nr, trait) %>%
  #arrange in a set way each time to ensure we use the same individuals
  arrange(id) %>%
  #keep only the first record for each individual
  slice_head()

### >> b) Community data ----

#Combine species datasets
species <- species_raw %>%
  #Rename 2019 samples to C for QUE
  mutate( 
    treatment = case_when(site == "QUE" & 
                            year %in% c(2018, 2019) ~ "C",
                          TRUE ~ treatment)) %>%
  #remove November samples (multiple sampling from 2019 - Puna Project)
  filter(treatment != "B") 

#remove raw files
rm('traits_raw',
   'species_raw')

### 3) supplementary data to add ----

traits = 
  #traits %>%
  read.csv("data/processed/LeafTraits_placeholder_chem.csv") %>%
  select(-X) %>%
  full_join(read.csv("data/processed/LeafTraits_CSR.csv") %>%
              select(-c(X, trait, value))) %>%
  distinct(year, season, month, site, treatment, plot_id, 
           individual_nr, id, functional_group, family, taxon, 
           burn_year, elevation, latitude, longitude, course,
           trait, value)

# End of script ----