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
  filter(site %in% c("QUE", "TRE") &
           year == 2020 |
           site == "ACJ" &
           year == 2019 |
           site == "QUE" &
           treatment == "B" &
           year == 2019)

#skim(traits_raw)

### >> b) Community data ----

species_raw <- read.csv(file.path("data", "raw", "PFTC3-Puna-PFTC5_Peru_2018-2020_CommunityCover_clean.csv"),
                        header = T,
                        sep = ",") %>%
  filter(site %in% c("QUE", "TRE") &
           year == 2020 |
           site == "ACJ" &
           year == 2019 |
           site == "QUE" &
           treatment == "B" &
           year == 2019)


#' This is where the 2020 Community data lives
#' but for record purposes keeping the 'legacy' link
#' 
#write.csv(gsheet2tbl("https://drive.google.com/file/d/1bfVdxXOCxcejbRDzEUEovI4OlHoX3BjA/view?usp=sharing") , 
#          file = here(path = "data/raw/community/PFTC5_2020_CommunityCover_raw.csv"), 
#          row.names = FALSE)

### 2) Data filtering ----

### >> a) Traits data ----

# clean data
traits <- traits_raw %>%
  #Rename 2019 samples to C for QUE
  mutate( 
    treatment = case_when(site == "QUE" & 
                            year == 2019 ~ "C",
                          TRUE ~ treatment)) %>%
  #remove November samples (multiple sampling from 2019 - Puna Project)
  filter(month != "November",
         treatment != "B") %>%
  ##REMOVING DUPLICATES FOR INDIVIDUALS
  #group by each individual at each plot for each site & treatment
  group_by(site, treatment, plot_id, taxon, individual_nr, trait) %>%
  #arrange in a set way each time to ensure we use the same individuals
  arrange(id) %>%
  #keep only the first record for each individual
  slice_head()

# check again
#skim(traits)


### >> b) Community data ----

#Combine species datasets
species <- species_raw %>%
  #Rename 2019 samples to C for QUE
  mutate( 
    treatment = case_when(site == "QUE" & 
                            year == 2019 ~ "C",
                          TRUE ~ treatment)) %>%
  filter(month != "November",
         treatment != "B")

#skim(species)

#remove raw files
rm('traits_raw',
   'species_raw')

# End of script ----