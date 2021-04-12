# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2020
#
# Group 3: Trait & taxonomic community response to fire and elevation


### 0) Preamble ----
### >> a) Dependencies ----
if(!require(skimr)){        # for quick overview of dataset
  install.packages("skimr")
  library(skimr)
}
library(tidyverse)
library(tidylog)
if(!require(stringr)){        # for string operations
  install.packages("stringr")
  library(stringr)
}
library(osfr) #for getting files off of osf
library(here) #uses working directory as starting point for paths
library(gsheet)


### >> c) Data from osf ----

 # osf_retrieve_node("gs8u6") %>%
 #  osf_ls_files() %>%
 #  #dataset folders of interest
 #  filter(name %in% c("traits", "community")) %>%
 #  osf_download(
 #    #where to download
 #    path = here(path = "data/raw"),
 #    #allows overwriting of folders
 #    conflicts = "overwrite"
 #  )

### 1) Data cleaning ----

### >> a) Traits data ----

# traits data - complete
traits_raw <- read.csv(file.path("data", "raw", "traits", "PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv"),
                       header = T,
                       sep = ",") %>%
  filter(year == 2020) %>%
  filter(site %in% c("QUE", "TRE")) %>%
  rbind(read.csv(file.path("data", "raw", "traits", "PFTC3-Puna-PFTC5_Peru_2018-2020_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",") %>%
          filter(year == 2019) %>%
          filter(site %in% c("QUE", "ACJ")))

skim(traits_raw)

### >> b) Community data ----

species_raw <- read.csv(file.path("data", "raw", "community", "PunaProject_Peru_2019_CommunityCover_clean.csv"),
                 header = T,
                 sep = ",") %>%
  filter(site %in% c("QUE", "ACJ")) %>%
  rename(species = specie) %>%
  rbind(read.csv(file.path("data", "raw", "community", "PFTC5_Peru_2020_CommunityCover_clean.csv"),
                 header = T,
                 sep = ",") %>%
          filter(site %in% c("QUE", "TRE")))


#' This is where the 2020 Community data lives
#' It is downloaded and saved as a .csv in the data/raw/community folder
#' but for record purposes keeping the 'legacy' link
#' 
#write.csv(gsheet2tbl("https://drive.google.com/file/d/1bfVdxXOCxcejbRDzEUEovI4OlHoX3BjA/view?usp=sharing") , 
#          file = here(path = "data/raw/community/PFTC5_2020_CommunityCover_raw.csv"), 
#          row.names = FALSE)

### 2) Data filtering ----

### >> a) Traits data ----

#TODO
#Filter out ACJ C from PFTC5

# clean data
traits <- traits_raw %>%
  
  ## BURNT QUE SITES FROM 2019 ARE OUR CONTROL SITES
  #Rename 2019 samples to C for QUE
  mutate( 
    treatment = case_when(site == "QUE" & 
                            year == 2019 ~ "C",
                          TRUE ~ treatment)) %>%
  
  ##REMOVE SAMPLING THAT OCCURED IN A DIFFERENT SEASON
  #remove November samples (multiple sampling from 2019 - Puna Project)
  filter(month != "November",
         #remove Sean's samples
         treatment != "OFF-PLOT",
         treatment != "B") %>%
  
  ##REMOVING DUPLICATES FOR INDIVIDUALS
  #group by each individual at each plot for each site & treatment
  group_by(site, treatment, plot_id, taxon, individual_nr, trait) %>%
  #arrange in a set way each time to ensure we use the same individuals
  arrange(id) %>%
  #keep only the first record for each individual
  slice_head()

# check again
skim(traits)


### >> b) Community data ----

#Combine species datasets
species <- species_raw %>%

  ## BURNT QUE SITES FROM 2019 ARE OUR CONTROL SITES
  #Rename 2019 samples to C for QUE
  mutate( 
    treatment = case_when(site == "QUE" & 
                            year == 2019 ~ "C",
                          treatment == "BB" ~ "NB",
                          TRUE ~ treatment)) %>%
  filter(month != "November",
         #remove Sean's samples
         treatment != "OFF-PLOT",
         treatment != "B")

skim(species)

## 3) Traits Trimming species with low cover ----

#TODO
#This needs to be contemplated further

#' This is to account for the 'incomplete' sampling effort for ACJ BB in 2020
#' Thus removing all traits records for species that had a cover greater than
#' 2.0 %


traits_trimmed =
inner_join(species %>%
             filter(cover >= 2.0) %>%
             select(cover, taxon, site, treatment, plot_id),
           traits) 

# End of script ----