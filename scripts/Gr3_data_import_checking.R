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
#' ------------------------------------------------------------------#


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

#' osf_retrieve_node("gs8u6") %>%
#'  osf_ls_files() %>%
  #dataset folders of interest
#'  filter(name %in% c("traits", "community")) %>%
#'  osf_download(
    #where to download
#'    path = here(path = "data/raw"),
    #allows overwriting of folders
#'    conflicts = "overwrite"
#'  )

### 1) Data cleaning ----

### >> a) Traits data ----

# traits data - complete
traits_raw <- read.csv(file.path("data", "raw", "traits", "PFTC5_Peru_2020_LeafTraits_clean.csv"),
                       header = T,
                       sep = ",") %>%
  #add Puna Project (2019 data)
  rbind(read.csv(file.path("data", "raw", "traits", "PunaProject_Peru_2019_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",") %>%
          #Keep only QUE and ACJ C sites
          filter(site == "QUE" |
                   site == "ACJ" & treatment == "C")) %>%
  
  ##CORRECTIONS FOR WRONG NAMING OF TREATMENTS
  mutate( 
    #Replace incorrect treatments for samples
    treatment = case_when(
      #Correct B sample for ACJ
      site == "ACJ" & year == 2020 &
        treatment == "B" ~ "BB",
      #Correct C sample for QUE
      site == "QUE" & year == 2020 &
        treatment == "C" ~ "BB",
      #Correct B sample for QUE
      site == "QUE" & year == 2020 &
        treatment == "B" ~ "BB",
      #Correct B sample for TRE
      site == "TRE" & year == 2020 &
        treatment == "B" ~ "BB",
      TRUE ~ treatment))

skim(traits_raw)

### >> b) Community data ----

#' I did corrections (cleaning) on the fly here to make sure names and columns match
#' when importing the QUE data for 2020
#' 2020 is then combined with the osf datasets after cleaning
#' this is then followed by some final filtering


species_files <- paste(file.path("data", "raw", "community"), 
                       dir(file.path("data", "raw", "community"), 
                           pattern = ""), sep = "/")

species_raw <- read.csv(file.path("data", "raw", "community", "PFTC3_Peru_2018_CommunityCover_clean.csv"),
                        header = T,
                        sep = ",") %>%
  #add Puna Project (2019 data)
  rbind(read.csv(file.path("data", "raw", "community", "PunaProject_Peru_2019_CommunityCover_clean.csv"),
                 header = T,
                 sep = ",")) %>%
  mutate(taxon = case_when(taxon == "Lachemilla cf vulcanica" ~ "Lachemilla cf. vulcanica",
                           str_detect(taxon,
                                      "alstonii") == TRUE ~ "Jamesonia alstonii",
                           TRUE ~ taxon),
         species = case_when(species == "cf vulcanica" ~ "cf. vulcanica",
                            str_detect(taxon,
                                       "alstonii") == TRUE ~ "alstonii",
                            TRUE ~ species),
         genus = case_when(str_detect(genus,
                                      "alstonii") == TRUE ~ "Jamesonia",
                           TRUE ~ genus))


#' This is where the 2020 Community data lives
#' It is downloaded and saved as a .csv in the data/raw/community folder
#' but for record purposes keeping the 'legacy' link as wee
#' 
#write.csv(gsheet2tbl("https://drive.google.com/file/d/1bfVdxXOCxcejbRDzEUEovI4OlHoX3BjA/view?usp=sharing") , 
#          file = here(path = "data/raw/community/PFTC5_2020_CommunityCover_raw.csv"), 
#          row.names = FALSE)


species_2020 <- read.csv(here(path = "data/raw/community/PFTC5_2020_CommunityCover_raw.csv"),
                         header = T,
                         sep = ",") %>%
  
  ## edit characters in and around _cf_
  mutate(taxon = str_replace_all(taxon, "_", " ")) %>% 
  mutate(taxon = str_replace(taxon, "cf", "cf.")) %>% 
  
  ##taxanomic corrections
  mutate(taxon = case_when(
    #misspelling
    taxon == "Viola pymaea" ~ "Viola pygmaea",
    #misspelling
    taxon == "Agrostis trichoides" ~ "Agrostis trichodes",
    #misspelling - CHECK
    taxon == "Lysipomia glanulifera" ~ "Lysipomia glandulifera",
    #misspelling - CHECK
    taxon == "Chusquea intipacarina" ~ "Chusquea intipaqariy",
    #misspelling - CHECK
    taxon == "Carex bomplandii" ~ "Carex bonplandii",
    #punctuation error
    taxon == "Hieracium cf.. mandonii" ~ "Hieracium cf. mandonii",
    #punctuation error
    taxon == "Diplostephium cf.. haenkei" ~ "Diplostephium cf. haenkei",
    #species change - was checked with Lucely
    taxon == "Calamagrostis cf.. macrophylla" ~ "Calamagrostis cf. amoena",
    #species change - was checked with Lucely
    taxon == "Geranium filipes" ~ "Geranium sessiliflorum",
    #species change - was checked with Lucely
    taxon == "Jamesonia alstonii" ~ "Jamesonia blepharum",
    #species change - was checked with Lucely
    taxon == "Bartsia inaequalis" ~ "Bartsia trichophylla",
    #species change - was checked with Lucely
    taxon == "Calamagrostis sp8" ~ "Anatherostipa hans-meyeri",
    #species change - was checked with Lucely
    taxon == "Calamagrostis sp7" ~ "Anatherostipa hans-meyeri",
    #species change - was checked with Lucely
    taxon == "Oreobolus sp1" ~ "Oreobolus goeppingeri",
    #species change - was checked with Lucely
    taxon == "Festuca 3 sharp" ~ "Festuca sp4",
    #species change - was checked with Lucely
    taxon == "Agrostis sp1" ~ "Agrostis trichodes",
    #species change - was checked with Lucely
    taxon == "Calamagrostis tricophylla" ~ "Calamagrostis cf. amoena",
    TRUE ~ taxon)) %>% 
  
  ##TIDY COLUMNS TO MATCH osf COLUMNS
  # split taxon into genus and species
  separate(taxon, 
           into = c("genus", "species"), 
           sep = "\\s", 2, 
           remove = FALSE,
           # keeps cf. and species taxon together
           extra = "merge") %>%
  #create treatment and plot_id
  separate(plot, 
           into = c("treatment", "plot_id"), 
           #sperates before last character
           sep = -1, 2, 
           remove = TRUE) %>%
  #add month
  mutate(month = rep("March",
                     nrow(.)),
         #add project
         project = rep("PFTC5",
                       nrow(.))) %>%
  #change '+' in cover to 0.5 and make numeric
  mutate(cover = as.numeric(case_when(cover == "+" ~ "0.5",
                                      TRUE ~ cover))) %>%
  # drop absent species (no cover value)
  filter(!cover == "") %>% 
  #add other cols from osf dataset - taxon, functional group and family
  left_join(.,
            species_raw  %>%
              dplyr::select(taxon, functional_group, family),
            by = 'taxon') %>%
  
  #REMOVE ANY DUPLICATES
  distinct() %>%
  
  #ADDING FAMILY AND FUNCTIONAL GROUP FOR Festuca cf. andina.
  mutate(
    functional_group = case_when(
      taxon == "Festuca cf. andina" ~ "Gramminoid",
      taxon == "Bartsia trichophylla" ~ "Forb",
      taxon == "seedling unknown" ~ NA_character_,
      TRUE ~ functional_group
    ),
    family = case_when(
      taxon == "Festuca cf. andina" ~ "Poaceae",
      taxon == "Bartsia trichophylla" ~ "Orobanchaceae",
      TRUE ~ family
    )) %>%
  
  ##SELECT ONLY COLUMNS THAT ARE IN THE osf DATA
  dplyr::select(year, project, month, site, treatment, plot_id, functional_group, family, genus, species, taxon, cover) %>%
  
  # REMOVE WHERE COVER = 0 i.e. absent
  filter(cover > 0)


### 2) Data filtering ----

### >> a) Traits data ----

# clean data
traits <- traits_raw %>%
  
  ##REMOVE NON- TARGET SITES
  #remove WAY sites - not needed
  filter(site %in% c("ACJ", "QUE", "TRE") |
           #Removing ACJ C 2020 
           site != "ACJ" & year != 2020 & treatment != "C") %>%
  
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
         treatment != "OFF-PLOT") %>%
  
  ##REMOVING DUPLICATES FOR INDIVIDUALS
  #group by each individual at each plot for each site & treatment
  group_by(site, treatment, plot_id, name_2020, individual_nr) %>%
  #arrange in a set way each time to ensure we use the same individuals
  arrange(id) %>%
  #keep only the first record for each individual
  slice_head()  %>%
  #rename full species name to match species dataset i.e. 'taxon'
  rename(taxon = name_2020)

# check again
skim(traits)


### >> b) Community data ----

#Combine species datasets
species <- species_raw %>%
  
  bind_rows(.,
            species_2020) %>%
  #samples from QUE 2019 will be considered C for our purposes
  mutate(treatment = ifelse(site == "QUE" & year == 2019,
                            "C",
                            treatment)) %>%
  #filter out the November samples from 2019
  filter(site == "QUE" & year == 2019 & month == "April" & treatment == "C" | 
           year == 2020 |
           site == "ACJ" & year == 2019 & month == "April" & treatment == "C")

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