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
#' - Add species cover data for ACJ Control from 2019. Check???
#' - check ACJ&TRE recording year with Lucely, 2019?!? -> 2b i, l.
#' - functional group column check genera -> 2b iii, l.143
#' - + cover values as 0.5? -> 2b iii, l. 141
#' - Need to add control traits data from QUE from 2019
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
if(!require(ggpomological)){  # for colour scheme
  devtools::install_github("gadenbuie/ggpomological")
  library(ggpomological)
}
if(!require(ggpomological)){  # for colour scheme
  install.packages("ggridges")
  library(ggridges)
}
library(paletteer)            # for "palettes_d" function to display colour schemes
library(ggdist)
library(osfr) #for getting files off of osf
library(here) #uses wd as starting point for paths
library(gsheet)


### >> b) Functions ----
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

### >> c) Data from osf ----

osf_retrieve_node("gs8u6") %>%
  osf_ls_files() %>%
  filter(name %in% c("traits", "community")) %>%
  osf_download(
    path = here(path = "data/raw"),
    conflicts = "overwrite"
  )

### 1) Data import ----
# traits data - complete
traits_raw <- read.csv(file.path("data", "raw", "traits", "PFTC5_Peru_2020_LeafTraits_clean.csv"),
                       header = T,
                       sep = ",") %>%
  #add Puna Project
  rbind(read.csv(file.path("data", "raw", "traits", "PunaProject_Peru_2019_LeafTraits_clean.csv"),
                 header = T,
                 sep = ",") %>%
          filter(site == "QUE"))



#TODO
# community data
species_files <- paste(file.path("data", "raw", "community"), 
                       dir(file.path("data", "raw", "community"), 
                           pattern = ""), sep = "/")

species_raw <- map_df(species_files, read_csv) %>%
  mutate(taxon = case_when(taxon == "Lachemilla cf vulcanica" ~ "Lachemilla cf. vulcanica",
                           taxon == "Jamesonia alstonii" ~ "Jamesonia alstonii",
                           TRUE ~ taxon),
         specie = case_when(specie == "cf vulcanica" ~ "cf. vulcanica",
                            specie == "Jamesonia alstonii" ~ "alstonii",
                            TRUE ~ specie),
         genus = case_when(genus == "Jamesonia alstonii" ~ "alstonii",
                           TRUE ~ genus))

species_2018 <- gsheet2tbl("https://drive.google.com/file/d/1bfVdxXOCxcejbRDzEUEovI4OlHoX3BjA/view?usp=sharing") %>%
  
  # edit character in and around _cf_
  mutate(taxon = str_replace_all(taxon, "_", " ")) %>% 
  mutate(taxon = str_replace(taxon, "cf", "cf.")) %>% 
  
  #taxanomic corrections
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
    #general fix
    taxon == "Calamagrostis cf.. macrophylla" ~ "Calamagrostis cf. macrophylla",
    TRUE ~ taxon)) %>% 
  
  # split taxon into genus and species
  separate(taxon, 
           into = c("genus", "specie"), 
           sep = "\\s", 2, 
           remove = FALSE,
           # keeps cf. and species taxon together
           extra = "merge") %>%
  
  #tidy columns to match with osf dataset
  separate(plot, 
           into = c("treatment", "plot_id"), 
           #sperate befor last character
           sep = -1, 2, 
           remove = TRUE) %>%
  mutate(month = rep("March",
                     nrow(.)),
         project = rep("PFTC3",
                       nrow(.))) %>%
  
  #change '+' in cover to 0.5 and make numeric and omit NA's
  mutate(cover = as.numeric(case_when(cover == "+" ~ "0.5",
                                      TRUE ~ cover))) %>%
  # drop absent species (no cover value)
  filter(!cover == "") %>% 
  
  #add other cols from osf dataset
  left_join(.,
            species_raw  %>%
              select(taxon, functional_group, family),
            by = 'taxon') %>%
  distinct() %>%

  select(year, project, month, site, treatment, plot_id, functional_group, family, genus, specie, taxon, cover)



#anti_join(species_2018, species_raw,
#          by = 'taxon') %>%
#  distinct()

### 2) Data cleaning ----

### >> a) Traits data ----
skim(traits_raw)

# clean data
traits <- traits_raw %>%
  #remove WAY sites - not needed
  filter(site %in% c("ACJ", "QUE", "TRE")) %>%
  mutate( 
    #Replace incorrect treatments for samples
    treatment = case_when(
      #Correct B sample for ACJ
      site == "ACJ" & 
        treatment == "B" ~ "BB",
      #Correct C sample for QUE
      site == "QUE" & 
        treatment == "C" ~ "BB",
      #Rename 2019 samples to C for QUE
      site == "QUE" & 
        year == 2019 ~ "C",
      #Correct B sample for QUE
      site == "QUE" & 
        treatment == "B" ~ "BB",
      #Correct B sample for QUE
      site == "TRE" & 
        treatment == "B" ~ "BB",
      TRUE ~ treatment)) %>%
  #remove Novemebr samples (multiple sampling from 2019 - Puna Project)
  filter(month != "November",
         #remove Sean's samples
         treatment != "OFF-PLOT") %>%
  
  #REMOVING DUPLICATES FOR INDIVIDUALS
  #group by each individual at each plot for each site & treatment
  group_by(site, treatment, plot_id, name_2020, individual_nr) %>%
  #arrange in a set way each time to ensure we use the same individuals
  arrange(id) %>%
  #keep only the first record for each individual
  slice_head()

# check again
skim(traits)


### >> b) Community data ----
species_combined <- species_raw %>%
  
  bind_rows(.,
            species_2018) %>%
  #samples from QUE 2018 will be considered C for our purposes
  mutate(treatment = ifelse(site == "QUE" & year == 2018,
                            "C",
                            treatment)) %>%
  filter(site == "QUE" & year == 2018 & treatment == "C" | 
           year == 2020 |
           site == "ACJ" & year == 2019 & month == "April" & treatment == "C")

skim(species_combined)


### 3) Data export ----

write.csv(traits, 
          file = here(path = "data/processed/traits_cleaned.csv"), 
          row.names = FALSE)

write.csv(species_combined, 
          file = here(path = "data/processed/species_cleaned.csv"), 
          row.names = FALSE)

# End of script ----
