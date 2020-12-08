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
species_raw <- map_df(species_files, read_csv)

### 2) Data cleaning ----

### >> a) Traits data ----
skim(traits_raw)

# clean data
traits <- traits_raw %>%
  #remove WAY sites - not needed
  filter(site != "WAY" &
           #remove Sean's samples
           treatment != "OFF-PLOT") %>%
  mutate( 
    #Replace incorrect treatments for samples
    treatment = case_when(
      #Correct B sample for ACJ
      site == "ACJ" & 
        treatment == "B" ~ "BB",
      #Correct C sample for QUE
      site == "QUE" & 
        treatment == "C" ~ "BB",
      #Rename 2019 samples to control for QUE
      site == "QUE" & 
        year == 2019 ~ "C",
      #Correct B sample for QUE
      site == "QUE" & 
        treatment == "B" ~ "BB",
      #Correct B sample for QUE
      site == "TRE" & 
        treatment == "B" ~ "BB",
      TRUE ~ treatment))

# check again
skim(traits)


### >> b) Community data ----
species <- species_raw %>%
  #select only target site = TRE, QUE, ACJ
  filter(site %in% c("QUE") &
           year == 2019)


# End of script ----
