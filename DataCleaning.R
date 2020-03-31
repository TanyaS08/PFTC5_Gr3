
# LIBRARIES ---------------------------------------------------------------
library("readxl")
library("writexl")
library("tidyverse")
library("tidylog")
library("lubridate")

# DATA IMPORT -------------------------------------------------------------

traits <- read.csv("PFTC5_Peru_2020_LeafTraits_cleaned_20-03-19.csv") %>%
  filter(., #Remove Sean's leaves
         Project == "T") %>%
  mutate_at(vars(Experiment, Site),
            as.character) %>%
  mutate(.,
         Experiment = ifelse(Site == "QUE" & Experiment %in% c("C", "B", NA), "BB", Experiment),
         )

traits.covid.safe <- read_csv("PFTC5_Peru_2020_LeafTraits_template_COVID_safe_TS.csv") %>%
  mutate_at(vars(Experiment, Site, Project),
            as.character) %>%
  mutate(.,
         Project = ifelse(Project == "TRUE", "T", Project)) %>%
  filter(., #Remove Sean's leaves
         Project == "T") 

traits.covid <- read_csv("PFTC5_Peru_2020_LeafTraits_template_COVID_TS.csv") %>%
  mutate_at(vars(Experiment, Site, Project),
            as.character) %>%
  mutate(.,
         Project = ifelse(Project == "TRUE", "T", Project)) %>%
  filter(., #Remove Sean's leaves
         Project == "T") 
  
tally.site_exp <- fire_traits_compl %>%
  group_by(.,
           Site,
           Experiment) %>%
  tally()

tally.covid.safe <- traits.covid.safe %>%
  group_by(.,
           Site,
           Experiment) %>%
  tally()

tally.covid <- traits.covid %>%
  group_by(.,
           Site,
           Experiment, Exodus_Stage) %>%
  tally()
