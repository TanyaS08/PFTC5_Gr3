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
#' - check ACJ&TRE recording year with Lucely, 2019?!? -> 2b i, l. 
#' - create functional group column from genus names -> 2b iii, l.143
#' - + cover values as 0.5? -> 2b iii, l. 141
#' ------------------------------------------------------------------#


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

### >> c) Functions ----
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

### 1) Data import ----
# traits data - complete
traits_raw <- read.csv(file.path("data", "data_raw", "PFTC5_Peru_2020_LeafTraits_cleaned_20-03-21.csv"), 
                         header = T, 
                         sep = ",")

# community data
species_files <- paste(file.path("data", "data_raw"), dir(file.path("data", "data_raw"), pattern = "communitydata"), sep = "/")
species_raw <- map_df(species_files, read_csv)


### 2) Data cleaning ----

### >> a) Traits data ----
skim(traits_raw)

# clean data
traits <- traits_raw %>% 
  # LeafArea_cm2 is a factor, make numeric
  mutate(LeafArea_cm2 = as.numeric(LeafArea_cm2)) %>% 

  # create Taxon column from Genus + species
  mutate(Taxon = as.factor(paste(Genus, " ", Species))) %>% 

  # Average leaf thickness measurements
  mutate(Leaf_Thickness_avg_mm = rowMeans(select(.,
                                             Leaf_Thickness_1_mm,
                                             Leaf_Thickness_2_mm,
                                             Leaf_Thickness_3_mm), na.rm=TRUE)) %>%

  # QUE contains C samples, have to be BB -> recode 
  mutate_at(vars(Experiment, Site), as.character) %>% 
  mutate(.,
         Experiment = ifelse(Site == "QUE" & Experiment %in% c("C", "B"), "BB", Experiment)
         ) %>% 
  
  # keep only samples from traits (T) project
  filter(Project == "T") %>% 
  
  # exclude cases with NA in Experiment, just for ease
  drop_na(Experiment) %>% 
  
  # also drop WAY samples, as not our focus
  filter(!Site == "WAY") %>% 
  
  # rename columns
  rename(Treatment = Experiment, PlotID = Plot_ID, Comment = Remark) %>% 
  
  # create date variable from day
  mutate(Date = as.Date(paste0("2020-03-", Day)))

# check again
skim(traits)


### >> b) Community data ----
species <- species_raw %>% 
  
  # rename columns to first letter upper case
  rename_all(capwords) %>% 
  
  # rename "name" to "Taxon"
  rename(Taxon = Name, PlotID = Plot) %>% 
  
  # # recode year 2019 to 2020 -> check with Lucely! ...
  # mutate(year = as.integer(recode(year, "2019" = "2020"))) %>% 
  
  # drop absent species
  filter(!Cover == "") %>% 
  
  # remove dots after "cf"
  mutate(Taxon = str_remove(Taxon, "\\.")) %>% 
  # ...and replace underscores with spaces in Taxon column
  mutate(Taxon = str_replace(Taxon, "_", " ")) %>% 
  
  # extract cfs into new column...
  mutate(Cf = str_extract(Taxon, "cf |cf_")) %>% 
  # ...remove extra space/_ from cf column...
  mutate(Cf = str_remove(Cf, "\\s|_")) %>% 
  # ...and delete "cf "/"cf_" from Taxon column
  mutate(Taxon = str_remove(Taxon, "cf |cf_")) %>% 
  
  # split Taxon into genus and species
  separate(Taxon, into = c("Genus", "Species"), 
           sep = "\\s", 2, 
           remove = FALSE) %>% 
  
  # drop all-NA "real_species_name" & "habit" columns
  select_if(function(x){!all(is.na(x))}) %>% 
  
  # extract treatment from plot
  mutate(Treatment = str_extract(PlotID, "[:alpha:]*")) %>% 
  
  # recode "+" cover value to 0.5
  mutate(Cover = as.numeric(recode(Cover, "+" = "0.5"))) %>% 
  
  # convert all factors back to factors
  mutate_at(vars(Site, PlotID, Taxon, Genus, Species, Fertile, Seedling, Observer, Sampled, Treatment), factor) # %>% 

# create functional group column based on genus name
## TBC, e.g. with something like
# mutate(., functional_group = ifelse(name %in% graminoid_taxa, 
#                                       "graminoid", 
#                                       "forb"))
  

### 3) Summary graphs ----
# N species sampled for traits per site and treatment
traits %>% group_by(Site, Treatment) %>% 
  summarise(n_taxa = n_distinct(Taxon)) %>% 
  ggplot(aes(x = Site, y = n_taxa, fill = Treatment)) +
    geom_bar(stat = "identity", position = position_dodge2(preserve = "single")) +
    scale_fill_manual(values = c(theme_red, theme_blue)) +
    scale_x_discrete(limits = c("ACJ", "TRE", "QUE")) +
    theme_bw() +
    labs(y = "total no. taxa")

#plant height density plot
traits %>% #group_by(Site) %>% 
  ggplot(aes(Plant_Height_cm, fill = Site)) +
  geom_density(alpha = .5, kernel = "gaussian") +
  scale_fill_manual(values = c(theme_darkblue, theme_green, theme_yellow)) +
  theme_bw() +
  labs(y = "density")

#plant height density plot - split by treatment
traits %>% 
  #combine both Site and treatment to one variable
  unite(Plot,
        c(Site, Treatment),
        sep = " ", remove = FALSE) %>% 
  ggplot(aes(Plant_Height_cm, fill = Plot)) +
  geom_density(alpha = .5, kernel = "gaussian") +
  #scale_fill_manual(values = c(theme_darkblue, theme_green, theme_yellow)) +
  theme_bw() +
  labs(y = "density")

# End of script ----