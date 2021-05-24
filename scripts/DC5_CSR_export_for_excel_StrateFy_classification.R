# GRIMES CSR 

# Run '0_data_import.R' to get the species and traits datasets. Get an average value for leaf area, leaf dry matter content and SLA per species. Convert to the approprite units required by 'StrateFy'. Export file as .csv to run the 'StrateFy' functions from the Pierce et al. (2017) paper. Read the output back in, subset to key columns (overall strategy, plus C,S,R components), and merge via taxon, back to main dataset.

# Pierce, S., Negreiros, D., Cerabolini, B. E. L., Kattge, J., Díaz, S., Kleyer, M., et al. (2017). A global method for calculating plant CSR ecological strategies applied across biomes worldwide. Functional Ecology,31, 444–457. <https://doi.org/10.1111/1365-2435.12722> 

### Packages -----
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
library(fs)
library(readxl)

### Call source script ----
source(here::here(path = "scripts/0_data_import.R"))




# Create taxon subset to use within StrateFy.

# How many unique species do we have?
taxon_list <- traits %>% 
  ungroup() %>% 
  distinct(taxon) %>% 
  pull(taxon)

# Create wide dataset for all traits
traits_wide <-
  traits %>% 
  pivot_wider(id_cols = -c(trait,value),
              names_from = trait,
              values_from = value,
              values_fill = NA)

# Create average trait LDMC, Leaf area and SLA for each species
# Not taking into account burn history, or elevational variation
summary_trait_species <- 
  traits_wide %>% 
  group_by(taxon) %>%
  summarise(mean_LA_cm2 = mean(leaf_area_cm2, na.rm = TRUE),
            mean_LDMC = mean(ldmc, na.rm = TRUE),
            mean_SLA_cm2_g = mean(sla_cm2_g, na.rm = TRUE))

# The units required by the StrateFy excel are actually; 
# LA (leaf area (mm2))
# LDMC (leaf dry matter content (%, dry weight/fresh weigh * 100))
# SLA (specific leaf area (mm2 mg-1))
# While we have LA (cm2), SLA (cm2_g) and LDMC (dry/fresh)

summary_trait_species$mean_LA_mm2 <- (summary_trait_species$mean_LA_cm2*100) #convert cm2 to mm2
summary_trait_species$mean_LDMC_pc <- (summary_trait_species$mean_LDMC*100) # to %
summary_trait_species$mean__SLA_mm2_mg <- (summary_trait_species$mean_SLA_cm2_g/10) # cm2/g to mm2/mg

write.csv(summary_trait_species, file = "data/CSR/CSR_traits_input_taxon.csv")
