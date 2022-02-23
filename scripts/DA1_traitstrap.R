#' Calculating bootstrapped means using {traitstrap}

### Call source script----

source(here::here(path = "scripts/0_data_import.R"))

### Packages----

# install.packages("remotes")
#remotes::install_github("richardjtelford/traitstrap")
library(traitstrap)

species %>%
  select(taxon, cover, site, treatment, plot_id) %>%
  mutate(treatment = as.factor(treatment)) %>%
  str()

### Trait Imputation ----

# Bootstrapping of control datasets  ----

trait_imputation_control = 
  trait_impute(
    # input data (mandatory)
    comm = species %>%
      select(taxon, cover, site, treatment, plot_id, year, season, month) %>%
      mutate(treatment = as.factor(treatment)) %>% 
      filter(treatment == C),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id, year, season, month) %>%
      mutate(treatment = as.factor(treatment)) %>% 
      filter(treatment == C),
    
    # specifies columns in your data (mandatory)
    abundance_col = "cover",
    taxon_col = "taxon",
    trait_col = "trait",
    value_col = "value",
    
    # specifies sampling hierarchy
    scale_hierarchy = c("year", "season", "month","site", "plot_id"),
  
    
    # min number of samples
    min_n_in_sample = 3
  )

# Bootstrapping of burnt datasets  ----

trait_imputation_burnt = 
  trait_impute(
    # input data (mandatory)
    comm = species %>%
      select(taxon, cover, site, treatment, plot_id, year, season, month) %>%
      mutate(treatment = as.factor(treatment)) %>% 
      filter(treatment == NB),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id, year, season, month) %>%
      mutate(treatment = as.factor(treatment)) %>% 
      filter(treatment == NB),
    
    # specifies columns in your data (mandatory)
    abundance_col = "cover",
    taxon_col = "taxon",
    trait_col = "trait",
    value_col = "value",
    
    # specifies sampling hierarchy
    scale_hierarchy = c("year", "season", "month","site", "plot_id"),
    
    
    # min number of samples
    min_n_in_sample = 3
  )

# Nonparametric Bootstrapping  ----

bootstrapped_moments = 
  trait_np_bootstrap(
    trait_imputation, 
    nrep = 20
  )


# Summarise Bootstrapping Output  ----

sum_boot_moment <- trait_summarise_boot_moments(bootstrapped_moments)

# Some 'random' plots  ----

autoplot(trait_imputation) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

autoplot(trait_imputation_2) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

autoplot(trait_imputation_3) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

# Summarise Bootstrapping Output  ----

Imputed_traits_fullcommunity <- Trait_impute_per_year(com_dat = community_for_boostrapping, trait_dat = traitdata_2)

sum_boot_moment <- trait_summarise_boot_moments(trait_imputation_3)
sum_boot_moment
