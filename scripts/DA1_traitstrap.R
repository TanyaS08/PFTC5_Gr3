#' Calculating bootstrapped means using {traitstrap}

### Call source script----

source(here::here(path = "scripts/0_data_import.R"))

### Packages----

# install.packages("remotes")
remotes::install_github("richardjtelford/traitstrap")
# library(traitstrap)

species %>%
  select(taxon, cover, site, treatment, plot_id) %>%
  mutate(treatment = as.factor(treatment)) %>%
  str()

### Trait Imputation ----

trait_imputation = 
  trait_impute(
    # input data (mandatory)
    comm = species %>%
      select(taxon, cover, site, treatment, plot_id) %>%
      mutate(treatment = as.factor(treatment)),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id) %>%
      mutate(treatment = as.factor(treatment)),
    
    # specifies columns in your data (mandatory)
    abundance_col = "cover",
    taxon_col = "taxon",
    trait_col = "trait",
    value_col = "value",
    
    # specifies sampling hierarchy
    scale_hierarchy = c("site", "plot_id"),
    
    # specifying experimental design
    treatment_col = "treatment",
    treatment_level = "site",
    
    # min number of samples
    min_n_in_sample = 3
  )

# no experimental design

trait_imputation_2 = 
  trait_impute(
    # input data (mandatory)
    comm = species %>%
      select(taxon, cover, site, treatment, plot_id) %>%
      mutate(treatment = as.factor(treatment)),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id) %>%
      mutate(treatment = as.factor(treatment)),
    
    # specifies columns in your data (mandatory)
    abundance_col = "cover",
    taxon_col = "taxon",
    trait_col = "trait",
    value_col = "value",
    
    # specifies sampling hierarchy
    scale_hierarchy = c("site", "treatment", "plot_id"),
    
    # min number of samples
    min_n_in_sample = 3
  )

# imputation 3 - trial

trait_imputation_3 = 
  trait_impute(
    # input data (mandatory)
    comm = species %>%
      select(taxon, cover, site, treatment, plot_id, year, season) %>%
      mutate(treatment = as.factor(treatment)),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id, year, season) %>%
      mutate(treatment = as.factor(treatment)),
    
    # specifies columns in your data (mandatory)
    abundance_col = "cover",
    taxon_col = "taxon",
    trait_col = "trait",
    value_col = "value",
    
    # specifies sampling hierarchy
    scale_hierarchy = c("year", "season", "site", "plot_id"),
    
    # specifying experimental design
    treatment_col = "treatment", #The treatment does not do one treatment and one control. It is designed for several treatments and one control. So we need to make treatmentA = burnt, treatmentB = control, and a fake (non existing) control
    treatment_level = "site",
    
    # min number of samples
    min_n_in_sample = 3
  )

# Nonparametric Bootstrapping  ----

bootstrapped_moments = 
  trait_np_bootstrap(
    trait_imputation, 
    nrep = 200
  )


# Summarise Bootstrapping Output  ----

sum_boot_moment <- trait_summarise_boot_moments(
  np_bootstrapped_moments
)
sum_boot_moment

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
