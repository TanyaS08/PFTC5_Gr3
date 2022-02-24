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
      filter(treatment == "C"),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id, year, season, month) %>%
      mutate(treatment = as.factor(treatment)) %>% 
      filter(treatment == "C"),
    
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
      filter(treatment == "NB"),
    traits = traits %>%
      select(taxon, trait, value, site, treatment, plot_id, year, season, month) %>%
      mutate(treatment = as.factor(treatment)) %>% 
      filter(treatment == "NB"),
    
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

bootstrapped_moments_control = 
  trait_np_bootstrap(
    trait_imputation_control, 
    nrep = 200
  )

bootstrapped_moments_burnt = 
  trait_np_bootstrap(
    trait_imputation_burnt, 
    nrep = 200
  )


# Summarize Bootstrapping Output  ----

sum_boot_moment_control <- trait_summarise_boot_moments(bootstrapped_moments_control) %>% 
  mutate(treatment = "C")

sum_boot_moment_burnt <- trait_summarise_boot_moments(bootstrapped_moments_burnt) %>% 
  mutate(treatment = "NB")

# Merge bootstrapped data sets  ----

sum_boot_moments_full <- sum_boot_moment_control %>% 
  bind_rows(sum_boot_moment_burnt)


# Some 'random' plots  ----

autoplot(trait_imputation_control) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

autoplot(trait_imputation_burnt) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))

sum_boot_moments_full %>% ggplot(aes(x = treatment, y = mean, fill = treatment)) + geom_boxplot() + facet_grid(trait ~ site, scales = "free")
