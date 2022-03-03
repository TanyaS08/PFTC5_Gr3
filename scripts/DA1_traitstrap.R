#' Calculating bootstrapped means using {traitstrap}

### Call source script----

source(here::here(path = "scripts/0_data_import.R"))

### Packages----

# install.packages("remotes")
# remotes::install_github("richardjtelford/traitstrap")
library(traitstrap)
library(purrr)
library(patchwork) #this is for plots remove later

### Wrangling----

# create 'internal' df's taht have the correct 'grammar' for traitsftap functions

species_ =
  species %>%
  ungroup() %>%
  select(taxon, cover, site, treatment, plot_id, year, season, month) %>%
  mutate(treatment = as.factor(treatment),
         plot_id = as.factor(plot_id))

traits_ = 
  traits %>%
  ungroup() %>%
  select(taxon, trait, value, site, treatment, plot_id, year, season, month) %>%
  mutate(treatment = as.factor(treatment),
         plot_id = as.factor(plot_id))

# make lists with the various combos of grouping vars

comm_list =
  c(species_ %>%
      split(list(.$treatment, .$site), drop = TRUE),
    species_ %>%
      split(.$site),
    species_ %>%
      split(.$treatment),
    list(species_))


trait_list = 
  c(traits_ %>%
      split(list(.$treatment, .$site), drop = TRUE),
    traits_ %>%
      split(.$site),
    traits_ %>%
      split(.$treatment),
    list(traits_))

# list with scale specified
hierarchy = 
  c(rep(list(c("year", "season", "month", "plot_id")), 6),              #treatment.site
    rep(list(c("year", "season", "month", "treatment", "plot_id")), 3), #site
    rep(list(c("year", "season", "month", "site", "plot_id")), 2),      #treatment
    rep(list(c("year", "season", "month", "site", "treatment", "plot_id")), 1)       #none
  )

### Trait Imputation ----

# initiate empty list
trait_impute_list = vector(mode = "list", length = length(trait_list))

for (i in 1:length(trait_list)) {
  
  trait_impute_list[[i]] =
    trait_impute(
      # input data (mandatory)
      comm = comm_list[[i]],
      traits = trait_list[[i]],
      
      # specifies columns in your data (mandatory)
      abundance_col = "cover",
      taxon_col = "taxon",
      trait_col = "trait",
      value_col = "value",
      
      # specifies sampling hierarchy
      scale_hierarchy = hierarchy[[i]],
      
      # min number of samples
      min_n_in_sample = 3
    )
  
}

# combine by how dataframes were split

trait_impute_treat.site = 
  do.call(rbind.data.frame,trait_impute_list[1:6]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("treat.site")) %>%
  select(-c(site_trait, treatment_trait)) %>%
  rename(site = site_comm,
         treatment = treatment_comm)

trait_impute_site = 
  do.call(rbind.data.frame,trait_impute_list[7:9]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("site")) %>%
  select(-c(site_trait)) %>%
  rename(site = site_comm)

trait_impute_treat = 
  do.call(rbind.data.frame,trait_impute_list[10:11]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("treat")) %>%
  select(-c(treatment_trait)) %>%
  rename(treatment = treatment_comm)

trait_impute_no.split = 
  do.call(rbind.data.frame,trait_impute_list[12]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("no.split"))

#some plots

plots <-
  rbind(trait_impute_treat.site,
        trait_impute_site,
        trait_impute_treat,
        trait_impute_no.split) %>%
  unite(
    #new variable name
    plot,
    #cols to combine
    c(site, treatment),
    sep = " ", remove = FALSE
  ) %>%
  filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "Wet_Mass_g")) %>%
  #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
  mutate(# Rename traits for labels
    trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                      trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                      trait == "ldmc" ~ "LDMC",
                      trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)")) %>%
  filter(!is.na(trait))

ggplot(plots) +
  geom_density_ridges(aes(y = site,
                          x = value,
                          fill = split_by,
                          colour = split_by),
                      alpha = 0.6) +
  facet_grid(cols = vars(trait),
             rows = vars(treatment),
             scales = "free_x",
             labeller = label_parsed) +
  labs(y = "Density",
       x = "Trait Value") +
  theme_classic() +
  ggplot(plots) +
  geom_boxplot(aes(x = paste(site, treatment),
                   y = value,
                   colour = split_by)) +
  facet_wrap(vars(trait),
             nrow = 1,
             scales = "free",
             labeller = label_parsed)  +
  theme_classic()+ 
  plot_layout(ncol = 1)

# Nonparametric Bootstrapping  ----

# initiate empty list
trait_bootstrap_list = vector(mode = "list", length = length(trait_impute_list))

for (i in 1:length(trait_impute_list)) {
  
  trait_bootstrap_list[[i]] = 
    trait_np_bootstrap(
      trait_impute_list[[i]], 
      nrep = 20
    )
  
}

# combine by how dataframes were split

trait_bootstrap_treat.site = 
  do.call(rbind.data.frame,trait_bootstrap_list[1:6]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("treat.site"),
         split_by = as.factor("treat.site"),
         split_by = as.factor("treat.site"))

trait_bootstrap_site = 
  do.call(rbind.data.frame,trait_bootstrap_list[7:9]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("site")) %>%
  select(-c(site_trait)) %>%
  rename(site = site_comm)

trait_bootstrap_treat = 
  do.call(rbind.data.frame,trait_bootstrap_list[10:11]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("treat")) %>%
  select(-c(treatment_trait)) %>%
  rename(treatment = treatment_comm)

trait_bootstrap_no.split = 
  do.call(rbind.data.frame,trait_bootstrap_list[12]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("no.split"))


# Summarise Bootstrapping Output  ----

sum_boot_moment <- trait_summarise_boot_moments(
  np_bootstrapped_moments
)
sum_boot_moment

# Some 'random' plots  ----

autoplot(trait_impute_list[[1]]) + 
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5))
