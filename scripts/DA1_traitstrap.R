#' Calculating bootstrapped means using {traitstrap}

### Call source script ----

source(here::here(path = "scripts/0_data_import.R"))

### Packages ----

# install.packages("remotes")
# remotes::install_github("richardjtelford/traitstrap")
library(traitstrap)
library(purrr)
library(patchwork) #this is for plots remove later

### Wrangling ----
# create 'internal' df's that have the correct 'grammar' for traitsftap functions

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

### Traitstrap ----

#' Note we will split the dataset six ways based on the different combos of
#' site and treatment

### >> a) Split datasets and make list ----

comm =
  species_ %>%
  #split by site & treatment
  split(list(.$treatment, .$site))

trait = 
  traits_ %>%
  #split by site & treatment
  split(list(.$treatment, .$site))

### >> b) Impute ----

# initiate empty list
impute_trait = vector(mode = "list", length = length(trait))

for (i in 1:length(trait)) {
  
  impute_trait[[i]] =
    trait_impute(
      # input data (mandatory)
      comm = comm[[i]],
      traits = trait[[i]],
      
      # specifies columns in your data (mandatory)
      abundance_col = "cover",
      taxon_col = "taxon",
      trait_col = "trait",
      value_col = "value",
      
      # specifies sampling hierarchy
      scale_hierarchy = c("year", "season", "month", "plot_id"),
      
      # min number of samples
      min_n_in_sample = 3
    )
  
}

### >> c) Bootstrap raw ----

bootstrap_raw = vector(mode = "list", length = length(impute_trait))

for (i in 1:length(impute_trait)) {
  
  bootstrap_raw[[i]] = 
    trait_np_bootstrap(
      impute_trait[[i]], 
      sample_size = 200,
      raw = TRUE
    )
  
}

# Export this as a .csv

write.csv(do.call(rbind.data.frame, bootstrap_raw[1:6]),
          file = here::here(path = "data/processed/traits_traitstrapped_raw.csv"))

### >> c.1) Plot

ggplot(do.call(rbind.data.frame, bootstrap_raw[1:6]) %>%
         ungroup() %>%
         #combine both Site and treatment to one variable
         unite(
           #new variable name
           plot,
           #cols to combine
           c(site_comm, treatment_comm),
           sep = " ", remove = FALSE
         ) %>%
         filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "wet_mass_g")) %>%
         #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
         mutate(site_comm = case_when(site_comm == "ACJ" ~ "3 468 m.a.s.l.",
                                 site_comm == "TRE" ~ "3 715 m.a.s.l.",
                                 site_comm == "QUE" ~ "3 888 m.a.s.l"),
                # Rename traits for labels
                trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                                  trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                                  trait == "ldmc" ~ "LDMC",
                                  trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)"))) +
  geom_density_ridges(aes(y = site_comm,
                          x = value,
                          fill = plot,
                          colour = plot),
                      alpha = 0.6) +
  facet_wrap(vars(trait),
             scales = "free_x",
             labeller = label_parsed,
             ncol = 2)  +
  scale_fill_manual(name = "Plot",
                    values = colours_site$c,
                    breaks = colours_site$t) +
  scale_colour_manual(name = "Plot",
                      values = colours_site$c,
                      breaks = colours_site$t) +
  labs(y = "Density",
       x = "Trait Value") +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Raw bootstrapped distribution")

### >> d) Bootstrap summary ----

# initiate empty list
sum_bootstrap = vector(mode = "list", length = length(impute_trait))

sites = c(rep("ACJ", 2), rep("QUE", 2), rep("TRE", 2))
treatments = rep(c("C", "NB"), 3)

for (i in 1:length(sum_bootstrap)) {
  
  x = 
    trait_np_bootstrap(
      impute_trait[[i]], 
      nrep = 200
    )  %>%
    mutate(site = paste(sites[i]),
           treatment = treatments[i])
  
  # 'reassign' sacle attributes the site & treatment level
  
  attr(x, "attrib")$scale_hierarchy = c("treatment", "site", "trait")
  
  # now we summarise at the new hierachy
  sum_bootstrap[[i]] = 
    trait_summarise_boot_moments(x) %>%
    # and make the output data 'pretty'
    pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
                 names_to = 'moment',
                 values_to = 'estimate') %>%
    pivot_longer(cols = contains('ci_high'),
                 names_to = 'ci_high_moment',
                 values_to = 'ci_high') %>%
    mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
    filter(ci_high_moment == moment) %>%
    pivot_longer(cols = contains('ci_low'),
                 names_to = 'ci_low_moment',
                 values_to = 'ci_low') %>%
    mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
    filter(ci_low_moment == moment) %>%
    select(-c(ci_low_moment, ci_high_moment)) %>%
    mutate(moment = case_when(moment == 'var' ~ 'variance',
                              moment == 'kurt' ~ 'kurtosis',
                              moment == 'skew' ~ 'skewness',
                              TRUE ~ moment)) %>%
    group_by(treatment, site, trait, moment) %>%
    summarise(estimate = mean(estimate),
              ci_high = mean(ci_high),
              ci_low = mean(ci_low))
}

# Export this as a .csv

write.csv(do.call(rbind.data.frame, sum_bootstrap[1:6]),
          file = here::here(path = "data/processed/traits_traitstrapped_moments.csv"))

### >> d.1) Plot

ggplot(do.call(rbind.data.frame, sum_bootstrap[1:6])%>%
         unite(
           #new variable name
           plot,
           #cols to combine
           c(site, treatment),
           sep = " ", remove = FALSE
         ) %>%
         filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "wet_mass_g")) %>%
         #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
         mutate(# Rename traits for labels
           trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                             trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                             trait == "ldmc" ~ "LDMC",
                             trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)"))) +
  geom_pointrange(aes(x = site,
                      y = estimate,
                      ymin = ci_low,
                      ymax = ci_high,
                      colour = treatment,
                      group = treatment),
                  position = position_dodge2(width = 0.5, padding = 0.5)) +
  facet_wrap(vars(moment, trait),
             ncol = 4,
             scales = "free",
             labeller = label_parsed)  +
  theme_classic()

### Different dataset splits ----

#' Note this splits the dataset either by site, treatment or treatment and site
#' and then traitstraps at the level of the dataset (as opposed to not splitting
#' and then having the scale hiearchy act across all scales)

# make lists with the various combos of grouping vars
# this is easier to loop through

comm_list =
  c(species_ %>%
      split(list(.$treatment, .$site), drop = TRUE), #split by site & treatment
    species_ %>%
      split(.$site), #split by site
    species_ %>%
      split(.$treatment), #split by treatment
    list(species_))


trait_list = 
  c(traits_ %>%
      split(list(.$treatment, .$site), drop = TRUE), #split by site & treatment
    traits_ %>%
      split(.$site, drop = TRUE), #split by site & treatment
    traits_ %>%
      split(.$treatment, drop = TRUE), #split by site & treatment
    list(traits_))

# list with scale specified in traitstrap imputation
hierarchy = 
  c(rep(list(c("year", "season", "month", "plot_id")), 6),              #treatment.site
    rep(list(c("year", "season", "month", "treatment", "plot_id")), 3), #site
    rep(list(c("year", "season", "month", "site", "plot_id")), 2),      #treatment
    rep(list(c("year", "season", "month", "site", "treatment", "plot_id")), 1)       #none
  )

### Trait Imputation

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

# Nonparametric Bootstrapping
# note here we are outputting the raw bootstrapped data

# initiate empty list
trait_bootstrap_list = vector(mode = "list", length = length(trait_impute_list))

for (i in 1:length(trait_impute_list)) {
  
  trait_bootstrap_list[[i]] = 
    trait_np_bootstrap(
      trait_impute_list[[i]], 
      sample_size = 200,
      raw = TRUE
    )

  }

# combine by how dataframes were split

trait_bootstrap_treat.site = 
  do.call(rbind.data.frame,trait_bootstrap_list[1:6]) %>%
  ungroup() %>%
  mutate(split_by = as.factor("treat.site")) %>%
  select(-c(site_trait, treatment_trait)) %>%
  rename(site = site_comm,
         treatment = treatment_comm)

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

# plotting

plots <-
  rbind(trait_bootstrap_treat.site,
        trait_bootstrap_site,
        trait_bootstrap_treat,
        trait_bootstrap_no.split) %>%
  unite(
    #new variable name
    plot,
    #cols to combine
    c(site, treatment),
    sep = " ", remove = FALSE
  ) %>%
  filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "wet_mass_g")) %>%
  mutate(# Rename traits for labels
    trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                      trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                      trait == "ldmc" ~ "LDMC",
                      trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)"))

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


# Summarise Bootstrapping Output
# this runs `trait_np_bootstrap` a second time but not the raw data

# initiate empty list
sum_bootstrap_list = vector(mode = "list", length = length(trait_bootstrap_list))


# TODO
#' change  attributes for summary so that summaries are at the site/treamtent 
#' level and not all the way down to plot

for (i in 1:length(sum_bootstrap_list)) {
  
  sum_bootstrap_list[[i]] = 
    trait_summarise_boot_moments(
      trait_np_bootstrap(
        trait_impute_list[[i]], 
        nrep = 200
      )
    )
}

# we can now plot this
# although there must be a better way to map through the list...

sum_bootstrap = 
  rbind(sum_bootstrap_list[[1]] %>%
          mutate(site = "ACJ",
                 treatment = "C",
                 split_by = as.factor("treat.site")),
        sum_bootstrap_list[[2]] %>%
          mutate(site = "ACJ",
                 treatment = "NB",
                 split_by = as.factor("treat.site")),
        sum_bootstrap_list[[3]] %>%
          mutate(site = "QUE",
                 treatment = "C",
                 split_by = as.factor("treat.site")),
        sum_bootstrap_list[[4]] %>%
          mutate(site = "QUE",
                 treatment = "NB",
                 split_by = as.factor("treat.site")),
        sum_bootstrap_list[[5]] %>%
          mutate(site = "TRE",
                 treatment = "C",
                 split_by = as.factor("treat.site")),
        sum_bootstrap_list[[6]] %>%
          mutate(site = "TRE",
                 treatment = "NB",
                 split_by = as.factor("treat.site")),
        sum_bootstrap_list[[7]] %>%
          mutate(site = "ACJ",
                 split_by = as.factor("site")),
        sum_bootstrap_list[[8]] %>%
          mutate(site = "QUE",
                 split_by = as.factor("site")),
        sum_bootstrap_list[[9]] %>%
          mutate(site = "TRE",
                 split_by = as.factor("site")),
        sum_bootstrap_list[[10]] %>%
          mutate(treatment = "C",
                 split_by = as.factor("treat")),
        sum_bootstrap_list[[11]] %>%
          mutate(treatment = "NB",
                 split_by = as.factor("treat")),
        sum_bootstrap_list[[12]] %>%
          mutate(split_by = as.factor("no.split"))) %>%
  filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "wet_mass_g")) %>%
  mutate(# Rename traits for labels
    trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                      trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                      trait == "ldmc" ~ "LDMC",
                      trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)")) %>%
  pivot_longer(cols = c('mean', 'var', 'skew', 'kurt'),
               names_to = 'moment',
               values_to = 'estimate') %>%
  pivot_longer(cols = contains('ci_high'),
               names_to = 'ci_high_moment',
               values_to = 'ci_high') %>%
  mutate(ci_high_moment = str_to_lower(str_extract(ci_high_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_high_moment == moment) %>%
  pivot_longer(cols = contains('ci_low'),
               names_to = 'ci_low_moment',
               values_to = 'ci_low') %>%
  mutate(ci_low_moment = str_to_lower(str_extract(ci_low_moment,'[[:alpha:]]*$'))) %>%
  filter(ci_low_moment == moment) %>%
  select(-c(global, ci_low_moment, ci_high_moment)) %>%
  mutate(moment = case_when(moment == 'var' ~ 'variance',
                            moment == 'kurt' ~ 'kurtosis',
                            moment == 'skew' ~ 'skewness',
                            TRUE ~ moment)) %>%
  group_by(treatment, site, split_by, trait, moment) %>%
  summarise(estimate = mean(estimate),
            ci_high = mean(ci_high),
            ci_low = mean(ci_low))

ggplot(sum_bootstrap) +
  geom_pointrange(aes(x = paste(site, treatment),
                      y = estimate,
                      ymin = ci_low,
                      ymax = ci_high,
                      colour = split_by,
                      group = split_by),
                  position = position_dodge2(width = 0.5, padding = 0.5)) +
  facet_wrap(vars(moment, trait),
             ncol = 4,
             scales = "free",
             labeller = label_parsed)  +
  theme_classic()


# Free space  ----

ggplot(trait_bootstrap_treat.site %>%
         unite(
           #new variable name
           plot,
           #cols to combine
           c(site, treatment),
           sep = " ", remove = FALSE
         ) %>%
         filter(trait %notin% c("dry_mass_g", "leaf_area_cm2", "wet_mass_g")) %>%
         #NOTE We can keep this as site names but from a reader perspective elevation may be more meaningful
         mutate(# Rename traits for labels
           trait = case_when(trait == "plant_height_cm" ~ "Plant~height~(cm)",
                             trait == "sla_cm2_g" ~ "SLA~(cm^{2}/g)",
                             trait == "ldmc" ~ "LDMC",
                             trait == "leaf_thickness_mm" ~ "Leaf~thickness~(mm)"))) +
  geom_density_ridges(aes(y = site,
                          x = value,
                          fill = treatment,
                          colour = treatment),
                      alpha = 0.6) +
  facet_wrap(vars(trait),
             scales = "free_x",
             labeller = label_parsed,
             ncol = 2) +
  labs(y = "Density",
       x = "Trait Value") +
  theme_classic() +
  labs(title = "Raw bootstrapped distribution when dataset split by site & treatment")

