# Plant functional trait course 5
# Cusco/Wayqecha, Peru - March 2020
#
# Group 3: Trait & taxonomic community response to fire and elevation
# Authors: Dagmar D. Egelkraut, Lucely Vilca Bustamante, Sonya Geange,
#          Korina Ocampo-Zuleta, Jonathan von Oppen, Jess Rickenback,
#          Tanya Strydom
# Contact: dagmar.egelkraut@uib.no

#' ------------------------------------------------------------------#
#' THIS SCRIPT WILL BE USED TO RUN THE FINAL MODELS (most likely)
#' ------------------------------------------------------------------#


### 0) Preamble ----
### >> a) Dependencies ----
library(tidyverse)
library(lmerTest) #can never remember if it should be lme4

### 1) Loop for trait LMMs ----

#if assigning new dataset to 'x'
#data need to be in 'long' format
#where all traits are in one column and then all the predictor variables in their own column

#as a hypothetical
data %>%
#can use dplyr::pivot_longer()
  pivot_longer(.,
               #columns not to be pivoted but are 'retained' i.e. not the traits
               cols = -c(burn_treatment,
                         elevation,
                         plot),
               #name of traits column
               names_to  = "trait",
               #name of traits value column
               values_to = "trait_val")
#might need to make `traits` a factor


for (i in 
     #this will loop over each trait
     levels(x$trait)) {
  #subset of df 'x' by trait
  dat = x[x$variable==i, ]
  
  #for now this just prints model summary
  print(summary(
    #add model here
    lmer(trait_val ~ burn_treatment * elevation + (1|plot),
                     data = dat)))
}

#TODO
#save model outputs as e.g. .txt file