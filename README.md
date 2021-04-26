# **Plant functional trait course 5**

Cusco/Wayqecha, Peru - March 2020

**Group 3: Trait & taxonomic community response to fire and elevation**

**Contact:** dagmar.egelkraut@uib.no

Repository for a proposed manuscript focusing on the effects of fire and 
elevation on species composition and functional trait in the high 
Andean moist Puna grasslands. This repository is used to store code 
associated with the data analysis portion of the manuscript. The 
proposal can be found [here](https://docs.google.com/document/d/1CN_nDSyvQGwecFTCOalYo6LrnpownpS0l16awSFydFE/edit?usp=sharing)

## Downloading and 'cleaning' the data

This can be done by running the `scripts/0_data_import.R` file. This will then 
download the data as well as filter/clean the data so that we only have the data 
that we will be using for our analyses.

### Downloading data from _osf_

The raw datasets can be retrieved from _osf_ using `{dataDownloader}` - 
you will need to install this using `devtools::install_github("Between-the-Fjords/dataDownloader")` 
if you haven't done so previously. These data will be stored in the 
newly created `data/raw` folder.

You can 'run'/call this script by calling 
`source(here::here(path = "scripts/0_data_import.R"))` at the 
beginning of your script. which will also add the two cleaned/filtered 
datasets to your environment as `traits` (for functional traits) and 
`species` (for community composition) as well as ensuring that you have 
the most up to date dataset stored on you machine.

### Cleaning/filtering the data

The `scripts/0_data_import.R` code will also filter out the the sites 
relevant for our analyses. AS we are downloading the _entire_ Puna 
dataset form _osf_ we need to filter the data so that we have the following 
sites and treatments form the following years:

| Site | Treatment | Year |
| :----| :-------- | :--- |
| QUE  | C         | 2019 |
| QUE  | NB        | 2020 |
| TRE  | C         | 2020 |
| TRE  | NB        | 2020 |
| ACJ  | C         | 2019 |
| ACJ  | NB        | 2019 |


> ⚠️ Note that `data/` is in the `.gitignore` this means you have to run the dta import 
> script at least once regardless and that anything you export to the data folder will 
> not be pushed to the repo and other will not be ablye to use that output unless you tell 
> them which script to run to source that output.

## Script naming structure

Each [subtask](https://docs.google.com/spreadsheets/d/1G2w4rHiUkQ1iI5b7U_5dhyf1U87eOyaMcTBNFT4uq3w) 
related to/needing a 
coding workflow should be contained within its own script and should be 
named starting with the subtask number and a brief descriptive name. 
Scripts should be placed in the `scripts/` folder. This means we can 
keep track of each task separately.

## Data visualisation

> :construction: still under construction :construction:

The idea is to centralise and standardise plotting themes and colour 
palettes using `scripts/plotting_aesthetics.R`. This script will call a 
custom `{ggplot2}` theme that you can call at the end of your plotting 
code chunk that will standardise aesthetics across the board. You will 
still be able to include your own theme tweaks after this by just 
calling `theme()` again after. There will also be a colour theme which 
will standadise colours used for different treatments and sites which 
you can then call when you specify `scale_*_manual` - example pending...

Calling this script using `source(here::here(path = "scripts/plotting_aesthetics.R"))` 
will import the needed themes etc.

A preliminary figure of trait distributions because why not! :wink:

![](https://github.com/TanyaS08/PFTC5_Gr3/blob/master/output/traits_density_plots.png?raw=true)
