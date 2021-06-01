# README Plant functional trait course 5 - group 3

Cusco/Wayqecha, Peru - March 2020

**Group 3: Trait & taxonomic community response to fire and elevation**

**Contact:** dagmar.egelkraut@uib.no

> For repo access contact Tanya either on Slack or shoot her an
> email: tanya.strydom@icloud.com

Repository for a proposed manuscript focusing on the effects of fire and
elevation on species composition and functional trait in the high
Andean moist Puna grasslands. This repository is used to store code
associated with the data analysis portion of the manuscript. The
proposal can be found [here](https://docs.google.com/document/d/1CN_nDSyvQGwecFTCOalYo6LrnpownpS0l16awSFydFE/edit?usp=sharing)

- [README Plant functional trait course 5 - group 3](#readme-plant-functional-trait-course-5---group-3)
  - [Downloading and 'cleaning' the data](#downloading-and-cleaning-the-data)
    - [Downloading data from _osf_](#downloading-data-from-osf)
    - [Cleaning/filtering the data](#cleaningfiltering-the-data)
  - [Script naming structure](#script-naming-structure)
  - [Working on subtasks - using pull requests](#working-on-subtasks---using-pull-requests)
    - [DC2 - Placeholder chemical traits data](#dc2---placeholder-chemical-traits-data)
    - [DC5 - Allocation of Grimes CSR strategy for FT's](#dc5---allocation-of-grimes-csr-strategy-for-fts)
  - [Data visualisation](#data-visualisation)

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

:rocket: **RELEASE NOTE** calling
`source(here::here(path = "scripts/0_data_import.R"))` now also incorporates
the CSR traits as well as temporary chemical traits. This will be updated in
such a way that when we get the new chemical traits data it should all
seamlessly integrate and none of the scripts should break...

> Note if you do call `source(here::here(path = "scripts/0_data_import.R"))`
> at the beginning of your script it does mean you will always be using the
> most complete/updated dataset if we do end up changing which data we want
> to keep or exclude from analyses.

### Cleaning/filtering the data

The `scripts/0_data_import.R` code will also filter out the the sites
relevant for our analyses. AS we are downloading the _entire_ Puna
dataset form _osf_ we need to filter the data so that we have the following
sites and treatments form the following years:

| Site | Treatment | Year             |
| :----| :-------- | :--------------- |
| QUE  | C         | 2019             |
| QUE  | NB        | 2020             |
| TRE  | C         | 2018, 2019, 2020 |
| TRE  | NB        | 2019, 2020       |
| ACJ  | C         | 2018, 2019, 2020 |
| ACJ  | NB        | 2019, 2020       |


> ⚠️ Note that `data/` is in the `.gitignore` this means you have to run
> the data import script at least once regardless and that anything you
> export to the data folder will not be pushed to the repo and others
> will not be able to use that output unless you tell them which script
> to run to source that output.

## Script naming structure

Each [subtask](https://docs.google.com/spreadsheets/d/1G2w4rHiUkQ1iI5b7U_5dhyf1U87eOyaMcTBNFT4uq3w)
related to/needing a
coding workflow should be contained within its own script and should be
named starting with the subtask number and a brief descriptive name.
Scripts should be placed in the `scripts/` folder. This means we can
keep track of each task separately.

## Working on subtasks - using pull requests

Ideally each subtask should be on a new branch. This means that each subtask
can be turned into a pull request (PR) allowing us to easily see the full
commit history for that subtask and also allows subgroup members to request
reviews/feedback from each other as well as have conversation threads. PRs can  
initially be marked as drafts and once ready (i.e. completed) it can be
marked as ready for review and then merged into the `master` branch.

branches should be named after the subtask code - same for the PR (although this
can be a bit more comprehensive/descriptive).

### DC2 - Placeholder chemical traits data

Code for scraping and appending chemical traits data to the traits dataset can
be found in `scripts/DC2_chem_trait_database.R`. running this script will result
in having the `traits` df in your R environment also containing said chemical
traits.

We have scraped traits data from the BIEN database and TRY. Not all traits
data are available in BIEN and/or TRY so we do not have any data for N<sub>15</sub>
isotopes as well as N:P. Of the other chemical traits data we do not have full coverage
(even at the family level) for all of the traits. In addition we've used the
same trait mean/value across the dataset _i.e._ there is no variation between
species/genera/families - mostly because this is placeholder data it seems
wasteful to expend energy on trying to generate variation across sites

A quick overview of trait distribution featuring scraped chem traits:

![](https://github.com/TanyaS08/PFTC5_Gr3/blob/DC.2_chemtraits/output/traits_density_plots_w_chem.png?raw=true)

### DC5 - Allocation of Grimes CSR strategy for FT's

CSR scores were calcualted for each individual based using StrateFY. This process
is not automated so the output is saved in `data/processed/` and has been appended
to the original leaf traits dataset. it has also been integrated into
`scripts/0_data_import.R` so calling that script will autmatically 'add' the CSR
traits to the `traits` df to your environment

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
