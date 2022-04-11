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
    - [Cleaning/filtering the data](#cleaning/filtering-the-data)
    - [Datasets to use](#Datasets-to-use)
  - [Script naming structure](#script-naming-structure)
  - [Working on subtasks - using pull requests](#working-on-subtasks---using-pull-requests)
  - [Brief summaries of subtasks](#brief-summaries-of-subtasks)
    - [DC5 - Allocation of Grimes CSR strategy for FT's](#dc5---allocation-of-grimes-csr-strategy-for-fts)

## Downloading and 'cleaning' the data

This can be done by running the `scripts/0_data_import.R` file. This will then
download the data as well as filter/clean the data so that we only have the data
that we will be using for our analyses.

### Cleaning/filtering the data

The `scripts/0_data_import.R` code will also filter out the the sites
relevant for our analyses. As we are downloading the _entire_ Puna
dataset form _osf_ we need to filter the data so that we have the following
sites and treatments from the following years:

| Site | Treatment | Dry Season       | Wet Season       |
| :----| :-------- | :--------------- | :--------------- |
| QUE  | C         | 2019             | 2019             |
| QUE  | NB        |                  | 2020             |
| TRE  | C         | 2019             | 2018, 2019, 2020 |
| TRE  | NB        | 2019             | 2019, 2020       |
| ACJ  | C         | 2019             | 2018, 2019, 2020 |
| ACJ  | NB        | 2019             | 2020             |


### Datasets to use

Given the Frankenstein nature of the dataset we have used the 
[traitstrap (see preprint here)](https: //doi.org/10.22541/au.162196147.76797968/v1)
package to help smooth out some of the gaps in the data. A
short vignette can be found [here](https://github.com/richardjtelford/traitstrap/blob/main/vignettes/traitstrap-workflow.Rmd).

The workflow itself is in `scripts/DA1_traitstrap.R` if you are interested.
The bootstrapped data can be found in `data/processed` as two different datasets.
The `traits_traitstrapped_raw.csv` has trait values at the individual level (for
the different treatment/sites/plot_id combos) and is 'similar' to the raw data 
downloaded to `osf` in terms of how it looks but of course the data are generated
using the bootstrapping simulations from traitstrap. The other data file
`traits_traitstrapped_moments.csv` has the moment summaries for the distributions
for the different treatment/sites/plot_id combos. That is this dataset will give you
the equivalent of the community weighted mean for example.

In summary: import and use the `traits_traitstrapped_raw.csv` dataset if working with
individual trait-level questions and `traits_traitstrapped_moments.csv` when concerned
with community-level work. 

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

## Brief summaries of subtasks

### DC5 - Allocation of Grimes CSR strategy for FT's

CSR scores were calculated for each individual based on trait values 
using StrateFY. This process
is not automated so the output is saved in `data/processed/` and has been appended
to the original leaf traits dataset. it has also been integrated into
`scripts/0_data_import.R` so calling that script will automatically 'add' the CSR
traits to the `traits` df to your environment