**Plant functional trait course 5**

Cusco/Wayqecha, Peru - March 2020

**Group 3: Trait & taxonomic community response to fire and elevation**

**Contact:** dagmar.egelkraut@uib.no

Repository for a proposed manuscript focusing on the effects of fire and elevation on species composition and functional trait in the high Andean moist Puna grasslands. This repository is used to store code associated with the data analysis portion of the manuscript.

**Getting 'clean' data**

The raw data are called from _osf_ and then filtered based on the sites that we will be using in `scripts/0_data_import.R` which produces a `traits` and a `species` dataframe. You can 'run'/call this script by calling `source(here::here(path = "scripts/0_data_import.R"))` at the beginning of your script. Fromt here you can manipulate that data as needed.

**Script naming structure**

Each subtask related to/needing a coding workflow should be contained within its own script and should be named starting with the subtask number and a brief descriptive name. Scripts should be placed in the `scrpits/'` folder
