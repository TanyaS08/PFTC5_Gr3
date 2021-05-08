# StrateFy Data Import

#After running the StrateFy excel doc (now saved as a .csv), we save it as a .csv and open here, ignoring the first 3 rows of headers and contextual information.


# Import data
### Call source script ---
source(here::here(path = "scripts/DC5_CSR_export_for_excel_StrateFy_classification.R"))

# Import the CSR StrateFy output (excel format)
csr_taxon <- read_excel("data/CSR/CSR_StrateFY_output.xlsx",
                        sheet = 1, col_names = TRUE, na = "NA", skip = 3)


# Append relevant material to main dataset

# We only want the 'species binomial' which is our taxon, and then a) 'Strategy_class', b) the S, C and R (%), and c) the RGB Red, Green and Blue for colour plotting, if we every wanted it. Relabel columns from StrateFy.


# Subset to only columns of interest
csr_taxon_subset <-
  csr_taxon %>% select(`species binomial`, # Our provided taxon
                       `Strategy class`,   # The derived CSR strategy class
                       `S (%)`, `C (%)`, `R (%)`) # The specific C,S,R values


# Relabel columns
csr_taxon_subset <- rename(csr_taxon_subset, c(taxon = `species binomial`,
                                               csr_strategy = `Strategy class`,
                                               s_percent = `S (%)`,
                                               c_percent = `C (%)`,
                                               r_percent = `R (%)`))

# Merge back with original traits_wide dataset, based on taxon ID.
traits_csr <- left_join(traits_wide, csr_taxon_subset, by="taxon") %>% 
  pivot_longer(cols = c(dry_mass_g, ldmc, leaf_area_cm2, leaf_thickness_mm,
                        plant_height_cm,
                        sla_cm2_g, wet_mass_g),
               names_to = 'trait',
               values_to = 'value')

dir.create("data/processed")
write.csv(traits_csr, file = "data/processed/traits_csr_wide.csv")
