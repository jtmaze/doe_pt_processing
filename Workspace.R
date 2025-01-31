#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Libraries & File Paths -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library('tidyverse')
library('plotly')
library('glue')
library('readxl')

source("./scripts/qaqc_functions.R")

compiled_path <-'./data/compiled_stage_2.xlsx'
meta_data_path <- './data/Wetland_well_metadata_1.xlsx'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Inspect Site Meta Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique_wetland_wells <- read_excel(meta_data_path, sheet="Wetland_and_well_info") %>% 
  pull('Site_ID') %>% 
  unique()

print(unique_wetland_wells)

site <- unique_wetland_wells[50]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Explore Timeseries -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test <- site_ts_from_xlsx_sheet(compiled_path, site)
t2 <- fetch_water_checks(meta_data_path, site) 

make_site_ts(test, "depth", t2)


