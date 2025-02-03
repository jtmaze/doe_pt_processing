#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Libraries & File Paths -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library('tidyverse')
library('plotly')
library('glue')
library('readxl')

source("./scripts/qaqc_functions.R")
source("./scripts/data_read_functions.R")

compiled_path <-'./data/compiled_stage_2.xlsx'
meta_data_path <- './data/Wetland_well_metadata_1.xlsx'
status_path <- './data/Post_Processing_Well_Status.xlsx'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Inspect Site Meta Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unique_wetland_wells <- read_excel(meta_data_path, sheet="Wetland_and_well_info") %>% 
  pull('Site_ID') %>% 
  unique()

data_colnames <- colnames(read_excel(compiled_path, sheet=unique_wetland_wells[3]))
print(data_colnames)


print(unique_wetland_wells)

site <- unique_wetland_wells[12]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Explore Timeseries -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- site_ts_from_xlsx_sheet(compiled_path, site)
status <- fetch_post_process_status(status_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 

make_site_ts2(site_ts=data, 
              y_vars = c("depth", "sensor_depth", "Water_press"), 
              qaqc_df = qaqc)


