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

# Example I)
# One of the offset values is dramatically wrong
# Time series shits dramatically in November 2023

# Example II)
# Offset appears to have changed gradually over the deployment
#site <- unique_wetland_wells[2]

# Example III)
# Example where first iteration of offset is better
site <- unique_wetland_wells[15]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Fetch Site Specific Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

data <- site_ts_from_xlsx_sheet(compiled_path, site)
status <- fetch_post_process_status(status_path, site)
qaqc <- fetch_water_checks(meta_data_path, site) 
pivot_hist <- fetch_pivot_history(meta_data_path, site)

# Merge the pivot history with the site data
data_full <- merge(data, pivot_hist, by="Site_ID", all=TRUE) 
# Merge the status notes with data_full 
data_full <- merge(data_full, 
                   status %>% select(c(Site_ID, Notes)), 
                   by="Site_ID", 
                   all=TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Apply different offsets -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

drop_cols <- c(
  "P_G/L_date_1",
  "P_G/L_date_2", 
  "P_G/L_date_3", 
  "P_G/L_date_4" 
)

data_full <- data_full %>% select(-any_of(drop_cols))

data_full <- data_full %>% 
  mutate(depth_v1 = sensor_depth - offset_m_1,
         depth_v2 = sensor_depth - offset_m_2,
         depth_v3 = sensor_depth - offset_m_3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Plot a site -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
make_site_ts(site_ts=data_full, 
              y_vars = c("depth","depth_v1", "depth_v2","depth_v3"), 
              qaqc_df = qaqc)


