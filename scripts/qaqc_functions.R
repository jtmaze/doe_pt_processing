#####

library(tidyverse)
library(readxl)
library(plotly)
library(glue)
library(rlang)

make_site_ts <- function(site_ts, 
                         y_vars, # A vector of y variables to plot
                         qaqc_df = NULL, 
                         trace_names = NULL) {
  
  # Get site ID
  site_id <- site_ts %>% 
    pull(Site_ID) %>% 
    unique()
  
  # Initialize plot
  fig <- plot_ly()
  
  # Add trace for each y variable
  for (i in seq_along(y_vars)) {
    y_var <- y_vars[i]
    name <- if (!is.null(trace_names)) trace_names[i] else y_var
    
    fig <- fig %>%
      add_trace(
        data = site_ts,
        x = ~Date,
        y = as.formula(paste0("~", y_var)),
        name = name,
        type = "scatter",
        mode = "lines"
      )
  }
  
  # Add QAQC points if provided
  if (!is.null(qaqc_df)) {
    fig <- fig %>%
      add_trace(
        data = qaqc_df,
        x = ~date,
        y = ~meter,
        type = "scatter",
        mode = "markers",
        marker = list(color = "red", size = 10),
        name = "QA/QC"
      )
  }
  
  fig %>% 
    layout(title = glue("PT Data for Wetland {site_id}"))
}



#make_multiple_sites_ts <- function()
