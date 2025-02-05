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

calculate_chk_ts_diffs <- function(ts, qaqc_df, version){
# Calculates the difference between sensor timeseries and the QAQC dataframe.
# Inputs: 
#   - ts: a dataframe with timeseries data. Must include a column named "Date" and 
#         a numeric column corresponding to the value specified by 'version'.
#   - qaqc_df: a dataframe with water level checks and dates. Must include columns "date" and "meter".
#   - version: a string indicating the column name in 'ts' to use.
# Returns:
#   - A dataframe with the verion, date, diff, chk_m and logger_date_mean_trimmed
  results_list <- vector("list", nrow(qaqc_df))
  for(i in seq_len(nrow(qaqc_df))) {
    # Extract the observation date and QAQC meter value for the current row.
    obs_date <- qaqc_df$date[i]
    chk_m <- qaqc_df$meter[i]
    logger_date <- ts %>% 
      mutate(Date_only = as.Date(Date)) %>% 
      filter(Date_only == obs_date) %>% 
      pull(!!sym(version))
    
    # Handle cases where there's no logger data
    if (length(logger_date) == 0) {
      results_list[[i]] <- tibble(
        version=version,
        date=obs_date,
        diff=NA,
        chk_m=chk_m,
        logger_date_mean_trimmed=NA
      )
      next
    }
    # On download dates, PT might be above ground for a while. 
    # The date's mean omits observatoins +-2 sd to prevent this
    logger_date_mean <- mean(logger_date, na.rm=TRUE)
    logger_date_sd <- sd(logger_date, na.rm=TRUE)
    trimmed <- logger_date[abs(logger_date - logger_date_mean) <= 2 * logger_date_sd]
    logger_date_mean_trimmed <- mean(trimmed)
    
    diff <- chk_m - logger_date_mean_trimmed
    results_list[[i]] <- tibble(
      version=version,
      date=obs_date,
      diff=diff,
      chk_m=chk_m,
      logger_date_mean_trimmed=logger_date_mean_trimmed
    )
  }
  
  df <- bind_rows(results_list)
  return(df)
}

#make_multiple_sites_ts <- function()
