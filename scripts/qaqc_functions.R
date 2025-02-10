#####

library(tidyverse)
library(readxl)
library(plotly)
library(glue)
library(rlang)

calc_stages_from_offsets <- function(ts_data, pivot_history){
  
  data_full <- merge(ts_data, pivot_history, by="Site_ID", all=TRUE) %>% 
    # P to G and P to L columns are redundant, bc offsets are calculated. 
    select(
      -matches("^P_[LG]_cm_")
    ) %>% 
    # 2) For each offset_m_N, create depth_vN = sensor_depth - offset_m_N
    mutate(
      across(
        matches("^offset_m_(\\d+)$"), 
        ~ sensor_depth - .,
        .names = "depth_v{gsub('offset_m_', '', .col)}"
      )
    ) %>% 
    # 3) Calculate the average offset across all offset_m_N columns
    mutate(
      offset_avg = rowMeans(across(matches("^offset_m_(\\d+)$")), na.rm = TRUE),
      depth_avg  = sensor_depth - offset_avg
    )
  return(data_full)
}

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

make_checks_df <- function(data_full, qaqc){
  
  all_cols <- colnames(data_full)
  v_cols <- grep("depth_v", all_cols, value=TRUE)
  check_cols <- c(v_cols, "original_depth", "depth_avg")

  checks_list <- vector("list", length(check_cols))
  for(i in seq_along(check_cols)){
    check <- calculate_chk_ts_diffs(data_full, qaqc, check_cols[i])
    checks_list[[i]] <- check
  }
  
  checks <- bind_rows(checks_list)
  return(checks)
}

plot_checks <- function(checks_df){
# Makes a dot plot of the checks dataframe
  p <- ggplot(data=checks,
              mapping=aes(x=factor(version), y=diff)) +
    geom_point(aes(color = date),  
               size = 5, 
               stroke = 2) +
    stat_summary(fun=mean, geom="point", shape=4, size=5, stroke=3, color="red") +
    scale_color_gradient(low='green', high="purple") +
    geom_hline(yintercept=0, color="tomato", linewidth=2) +
    theme_bw() + 
    labs(title = paste0("Checking Site: ", site),
         y = "Field - Logger Water Level (m)",
         x = "Dataset Version",
         color = "Date")
  
  print(p)
}

quick_plot_offset <- function(offsets_to_use){
  # Convert the one-row wide data into a long format
  offsets_long <- offsets_to_use %>%
    pivot_longer(cols = everything(), 
                 names_to = "offset_name", 
                 values_to = "offset_value")
  
  # Calculate the mean of all offsets
  offsets_mean <- data.frame(
    offset_name = "Mean",
    offset_value = mean(offsets_long$offset_value, na.rm = TRUE)
  )
  
  # Create a simple plot
  p <- ggplot(offsets_long, aes(x = offset_name, y = offset_value)) +
    # Larger points for individual offsets
    geom_point(size = 7) +
    # Large red X for the mean offset
    geom_point(
      data = offsets_mean,
      aes(x = offset_name, y = offset_value),
      color = "red", 
      shape = 4,   # shape=4 is an 'X'
      size = 6,
      stroke = 2.5
    ) +
    labs(
      x = "Offset Type",
      y = "Offset (m)",
      title = "Quick Plot of Offsets"
    ) +
    theme_minimal() +
    # Ensure "Mean" appears as a discrete category on the x-axis after the others
    scale_x_discrete(limits = c(unique(offsets_long$offset_name), "Mean"))
  
  print(p)
}
#make_multiple_sites_ts <- function()
