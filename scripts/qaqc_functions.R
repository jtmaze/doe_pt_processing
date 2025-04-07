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
  
  if ("revised_depth" %in% all_cols) {
    check_cols <- c(check_cols, "revised_depth")
  }

  checks_list <- vector("list", length(check_cols))
  for(i in seq_along(check_cols)){
    check <- calculate_chk_ts_diffs(data_full, qaqc, check_cols[i])
    checks_list[[i]] <- check
  }
  
  checks <- bind_rows(checks_list)
  return(checks)
}

plot_checks <- function(checks_df, site){
# Makes a dot plot of the checks dataframe
  p <- ggplot(data=checks_df,
              mapping=aes(x=factor(version), y=diff)) +
    geom_point(aes(color = date),  
               size = 5, 
               stroke = 2) +
    stat_summary(fun=mean, geom="point", shape=4, size=5, stroke=3, color="red") +
    scale_color_gradient(low='green', high="purple") +
    geom_hline(yintercept=0, color="tomato", linewidth=2) +
    theme_bw() + 
    ylim(-0.3, 0.3) +
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

quick_plot_offset2 <- function(offsets_to_use){
  
  # 1) Identify all columns for dates and offsets based on a common pattern
  date_cols   <- grep("^P_G/L_date_[0-9]+$", names(offsets_to_use), value = TRUE)
  offset_cols <- grep("^offset_m_[0-9]+$",   names(offsets_to_use), value = TRUE)
  
  # 2) Pivot offset columns to long format
  offsets_long <- offsets_to_use %>%
    select(all_of(offset_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "offset_id",
      names_pattern = "^offset_m_(.*)$",   # capture the trailing number
      values_to = "offset_value"
    )
  
  # 3) Pivot date columns to long format
  dates_long <- offsets_to_use %>%
    select(all_of(date_cols)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "date_id",
      names_pattern = "^P_G/L_date_(.*)$", # capture the trailing number
      values_to = "date_value"
    )
  
  # 4) Join offsets and dates on their shared index (the trailing number in the names)
  combined <- left_join(
    offsets_long,
    dates_long,
    by = c("offset_id" = "date_id")
  )
  
  # 5) Calculate the mean offset (ignoring dates). 
  #    We'll give it a special 'offset_id' called "Mean" for plotting on the same axis.
  offsets_mean <- data.frame(
    offset_id   = "Mean",
    offset_value = mean(combined$offset_value, na.rm = TRUE),
    date_value   = NA  # No date for the mean point
  )
  
  # 6) Create the plot
  p <- ggplot(combined, 
              aes(x = offset_id, y = offset_value, color = factor(date_value))) +
    geom_point(size = 7) +
    # Large red X for the mean offset (turn off inherit.aes so it doesn't try to use color = factor(NA))
    geom_point(
      data = offsets_mean,
      aes(x = offset_id, y = offset_value),
      color = "red", 
      shape = 4,   # shape=4 is an 'X'
      size = 6,
      stroke = 2.5,
      inherit.aes = FALSE
    ) +
    labs(
      x = "Offset Index",
      y = "Offset (m)",
      color = "Date",
      title = "Quick Plot of Offsets Colored by Date"
    ) +
    theme_minimal() +
    theme(
      # Make axis titles and text bold
      axis.title = element_text(face = "bold", size=16),
      axis.text  = element_text(face = "bold", size=12),
      
      # Make gridlines thicker
      panel.grid.major = element_line(size = 1),  # Adjust size for major gridlines
      panel.grid.minor = element_line(size = 0.5) # Adjust size for minor gridlines (if present)
    ) +
    # Ensure "Mean" appears on the x-axis after all numeric indices
    scale_x_discrete(limits = c(unique(combined$offset_id), "Mean"))
  
  print(p)
}

