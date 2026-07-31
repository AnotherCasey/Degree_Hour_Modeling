library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)


### Identify and flag water temperature values failing QC tests
### Casey Harper

#Load and prepare the data

# Set current wd
setwd("C:/Users/zombi/Desktop/Capstone")

# Path to the CSV in current wd
path <- "YAQUINA_TEMP_QC_READY.csv"

# Sanity check: does the file exist?
file.exists(path)   # should return TRUE

# Read the CSV
df <- read_csv(path, show_col_types = FALSE) %>%
  mutate(temp_raw = temp,
  month_day = format(datetime, "%m-%d"),
  year = year(datetime),
  month = month(datetime)
  ) %>%
  filter(
    month_day >= "05-01",
    month_day < "11-01",
    # Keep only standard 10-minute timestamps:
    minute(datetime) %% 10 == 0
  )

# Load monthly site-specific temperature difference summary
## Contains standard deviations from the mean temp diff for QC thresholds

summary_path <- "C:/Users/zombi/Desktop/Capstone/TEMP_DIFF_SUMMARY_MONTHLY.csv"
file.exists(summary_path)

# Calculate threshold
temp_diff_summary_monthly <- read_csv(summary_path, show_col_types = FALSE) %>%
  select(site_ID, month, mean_temp_diff, sd_temp_diff) %>%
  mutate(
    threshold = abs(mean_temp_diff) + 4 * sd_temp_diff
  )

# Load monthly site-specific temperature climatology summary
# Used for QARTOD-style Climatology Test

clim_path <- "Temp_Summary_Monthly.csv"
file.exists(clim_path)

temp_summary_monthly <- read_csv(clim_path, show_col_types = FALSE) %>%
  select(
    site_ID,
    month,
    mean_temp_season,
    sd_temp_season,
    temp_upper_4sd,
    temp_lower_4sd
  )

#
# QC Tests
#

# Data point n-1 exceeds a selected threshold relative to calculated mean of neighboring data points (n-2 and n0) to form spike reference

df_qc <- df %>%
  left_join(temp_diff_summary_monthly, by = c("site_ID", "month")) %>%
  left_join(temp_summary_monthly, by = c("site_ID", "month")) %>%
  group_by(site_ID, year) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(
    prev_temp = lag(temp_raw),
    next_temp = lead(temp_raw),
    
    prev_datetime = lag(datetime),
    next_datetime = lead(datetime),
    
    time_diff_min = as.numeric(
      difftime(datetime, prev_datetime, units = "mins")
    ),
    
    next_time_diff_min = as.numeric(
      difftime(next_datetime, datetime, units = "mins")
    ),
    
    temp_diff = temp_raw - prev_temp,
    
    neighbor_mean = if_else(
      time_diff_min == 10 & next_time_diff_min == 10,
      (prev_temp + next_temp) / 2,
      NA_real_
    ),
    
    neighbor_residual = temp_raw - neighbor_mean,
    
    flag_neighbor_spike =
      !is.na(temp_raw) &
      !is.na(neighbor_mean) &
      abs(neighbor_residual) > threshold,
    
    flag_rate_change =
      !is.na(temp_raw) &
      !is.na(temp_diff) &
      time_diff_min == 10 &
      abs(temp_diff - mean_temp_diff) > threshold,
    
    flag_climatology =
      !is.na(temp_raw) &
      !is.na(temp_lower_4sd) &
      !is.na(temp_upper_4sd) &
      (
        temp_raw < temp_lower_4sd |
          temp_raw > temp_upper_4sd
      ),
    
    flag_not_evaluated =
      !is.na(temp_raw) &
      is.na(neighbor_mean) &
      (
        is.na(temp_diff) |
          time_diff_min != 10
      ) &
      (
        is.na(temp_lower_4sd) |
          is.na(temp_upper_4sd)
      ),
    
    n_qc_flags = rowSums(
      cbind(
        flag_neighbor_spike,
        flag_rate_change,
        flag_climatology
      ),
      na.rm = TRUE
    ),
    
    qc_flag = case_when(
      is.na(temp_raw) ~ "missing",
      flag_not_evaluated ~ "not_evaluated",
      n_qc_flags >= 2 ~ "multiple_flags",
      flag_neighbor_spike ~ "neighbor_spike",
      flag_rate_change ~ "rate_change",
      flag_climatology ~ "climatology",
      TRUE ~ "none"
    )
  ) %>%
  ungroup()

# Determine how far beyond the threshold each value is
df_qc <- df_qc %>%
  mutate(
    
    neighbor_excess =
      abs(neighbor_residual) - threshold,
    
    rate_excess =
      abs(temp_diff - mean_temp_diff) - threshold,
    
    clim_excess = case_when(
      temp_raw > temp_upper_4sd ~
        temp_raw - temp_upper_4sd,
      
      temp_raw < temp_lower_4sd ~
        temp_lower_4sd - temp_raw,
      
      TRUE ~
        0
    ),
    # Calculate an extremity score for distance beyond threshold
    neighbor_severity = 
      abs(neighbor_residual) / threshold,
    
    rate_change_severity =
      abs(temp_diff - mean_temp_diff) / threshold,
    
    climatology_severity =
      abs(temp_raw - mean_temp_season) /
      (4 * sd_temp_season)
    
  )


# Useful Summaries

# df_qc %>%
#   count(qc_flag) %>%
#   mutate(
#     pct = round(100 * n / sum(n), 3)
#   )

bind_rows(
  
  df_qc %>%
    filter(flag_neighbor_spike) %>%
    summarise(
      test = "neighbor_spike",
      n_flags = n(),
      median_severity = median(neighbor_severity, na.rm = TRUE),
      mean_severity = mean(neighbor_severity, na.rm = TRUE),
      p95_severity = quantile(
        neighbor_severity,
        0.95,
        na.rm = TRUE
      )
    ),
  
  df_qc %>%
    filter(flag_rate_change) %>%
    summarise(
      test = "rate_change",
      n_flags = n(),
      median_severity = median(rate_change_severity, na.rm = TRUE),
      mean_severity = mean(rate_change_severity, na.rm = TRUE),
      p95_severity = quantile(
        rate_change_severity,
        0.95,
        na.rm = TRUE
      )
    ),
  
  df_qc %>%
    filter(flag_climatology) %>%
    summarise(
      test = "climatology",
      n_flags = n(),
      median_severity = median(climatology_severity, na.rm = TRUE),
      mean_severity = mean(climatology_severity, na.rm = TRUE),
      p95_severity = quantile(
        climatology_severity,
        0.95,
        na.rm = TRUE
      )
    )
  
)

# Read flagged observations with matched tide values
flagged_tides <- read_csv(
  "Full_Flagged_Points_With_Tides.csv",
  show_col_types = FALSE,
  col_types = cols(
    datetime = col_character()
  )
) %>%
  mutate(
    datetime = ymd_hms(
      datetime,
      tz = "UTC",
      quiet = TRUE
    )
  ) %>%
  select(
    site_ID,
    datetime,
    tide_m
  )

# Join tide_m onto df_qc
df_qc <- df_qc %>%
  left_join(
    flagged_tides,
    by = c("site_ID", "datetime")
  )

# Verify the join

df_qc %>%
  summarise(
    rows_with_tide = sum(!is.na(tide_m)),
    rows_without_tide = sum(is.na(tide_m))
  )

# Confirm all flagged rows received a tide value

# df_qc %>%
#   filter(qc_flag != "none", qc_flag != "missing") %>%
#   summarise(
#     flagged_rows = n(),
#     flagged_rows_with_tide = sum(!is.na(tide_m)),
#     flagged_rows_without_tide = sum(is.na(tide_m))
#   )

# Create a streamlined dataframe for QC 

# Create streamlined qc dataframe
df_qc_filter <- df_qc %>%
  select(
    site_ID,
    datetime,
    salinity,
    temp_raw,
    temp,
    prev_temp,
    next_temp,
    qc_flag,
    tide_m
  ) %>%
  arrange(site_ID, datetime)

# Remove flagged values only when:
# 1) Temperature >= 15°C
# 2) QC flagged
# 3) At least one indicator of a sensor anomaly is present:
#    - missing salinity
#    - missing previous temperature
#    - missing next temperature

df_qc_filter <- df_qc_filter %>%
  mutate(
    # Combine all applicable contextual removal reasons
    removal_reason = paste0(
      if_else(is.na(salinity), "missing_salinity;", ""),
      if_else(is.na(prev_temp), "gap_before;", ""),
      if_else(is.na(next_temp), "gap_after;", "")
    ),
    
    # Remove the final trailing semicolon
    removal_reason = sub(";$", "", removal_reason),
    
    # Remove only flagged temperatures >= 15°C that have
    # at least one contextual reason
    removed_qc = if_else(
      qc_flag != "none" &
        !is.na(temp) &
        temp >= 15 &
        removal_reason != "",
      1L,
      0L
    ),
    
    # Label all retained observations
    removal_reason = if_else(
      removed_qc == 1L,
      removal_reason,
      "kept"
    ),
    
    # Retain the row but replace removed temperatures with NA
    temp = if_else(
      removed_qc == 1L,
      NA_real_,
      temp
    )
  )

write_csv(
 df_qc_filter,
 "YAQUINA_TEMP_INTERPOLATION_READY.csv"
)

### Tide Exploration

# Explore tide distribution

# df_qc_filter %>%
#   filter(qc_flag != "none") %>%
#   summarise(
#     min = min(tide_m, na.rm = TRUE),
#     q10 = quantile(tide_m, 0.10, na.rm = TRUE),
#     q25 = quantile(tide_m, 0.25, na.rm = TRUE),
#     median = median(tide_m, na.rm = TRUE),
#     q75 = quantile(tide_m, 0.75, na.rm = TRUE),
#     q90 = quantile(tide_m, 0.90, na.rm = TRUE),
#     max = max(tide_m, na.rm = TRUE)
  # )

# Breakdown of how many flagged values removed after meeting criteria would still be removed
# by occurring during a high tide and broken down my flag type

# df_qc_filter %>%
#   filter(
#     qc_flag != "none",
#     temp >= 15,
#     !is.na(salinity),
#     !is.na(prev_temp),
#     !is.na(next_temp),
#     tide_m >= 2.14
#   ) %>%
#   count(qc_flag)

# Calculate the percentage of each flag type that occurred during the upper 10% of tides

# df_qc_filter %>%
#   filter(qc_flag != "none", temp >= 15) %>%
#   mutate(high_tide = tide_m >= 2.14) %>%
#   count(qc_flag, high_tide) %>%
#   group_by(qc_flag) %>%
#   mutate(percent = 100 * n / sum(n))


# Prepare dataframe for export

# df_qc_export <- df_qc %>%
#   filter(qc_flag != "missing", 
#          qc_flag != "none") %>%
#   select(
#     site_ID,
#     year,
#     month,
#     datetime,
#     salinity,
#     temp_raw,
#     threshold,
#     
#     prev_temp,
#     next_temp,
#     temp_diff,
#     time_diff_min,
#     
#     neighbor_mean,
#     neighbor_residual,
#     
#     mean_temp_season,
#     sd_temp_season,
#     temp_lower_4sd,
#     temp_upper_4sd,
#     
#     flag_neighbor_spike,
#     neighbor_severity,
#     flag_rate_change,
#     rate_change_severity,
#     flag_climatology,
#     climatology_severity,
#     flag_not_evaluated,
#     n_qc_flags,
#     
#     qc_flag
#   )
# 
# write_csv(
#  df_qc_export,
#  "TEMP_QC_FLAGS_17Jun2026.csv"
# )

# Optional - Save outputs
# -----------------------------
# write_csv(df_timestamp, "ENV_DATA_18_25_YAQUINA_FULLTIMESTAMP")
# write_csv(df_qc, "ENV_DATA_18_25_YAQUINA_neighbor_spike_qc.csv")
# write_csv(qc_summary, "ENV_DATA_18_25_YAQUINA_neighbor_spike_summary.csv")


#
# Create Plots
#

# Create output folder
# dir.create("QC_Flag_Plots_Monthly_17Jun", showWarnings = FALSE)
# 
# # Site-year combinations with data
# site_years <- df_qc %>%
#   distinct(site_ID, year) %>%
#   arrange(site_ID, year)
# 
# for (i in seq_len(nrow(site_years))) {
#   
#   site_to_plot <- site_years$site_ID[i]
#   year_to_plot <- site_years$year[i]
#   
#   plot_start <- as.Date(paste0(year_to_plot, "-05-01"))
#   plot_end   <- as.Date(paste0(year_to_plot, "-11-01"))
#   
#   test <- df_qc %>%
#     filter(
#       site_ID == site_to_plot,
#       year == year_to_plot
#     ) %>%
#     mutate(
#       plot_date = as.Date(datetime)
#     ) %>%
#     filter(
#       plot_date >= plot_start,
#       plot_date < plot_end
#     )
#   
#   flags <- test %>%
#     filter(
#       (flag_neighbor_spike & neighbor_severity >= 1.5) |
#         (flag_rate_change & rate_change_severity >= 2) |
#         (flag_climatology & climatology_severity >= 1.5)
#     )
#   
#   subtitle_text <- paste0(
#     "May 1–Nov 1; severity ",
#     nrow(flags),
#     " QC flags"
#   )
#   
#   p <- ggplot(test, aes(x = plot_date, y = temp_raw)) +
#     geom_line(color = "blue2", alpha = 0.35, linewidth = 0.25) +
#     geom_point(
#       data = flags,
#       aes(x = plot_date, y = temp_raw, color = qc_flag),
#       size = 2.2
#     ) +
#     scale_color_manual(
#       values = c(
#         "neighbor_spike" = "orange",
#         "rate_change" = "blue",
#         "climatology" = "green",
#         "multiple_flags" = "red"
#       ),
#       labels = c(
#         "neighbor_spike" = "Neighbor spike",
#         "rate_change" = "Rate change",
#         "climatology" = "Monthly mean",
#         "multiple_flags" = "Multiple flags"
#       ),
#       name = "QC flag"
#     ) +
#     scale_x_date(
#       limits = c(plot_start, plot_end),
#       date_breaks = "7 days",
#       date_labels = "%b %d"
#     ) +
#     scale_y_continuous(
#       breaks = seq(5, 45, by = 5)
#     ) +
#     coord_cartesian(
#       ylim = c(5, 45)
#     ) +
#     labs(
#       title = paste("QC Flags —", site_to_plot, year_to_plot),
#       subtitle = subtitle_text,
#       x = NULL,
#       y = "Temp (°C)"
#     ) +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
#       panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
#       panel.grid.minor = element_blank(),
#       legend.position = "bottom",
#       axis.text.x = element_text(angle = 45, hjust = 1)
#     )
#   
#   ggsave(
#     filename = file.path(
#       "QC_Flag_Plots_Monthly_17Jun",
#       paste0("QC_Flags_", site_to_plot, "_", year_to_plot, ".png")
#     ),
#     plot = p,
#     width = 14,
#     height = 10,
#     dpi = 300
#   )
# }

# Create output folder
# dir.create("QC_Flag_Plots_Zoomed_Severity_2", showWarnings = FALSE)
# 
# # Site-year combinations with data
# site_years <- df_qc %>%
#   distinct(site_ID, year) %>%
#   arrange(site_ID, year)
# 
# for (i in seq_len(nrow(site_years))) {
#   
#   site_to_plot <- site_years$site_ID[i]
#   year_to_plot <- site_years$year[i]
#   
#   # Full seasonal bounds
#   season_start <- as.Date(paste0(year_to_plot, "-05-01"))
#   season_end   <- as.Date(paste0(year_to_plot, "-11-01"))
#   
#   # Filter to one site and year
#   test <- df_qc %>%
#     filter(
#       site_ID == site_to_plot,
#       year == year_to_plot
#     ) %>%
#     mutate(
#       plot_date = as.Date(datetime)
#     ) %>%
#     filter(
#       plot_date >= season_start,
#       plot_date < season_end
#     )
#   
#   # Keep only flags with severity >= 2
#   flags <- test %>%
#     filter(
#       (flag_neighbor_spike & neighbor_severity >= 2) |
#         (flag_rate_change & rate_change_severity >= 2) |
#         (flag_climatology & climatology_severity >= 2)
#     )
#   
#   # Skip plot if there are no severity >= 1.5 flags
#   if (nrow(flags) == 0) {
#     next
#   }
#   
#   # Dynamic zoom window:
#   # 14 days before first severe flag through 14 days after last severe flag
#   plot_start <- min(flags$plot_date, na.rm = TRUE) - 14
#   plot_end   <- max(flags$plot_date, na.rm = TRUE) + 14
#   
#   # Keep zoom window inside May 1-Nov 1
#   plot_start <- max(plot_start, season_start)
#   plot_end   <- min(plot_end, season_end)
#   
#   # Filter background data to the zoom window
#   test_zoom <- test %>%
#     filter(
#       plot_date >= plot_start,
#       plot_date <= plot_end
#     )
#   
#   flags_zoom <- flags %>%
#     filter(
#       plot_date >= plot_start,
#       plot_date <= plot_end
#     )
#   
#   subtitle_text <- paste0(
#     "Severity ≥ 1.5; ",
#     nrow(flags_zoom),
#     " QC flags; showing 14 days before first flag through 14 days after last flag"
#   )
#   
#   p <- ggplot(test_zoom, aes(x = datetime, y = temp_raw)) +
#     geom_line(
#       color = "blue2",
#       alpha = 0.35,
#       linewidth = 0.25
#     ) +
#     geom_point(
#       data = flags_zoom,
#       aes(x = plot_date, y = temp_raw, color = qc_flag),
#       size = 2.2
#     ) +
#     scale_color_manual(
#       values = c(
#         "neighbor_spike" = "orange",
#         "rate_change" = "blue",
#         "climatology" = "green",
#         "multiple_flags" = "red"
#       ),
#       labels = c(
#         "neighbor_spike" = "Neighbor spike",
#         "rate_change" = "Rate change",
#         "climatology" = "Monthly mean",
#         "multiple_flags" = "Multiple flags"
#       ),
#       name = "QC flag"
#     ) +
#     scale_x_datetime(
#       limits = c(
#         as.POSIXct(plot_start),
#         as.POSIXct(plot_end + 1)
#       ),
#       date_breaks = "7 days",
#       date_labels = "%b %d"
#     ) +
#     scale_y_continuous(
#       breaks = seq(5, 45, by = 5)
#     ) +
#     coord_cartesian(
#       ylim = c(5, 45)
#     ) +
#     labs(
#       title = paste("Zoomed QC Flags —", site_to_plot, year_to_plot),
#       subtitle = subtitle_text,
#       x = NULL,
#       y = "Temp (°C)"
#     ) +
#     theme_minimal() +
#     theme(
#       panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
#       panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
#       panel.grid.minor = element_blank(),
#       legend.position = "bottom",
#       axis.text.x = element_text(angle = 45, hjust = 1)
#     )
#   
#   ggsave(
#     filename = file.path(
#       "QC_Flag_Plots_Zoomed_Severity_2",
#       paste0("QC_Flags_Zoomed_", site_to_plot, "_", year_to_plot, ".png")
#     ),
#     plot = p,
#     width = 14,
#     height = 10,
#     dpi = 300
#   )
# }


# Count how many flags occur on any given date
# multi_site_flag_dates <- df_qc %>%
#   filter(
#     qc_flag %in% c(
#       "neighbor_spike",
#       "rate_change",
#       "climatology",
#       "multiple_flags"
#     )
#   ) %>%
#   mutate(
#     flag_date = as.Date(datetime)
#   ) %>%
#   group_by(flag_date) %>%
#   summarise(
#     n_flags = n(),
#     n_sites_flagged = n_distinct(site_ID),
#     sites_flagged = paste(sort(unique(site_ID)), collapse = ", "),
#     .groups = "drop"
#   ) %>%
#   filter(n_sites_flagged >= 2) %>%
#   arrange(flag_date)
# 
# print(multi_site_flag_dates)
# 
# write_csv(
#   multi_site_flag_dates,
#   "MULTI_SITE_QC_FLAGS_BY_DATE.csv"
# )