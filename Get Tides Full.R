# Pull NOAA tide station data matched to timestamp to water levels at time of water temperature measurement

library(tidyverse)
library(lubridate)
library(httr2)
library(ggplot2)

# Path to CSV
path <- "C:/Users/zombi/Desktop/Capstone/TEMP_QC_FLAGS_17Jun2026.csv"

# Sanity check: does the file exist?
file.exists(path)   # should return TRUE

# Read the CSV
dat_tides <- read_csv(
  path,
  show_col_types = FALSE,
  col_types = cols(
    datetime = col_character()
  )
) |>
  mutate(
    datetime_utc = ymd_hms(
      datetime,
      tz = "UTC",
      quiet = TRUE
    ),
    year = year(datetime_utc)
  )

station <- "9435380" # South Beach, OR

#--------------------------------------------------
# Use the complete set of flagged observations
#--------------------------------------------------

site_year_subset <- dat_tides |>
  filter(
    !is.na(datetime_utc),
    !is.na(qc_flag),
    qc_flag != "none"
  )

# Check how many flagged observations will be matched
site_year_subset |>
  count(qc_flag, name = "n_flags") |>
  arrange(desc(n_flags))

# Identify only the year-month combinations actually represented for pulling tide data
months_needed <- site_year_subset |>
  transmute(
    year = lubridate::year(datetime),
    month = lubridate::month(datetime)
  ) |>
  distinct() |>
  arrange(year, month)


# years needed for NOAA query
years_needed <- sort(unique(site_year_subset$year))

# Monthly NOAA 6-minute water level pull
get_6min_water_level_month <- function(year, month, station) {
  start_date <- ymd(sprintf("%s-%02d-01", year, month))
  end_date <- ceiling_date(start_date, "month") - days(1)
  
  req <- request("https://api.tidesandcurrents.noaa.gov/api/prod/datagetter") |>
    req_url_query(
      product = "water_level",
      application = "capstone_project",
      begin_date = format(start_date, "%Y%m%d"),
      end_date = format(end_date, "%Y%m%d"),
      station = station,
      time_zone = "gmt",
      units = "metric",
      datum = "MLLW",
      format = "json"
    )
  
  json <- req_perform(req) |> resp_body_json()
  
  if (!is.null(json$error) || is.null(json$data)) {
    warning(paste("NOAA error:", json$error$message))
    return(tibble())
  }
  
  tibble(
    tide_datetime_utc = ymd_hm(map_chr(json$data, "t"), tz = "UTC"),
    tide_m = as.numeric(map_chr(json$data, "v"))
  )
}

tides_6min <- months_needed |>
  pmap_dfr(
    \(year, month) {
      get_6min_water_level_month(
        year = year,
        month = month,
        station = station
      )
    }
  ) |>
  filter(
    !is.na(tide_datetime_utc),
    !is.na(tide_m)
  ) |>
  arrange(tide_datetime_utc) |>
  distinct(tide_datetime_utc, .keep_all = TRUE)

nearest_tide_match <- function(temp_datetime, tide_datetime, tide_m) {
  temp_num <- as.numeric(temp_datetime)
  
  tide_df <- tibble(
    tide_datetime = tide_datetime,
    tide_m = tide_m
  ) |>
    filter(!is.na(tide_datetime), !is.na(tide_m)) |>
    arrange(tide_datetime)
  
  tide_num <- as.numeric(tide_df$tide_datetime)
  
  before_idx <- findInterval(temp_num, tide_num)
  after_idx <- before_idx + 1
  
  before_idx[before_idx < 1] <- 1
  after_idx[after_idx > length(tide_num)] <- length(tide_num)
  
  before_diff <- abs(temp_num - tide_num[before_idx])
  after_diff <- abs(temp_num - tide_num[after_idx])
  
  nearest_idx <- ifelse(before_diff <= after_diff, before_idx, after_idx)
  
  tibble(
    tide_datetime_utc = tide_df$tide_datetime[nearest_idx],
    tide_datetime = with_tz(tide_df$tide_datetime[nearest_idx], "America/Los_Angeles"),
    tide_m = tide_df$tide_m[nearest_idx],
    tide_time_diff_min = abs(as.numeric(difftime(
      temp_datetime,
      tide_df$tide_datetime[nearest_idx],
      units = "mins"
    )))
  )
}

nearest_tides <- nearest_tide_match(
  temp_datetime = site_year_subset$datetime_utc,
  tide_datetime = tides_6min$tide_datetime_utc,
  tide_m = tides_6min$tide_m
)

temp_with_tide <- bind_cols(site_year_subset, nearest_tides)

# Check max time mismatch
temp_with_tide |>
  summarize(
    max_tide_time_diff_min = max(tide_time_diff_min, na.rm = TRUE),
    mean_tide_time_diff_min = mean(tide_time_diff_min, na.rm = TRUE)
  )

# Flagged-only points for highlighting
flagged_points <- temp_with_tide |>
  filter(!is.na(qc_flag), qc_flag != "none")

flag_tides <- temp_with_tide |>
  select(
    site_ID,
    datetime,
    datetime_utc,
    temp_raw,
    qc_flag,
    tide_m,
    tide_datetime,
    tide_time_diff_min
  ) |>
  arrange(site_ID, datetime)

#--------------------------------------------------
# Validate complete matching
#--------------------------------------------------

match_summary <- temp_with_tide |>
  summarize(
    total_flagged_rows = n(),
    matched_tide_rows = sum(!is.na(tide_m)),
    unmatched_tide_rows = sum(is.na(tide_m)),
    max_time_difference_min = max(
      tide_time_diff_min,
      na.rm = TRUE
    ),
    mean_time_difference_min = mean(
      tide_time_diff_min,
      na.rm = TRUE
    )
  )

print(match_summary)

# Show any observations that were not matched
unmatched_flags <- temp_with_tide |>
  filter(
    is.na(tide_m) |
      is.na(tide_datetime_utc)
  )

print(unmatched_flags)

write_csv(
  flag_tides,
  "C:/Users/zombi/Desktop/Capstone/Full_Flagged_Points_With_Tides.csv"
)


#-----------------------------
# Plotting
#-----------------------------

# library(tidyverse)
# library(lubridate)

# # Read full temperature dataset and make sure it is parsed in the same timezone
# 
# temp_full <- read_csv(
#   "C:/Users/zombi/Desktop/Capstone/YAQUINA_TEMP_QC_READY.csv",
#   show_col_types = FALSE
# ) |>
#   mutate(
#     datetime = parse_date_time(
#       datetime,
#       orders = c("ymd HMS", "ymd HM"),
#       tz = "America/Los_Angeles"
#     )
#   )

# If needed, uncomment if datetime is not automatically parsed:
# temp_full <- temp_full |>
#   mutate(
#     datetime = parse_date_time(
#       datetime,
#       orders = c("ymd HMS", "ymd HM"),
#       tz = "America/Los_Angeles"
#     )
#   )

#-----------------------------
# Plot settings
#-----------------------------
# plot_site  <- "KS"
# plot_year  <- 2021
# plot_month <- 5
# 
# #-----------------------------
# # Filter full temperature data
# #-----------------------------
# temp_plot <- temp_full |>
#   filter(
#     site_ID == plot_site,
#     datetime >= ymd_hms("2021-05-11 00:00:00",
#                         tz = "America/Los_Angeles"),
#     datetime <= ymd_hms("2021-05-19 23:59:59",
#                         tz = "America/Los_Angeles")
#   )
# 
# #-----------------------------
# # Filter flagged observations
# #-----------------------------
# flags_plot <- flag_tides |>
#   filter(
#     site_ID == plot_site,
#     datetime >= ymd_hms("2021-05-11 00:00:00",
#                         tz = "America/Los_Angeles"),
#     datetime <= ymd_hms("2021-05-19 23:59:59",
#                         tz = "America/Los_Angeles")
#   ) |>
#   select(site_ID, datetime, qc_flag, tide_m, tide_datetime, tide_time_diff_min) |>
#   left_join(
#     temp_plot |>
#       select(site_ID, datetime, flag_temp = temp),
#     by = c("site_ID", "datetime")
#   )
# 
# #-----------------------------
# # Filter NOAA tide data
# #-----------------------------
# tide_plot <- tides_6min |>
#   mutate(
#     tide_datetime = with_tz(
#       tide_datetime_utc,
#       "America/Los_Angeles"
#     )
#   ) |>
#   filter(
#     tide_datetime >= ymd_hms("2021-05-11 00:00:00",
#                              tz = "America/Los_Angeles"),
#     tide_datetime <= ymd_hms("2021-05-19 23:59:59",
#                              tz = "America/Los_Angeles")
#   )
# 
# #-----------------------------
# # Scale tide to temperature axis
# #-----------------------------
# temp_range <- range(temp_plot$temp, na.rm = TRUE)
# tide_range <- range(tide_plot$tide_m, na.rm = TRUE)
# 
# scale_factor <- diff(temp_range) / diff(tide_range)
# 
# tide_plot <- tide_plot |>
#   mutate(
#     tide_scaled =
#       (tide_m - tide_range[1]) * scale_factor +
#       temp_range[1]
#   )
# 
# #-----------------------------
# # Plot
# #-----------------------------
# ggplot() +
#   
#   # Full temperature record
#   geom_line(
#     data = temp_plot,
#     aes(
#       x = datetime,
#       y = temp
#     ),
#     color = "black",
#     linewidth = 0.35
#   ) +
#   
#   # NOAA tide record
#   geom_line(
#     data = tide_plot,
#     aes(
#       x = tide_datetime,
#       y = tide_scaled
#     ),
#     color = "steelblue",
#     linewidth = 0.60
#   ) +
#   
#   # Flagged observations
#   geom_point(
#     data = flags_plot,
#     aes(
#       x = datetime,
#       y = flag_temp,
#       color = qc_flag
#     ),
#     size = 2.5
#   ) +
#   
#   scale_y_continuous(
#     name = "Temperature (°C)",
#     
#     breaks = seq(
#       floor(min(temp_plot$temp, na.rm = TRUE) / 2) * 2,
#       ceiling(max(temp_plot$temp, na.rm = TRUE) / 2) * 2,
#       by = 2
#     ),
#     
#     sec.axis = sec_axis(
#       transform = ~ (. - temp_range[1]) / scale_factor + tide_range[1],
#       
#       name = "Water level (m MLLW)",
#       
#       breaks = seq(
#         floor(min(tide_plot$tide_m, na.rm = TRUE) * 2) / 2,
#         ceiling(max(tide_plot$tide_m, na.rm = TRUE) * 2) / 2,
#         by = 0.5
#       )
#     )
#   ) +
# 
#   scale_x_datetime(
#     date_breaks = "1 day",
#     date_labels = "%b %d"
#   ) +
#   
#   labs(
#     title = paste0(
#       plot_site, " ",
#       month.name[plot_month], " ",
#       plot_year,
#       ": Temperature and Tide"
#     ),
#     x = "Date",
#     color = "QC Flag"
#   ) +
#   
#     theme(
#       plot.title = element_text(face = "bold"),
#       legend.position = "bottom",
#       axis.text.x = element_text(angle = 45, hjust = 1),
#       
#       axis.title.y.left = element_text(color = "black"),
#       axis.text.y.left = element_text(color = "black"),
#       
#       axis.title.y.right = element_text(color = "steelblue"),
#       axis.text.y.right = element_text(color = "steelblue"),
#       
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.grid.major.y = element_line(color = "grey90"),
#       panel.background = element_rect(fill = "white", color = "black")
#     )


# Plot flags with tide levels
# 
# # Flagged points only
# flagged_points <- temp_with_tide |>
#   filter(!is.na(qc_flag), qc_flag != "none")
# 
# months_to_plot <- 5:10
# 
# make_monthly_tide_temp_plot <- function(df) {
#   
#   site_name <- unique(df$site_ID)
#   plot_year <- unique(df$year)
#   
#   df <- df |>
#     mutate(month = month(datetime))
#   
#   tide_df <- tides_6min |>
#     mutate(
#       tide_datetime = with_tz(tide_datetime_utc, "America/Los_Angeles"),
#       year = year(tide_datetime),
#       month = month(tide_datetime)
#     ) |>
#     filter(
#       year == plot_year,
#       month %in% months_to_plot
#     )
#   
#   # Use fixed scaling across May-Oct for this site-year
#   temp_range <- range(df$temp_raw, na.rm = TRUE)
#   tide_range <- range(tide_df$tide_m, na.rm = TRUE)
#   
#   scale_factor <- diff(tide_range) / diff(temp_range)
#   
#   df <- df |>
#     filter(month %in% months_to_plot) |>
#     mutate(
#       temp_scaled = (temp_raw - temp_range[1]) * scale_factor + tide_range[1],
#       month_label = factor(
#         month.name[month],
#         levels = month.name[months_to_plot]
#       )
#     )
#   
#   tide_df <- tide_df |>
#     mutate(
#       month_label = factor(
#         month.name[month],
#         levels = month.name[months_to_plot]
#       )
#     )
#   
#   ggplot() +
#     geom_line(
#       data = tide_df,
#       aes(x = tide_datetime, y = tide_m),
#       color = "steelblue",
#       linewidth = 0.4
#     ) +
#     geom_point(
#       data = df,
#       aes(x = datetime, y = temp_scaled, color = qc_flag),
#       size = 2.2,
#       alpha = 0.9
#     ) +
#     facet_wrap(
#       ~ month_label,
#       scales = "free_x",
#       ncol = 2
#     ) +
#     scale_y_continuous(
#       name = "Water level, m MLLW",
#       sec.axis = sec_axis(
#         trans = ~ (. - tide_range[1]) / scale_factor + temp_range[1],
#         name = "Temperature, °C"
#       )
#     ) +
#     labs(
#       title = paste0(site_name, " ", plot_year, ": temperature flags and tide height by month"),
#       x = "Datetime",
#       color = "QC flag"
#     ) +
#     theme_bw() +
#     theme(
#       legend.position = "bottom",
#       strip.text = element_text(size = 11),
#       axis.text.x = element_text(angle = 45, hjust = 1)
#     )
# }
# 
# # 3. Create one plot per site-year-month
# monthly_tide_temp_plots <- temp_with_tide |>
#   filter(month(datetime) %in% months_to_plot) |>
#   group_by(site_ID, year, month = month(datetime)) |>
#   group_split() |>
#   map(make_monthly_tide_temp_plot)
# 
# # 4. View first monthly plot
# monthly_tide_temp_plots[[1]]


