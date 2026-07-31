# ============================================================
# Linear harmonic regression interpolation
# Fixed periods:
#   - 24-hour diel cycle
#   - 12.42-hour tidal cycle
#
# Input dataframe: df_qc_filter
# Required columns:
#   site_ID, datetime, temp
# ============================================================

# Set current working directory
setwd("C:/Users/zombi/Desktop/Capstone")

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(ggh4x)

# Read QC-cleaned dataset
path <- "YAQUINA_TEMP_INTERPOLATION_READY.csv"

df_interpolation <- read_csv(
  path,
  show_col_types = FALSE,
  col_types = cols(
    datetime = col_character()
  )
) %>%
  mutate(
    datetime = parse_date_time(
      datetime,
      orders = c(
        "ymd HMS",
        "ymd HM",
        "ymd"
      ),
      tz = "UTC"
    )
  ) %>%
  arrange(site_ID, datetime) %>%
  distinct(site_ID, datetime, .keep_all = TRUE)

# ------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------

interval_minutes <- 10

min_gap_minutes <- 60
max_gap_minutes <- 1440

fit_window_hours <- 24

minimum_observations <- 10


# ------------------------------------------------------------
# 3. Identify consecutive temperature gaps
# ------------------------------------------------------------

gap_summary <- df_interpolation %>%
  group_by(site_ID) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(
    temp_missing = is.na(temp),
    
    # Start a new run whenever missing/non-missing status changes
    gap_group = cumsum(
      temp_missing != lag(
        temp_missing,
        default = first(temp_missing)
      )
    )
  ) %>%
  filter(temp_missing) %>%
  group_by(site_ID, gap_group) %>%
  summarise(
    gap_start = min(datetime),
    gap_end = max(datetime),
    n_missing = n(),
    gap_minutes = n_missing * interval_minutes,
    gap_hours = gap_minutes / 60,
    .groups = "drop"
  ) %>%
  filter(
    gap_minutes > min_gap_minutes,
    gap_minutes < max_gap_minutes
  ) %>%
  arrange(site_ID, gap_start)


# Inspect the gaps selected for interpolation
print(gap_summary)


# ------------------------------------------------------------
# 4. Function to fit the linear harmonic regression
# ------------------------------------------------------------

fit_harmonic_model <- function(times, temperatures) {
  
  # Remove incomplete inputs
  valid <- !is.na(times) & !is.na(temperatures)
  
  times <- times[valid]
  temperatures <- temperatures[valid]
  
  if (length(temperatures) < minimum_observations) {
    return(NULL)
  }
  
  # Use the first fitting-window observation as time zero
  start_time <- min(times)
  
  time_hours <- as.numeric(
    difftime(times, start_time, units = "hours")
  )
  
  omega_24 <- 2 * pi / 24
  omega_12_42 <- 2 * pi / 12.42
  
  model_data <- tibble(
    temperature = temperatures,
    sin_24 = sin(omega_24 * time_hours),
    cos_24 = cos(omega_24 * time_hours),
    sin_12_42 = sin(omega_12_42 * time_hours),
    cos_12_42 = cos(omega_12_42 * time_hours)
  )
  
  model <- lm(
    temperature ~
      sin_24 +
      cos_24 +
      sin_12_42 +
      cos_12_42,
    data = model_data
  )
  
  list(
    model = model,
    start_time = start_time
  )
}


# ------------------------------------------------------------
# 5. Function to predict temperatures at new timestamps
# ------------------------------------------------------------

predict_harmonic <- function(model_object, new_times) {
  
  time_hours <- as.numeric(
    difftime(
      new_times,
      model_object$start_time,
      units = "hours"
    )
  )
  
  omega_24 <- 2 * pi / 24
  omega_12_42 <- 2 * pi / 12.42
  
  prediction_data <- tibble(
    sin_24 = sin(omega_24 * time_hours),
    cos_24 = cos(omega_24 * time_hours),
    sin_12_42 = sin(omega_12_42 * time_hours),
    cos_12_42 = cos(omega_12_42 * time_hours)
  )
  
  as.numeric(
    predict(
      model_object$model,
      newdata = prediction_data
    )
  )
}


# ------------------------------------------------------------
# 6. Function to interpolate one gap
# ------------------------------------------------------------

interpolate_one_gap <- function(
    site,
    gap_start,
    gap_end,
    input_data
) {
  
  window_start <- gap_start - hours(fit_window_hours)
  window_end <- gap_end + hours(fit_window_hours)
  
  site_window <- input_data %>%
    filter(
      site_ID == site,
      datetime >= window_start,
      datetime <= window_end
    ) %>%
    arrange(datetime)
  
  observed <- site_window %>%
    filter(!is.na(temp))
  
  if (nrow(observed) < minimum_observations) {
    return(
      tibble(
        site_ID = character(),
        datetime = as.POSIXct(character()),
        temp_interp = numeric()
      )
    )
  }
  
  model_object <- fit_harmonic_model(
    times = observed$datetime,
    temperatures = observed$temp
  )
  
  if (is.null(model_object)) {
    return(
      tibble(
        site_ID = character(),
        datetime = as.POSIXct(character()),
        temp_interp = numeric()
      )
    )
  }
  
  # Use timestamps already present in the dataset rather than
  # constructing a new sequence
  gap_times <- site_window %>%
    filter(
      datetime >= gap_start,
      datetime <= gap_end,
      is.na(temp)
    ) %>%
    pull(datetime)
  
  if (length(gap_times) == 0) {
    return(
      tibble(
        site_ID = character(),
        datetime = as.POSIXct(character()),
        temp_interp = numeric()
      )
    )
  }
  
  predictions <- predict_harmonic(
    model_object = model_object,
    new_times = gap_times
  )
  
  tibble(
    site_ID = site,
    datetime = gap_times,
    temp_interp = predictions
  )
}


# ------------------------------------------------------------
# 7. Run the model for every eligible gap
# ------------------------------------------------------------

interpolated_values <- pmap_dfr(
  gap_summary %>%
    select(site_ID, gap_start, gap_end),
  
  function(site_ID, gap_start, gap_end) {
    
    tryCatch(
      interpolate_one_gap(
        site = site_ID,
        gap_start = gap_start,
        gap_end = gap_end,
        input_data = df_interpolation
      ),
      
      error = function(e) {
        
        warning(
          paste0(
            "Harmonic regression failed for site ",
            site_ID,
            ", gap ",
            gap_start,
            " to ",
            gap_end,
            ": ",
            conditionMessage(e)
          )
        )
        
        tibble(
          site_ID = character(),
          datetime = as.POSIXct(character()),
          temp_interp = numeric()
        )
      }
    )
  }
)


# Inspect interpolated predictions
print(head(interpolated_values))

cat(
  "Number of harmonic predictions generated:",
  nrow(interpolated_values),
  "\n"
)


# ------------------------------------------------------------
# 8. Join predictions back into the full dataset
# ------------------------------------------------------------

df_interpolation_final <- df_interpolation %>%
  left_join(
    interpolated_values,
    by = c("site_ID", "datetime")
  ) %>%
  mutate(
    # Preserve the cleaned observed temperature
    temp_observed = temp,
    
    # Identify values actually filled by the model
    is_interpolated = if_else(
      is.na(temp) & !is.na(temp_interp),
      1L,
      0L
    ),
    
    # Use observed temperature when available;
    # otherwise use the harmonic prediction
    temp_filled = coalesce(temp, temp_interp)
  ) %>%
  arrange(site_ID, datetime)


# ------------------------------------------------------------
# 9. Sanity checks
# ------------------------------------------------------------

interpolation_summary <- df_interpolation_final %>%
  summarise(
    original_missing = sum(is.na(temp_observed)),
    interpolated_values = sum(is_interpolated == 1L),
    remaining_missing = sum(is.na(temp_filled))
  )

print(interpolation_summary)


interpolation_by_site <- df_interpolation_final %>%
  group_by(site_ID) %>%
  summarise(
    original_missing = sum(is.na(temp_observed)),
    interpolated_values = sum(is_interpolated == 1L),
    remaining_missing = sum(is.na(temp_filled)),
    .groups = "drop"
  )

print(interpolation_by_site)

# Export final interpolated dataset 

df_interpolation_export <- df_interpolation_final %>%
  mutate(
    temp = temp_raw
  ) %>%
  select(
    -prev_temp,
    -next_temp,
    -temp_interp,
    -temp_observed,
    -temp_raw
  )

# write_csv(
#   df_interpolation_export,
#   "YAQUINA_TEMP_INTERPOLATED.csv",
#   na = ""
# )

# ------------------------------------------------------------
# 10. Plot one interpolated gap
# ------------------------------------------------------------

# Select a gap to inspect.
# Change slice(1) to another row number to inspect a different gap.
selected_gap <- gap_summary %>%
  slice(25)

selected_site <- selected_gap$site_ID
selected_start <- selected_gap$gap_start
selected_end <- selected_gap$gap_end

# Display 24 hours before and after the gap
plot_window_start <- selected_start - hours(24)
plot_window_end <- selected_end + hours(24)

selected_gap_plot_data <- df_interpolation_final %>%
  filter(
    site_ID == selected_site,
    datetime >= plot_window_start,
    datetime <= plot_window_end
  )

ggplot(
  selected_gap_plot_data,
  aes(x = datetime)
) +
  geom_line(
    aes(y = temp_observed),
    linewidth = 0.5,
    na.rm = TRUE
  ) +
  geom_line(
    aes(y = temp_filled),
    linewidth = 0.7,
    linetype = "dashed",
    na.rm = TRUE
  ) +
  geom_point(
    data = selected_gap_plot_data %>%
      filter(is_interpolated == 1L),
    aes(y = temp_interp),
    size = 2
  ) +
  annotate(
    "rect",
    xmin = selected_start,
    xmax = selected_end,
    ymin = -Inf,
    ymax = Inf,
    alpha = 0.12
  ) +
  labs(
    title = paste(
      "Harmonic interpolation at site",
      selected_site
    ),
    subtitle = paste(
      "Gap:",
      format(selected_start, "%Y-%m-%d %H:%M"),
      "to",
      format(selected_end, "%Y-%m-%d %H:%M")
    ),
    x = "Datetime",
    y = "Temperature (°C)",
    caption = paste(
      "Solid line = observed temperature;",
      "dashed line and points = harmonic-filled series"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# ------------------------------------------------------------
# Diagnose all missing-temperature gaps
# ------------------------------------------------------------

all_gap_summary <- df_interpolation %>%
  group_by(site_ID) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(
    temp_missing = is.na(temp),
    
    gap_group = cumsum(
      temp_missing != lag(
        temp_missing,
        default = first(temp_missing)
      )
    )
  ) %>%
  filter(temp_missing) %>%
  group_by(site_ID, gap_group) %>%
  summarise(
    gap_start = min(datetime),
    gap_end = max(datetime),
    n_missing = n(),
    gap_minutes = n_missing * interval_minutes,
    gap_hours = gap_minutes / 60,
    .groups = "drop"
  ) %>%
  mutate(
    gap_category = case_when(
      gap_minutes <= min_gap_minutes ~
        "Too short: 60 minutes or less",
      
      gap_minutes >= max_gap_minutes ~
        "Too long: 24 hours or longer",
      
      TRUE ~
        "Eligible duration: >60 minutes and <24 hours"
    )
  )

gap_category_summary <- all_gap_summary %>%
  group_by(site_ID, gap_category) %>%
  summarise(
    number_of_gaps = n(),
    missing_observations = sum(n_missing),
    .groups = "drop"
  ) %>%
  arrange(site_ID, gap_category)

print(gap_category_summary, n = Inf)

gap_category_wide <- gap_category_summary %>%
  pivot_wider(
    names_from = gap_category,
    values_from = c(number_of_gaps, missing_observations),
    values_fill = 0
  )

print(gap_category_wide, n = Inf)

eligible_gap_diagnostic <- all_gap_summary %>%
  filter(
    gap_minutes > min_gap_minutes,
    gap_minutes < max_gap_minutes
  ) %>%
  group_by(site_ID) %>%
  summarise(
    eligible_gaps = n(),
    eligible_missing_observations = sum(n_missing),
    .groups = "drop"
  ) %>%
  full_join(
    interpolation_by_site %>%
      select(
        site_ID,
        interpolated_values,
        remaining_missing
      ),
    by = "site_ID"
  ) %>%
  mutate(
    eligible_but_not_interpolated =
      eligible_missing_observations - interpolated_values
  ) %>%
  arrange(site_ID)

print(eligible_gap_diagnostic, n = Inf)

# ------------------------------------------------------------
# 11. Cross-validation with artificial gaps
# ------------------------------------------------------------

set.seed(529)

n_cv_gaps <- 500
max_cv_attempts <- 10000

# Draw artificial gap lengths from the lengths of actual
# eligible gaps in the dataset
eligible_gap_lengths <- gap_summary$n_missing

if (length(eligible_gap_lengths) == 0) {
  stop("No eligible real gap lengths are available.")
}

# Split the input dataframe by site for faster sampling
site_data_list <- df_interpolation %>%
  arrange(site_ID, datetime) %>%
  group_split(site_ID)

names(site_data_list) <- vapply(
  site_data_list,
  function(x) unique(x$site_ID),
  character(1)
)

# Function to create and evaluate one artificial gap

cross_validate_one_gap <- function(
    gap_id,
    input_data,
    gap_length,
    fit_window_hours = 24,
    minimum_observations = 10,
    interval_minutes = 10
) {
  
  n_rows <- nrow(input_data)
  
  if (n_rows < gap_length) {
    return(NULL)
  }
  
  possible_start_indices <- seq_len(
    n_rows - gap_length + 1
  )
  
  start_index <- sample(
    possible_start_indices,
    size = 1
  )
  
  end_index <- start_index + gap_length - 1
  
  artificial_gap <- input_data[
    start_index:end_index,
  ]
  
  # Artificial gap must contain only originally observed values
  if (any(is.na(artificial_gap$temp))) {
    return(NULL)
  }
  
  # Artificial gap must consist of consecutive 10-minute timestamps
  if (
    nrow(artificial_gap) > 1 &&
    any(
      diff(as.numeric(artificial_gap$datetime)) !=
      interval_minutes * 60
    )
  ) {
    return(NULL)
  }
  
  gap_start <- min(artificial_gap$datetime)
  gap_end <- max(artificial_gap$datetime)
  
  window_start <- gap_start - hours(fit_window_hours)
  window_end <- gap_end + hours(fit_window_hours)
  
  fitting_data <- input_data %>%
    filter(
      datetime >= window_start,
      datetime <= window_end,
      
      # Exclude all values within the artificial gap
      datetime < gap_start | datetime > gap_end,
      
      !is.na(temp)
    ) %>%
    arrange(datetime)
  
  if (nrow(fitting_data) < minimum_observations) {
    return(NULL)
  }
  
  model_object <- fit_harmonic_model(
    times = fitting_data$datetime,
    temperatures = fitting_data$temp
  )
  
  if (is.null(model_object)) {
    return(NULL)
  }
  
  predicted_temperature <- predict_harmonic(
    model_object = model_object,
    new_times = artificial_gap$datetime
  )
  
  tibble(
    gap_id = gap_id,
    site_ID = unique(artificial_gap$site_ID),
    gap_start = gap_start,
    gap_end = gap_end,
    gap_length_records = gap_length,
    gap_length_hours = gap_length *
      interval_minutes / 60,
    datetime = artificial_gap$datetime,
    observed_temp = artificial_gap$temp,
    predicted_temp = predicted_temperature,
    residual = predicted_temp - observed_temp,
    absolute_error = abs(residual),
    squared_error = residual^2
  )
}

# Generate 500 artifical gaps

cv_results_list <- vector(
  mode = "list",
  length = n_cv_gaps
)

successful_gaps <- 0
attempts <- 0

while (
  successful_gaps < n_cv_gaps &&
  attempts < max_cv_attempts
) {
  
  attempts <- attempts + 1
  
  sampled_site <- sample(
    names(site_data_list),
    size = 1
  )
  
  sampled_gap_length <- sample(
    eligible_gap_lengths,
    size = 1,
    replace = TRUE
  )
  
  result <- cross_validate_one_gap(
    gap_id = successful_gaps + 1,
    input_data = site_data_list[[sampled_site]],
    gap_length = sampled_gap_length,
    fit_window_hours = fit_window_hours,
    minimum_observations = minimum_observations,
    interval_minutes = interval_minutes
  )
  
  if (!is.null(result)) {
    successful_gaps <- successful_gaps + 1
    cv_results_list[[successful_gaps]] <- result
  }
}

if (successful_gaps < n_cv_gaps) {
  warning(
    paste(
      "Only",
      successful_gaps,
      "successful artificial gaps were generated after",
      attempts,
      "attempts."
    )
  )
}

cv_results <- bind_rows(
  cv_results_list[
    seq_len(successful_gaps)
  ]
)

cat(
  "Successful artificial gaps:",
  successful_gaps,
  "\n"
)

cat(
  "Total temperature observations predicted:",
  nrow(cv_results),
  "\n"
)

# ------------------------------------------------------------
# 12. Overall cross-validation metrics
# ------------------------------------------------------------

overall_fit <- lm(
  predicted_temp ~ observed_temp,
  data = cv_results
)

overall_fit_summary <- summary(overall_fit)

cv_overall_metrics <- cv_results %>%
  summarise(
    artificial_gaps = n_distinct(gap_id),
    predicted_observations = n(),
    
    bias = mean(
      predicted_temp - observed_temp,
      na.rm = TRUE
    ),
    
    MAE = mean(
      abs(predicted_temp - observed_temp),
      na.rm = TRUE
    ),
    
    RMSE = sqrt(
      mean(
        (predicted_temp - observed_temp)^2,
        na.rm = TRUE
      )
    ),
    
    correlation = cor(
      observed_temp,
      predicted_temp,
      use = "complete.obs"
    )
  ) %>%
  mutate(
    R_squared = overall_fit_summary$r.squared,
    adjusted_R_squared =
      overall_fit_summary$adj.r.squared
  )

print(cv_overall_metrics)

# Site-level validation summary

cv_metrics_by_site <- cv_results %>%
  group_by(site_ID) %>%
  group_modify(~ {
    
    fit <- lm(
      predicted_temp ~ observed_temp,
      data = .x
    )
    
    fit_summary <- summary(fit)
    
    tibble(
      artificial_gaps = n_distinct(.x$gap_id),
      predicted_observations = nrow(.x),
      
      bias = mean(
        .x$predicted_temp - .x$observed_temp,
        na.rm = TRUE
      ),
      
      MAE = mean(
        abs(.x$predicted_temp - .x$observed_temp),
        na.rm = TRUE
      ),
      
      RMSE = sqrt(
        mean(
          (.x$predicted_temp - .x$observed_temp)^2,
          na.rm = TRUE
        )
      ),
      
      correlation = cor(
        .x$observed_temp,
        .x$predicted_temp,
        use = "complete.obs"
      ),
      
      R_squared = fit_summary$r.squared,
      
      adjusted_R_squared =
        fit_summary$adj.r.squared
    )
  }) %>%
  ungroup() %>%
  arrange(site_ID)

print(cv_metrics_by_site)

# ------------------------------------------------------------
# 13. Observed versus predicted temperature plot
# ------------------------------------------------------------

ggplot(
  cv_results,
  aes(
    x = observed_temp,
    y = predicted_temp
  )
) +
  geom_point(
    alpha = 0.25,
    size = 1
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  coord_equal() +
  labs(
    title = "Harmonic regression cross-validation",
    subtitle = paste(
      successful_gaps,
      "artificial gaps"
    ),
    x = "Observed temperature (°C)",
    y = "Harmonic-predicted temperature (°C)",
    caption = "Dashed line represents perfect 1:1 agreement"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# ------------------------------------------------------------
# 13. Observed vs predicted by site
# ------------------------------------------------------------

# Create annotation labels for each site
panel_labels <- cv_metrics_by_site %>%
  mutate(
    label = paste0(
      "RMSE = ", sprintf("%.2f", RMSE), " °C\n",
      "Adj. R² = ", sprintf("%.3f", adjusted_R_squared)
    )
  )

# Observed versus predicted plot by site
ggplot(
  cv_results,
  aes(
    x = observed_temp,
    y = predicted_temp
  )
) +
  
  geom_point(
    color = "#2C7FB8",
    alpha = 0.55,
    size = 1.2
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +
  
  geom_text(
    data = panel_labels,
    aes(
      x = -Inf,
      y = Inf,
      label = label
    ),
    inherit.aes = FALSE,
    hjust = -0.1,
    vjust = 1.2,
    size = 3.3
  ) +
  
  ggh4x::facet_wrap2(
    ~site_ID,
    ncol = 3,
    scales = "fixed",
    axes = "all"
  ) +
  
  coord_equal() +
  
  labs(
    title = "Observed versus harmonic-predicted temperature by site",
    subtitle = "Cross-validation using artificial gaps",
    x = "Observed temperature (°C)",
    y = "Predicted temperature (°C)"
  ) +
  
  scale_x_continuous(
    breaks = seq(10, 26, by = 5)
  ) +
  
  scale_y_continuous(
    breaks = seq(10, 26, by = 5)
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major = element_line(
      color = "grey78",
      linewidth = 0.45
    ),
    
    panel.grid.minor = element_line(
      color = "grey90",
      linewidth = 0.25
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 11
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      color = "black"
    )
  )

# ------------------------------------------------------------
# 14. Plot one artificial validation gap
# ------------------------------------------------------------

# selected_cv_gap_id <- 1
# 
# selected_cv_gap <- cv_results %>%
#   filter(gap_id == selected_cv_gap_id)
# 
# selected_cv_site <- unique(
#   selected_cv_gap$site_ID
# )
# 
# selected_cv_start <- min(
#   selected_cv_gap$gap_start
# )
# 
# selected_cv_end <- max(
#   selected_cv_gap$gap_end
# )
# 
# cv_plot_data <- df_interpolation %>%
#   filter(
#     site_ID == selected_cv_site,
#     datetime >= selected_cv_start - hours(24),
#     datetime <= selected_cv_end + hours(24)
#   ) %>%
#   left_join(
#     selected_cv_gap %>%
#       select(
#         datetime,
#         predicted_temp
#       ),
#     by = "datetime"
#   ) %>%
#   mutate(
#     display_observed = if_else(
#       datetime >= selected_cv_start &
#         datetime <= selected_cv_end,
#       NA_real_,
#       temp
#     )
#   )
# 
# ggplot(
#   cv_plot_data,
#   aes(x = datetime)
# ) +
#   geom_line(
#     aes(y = temp),
#     alpha = 0.35,
#     linewidth = 0.5
#   ) +
#   geom_line(
#     aes(y = display_observed),
#     linewidth = 0.7,
#     na.rm = TRUE
#   ) +
#   geom_point(
#     data = selected_cv_gap,
#     aes(
#       x = datetime,
#       y = observed_temp
#     ),
#     shape = 1,
#     size = 2
#   ) +
#   geom_line(
#     data = selected_cv_gap,
#     aes(
#       x = datetime,
#       y = predicted_temp
#     ),
#     linetype = "dashed",
#     linewidth = 0.8
#   ) +
#   geom_point(
#     data = selected_cv_gap,
#     aes(
#       x = datetime,
#       y = predicted_temp
#     ),
#     size = 1.5
#   ) +
#   annotate(
#     "rect",
#     xmin = selected_cv_start,
#     xmax = selected_cv_end,
#     ymin = -Inf,
#     ymax = Inf,
#     alpha = 0.1
#   ) +
#   labs(
#     title = paste(
#       "Artificial gap cross-validation:",
#       selected_cv_site
#     ),
#     subtitle = paste(
#       format(selected_cv_start, "%Y-%m-%d %H:%M"),
#       "to",
#       format(selected_cv_end, "%Y-%m-%d %H:%M")
#     ),
#     x = "Datetime",
#     y = "Temperature (°C)",
#     caption = paste(
#       "Open points = withheld observations;",
#       "dashed line and filled points = harmonic predictions"
#     )
#   ) +
#   theme_minimal() +
#   theme(
#     plot.title = element_text(face = "bold")
#   )
# 
# 
