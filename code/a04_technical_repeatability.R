source(file.path("code", "helpers.R"))

analysis_id <- "A04"
analysis_dir <- analysis_output_dir("a04_technical_repeatability")

take <- read_frozen_csv("ir_band_metrics.csv")
condition <- read_frozen_csv("condition_band_summary.csv")

repeated <- take %>%
  filter(
    n_takes_in_cell == 3,
    band_scheme == "octave",
    centre_frequency_hz %in% CORE_OCTAVE_BANDS
  ) %>%
  mutate(
    T20 = if_else(t20_strict_valid, t20_s, NA_real_),
    T30 = if_else(t30_strict_valid, t30_s, NA_real_),
    EDT = if_else(edt_strict_valid, edt_s, NA_real_),
    C50 = if_else(clarity_valid, c50_db, NA_real_),
    C80 = if_else(clarity_valid, c80_db, NA_real_)
  ) %>%
  select(
    receiver_cell_id, room_id, condition_id, source_id, receiver_id,
    centre_frequency_hz, take_order_within_cell, T20, T30, EDT, C50, C80
  ) %>%
  pivot_longer(cols = c(T20, T30, EDT, C50, C80), names_to = "metric", values_to = "value")

repeatability <- repeated %>%
  group_by(
    receiver_cell_id, room_id, condition_id, source_id, receiver_id,
    centre_frequency_hz, metric
  ) %>%
  summarise(
    n_valid_takes = sum(is.finite(value)),
    mean_value = safe_mean(value),
    within_take_sd = safe_sd(value),
    within_take_range = if (sum(is.finite(value)) >= 2) diff(range(value, na.rm = TRUE)) else NA_real_,
    within_take_cv = if (
      first(metric) %in% c("T20", "T30", "EDT") && is.finite(mean_value) && mean_value != 0
    ) within_take_sd / mean_value else NA_real_,
    .groups = "drop"
  )

spatial_sd <- condition %>%
  filter(band_scheme == "octave", centre_frequency_hz %in% CORE_OCTAVE_BANDS) %>%
  select(
    room_id, condition_id, source_scope, centre_frequency_hz,
    T20 = t20_s_sd_between_receivers,
    T30 = t30_s_sd_between_receivers,
    EDT = edt_s_sd_between_receivers,
    C50 = c50_db_sd_between_receivers,
    C80 = c80_db_sd_between_receivers
  ) %>%
  pivot_longer(cols = c(T20, T30, EDT, C50, C80), names_to = "metric", values_to = "between_receiver_sd")

repeatability <- repeatability %>%
  left_join(
    spatial_sd,
    by = c(
      "room_id", "condition_id", "source_id" = "source_scope",
      "centre_frequency_hz", "metric"
    )
  ) %>%
  mutate(within_to_spatial_sd_ratio = within_take_sd / between_receiver_sd)

metric_summary <- repeatability %>%
  group_by(metric) %>%
  summarise(
    n_cell_bands = n(),
    n_estimable_sd = sum(is.finite(within_take_sd)),
    median_within_sd = median(within_take_sd, na.rm = TRUE),
    p95_within_sd = quantile(within_take_sd, 0.95, na.rm = TRUE, names = FALSE),
    max_within_range = max(within_take_range, na.rm = TRUE),
    median_cv = if (all(is.na(within_take_cv))) NA_real_ else median(within_take_cv, na.rm = TRUE),
    median_within_to_spatial_sd_ratio = median(within_to_spatial_sd_ratio, na.rm = TRUE),
    .groups = "drop"
  )

worst_cells <- repeatability %>%
  filter(is.finite(within_to_spatial_sd_ratio)) %>%
  group_by(metric) %>%
  slice_max(within_to_spatial_sd_ratio, n = 3, with_ties = FALSE) %>%
  ungroup()

readr::write_csv(repeatability, file.path(analysis_dir, "technical_repeatability_by_cell_band.csv"))
readr::write_csv(metric_summary, file.path(analysis_dir, "technical_repeatability_summary.csv"))
readr::write_csv(worst_cells, file.path(analysis_dir, "technical_repeatability_worst_cells.csv"))

t20 <- metric_summary %>% filter(metric == "T20")
results <- c(
  "# A04 results",
  "",
  "## Outcome",
  "",
  sprintf("The five repeated receiver cells yield %d core-band T20 cell-band checks with at least two strict-valid takes.", t20$n_estimable_sd),
  "",
  sprintf("- Median within-take T20 SD: %.4f s; 95th percentile: %.4f s.", t20$median_within_sd, t20$p95_within_sd),
  sprintf("- Median T20 within-take CV: %.1f%%.", 100 * t20$median_cv),
  sprintf("- Median ratio of within-take T20 SD to between-receiver SD: %.2f.", t20$median_within_to_spatial_sd_ratio),
  "- Endpoint-, band-, condition-, and source-specific exceptions are retained in the detailed and worst-cell tables.",
  "",
  "## Decision",
  "",
  "Technical takes remain averaged within receiver cells. Their variation is reported as a local repeatability bound and cannot be used to increase the receiver sample size or to claim dataset-wide reliability.",
  "",
  "## Assumptions and limitations",
  "",
  "All repeated cells are Room 2 R5, and only five source-condition cells were repeated. The comparison with spatial SD is descriptive because technical and spatial variation are not sampled on a balanced crossed design."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c("ir_band_metrics.csv", "condition_band_summary.csv"),
  outputs = c(
    "technical_repeatability_by_cell_band.csv", "technical_repeatability_summary.csv",
    "technical_repeatability_worst_cells.csv", "results.md", "session_info.txt", "run.log"
  ),
  checks = c("five three-take receiver cells", "take retained only as technical replicate")
)
