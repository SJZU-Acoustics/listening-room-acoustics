source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A11"
analysis_dir <- analysis_output_dir("a11_listening_position")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver) %>%
  mutate(room_id = as.numeric(as.character(room_id)))

listener_map <- tibble(room_id = c(1, 2), listening_receiver_id = c(4, 5))

long <- primary %>%
  left_join(listener_map, by = "room_id") %>%
  mutate(is_listening_position = receiver_id == listening_receiver_id) %>%
  select(
    room_id, condition_id, receiver_id, listening_receiver_id,
    is_listening_position, centre_frequency_hz,
    T20 = t20_s_strict, EDT = edt_s_strict, C80 = c80_db_strict
  ) %>%
  pivot_longer(cols = c(T20, EDT, C80), names_to = "metric", values_to = "value")

position_comparison <- long %>%
  group_by(metric, room_id, condition_id, centre_frequency_hz) %>%
  group_modify(~ {
    listening_value <- .x$value[.x$is_listening_position]
    listening_value <- if (length(listening_value)) listening_value[[1]] else NA_real_
    field <- .x$value[!.x$is_listening_position & is.finite(.x$value)]
    all_values <- .x$value[is.finite(.x$value)]
    field_mean <- if (length(field)) mean(field) else NA_real_
    field_sd <- if (length(field) >= 2) sd(field) else NA_real_
    field_q1 <- if (length(field) >= 2) quantile(field, 0.25, names = FALSE) else NA_real_
    field_q3 <- if (length(field) >= 2) quantile(field, 0.75, names = FALSE) else NA_real_
    percentile <- if (is.finite(listening_value)) {
      (sum(all_values < listening_value) + 0.5 * sum(all_values == listening_value)) / length(all_values)
    } else {
      NA_real_
    }
    tibble(
      listening_receiver_id = first(.x$listening_receiver_id),
      listening_value = listening_value,
      n_other_receivers = length(field),
      other_receiver_mean = field_mean,
      other_receiver_median = if (length(field)) median(field) else NA_real_,
      other_receiver_sd = field_sd,
      other_receiver_q1 = field_q1,
      other_receiver_q3 = field_q3,
      listening_minus_other_mean = listening_value - field_mean,
      listening_z_against_others = (listening_value - field_mean) / field_sd,
      listening_percentile_all_receivers = percentile,
      within_other_receiver_iqr = is.finite(listening_value) &&
        is.finite(field_q1) && listening_value >= field_q1 && listening_value <= field_q3
    )
  }) %>%
  ungroup()

position_summary <- position_comparison %>%
  group_by(metric, room_id, listening_receiver_id) %>%
  summarise(
    n_scheduled_cells = n(),
    n_valid_listening_cells = sum(is.finite(listening_value)),
    median_absolute_z = median(abs(listening_z_against_others), na.rm = TRUE),
    maximum_absolute_z = max(abs(listening_z_against_others), na.rm = TRUE),
    proportion_within_field_iqr = mean(within_other_receiver_iqr[is.finite(listening_value)], na.rm = TRUE),
    proportion_central_10_to_90_percent = mean(
      listening_percentile_all_receivers >= 0.1 & listening_percentile_all_receivers <= 0.9,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

effect_specs <- tribble(
  ~metric, ~column, ~improvement,
  "T20", "t20_s_strict", "lower",
  "EDT", "edt_s_strict", "lower",
  "C80", "c80_db_strict", "higher"
)
effect_sets <- vector("list", nrow(effect_specs))
for (i in seq_len(nrow(effect_specs))) {
  effect_sets[[i]] <- prepare_factorial_effects(
    primary,
    effect_specs$column[[i]],
    effect_specs$metric[[i]],
    improvement = effect_specs$improvement[[i]],
    scale = "difference"
  )
}

receiver_curtain <- bind_rows(effect_sets) %>%
  filter(effect_family == "curtain") %>%
  group_by(metric, room_id, receiver_id, receiver_block, centre_frequency_hz) %>%
  summarise(band_effect = mean(effect_value), .groups = "drop") %>%
  group_by(metric, room_id, receiver_id) %>%
  summarise(
    curtain_effect = mean(band_effect),
    n_complete_bands = n(),
    .groups = "drop"
  ) %>%
  left_join(listener_map, by = "room_id")

curtain_representativeness <- receiver_curtain %>%
  group_by(metric, room_id, listening_receiver_id) %>%
  group_modify(~ {
    listener_id <- .y$listening_receiver_id[[1]]
    listening_effect <- .x$curtain_effect[.x$receiver_id == listener_id]
    listening_effect <- if (length(listening_effect)) listening_effect[[1]] else NA_real_
    others <- .x$curtain_effect[.x$receiver_id != listener_id]
    all_effects <- .x$curtain_effect
    percentile <- (
      sum(all_effects < listening_effect) + 0.5 * sum(all_effects == listening_effect)
    ) / length(all_effects)
    tibble(
      listening_curtain_effect = listening_effect,
      other_receiver_mean_effect = mean(others),
      listening_minus_other_mean_effect = listening_effect - mean(others),
      listening_effect_percentile = percentile,
      minimum_receiver_effect = min(all_effects),
      maximum_receiver_effect = max(all_effects),
      n_receivers = length(all_effects)
    )
  }) %>%
  ungroup()

plot_data <- position_comparison %>%
  filter(metric == "T20") %>%
  mutate(
    frequency = factor(frequency_label(centre_frequency_hz), levels = frequency_label(CORE_OCTAVE_BANDS)),
    condition_id = factor(condition_id, levels = CONDITION_LEVELS),
    room_label = paste("Room", room_id)
  )

position_plot <- ggplot(
  plot_data,
  aes(x = frequency, y = listening_minus_other_mean, colour = condition_id, group = condition_id)
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.5) +
  facet_wrap(~ room_label, ncol = 1) +
  scale_colour_manual(values = P31_COLOURS, labels = CONDITION_LABELS, name = NULL) +
  labs(
    title = "Listening-position T20 relative to the other receivers",
    subtitle = "Positive values mean longer T20 at the designated listening position",
    x = "Octave-band centre frequency (Hz)",
    y = "Listening position - other-position mean (s)"
  ) +
  theme_p31()

readr::write_csv(position_comparison, file.path(analysis_dir, "listening_position_cell_comparisons.csv"))
readr::write_csv(position_summary, file.path(analysis_dir, "listening_position_summary.csv"))
readr::write_csv(receiver_curtain, file.path(analysis_dir, "receiver_curtain_effects.csv"))
readr::write_csv(curtain_representativeness, file.path(analysis_dir, "listening_curtain_representativeness.csv"))
save_p31_plot(position_plot, file.path(analysis_dir, "listening_position_t20_deviation.png"), width_mm = 170, height_mm = 150)

t20_position <- position_summary %>% filter(metric == "T20")
t20_effect <- curtain_representativeness %>% filter(metric == "T20")

results <- c(
  "# A11 results",
  "",
  "## Outcome",
  "",
  sprintf("- T20 listening-position median absolute z-score against the other receivers: %s.", paste(sprintf("Room %d %.2f", t20_position$room_id, t20_position$median_absolute_z), collapse = "; ")),
  sprintf("- T20 listening positions fall within the other-receiver IQR in %s.", paste(sprintf("Room %d %.1f%% of valid condition-band cells", t20_position$room_id, 100 * t20_position$proportion_within_field_iqr), collapse = "; ")),
  sprintf("- Listening-position curtain effects: %s.", paste(sprintf("Room %d %.3f s versus %.3f s at the other receivers", t20_effect$room_id, t20_effect$listening_curtain_effect, t20_effect$other_receiver_mean_effect), collapse = "; ")),
  sprintf("- Listening-position T20 curtain-effect percentiles among the ten receivers: %s.", paste(sprintf("Room %d %.0fth", t20_effect$room_id, 100 * t20_effect$listening_effect_percentile), collapse = "; ")),
  "",
  "## Decision",
  "",
  "Listening-position results may be reported as position-specific context only. Any departure from the spatial field is retained rather than allowing one designated point to stand in for the room average.",
  "",
  "## Assumptions and limitations",
  "",
  "Percentiles are descriptive mid-ranks on ten measured positions. They do not represent a probability distribution of listeners, and no subjective response is available."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "listening_position_cell_comparisons.csv", "listening_position_summary.csv",
    "receiver_curtain_effects.csv", "listening_curtain_representativeness.csv",
    "listening_position_t20_deviation.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c("Room 1 R4", "Room 2 R5", "technical repeats remain receiver-averaged")
)
