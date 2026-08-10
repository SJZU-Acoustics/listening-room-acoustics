source(file.path("code", "helpers.R"))

analysis_id <- "A03"
analysis_dir <- analysis_output_dir("a03_endpoint_convergence")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver)

paired_decay <- primary %>%
  filter(is.finite(t20_s_strict), is.finite(t30_s_strict)) %>%
  mutate(
    t30_minus_t20_s = t30_s_strict - t20_s_strict,
    relative_difference = (t30_s_strict - t20_s_strict) /
      ((t30_s_strict + t20_s_strict) / 2)
  )

agreement_overall <- paired_decay %>%
  summarise(
    n_cells = n(),
    mean_t20_s = mean(t20_s_strict),
    mean_t30_s = mean(t30_s_strict),
    mean_difference_s = mean(t30_minus_t20_s),
    sd_difference_s = sd(t30_minus_t20_s),
    mean_relative_difference = mean(relative_difference),
    pearson_r = cor(t20_s_strict, t30_s_strict),
    ccc = concordance_correlation(t20_s_strict, t30_s_strict)
  )

agreement_by_band <- paired_decay %>%
  group_by(room_id, condition_id, centre_frequency_hz) %>%
  summarise(
    n_pairs = n(),
    mean_t20_s = mean(t20_s_strict),
    mean_t30_s = mean(t30_s_strict),
    mean_difference_s = mean(t30_minus_t20_s),
    sd_difference_s = sd(t30_minus_t20_s),
    se_difference_s = sd_difference_s / sqrt(n_pairs),
    ci_low_s = mean_difference_s - qt(0.975, df = n_pairs - 1) * se_difference_s,
    ci_high_s = mean_difference_s + qt(0.975, df = n_pairs - 1) * se_difference_s,
    pearson_r = if_else(n_pairs >= 3, cor(t20_s_strict, t30_s_strict), NA_real_),
    ccc = concordance_correlation(t20_s_strict, t30_s_strict),
    .groups = "drop"
  )

profile <- primary %>%
  select(room_id, condition_id, receiver_id, centre_frequency_hz,
         t20_s_strict, t30_s_strict, edt_s_strict) %>%
  pivot_longer(
    cols = c(t20_s_strict, t30_s_strict, edt_s_strict),
    names_to = "metric",
    values_to = "value_s"
  ) %>%
  mutate(
    metric = recode(metric,
                    t20_s_strict = "T20",
                    t30_s_strict = "T30",
                    edt_s_strict = "EDT")
  ) %>%
  group_by(room_id, condition_id, centre_frequency_hz, metric) %>%
  summarise(
    n_valid = sum(is.finite(value_s)),
    mean_s = safe_mean(value_s),
    sd_s = safe_sd(value_s),
    .groups = "drop"
  )

condition_wide <- profile %>%
  select(room_id, centre_frequency_hz, metric, condition_id, mean_s) %>%
  pivot_wider(names_from = condition_id, values_from = mean_s)

contrast_profile <- condition_wide %>%
  transmute(
    room_id,
    centre_frequency_hz,
    metric,
    curtain_no_carpet = open_no_carpet - closed_no_carpet,
    curtain_carpet = open_carpet - closed_carpet,
    carpet_open = open_no_carpet - open_carpet,
    carpet_closed = closed_no_carpet - closed_carpet
  ) %>%
  pivot_longer(
    cols = c(curtain_no_carpet, curtain_carpet, carpet_open, carpet_closed),
    names_to = "contrast_id",
    values_to = "reduction_s"
  )

direction_concordance <- contrast_profile %>%
  select(room_id, centre_frequency_hz, contrast_id, metric, reduction_s) %>%
  pivot_wider(names_from = metric, values_from = reduction_s) %>%
  mutate(
    t20_t30_same_direction = sign(T20) == sign(T30),
    t20_edt_same_direction = sign(T20) == sign(EDT)
  )

direction_summary <- direction_concordance %>%
  group_by(contrast_id) %>%
  summarise(
    n_room_bands = n(),
    t20_t30_concordance = mean(t20_t30_same_direction, na.rm = TRUE),
    t20_edt_concordance = mean(t20_edt_same_direction, na.rm = TRUE),
    .groups = "drop"
  )

plot_data <- agreement_by_band %>%
  mutate(
    condition_id = factor(condition_id, levels = CONDITION_LEVELS),
    frequency = factor(
      frequency_label(centre_frequency_hz),
      levels = frequency_label(CORE_OCTAVE_BANDS)
    ),
    room_label = paste("Room", room_id)
  )

agreement_plot <- ggplot(
  plot_data,
  aes(x = frequency, y = mean_difference_s, colour = condition_id, group = condition_id)
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_errorbar(aes(ymin = ci_low_s, ymax = ci_high_s), width = 0.12, linewidth = 0.35) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 1.5) +
  facet_wrap(~ room_label, ncol = 1) +
  scale_colour_manual(values = P31_COLOURS, labels = CONDITION_LABELS, name = NULL) +
  labs(
    title = "T30 minus T20 at matched receiver cells",
    subtitle = "Means and 95% t intervals across ten receiver positions",
    x = "Octave-band centre frequency (Hz)",
    y = "T30 - T20 (s)"
  ) +
  theme_p31()

readr::write_csv(agreement_overall, file.path(analysis_dir, "t20_t30_agreement_overall.csv"))
readr::write_csv(agreement_by_band, file.path(analysis_dir, "t20_t30_agreement_by_band.csv"))
readr::write_csv(profile, file.path(analysis_dir, "endpoint_condition_profiles.csv"))
readr::write_csv(contrast_profile, file.path(analysis_dir, "endpoint_contrast_profiles.csv"))
readr::write_csv(direction_concordance, file.path(analysis_dir, "endpoint_direction_concordance.csv"))
readr::write_csv(direction_summary, file.path(analysis_dir, "endpoint_direction_summary.csv"))
save_p31_plot(agreement_plot, file.path(analysis_dir, "t20_t30_agreement.png"))

curtain_concordance <- direction_summary %>%
  filter(str_starts(contrast_id, "curtain"))

results <- c(
  "# A03 results",
  "",
  "## Outcome",
  "",
  sprintf("T20 and T30 were jointly available for %d matched S1 receiver-condition-band cells.", agreement_overall$n_cells),
  "",
  sprintf("- Mean T30-T20 difference: %.3f s (cell-level Pearson r = %.3f; concordance correlation = %.3f).", agreement_overall$mean_difference_s, agreement_overall$pearson_r, agreement_overall$ccc),
  sprintf("- T20-T30 direction concordance for the two curtain contrasts ranges from %.1f%% to %.1f%% across room-band cells.", 100 * min(curtain_concordance$t20_t30_concordance), 100 * max(curtain_concordance$t20_t30_concordance)),
  sprintf("- T20-EDT direction concordance for the curtain contrasts ranges from %.1f%% to %.1f%%.", 100 * min(curtain_concordance$t20_edt_concordance), 100 * max(curtain_concordance$t20_edt_concordance)),
  "- Band-, room-, condition-, and contrast-specific values remain available in the CSV outputs; disagreement is not hidden by the global summaries.",
  "",
  "## Decision",
  "",
  "Strict T20 remains the primary Stage-2 endpoint. T30 and EDT remain sensitivity outcomes. This decision follows the predeclared hierarchy and validity rules rather than selecting the estimator with the largest observed treatment difference.",
  "",
  "## Assumptions and limitations",
  "",
  "Agreement reflects estimators derived from the same measured IRs and is therefore not independent replication. The t intervals in the diagnostic figure describe receiver dispersion within each fixed room-condition cell; they are not population-level room intervals."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))

outputs <- c(
  "t20_t30_agreement_overall.csv", "t20_t30_agreement_by_band.csv",
  "endpoint_condition_profiles.csv", "endpoint_contrast_profiles.csv",
  "endpoint_direction_concordance.csv", "endpoint_direction_summary.csv",
  "t20_t30_agreement.png", "results.md", "session_info.txt", "run.log"
)
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = outputs,
  checks = c("S1 only", "octave 125-8000 Hz", "strict-valid estimators")
)
