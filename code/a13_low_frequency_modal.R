source(file.path("code", "helpers.R"))

analysis_id <- "A13"
analysis_dir <- analysis_output_dir("a13_low_frequency_modal")

modes <- read_frozen_csv("room1_modes_to_200hz.csv")
critical <- read_frozen_csv("room1_critical_frequency.csv")
condition <- read_frozen_csv("condition_band_summary.csv")
receiver <- read_frozen_csv("receiver_band_metrics.csv")

mode_clusters <- modes %>%
  arrange(frequency_hz) %>%
  mutate(
    previous_gap_hz = frequency_hz - lag(frequency_hz),
    cluster_id = cumsum(is.na(previous_gap_hz) | previous_gap_hz > 1)
  ) %>%
  group_by(cluster_id) %>%
  summarise(
    cluster_min_hz = min(frequency_hz),
    cluster_max_hz = max(frequency_hz),
    cluster_centre_hz = mean(frequency_hz),
    n_modes = n(),
    n_axial = sum(mode_type == "axial"),
    mode_types = paste(sort(unique(mode_type)), collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(cluster_centre_hz)

critical_mode_counts <- critical %>%
  rowwise() %>%
  mutate(
    n_modes_at_or_below = sum(modes$frequency_hz <= critical_frequency_hz),
    n_axial_modes_at_or_below = sum(
      modes$frequency_hz <= critical_frequency_hz & modes$mode_type == "axial"
    )
  ) %>%
  ungroup()

legacy_targets <- tibble(target_frequency_hz = c(40, 60))
nearest_modes <- legacy_targets %>%
  rowwise() %>%
  reframe({
    distance <- abs(modes$frequency_hz - target_frequency_hz)
    nearest <- modes[distance == min(distance), ]
    tibble(
      target_frequency_hz = target_frequency_hz,
      p = nearest$p,
      q = nearest$q,
      r = nearest$r,
      mode_type = nearest$mode_type,
      nearest_mode_hz = nearest$frequency_hz,
      absolute_difference_hz = abs(nearest$frequency_hz - target_frequency_hz)
    )
  })

third_octave_bands <- receiver %>%
  filter(
    room_id == 1,
    band_scheme == "third_octave",
    centre_frequency_hz <= 200
  ) %>%
  distinct(
    centre_frequency_hz, lower_cutoff_hz, upper_cutoff_hz
  ) %>%
  arrange(centre_frequency_hz)

mode_count_by_third_octave <- third_octave_bands %>%
  rowwise() %>%
  mutate(
    n_modes = sum(
      modes$frequency_hz >= lower_cutoff_hz & modes$frequency_hz < upper_cutoff_hz
    ),
    n_axial_modes = sum(
      modes$frequency_hz >= lower_cutoff_hz & modes$frequency_hz < upper_cutoff_hz &
        modes$mode_type == "axial"
    )
  ) %>%
  ungroup()

diagnostic_decay <- condition %>%
  filter(
    source_scope == "S1",
    band_scheme == "octave",
    centre_frequency_hz %in% c(31.5, 63, 125, 250, 500, 1000)
  ) %>%
  select(
    room_id, condition_id, centre_frequency_hz,
    t20_s, t20_s_strict, t20_s_n_receivers_strict,
    t30_s, t30_s_strict, t30_s_n_receivers_strict
  )

decay_excess <- diagnostic_decay %>%
  select(room_id, condition_id, centre_frequency_hz, T20 = t20_s, T30 = t30_s) %>%
  pivot_longer(cols = c(T20, T30), names_to = "metric", values_to = "decay_s") %>%
  group_by(room_id, condition_id, metric) %>%
  summarise(
    low_31_63_mean_s = mean(decay_s[centre_frequency_hz %in% c(31.5, 63)]),
    transition_125_250_mean_s = mean(decay_s[centre_frequency_hz %in% c(125, 250)]),
    mid_500_1000_mean_s = mean(decay_s[centre_frequency_hz %in% c(500, 1000)]),
    low_to_mid_ratio = low_31_63_mean_s / mid_500_1000_mean_s,
    transition_to_mid_ratio = transition_125_250_mean_s / mid_500_1000_mean_s,
    .groups = "drop"
  )

mode_plot <- ggplot(modes, aes(x = frequency_hz, y = mode_type, colour = mode_type)) +
  geom_point(alpha = 0.7, size = 1.3, position = position_jitter(height = 0.08, width = 0)) +
  geom_vline(xintercept = c(40, 60), linetype = "dashed", linewidth = 0.4, colour = "#9B2226") +
  geom_vline(
    xintercept = critical$critical_frequency_hz[critical$scenario == "design_target"],
    linetype = "dotted", linewidth = 0.5, colour = "#555555"
  ) +
  scale_colour_manual(values = c(axial = "#9B2226", tangential = "#2A6F97", oblique = "#2A9D8F"), guide = "none") +
  scale_x_continuous(limits = c(20, 200), breaks = seq(20, 200, 20)) +
  labs(
    title = "Room-1 rectangular-bounds modes to 200 Hz",
    subtitle = "Dashed: inherited 40/60 Hz concerns; dotted: 0.4 s critical-frequency scenario",
    x = "Frequency (Hz)",
    y = NULL
  ) +
  theme_p31()

decay_plot_data <- diagnostic_decay %>%
  filter(room_id == 1) %>%
  mutate(
    frequency = factor(
      frequency_label(centre_frequency_hz),
      levels = frequency_label(c(31.5, 63, 125, 250, 500, 1000))
    ),
    condition_id = factor(condition_id, levels = CONDITION_LEVELS)
  )

decay_plot <- ggplot(
  decay_plot_data,
  aes(x = frequency, y = t20_s, colour = condition_id, group = condition_id)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = P31_COLOURS, labels = CONDITION_LABELS, name = NULL) +
  labs(
    title = "Room-1 diagnostic octave T20",
    subtitle = "31.5/63 Hz values are unrestricted diagnostics, not primary endpoints",
    x = "Octave-band centre frequency (Hz)",
    y = "T20 (s)"
  ) +
  theme_p31()

combined_plot <- patchwork::wrap_plots(
  mode_plot,
  decay_plot,
  ncol = 1,
  heights = c(1, 1.05)
)

readr::write_csv(mode_clusters, file.path(analysis_dir, "mode_clusters_within_1hz.csv"))
readr::write_csv(critical_mode_counts, file.path(analysis_dir, "critical_frequency_mode_counts.csv"))
readr::write_csv(nearest_modes, file.path(analysis_dir, "nearest_modes_to_legacy_frequencies.csv"))
readr::write_csv(mode_count_by_third_octave, file.path(analysis_dir, "mode_count_by_third_octave.csv"))
readr::write_csv(diagnostic_decay, file.path(analysis_dir, "diagnostic_low_frequency_decay.csv"))
readr::write_csv(decay_excess, file.path(analysis_dir, "low_frequency_decay_excess.csv"))
save_p31_plot(combined_plot, file.path(analysis_dir, "room1_modal_decay_diagnosis.png"), width_mm = 180, height_mm = 210)

design_critical <- critical_mode_counts %>% filter(scenario == "design_target")
measured_critical <- critical_mode_counts %>% filter(scenario == "measured_open_no_carpet_s1_midband")
room1_t20_excess <- decay_excess %>% filter(room_id == 1, metric == "T20")
largest_cluster <- mode_clusters %>% slice_max(n_modes, n = 1, with_ties = FALSE)

raw_extension_decision <- c(
  "# Raw-IR extension decision",
  "",
  "A narrowband modal-decay or Q-factor feature family is not opened at this point.",
  "",
  "The frozen evidence already establishes dense/clustered bounding-box modes below the measured critical-frequency scenario, low-frequency octave decay excess, and direct geometric proximity to the inherited 40/60 Hz concerns. A new narrowband estimator would require a documented Stage-1 builder extension, a refreshed manifest and later regeneration of the private deposit. It would not create room-2 geometry, treatment simulation, or post-treatment validation.",
  "",
  "Reconsider only if Prof Zhang selects modal persistence as the central Stage-3 contribution and explicitly authorises a new frozen feature family. Until then, the extra analysis cost and estimator dependence exceed its decision gain."
)
writeLines(raw_extension_decision, file.path(analysis_dir, "raw_ir_extension_decision.md"), useBytes = TRUE)

results <- c(
  "# A13 results",
  "",
  "## Outcome",
  "",
  sprintf("- Modes at or below the 0.4 s design critical frequency (%.1f Hz): %d total, including %d axial; at or below the measured mid-band scenario (%.1f Hz): %d total.", design_critical$critical_frequency_hz, design_critical$n_modes_at_or_below, design_critical$n_axial_modes_at_or_below, measured_critical$critical_frequency_hz, measured_critical$n_modes_at_or_below),
  sprintf("- Nearest bounding-box modes to the inherited concerns: %s.", paste(sprintf("%.0f Hz -> %.2f Hz %s (%d,%d,%d)", nearest_modes$target_frequency_hz, nearest_modes$nearest_mode_hz, nearest_modes$mode_type, nearest_modes$p, nearest_modes$q, nearest_modes$r), collapse = "; ")),
  sprintf("- Largest within-1-Hz mode cluster: %d modes centred at %.2f Hz.", largest_cluster$n_modes, largest_cluster$cluster_centre_hz),
  sprintf("- Room-1 unrestricted T20 31.5/63-to-500/1000-Hz ratios range from %.2f to %.2f across measured conditions.", min(room1_t20_excess$low_to_mid_ratio), max(room1_t20_excess$low_to_mid_ratio)),
  "",
  "## Decision",
  "",
  "The frozen geometry and octave diagnostics support a low-frequency modal-risk interpretation, but not a narrowband Q-factor or treatment-validation claim. The raw-IR extension is deferred as non-decision-changing at Stage 2.",
  "",
  "## Assumptions and limitations",
  "",
  "Rectangular bounding dimensions ignore the four chamfers and furnishings; octave decay blends multiple modes; 31.5/63 Hz strict coverage is insufficient for primary inference; and Room 2 has no geometry."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c(
    "room1_modes_to_200hz.csv", "room1_critical_frequency.csv",
    "condition_band_summary.csv", "receiver_band_metrics.csv"
  ),
  outputs = c(
    "mode_clusters_within_1hz.csv", "critical_frequency_mode_counts.csv",
    "nearest_modes_to_legacy_frequencies.csv", "mode_count_by_third_octave.csv",
    "diagnostic_low_frequency_decay.csv", "low_frequency_decay_excess.csv",
    "room1_modal_decay_diagnosis.png", "raw_ir_extension_decision.md",
    "results.md", "session_info.txt", "run.log"
  ),
  checks = c("frozen features only", "31.5/63 Hz diagnostic", "raw extension decision recorded")
)
