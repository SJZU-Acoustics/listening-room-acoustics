source(file.path("code", "helpers.R"))

analysis_id <- "A12"
analysis_dir <- analysis_output_dir("a12_design_target_gap")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
geometry <- read_frozen_csv("room1_geometry.csv")
critical_frequency <- read_frozen_csv("room1_critical_frequency.csv")

primary <- strict_primary_rows(receiver) %>%
  mutate(room_id = as.numeric(as.character(room_id)))

long <- primary %>%
  select(
    room_id, condition_id, receiver_id, centre_frequency_hz,
    T20 = t20_s_strict, T30 = t30_s_strict
  ) %>%
  pivot_longer(cols = c(T20, T30), names_to = "metric", values_to = "value_s")

targets <- tribble(
  ~target_id, ~target_s, ~target_status,
  "design_target", 0.4, "Adopted target in the unimplemented renovation proposal",
  "legacy_recommended", 0.28, "Lower legacy recommendation retained for reconciliation only"
)

target_gap <- long %>%
  tidyr::crossing(targets) %>%
  group_by(
    metric, room_id, condition_id, centre_frequency_hz,
    target_id, target_s, target_status
  ) %>%
  summarise(
    n_valid_receivers = sum(is.finite(value_s)),
    measured_mean_s = safe_mean(value_s),
    measured_sd_s = safe_sd(value_s),
    residual_gap_s = measured_mean_s - first(target_s),
    proportion_receivers_above_target = mean(value_s > first(target_s), na.rm = TRUE),
    .groups = "drop"
  )

target_overall <- target_gap %>%
  group_by(metric, room_id, condition_id, target_id, target_s, target_status) %>%
  summarise(
    equal_band_mean_s = mean(measured_mean_s),
    equal_band_residual_gap_s = mean(residual_gap_s),
    mean_band_proportion_above_target = mean(proportion_receivers_above_target),
    n_bands = n(),
    .groups = "drop"
  )

closest_conditions <- target_overall %>%
  group_by(metric, room_id, target_id) %>%
  slice_min(equal_band_mean_s, n = 1, with_ties = FALSE) %>%
  ungroup()

t20_condition_means <- target_gap %>%
  filter(metric == "T20", target_id == "design_target") %>%
  select(room_id, condition_id, centre_frequency_hz, measured_mean_s) %>%
  distinct() %>%
  pivot_wider(names_from = condition_id, values_from = measured_mean_s)

curtain_gap_closure <- bind_rows(
  t20_condition_means %>% transmute(
    room_id, centre_frequency_hz, carpet_state = "no_carpet",
    open_s = open_no_carpet, closed_s = closed_no_carpet
  ),
  t20_condition_means %>% transmute(
    room_id, centre_frequency_hz, carpet_state = "carpet",
    open_s = open_carpet, closed_s = closed_carpet
  )
) %>%
  mutate(
    target_s = 0.4,
    curtain_reduction_s = open_s - closed_s,
    initial_gap_s = open_s - target_s,
    residual_gap_s = closed_s - target_s,
    fraction_initial_gap_closed = curtain_reduction_s / initial_gap_s
  )

room1_volume <- geometry$prismatic_volume_m3[[1]]
room1_absorption <- target_gap %>%
  filter(metric == "T20", room_id == 1, target_id == "design_target") %>%
  transmute(
    room_id, condition_id, centre_frequency_hz,
    measured_t20_s = measured_mean_s,
    target_s,
    room_volume_m3 = room1_volume,
    current_sabine_equivalent_absorption_m2 = 0.161 * room_volume_m3 / measured_t20_s,
    target_sabine_equivalent_absorption_m2 = 0.161 * room_volume_m3 / target_s,
    additional_sabine_equivalent_absorption_m2 = pmax(
      0,
      target_sabine_equivalent_absorption_m2 - current_sabine_equivalent_absorption_m2
    ),
    interpretation = "Broad-band Sabine-equivalent diagnostic; not a product or post-treatment prediction"
  )

plot_data <- target_gap %>%
  filter(metric == "T20", target_id == "design_target") %>%
  mutate(
    frequency = factor(frequency_label(centre_frequency_hz), levels = frequency_label(CORE_OCTAVE_BANDS)),
    condition_id = factor(condition_id, levels = CONDITION_LEVELS),
    room_label = paste("Room", room_id)
  )

target_plot <- ggplot(
  plot_data,
  aes(x = frequency, y = measured_mean_s, colour = condition_id, group = condition_id)
) +
  geom_hline(yintercept = 0.4, linetype = "dashed", linewidth = 0.55, colour = "#9B2226") +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.7) +
  facet_wrap(~ room_label, ncol = 1) +
  scale_colour_manual(values = P31_COLOURS, labels = CONDITION_LABELS, name = NULL) +
  labs(
    title = "Measured strict T20 against the inherited 0.4 s design target",
    subtitle = "The target is a proposal scenario, not a verified compliance limit",
    x = "Octave-band centre frequency (Hz)",
    y = "T20 (s)"
  ) +
  theme_p31()

readr::write_csv(target_gap, file.path(analysis_dir, "condition_band_target_gaps.csv"))
readr::write_csv(target_overall, file.path(analysis_dir, "condition_target_gap_summary.csv"))
readr::write_csv(closest_conditions, file.path(analysis_dir, "closest_measured_conditions.csv"))
readr::write_csv(curtain_gap_closure, file.path(analysis_dir, "curtain_fraction_of_gap_closed.csv"))
readr::write_csv(room1_absorption, file.path(analysis_dir, "room1_sabine_absorption_diagnostic.csv"))
readr::write_csv(critical_frequency, file.path(analysis_dir, "room1_critical_frequency_context.csv"))
save_p31_plot(target_plot, file.path(analysis_dir, "t20_design_target_gap.png"), width_mm = 170, height_mm = 150)

best_t20 <- closest_conditions %>% filter(metric == "T20", target_id == "design_target")
gap_fraction <- curtain_gap_closure %>%
  group_by(room_id, carpet_state) %>%
  summarise(mean_fraction = mean(fraction_initial_gap_closed), .groups = "drop")
treated_absorption <- room1_absorption %>% filter(condition_id == "closed_carpet")

results <- c(
  "# A12 results",
  "",
  "## Outcome",
  "",
  sprintf("- Closest equal-band strict-T20 condition to 0.4 s: %s.", paste(sprintf("Room %d %s at %.3f s, leaving %.3f s", best_t20$room_id, best_t20$condition_id, best_t20$equal_band_mean_s, best_t20$equal_band_residual_gap_s), collapse = "; ")),
  sprintf("- Mean fraction of the initial 0.4 s gap closed by the curtain: %s.", paste(sprintf("Room %d %s %.1f%%", gap_fraction$room_id, gap_fraction$carpet_state, 100 * gap_fraction$mean_fraction), collapse = "; ")),
  sprintf("- In Room 1 closed-curtain/carpet measurements, the Sabine-equivalent additional absorption needed for 0.4 s has a median %.1f m2 and ranges from %.1f to %.1f m2 across core bands.", median(treated_absorption$additional_sabine_equivalent_absorption_m2), min(treated_absorption$additional_sabine_equivalent_absorption_m2), max(treated_absorption$additional_sabine_equivalent_absorption_m2)),
  sprintf("- The frozen Room-1 design-target critical-frequency scenario is %.1f Hz; all primary octave centres are above it, but the small-room/diffuse-field limitation remains.", critical_frequency$critical_frequency_hz[critical_frequency$scenario == "design_target"]),
  "",
  "## Decision",
  "",
  "Measured curtain closure improves decay but does not by itself reach the inherited 0.4 s scenario. Engineering translation must remain a quantified residual requirement for an unimplemented design, not a prediction that the proposed products would achieve the target.",
  "",
  "## Assumptions and limitations",
  "",
  "T20 is compared with a legacy RT target as the primary diagnostic; T30 remains available as sensitivity evidence. The Sabine calculation assumes a diffuse broad-band field and uses Room 1 geometry only. No room-2 geometry, treatment simulation, installation, or post-treatment validation exists."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c("receiver_band_metrics.csv", "room1_geometry.csv", "room1_critical_frequency.csv"),
  outputs = c(
    "condition_band_target_gaps.csv", "condition_target_gap_summary.csv",
    "closest_measured_conditions.csv", "curtain_fraction_of_gap_closed.csv",
    "room1_sabine_absorption_diagnostic.csv", "room1_critical_frequency_context.csv",
    "t20_design_target_gap.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c("target provenance explicit", "no compliance claim", "Room 1 geometry only")
)
