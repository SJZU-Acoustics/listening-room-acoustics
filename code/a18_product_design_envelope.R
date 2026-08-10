source(file.path("code", "helpers.R"))

analysis_id <- "A18"
analysis_dir <- analysis_output_dir("a18_product_design_envelope")
MODEL_BANDS <- c(125, 250, 500, 1000, 2000, 4000)
TARGET_S <- 0.4

receiver <- read_frozen_csv("receiver_band_metrics.csv")
geometry <- read_frozen_csv("room1_geometry.csv")
room_volume <- geometry$prismatic_volume_m3[[1]]

baseline <- receiver %>%
  filter(
    room_id == 1,
    source_id == "S1",
    condition_id == "closed_carpet",
    band_scheme == "octave",
    centre_frequency_hz %in% MODEL_BANDS
  ) %>%
  group_by(centre_frequency_hz) %>%
  summarise(
    n_receivers = sum(is.finite(t20_s_strict)),
    measured_t20_s = safe_mean(t20_s_strict),
    .groups = "drop"
  ) %>%
  mutate(
    room_volume_m3 = room_volume,
    current_sabine_equivalent_absorption_m2 = 0.161 * room_volume_m3 / measured_t20_s,
    target_sabine_equivalent_absorption_m2 = 0.161 * room_volume_m3 / TARGET_S,
    measured_residual_absorption_gap_m2 = pmax(
      0,
      target_sabine_equivalent_absorption_m2 - current_sabine_equivalent_absorption_m2
    )
  )

# The design's own Sabine calculation lives in the collaborator's client-facing
# Excel workbook, which is not part of the public deposit. The six-band vectors
# the calculation is built from are shipped instead, transcribed verbatim from
# that workbook (data/design_inputs/legacy_design_workbook_inputs.csv); the
# reconstruction check below still has to close to 1e-8.
workbook_inputs <- readr::read_csv(
  file.path("data", "design_inputs", "legacy_design_workbook_inputs.csv"),
  show_col_types = FALSE
) %>% arrange(centre_frequency_hz)

if (!identical(workbook_inputs$centre_frequency_hz, as.numeric(MODEL_BANDS))) {
  stop("The workbook model bands no longer match the registered six bands.", call. = FALSE)
}

eq_front_alpha <- workbook_inputs$eq50q_alpha
eq_front_area <- workbook_inputs$eq50q_front_area_m2
eq_side_area <- workbook_inputs$eq50q_side_area_m2
workbook_ceiling_alpha <- workbook_inputs$workbook_ceiling_alpha
workbook_ceiling_area <- workbook_inputs$ceiling_area_m2
workbook_carpet_alpha <- workbook_inputs$carpet_alpha
workbook_carpet_area <- workbook_inputs$carpet_area_m2
existing_wall_alpha <- workbook_inputs$existing_wall_alpha
existing_ceiling_alpha <- workbook_inputs$existing_ceiling_alpha
legacy_required_absorption <- workbook_inputs$legacy_required_absorption_m2
legacy_remaining_after_carpet <- workbook_inputs$legacy_remaining_after_carpet_m2

if (length(unique(c(eq_front_area, eq_side_area, workbook_ceiling_area, workbook_carpet_area))) < 4L) {
  stop("Unexpected loss of distinct component areas in the workbook.", call. = FALSE)
}

legacy_component_inputs <- tibble(
  centre_frequency_hz = MODEL_BANDS,
  eq50q_alpha = eq_front_alpha,
  eq50q_front_area_m2 = eq_front_area,
  eq50q_side_area_m2 = eq_side_area,
  eq50q_total_area_m2 = eq_front_area + eq_side_area,
  existing_wall_alpha = existing_wall_alpha,
  workbook_ceiling_alpha = workbook_ceiling_alpha,
  ceiling_area_m2 = workbook_ceiling_area,
  existing_ceiling_alpha = existing_ceiling_alpha,
  carpet_alpha = workbook_carpet_alpha,
  carpet_area_m2 = workbook_carpet_area
)

legacy_workbook_audit <- legacy_component_inputs %>%
  transmute(
    centre_frequency_hz,
    required_additional_absorption_m2 = legacy_required_absorption,
    eq50q_gross_absorption_m2 = eq50q_total_area_m2 * eq50q_alpha,
    ceiling_gross_absorption_m2 = ceiling_area_m2 * workbook_ceiling_alpha,
    carpet_gross_absorption_m2 = carpet_area_m2 * carpet_alpha,
    reconstructed_remaining_absorption_m2 = required_additional_absorption_m2 -
      eq50q_gross_absorption_m2 - ceiling_gross_absorption_m2 - carpet_gross_absorption_m2,
    workbook_remaining_absorption_m2 = legacy_remaining_after_carpet,
    reconstruction_error_m2 = reconstructed_remaining_absorption_m2 - workbook_remaining_absorption_m2,
    target_not_closed_under_workbook = workbook_remaining_absorption_m2 > 0
  )

if (max(abs(legacy_workbook_audit$reconstruction_error_m2)) > 1e-8) {
  stop("The legacy absorption arithmetic could not be reconstructed.", call. = FALSE)
}

library_path <- file.path("data", "design_inputs", "certified_absorption_library.csv")
certified_wide <- readr::read_csv(library_path, show_col_types = FALSE)
certified_long <- certified_wide %>%
  pivot_longer(
    starts_with("alpha_"),
    names_to = "frequency_label",
    values_to = "alpha"
  ) %>%
  mutate(centre_frequency_hz = as.numeric(str_remove(frequency_label, "alpha_"))) %>%
  select(-frequency_label)

certified_nrc_qc <- certified_long %>%
  filter(centre_frequency_hz %in% c(250, 500, 1000, 2000)) %>%
  group_by(panel_id, report_id, relationship_to_proposal, nrc_reported) %>%
  summarise(nrc_recomputed_raw = mean(alpha), .groups = "drop") %>%
  mutate(
    nrc_recomputed_nearest_005 = round(nrc_recomputed_raw / 0.05) * 0.05,
    reported_minus_recomputed = nrc_reported - nrc_recomputed_nearest_005,
    nrc_internal_match = abs(reported_minus_recomputed) < 0.025
  )

panel_octaves <- certified_long %>%
  filter(centre_frequency_hz %in% MODEL_BANDS) %>%
  select(
    panel_id, report_id, relationship_to_proposal,
    nrc_reported, centre_frequency_hz, ceiling_alpha = alpha
  )

workbook_panel <- tibble(
  panel_id = "workbook_6_18_assumption",
  report_id = "legacy-workbook",
  relationship_to_proposal = "exact_design_workbook_assumption",
  nrc_reported = NA_real_,
  centre_frequency_hz = MODEL_BANDS,
  ceiling_alpha = workbook_ceiling_alpha
)

all_panels <- bind_rows(panel_octaves, workbook_panel)

scenario_predictions <- all_panels %>%
  left_join(legacy_component_inputs, by = "centre_frequency_hz") %>%
  left_join(baseline, by = "centre_frequency_hz") %>%
  mutate(
    eq50q_net_absorption_m2 = eq50q_total_area_m2 * pmax(0, eq50q_alpha - existing_wall_alpha),
    ceiling_net_absorption_m2 = ceiling_area_m2 * pmax(0, ceiling_alpha - existing_ceiling_alpha),
    total_net_added_absorption_m2 = eq50q_net_absorption_m2 + ceiling_net_absorption_m2,
    predicted_absorption_m2 = current_sabine_equivalent_absorption_m2 + total_net_added_absorption_m2,
    predicted_t20_s = 0.161 * room_volume_m3 / predicted_absorption_m2,
    residual_to_0_4_s = predicted_t20_s - TARGET_S,
    at_or_below_0_4_s = predicted_t20_s <= TARGET_S,
    below_0_3_s = predicted_t20_s < 0.3,
    scenario_status = "Prospective replacement-adjusted Sabine-equivalent scenario; not validation"
  )

scenario_ranking <- scenario_predictions %>%
  group_by(panel_id, report_id, relationship_to_proposal, nrc_reported) %>%
  summarise(
    n_bands_at_or_below_0_4_s = sum(at_or_below_0_4_s),
    n_bands_below_0_3_s = sum(below_0_3_s),
    maximum_predicted_t20_s = max(predicted_t20_s),
    mean_predicted_t20_s = mean(predicted_t20_s),
    minimum_predicted_t20_s = min(predicted_t20_s),
    profile_range_s = max(predicted_t20_s) - min(predicted_t20_s),
    rmse_from_0_4_s = sqrt(mean((predicted_t20_s - TARGET_S)^2)),
    .groups = "drop"
  ) %>%
  arrange(desc(n_bands_at_or_below_0_4_s), maximum_predicted_t20_s, rmse_from_0_4_s) %>%
  mutate(design_envelope_rank = row_number(), .before = 1)

exact_certified <- scenario_predictions %>% filter(panel_id == "p6_18_round")
exact_workbook <- scenario_predictions %>% filter(panel_id == "workbook_6_18_assumption")
best_certified_id <- scenario_ranking %>%
  filter(relationship_to_proposal != "exact_design_workbook_assumption") %>%
  slice_min(design_envelope_rank, n = 1, with_ties = FALSE) %>%
  pull(panel_id)
best_certified <- scenario_predictions %>% filter(panel_id == best_certified_id)

gross_exact_sensitivity <- exact_certified %>%
  transmute(
    centre_frequency_hz,
    measured_t20_s,
    current_sabine_equivalent_absorption_m2,
    gross_added_absorption_m2 = eq50q_total_area_m2 * eq50q_alpha + ceiling_area_m2 * ceiling_alpha,
    replacement_adjusted_absorption_m2 = total_net_added_absorption_m2,
    gross_addition_predicted_t20_s = 0.161 * room_volume_m3 /
      (current_sabine_equivalent_absorption_m2 + gross_added_absorption_m2),
    replacement_adjusted_predicted_t20_s = predicted_t20_s,
    gross_minus_replacement_prediction_s = gross_addition_predicted_t20_s -
      replacement_adjusted_predicted_t20_s
  )

exact_coefficient_comparison <- legacy_component_inputs %>%
  select(centre_frequency_hz, workbook_ceiling_alpha) %>%
  left_join(
    panel_octaves %>%
      filter(panel_id == "p6_18_round") %>%
      select(centre_frequency_hz, certified_ceiling_alpha = ceiling_alpha),
    by = "centre_frequency_hz"
  ) %>%
  mutate(certified_minus_workbook_alpha = certified_ceiling_alpha - workbook_ceiling_alpha)

plot_data <- bind_rows(
  baseline %>% transmute(centre_frequency_hz, profile = "Measured baseline", t20_s = measured_t20_s),
  exact_workbook %>% transmute(centre_frequency_hz, profile = "6/18 workbook alpha", t20_s = predicted_t20_s),
  exact_certified %>% transmute(centre_frequency_hz, profile = "6/18 certified alpha", t20_s = predicted_t20_s),
  best_certified %>% transmute(centre_frequency_hz, profile = "Best certified envelope", t20_s = predicted_t20_s)
) %>%
  mutate(
    profile = factor(
      profile,
      levels = c(
        "Measured baseline", "6/18 workbook alpha",
        "6/18 certified alpha", "Best certified envelope"
      )
    )
  )

profile_colours <- c(
  "Measured baseline" = "#000000",
  "6/18 workbook alpha" = "#E69F00",
  "6/18 certified alpha" = "#0072B2",
  "Best certified envelope" = "#009E73"
)
profile_linetypes <- c(
  "Measured baseline" = "solid",
  "6/18 workbook alpha" = "dashed",
  "6/18 certified alpha" = "solid",
  "Best certified envelope" = "dotdash"
)
profile_labels <- c(
  "Measured baseline" = "Measured",
  "6/18 workbook alpha" = "Workbook 6/18",
  "6/18 certified alpha" = "Certified 6/18",
  "Best certified envelope" = "Best certified"
)

scenario_plot <- ggplot(plot_data, aes(x = centre_frequency_hz, y = t20_s, colour = profile, linetype = profile)) +
  geom_hline(yintercept = TARGET_S, linewidth = 0.4, colour = "#666666") +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.6) +
  scale_x_log10(breaks = MODEL_BANDS, labels = frequency_label) +
  scale_colour_manual(values = profile_colours, labels = profile_labels) +
  scale_linetype_manual(values = profile_linetypes, labels = profile_labels) +
  guides(
    colour = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  labs(x = "Octave-band centre frequency (Hz)", y = "Measured or scenario T20 (s)") +
  theme_classic(base_family = "Arial", base_size = 9) %+replace%
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.45),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length = grid::unit(0.10, "cm"),
    axis.title = element_text(size = 10, colour = "black"),
    axis.text = element_text(size = 9, colour = "black"),
    legend.position = "top",
    legend.justification = "left",
    legend.box.just = "left",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.background = element_blank(),
    legend.key.width = grid::unit(9, "mm"),
    legend.spacing.x = grid::unit(1.5, "mm"),
    legend.margin = margin(0, 0, 2, 0),
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path(analysis_dir, "product_design_scenarios.png"), scenario_plot,
  width = 85, height = 85, units = "mm", dpi = 600, bg = "white"
)

readr::write_csv(baseline, file.path(analysis_dir, "measured_closed_carpet_baseline.csv"))
readr::write_csv(legacy_component_inputs, file.path(analysis_dir, "legacy_component_inputs.csv"))
readr::write_csv(legacy_workbook_audit, file.path(analysis_dir, "legacy_workbook_claim_audit.csv"))
readr::write_csv(certified_nrc_qc, file.path(analysis_dir, "certified_library_nrc_qc.csv"))
readr::write_csv(exact_coefficient_comparison, file.path(analysis_dir, "exact_6_18_coefficient_comparison.csv"))
readr::write_csv(scenario_predictions, file.path(analysis_dir, "product_envelope_predictions.csv"))
readr::write_csv(scenario_ranking, file.path(analysis_dir, "product_envelope_ranking.csv"))
readr::write_csv(gross_exact_sensitivity, file.path(analysis_dir, "gross_vs_replacement_sensitivity.csv"))

legacy_unclosed <- sum(legacy_workbook_audit$target_not_closed_under_workbook)
exact_rank <- scenario_ranking %>% filter(panel_id == "p6_18_round")
workbook_rank <- scenario_ranking %>% filter(panel_id == "workbook_6_18_assumption")
best_rank <- scenario_ranking %>% filter(panel_id == best_certified_id)
nrc_mismatches <- certified_nrc_qc %>% filter(!nrc_internal_match)
exact_unresolved <- exact_certified %>%
  filter(!at_or_below_0_4_s) %>%
  arrange(centre_frequency_hz)

results <- c(
  "# A18 results",
  "",
  "**Verdict:** The product archive supports a transparent prospective design envelope, but falsifies the legacy full-band 0.4 s claim: even the exact certified 6/18 scenario leaves low-band gaps and remains unvalidated.",
  "",
  "## Outcome",
  "",
  sprintf("- The legacy workbook arithmetic reproduces exactly, but its residual absorption remains positive in %d of six claimed bands; the workbook therefore does not support its own full-band 0.4 s statement.", legacy_unclosed),
  sprintf("- The matching certified 6/18 report has octave coefficients %.2f-%.2f higher than the workbook assumptions (median difference %.2f).", min(exact_coefficient_comparison$certified_minus_workbook_alpha), max(exact_coefficient_comparison$certified_minus_workbook_alpha), median(exact_coefficient_comparison$certified_minus_workbook_alpha)),
  sprintf("- From the measured closed-curtain/carpet baseline, the replacement-adjusted 6/18 certified scenario reaches 0.4 s in %d/6 bands; the unresolved bands are %s, and its maximum predicted T20 is %.3f s. The workbook-coefficient version reaches %d/6 bands with maximum %.3f s.", exact_rank$n_bands_at_or_below_0_4_s, paste(paste0(frequency_label(exact_unresolved$centre_frequency_hz), " Hz"), collapse = " and "), exact_rank$maximum_predicted_t20_s, workbook_rank$n_bands_at_or_below_0_4_s, workbook_rank$maximum_predicted_t20_s),
  sprintf("- The best same-area certified envelope is %s (%s): %d/6 bands at or below 0.4 s, maximum predicted T20 %.3f s, and %d bands below 0.3 s.", best_rank$panel_id, best_rank$report_id, best_rank$n_bands_at_or_below_0_4_s, best_rank$maximum_predicted_t20_s, best_rank$n_bands_below_0_3_s),
  sprintf("- Certified-library QA finds %d/%d reported NRC values inconsistent with recomputation to the nearest 0.05; band coefficients, not NRC labels, drive all scenarios.", nrow(nrc_mismatches), nrow(certified_nrc_qc)),
  "",
  "## Decision",
  "",
  "The product records make a prospective design-envelope analysis feasible, but they do not validate the renovation. The exact proposed perforation should be represented by the certified band coefficients rather than the weaker workbook values. Any retained design claim must report the unresolved bands and distinguish the documented calculable subset from side curtains, 60 Hz traps and other components without usable coefficients.",
  "",
  "## Assumptions and limitations",
  "",
  "Predictions use measured T20 as a Sabine-equivalent decay diagnostic, corrected Room-1 volume and legacy surface-absorption assumptions. They subtract the covered wall/ceiling absorption before adding products. The certified systems were tested in their reported reverberation-room assemblies; most are alternatives rather than the proposed 6/18 construction, and even the closest report does not reproduce the sloped in-room cavity exactly. The calculations are prospective scenarios, not simulation or post-treatment evidence.",
  "",
  "The design's own six-band absorption assumptions are transcribed in `data/design_inputs/legacy_design_workbook_inputs.csv`; the reconstruction check above confirms they reproduce the workbook's own arithmetic."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c(
    "receiver_band_metrics.csv", "room1_geometry.csv",
    "legacy_design_workbook_inputs.csv", "certified_absorption_library.csv"
  ),
  outputs = c(
    "measured_closed_carpet_baseline.csv", "legacy_component_inputs.csv",
    "legacy_workbook_claim_audit.csv", "certified_library_nrc_qc.csv",
    "exact_6_18_coefficient_comparison.csv", "product_envelope_predictions.csv",
    "product_envelope_ranking.csv", "gross_vs_replacement_sensitivity.csv",
    "product_design_scenarios.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c(
    "legacy arithmetic reconstructed", "covered-surface absorption subtracted",
    "measured closed-curtain/carpet baseline", "no intervention-validation claim"
  )
)
