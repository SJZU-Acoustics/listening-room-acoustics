source(file.path("code", "helpers.R"))

analysis_id <- "A23"
analysis_dir <- analysis_output_dir("a23_post_review_scenario_envelope")

# Post-review sensitivity (2026-08-08 AI-review triage): formula and coefficient
# uncertainty envelope for the closest-certified replacement-adjusted scenario.
# Deterministic arithmetic on the frozen A18 inputs; no new hypothesis tests.

ROOM_VOLUME_M3 <- 139.1758441
# Total interior surface from the frozen DXF construction (convex hull of the
# WALL-layer z = 0 vertices, reproducing footprint_area_m2 = 48.47643473 m2):
# perimeter 27.290101 m, height 2.871 m, walls 78.3499 m2, floor + ceiling
# 2 x 48.47643473 m2.
TOTAL_SURFACE_M2 <- 2 * 48.47643473 + 27.290101 * 2.871

ALPHA_NEW_RELATIVE <- 0.10   # certified-coefficient perturbation (inter-laboratory order)
ALPHA_EXISTING_ABSOLUTE <- 0.02  # existing-surface coefficient perturbation

predictions <- readr::read_csv(
  file.path(OUTPUT_ROOT, "a18_product_design_envelope", "product_envelope_predictions.csv"),
  show_col_types = FALSE
) %>%
  filter(panel_id == "p6_18_round") %>%
  transmute(
    centre_frequency_hz,
    measured_t20_s,
    eq50q_area_m2 = eq50q_total_area_m2,
    ceiling_area_m2,
    eq50q_alpha,
    ceiling_alpha,
    existing_wall_alpha,
    existing_ceiling_alpha,
    frozen_predicted_t20_s = predicted_t20_s
  )

sabine_t <- function(absorption_m2) 0.161 * ROOM_VOLUME_M3 / absorption_m2

eyring_t <- function(absorption_m2) {
  mean_alpha <- absorption_m2 / TOTAL_SURFACE_M2
  0.161 * ROOM_VOLUME_M3 / (-TOTAL_SURFACE_M2 * log(1 - mean_alpha))
}

scenario_t <- function(data, formula, alpha_new_scale, alpha_existing_shift) {
  data %>%
    mutate(
      alpha_eq50q = pmin(eq50q_alpha * alpha_new_scale, 1),
      alpha_ceiling = pmin(ceiling_alpha * alpha_new_scale, 1),
      alpha_wall_existing = pmax(existing_wall_alpha + alpha_existing_shift, 0),
      alpha_ceiling_existing = pmax(existing_ceiling_alpha + alpha_existing_shift, 0),
      added_absorption_m2 =
        eq50q_area_m2 * pmax(0, alpha_eq50q - alpha_wall_existing) +
        ceiling_area_m2 * pmax(0, alpha_ceiling - alpha_ceiling_existing),
      baseline_absorption_m2 = if (formula == "sabine") {
        0.161 * ROOM_VOLUME_M3 / measured_t20_s
      } else {
        TOTAL_SURFACE_M2 *
          (1 - exp(-0.161 * ROOM_VOLUME_M3 / (TOTAL_SURFACE_M2 * measured_t20_s)))
      },
      scenario_t20_s = if (formula == "sabine") {
        sabine_t(baseline_absorption_m2 + added_absorption_m2)
      } else {
        eyring_t(baseline_absorption_m2 + added_absorption_m2)
      }
    ) %>%
    transmute(centre_frequency_hz, measured_t20_s, frozen_predicted_t20_s,
              added_absorption_m2, scenario_t20_s)
}

variants <- tribble(
  ~variant_id, ~formula, ~alpha_new_scale, ~alpha_existing_shift,
  "sabine_base", "sabine", 1.0, 0.0,
  "sabine_conservative", "sabine", 1 - ALPHA_NEW_RELATIVE, ALPHA_EXISTING_ABSOLUTE,
  "sabine_optimistic", "sabine", 1 + ALPHA_NEW_RELATIVE, -ALPHA_EXISTING_ABSOLUTE,
  "eyring_base", "eyring", 1.0, 0.0,
  "eyring_conservative", "eyring", 1 - ALPHA_NEW_RELATIVE, ALPHA_EXISTING_ABSOLUTE,
  "eyring_optimistic", "eyring", 1 + ALPHA_NEW_RELATIVE, -ALPHA_EXISTING_ABSOLUTE
)

envelope <- bind_rows(lapply(seq_len(nrow(variants)), function(i) {
  v <- variants[i, ]
  scenario_t(predictions, v$formula, v$alpha_new_scale, v$alpha_existing_shift) %>%
    mutate(
      variant_id = v$variant_id,
      formula = v$formula,
      alpha_new_scale = v$alpha_new_scale,
      alpha_existing_shift = v$alpha_existing_shift,
      at_or_below_0_4_s = scenario_t20_s <= 0.4,
      .before = 1
    )
}))

# The Sabine base variant must reproduce the frozen A18 scenario exactly.
base_check <- envelope %>%
  filter(variant_id == "sabine_base") %>%
  mutate(error_s = abs(scenario_t20_s - frozen_predicted_t20_s))
if (max(base_check$error_s) > 1e-9) {
  stop("Sabine base variant does not reproduce the frozen A18 scenario.", call. = FALSE)
}

variant_summary <- envelope %>%
  group_by(variant_id, formula, alpha_new_scale, alpha_existing_shift) %>%
  summarise(
    n_bands = n(),
    n_bands_at_or_below_0_4_s = sum(at_or_below_0_4_s),
    t20_125_s = scenario_t20_s[centre_frequency_hz == 125],
    t20_250_s = scenario_t20_s[centre_frequency_hz == 250],
    t20_2000_s = scenario_t20_s[centre_frequency_hz == 2000],
    t20_4000_s = scenario_t20_s[centre_frequency_hz == 4000],
    .groups = "drop"
  ) %>%
  arrange(match(variant_id, variants$variant_id))

band_range <- envelope %>%
  group_by(centre_frequency_hz) %>%
  summarise(
    minimum_scenario_t20_s = min(scenario_t20_s),
    maximum_scenario_t20_s = max(scenario_t20_s),
    n_variants_at_or_below_0_4_s = sum(at_or_below_0_4_s),
    .groups = "drop"
  )

readr::write_csv(envelope, file.path(analysis_dir, "scenario_envelope.csv"))
readr::write_csv(variant_summary, file.path(analysis_dir, "variant_summary.csv"))
readr::write_csv(band_range, file.path(analysis_dir, "band_range.csv"))

results <- c(
  "# A23 results (post-review scenario uncertainty envelope)",
  "",
  sprintf(
    "Closest-certified replacement-adjusted scenario; V = %.4f m3, S = %.4f m2 (DXF hull).",
    ROOM_VOLUME_M3, TOTAL_SURFACE_M2
  ),
  sprintf(
    "Perturbations: certified/new coefficients x(1 +/- %.2f), existing-surface coefficients +/- %.2f absolute.",
    ALPHA_NEW_RELATIVE, ALPHA_EXISTING_ABSOLUTE
  ),
  "",
  "## Variants",
  "",
  sprintf(
    "- %s: %d/6 bands at or below 0.4 s (125 Hz %.3f, 250 Hz %.3f, 2 kHz %.3f, 4 kHz %.3f s)",
    variant_summary$variant_id, variant_summary$n_bands_at_or_below_0_4_s,
    variant_summary$t20_125_s, variant_summary$t20_250_s,
    variant_summary$t20_2000_s, variant_summary$t20_4000_s
  ),
  "",
  "## Band ranges across the six variants",
  "",
  sprintf(
    "- %g Hz: %.3f to %.3f s (%d/6 variants at or below 0.4 s)",
    band_range$centre_frequency_hz, band_range$minimum_scenario_t20_s,
    band_range$maximum_scenario_t20_s, band_range$n_variants_at_or_below_0_4_s
  )
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c(
    "exploration/a18_product_design_envelope/product_envelope_predictions.csv",
    "data/from-author-2026-08-07 DXF hull geometry (perimeter 27.290101 m)"
  ),
  outputs = c(
    "scenario_envelope.csv", "variant_summary.csv", "band_range.csv",
    "results.md", "session_info.txt", "run.log"
  ),
  checks = c(
    "Sabine base variant reproduces frozen A18 scenario to < 1e-9 s",
    "deterministic arithmetic; no new hypothesis tests"
  )
)
