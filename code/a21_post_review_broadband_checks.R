source(file.path("code", "helpers.R"))

analysis_id <- "A21"
analysis_dir <- analysis_output_dir("a21_post_review_broadband_checks")

# Post-review sensitivity checks on the frozen core (2026-08-08 AI-review triage).
# Descriptive only: no new hypothesis tests, no change to any declared family.

receiver_metrics <- read_frozen_csv("receiver_band_metrics.csv")

COMMON_BANDS <- c(500, 1000, 2000, 4000, 8000)

strict_cells <- receiver_metrics %>%
  filter(
    source_id == "S1",
    band_scheme == "octave",
    centre_frequency_hz %in% CORE_OCTAVE_BANDS
  ) %>%
  transmute(
    room_id, receiver_id, condition_id,
    centre_frequency_hz = as.numeric(centre_frequency_hz),
    t20_strict = t20_s_strict,
    strict_available = is.finite(t20_s_strict)
  )

# ---------------------------------------------------------------------------
# 1. Common-band receiver curtain estimates (500 Hz - 8 kHz, complete support)
# ---------------------------------------------------------------------------
receiver_band_effects <- strict_cells %>%
  filter(strict_available) %>%
  select(-strict_available) %>%
  pivot_wider(names_from = condition_id, values_from = t20_strict) %>%
  filter(
    is.finite(open_no_carpet), is.finite(closed_no_carpet),
    is.finite(open_carpet), is.finite(closed_carpet)
  ) %>%
  mutate(
    curtain_effect_s = ((open_no_carpet - closed_no_carpet) +
                          (open_carpet - closed_carpet)) / 2
  )

common_band_receivers <- receiver_band_effects %>%
  filter(centre_frequency_hz %in% COMMON_BANDS) %>%
  group_by(room_id, receiver_id) %>%
  summarise(
    n_bands = n(),
    curtain_effect_common_s = mean(curtain_effect_s),
    .groups = "drop"
  )

if (nrow(common_band_receivers) != 20 || any(common_band_receivers$n_bands != 5)) {
  stop("Common-band support is not complete at all 20 receivers.", call. = FALSE)
}

# Cross-check against the all-available-band receiver estimates. These are the
# values Figure 3b plots; taking them from A10's own output rather than from the
# figure's locked copy keeps the module independent of display build order.
locked_receivers <- readr::read_csv(
  file.path(OUTPUT_ROOT, "a10_spatial_uniformity", "receiver_curtain_effects.csv"),
  show_col_types = FALSE
) %>% filter(metric == "T20")
comparison <- common_band_receivers %>%
  left_join(
    locked_receivers %>%
      transmute(room_id = as.numeric(room_id), receiver_id = as.numeric(receiver_id),
                curtain_effect_locked_s = curtain_effect, n_bands_locked = n_complete_bands),
    by = c("room_id", "receiver_id")
  ) %>%
  mutate(shift_s = curtain_effect_common_s - curtain_effect_locked_s)

common_band_summary <- comparison %>%
  group_by(room_id) %>%
  summarise(
    n_receivers = n(),
    n_positive = sum(curtain_effect_common_s > 0),
    minimum_s = min(curtain_effect_common_s),
    maximum_s = max(curtain_effect_common_s),
    maximum_abs_shift_s = max(abs(shift_s)),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 2. Strict-validity balance across curtain states at 125 and 250 Hz
# ---------------------------------------------------------------------------
validity_balance <- strict_cells %>%
  filter(centre_frequency_hz %in% c(125, 250)) %>%
  mutate(curtain_state = if_else(str_starts(condition_id, "closed"), "closed", "open")) %>%
  group_by(centre_frequency_hz, curtain_state) %>%
  summarise(
    n_cells = n(),
    n_strict_valid = sum(strict_available),
    .groups = "drop"
  )

validity_by_state_band <- strict_cells %>%
  group_by(room_id, condition_id, centre_frequency_hz) %>%
  summarise(
    n_cells = n(),
    n_strict_valid = sum(strict_available),
    .groups = "drop"
  )

# ---------------------------------------------------------------------------
# 3. Receiver-level CWT / octave-decay convergence (clustering-free Spearman)
# ---------------------------------------------------------------------------
low_frequency <- read_extension_csv("low_frequency_cwt_receiver.csv")

convergence_pairs <- tribble(
  ~comparison_id, ~cwt_frequency_hz, ~octave_frequency_hz,
  "CWT42_vs_octave31.5", 42, 31.5,
  "CWT60_vs_octave63", 60, 63
)

octave_diagnostic <- receiver_metrics %>%
  filter(
    source_id == "S1", band_scheme == "octave",
    centre_frequency_hz %in% c(31.5, 63)
  ) %>%
  transmute(
    receiver_cell_id, room_id, receiver_id, condition_id,
    octave_frequency_hz = as.numeric(centre_frequency_hz),
    t20_s_strict
  )

convergence_rows <- vector("list", nrow(convergence_pairs))
for (i in seq_len(nrow(convergence_pairs))) {
  pair <- convergence_pairs[i, ]
  cell_pairs <- low_frequency %>%
    filter(
      source_id == "S1", feature_valid,
      as.numeric(centre_frequency_hz) == pair$cwt_frequency_hz
    ) %>%
    select(receiver_cell_id, cwt_late_to_early_db = late_to_early_db) %>%
    inner_join(
      octave_diagnostic %>% filter(octave_frequency_hz == pair$octave_frequency_hz),
      by = "receiver_cell_id"
    ) %>%
    filter(is.finite(t20_s_strict))
  cell_rho <- cor(cell_pairs$cwt_late_to_early_db, cell_pairs$t20_s_strict,
                  method = "spearman")
  receiver_level <- cell_pairs %>%
    group_by(room_id, receiver_id) %>%
    summarise(
      n_states = n(),
      cwt_late_to_early_db = mean(cwt_late_to_early_db),
      t20_s_strict = mean(t20_s_strict),
      .groups = "drop"
    )
  receiver_rho <- cor(receiver_level$cwt_late_to_early_db, receiver_level$t20_s_strict,
                      method = "spearman")
  convergence_rows[[i]] <- tibble(
    comparison_id = pair$comparison_id,
    cwt_frequency_hz = pair$cwt_frequency_hz,
    octave_frequency_hz = pair$octave_frequency_hz,
    n_cell_pairs = nrow(cell_pairs),
    spearman_rho_cell_level = cell_rho,
    n_receiver_profiles = nrow(receiver_level),
    minimum_states_per_receiver = min(receiver_level$n_states),
    spearman_rho_receiver_level = receiver_rho
  )
}
receiver_level_convergence <- bind_rows(convergence_rows)

# Cross-check the cell-level values against the frozen A19 output
a19_convergence <- readr::read_csv(
  file.path(OUTPUT_ROOT, "a19_low_frequency_cwt", "octave_decay_convergence.csv"),
  show_col_types = FALSE
)
for (i in seq_len(nrow(receiver_level_convergence))) {
  row <- receiver_level_convergence[i, ]
  frozen <- a19_convergence %>% filter(comparison_id == row$comparison_id)
  if (abs(row$spearman_rho_cell_level - frozen$spearman_rho_strict) > 1e-9 ||
      row$n_cell_pairs != frozen$n_strict) {
    stop("Cell-level convergence does not reproduce A19: ", row$comparison_id, call. = FALSE)
  }
}

readr::write_csv(comparison, file.path(analysis_dir, "common_band_receiver_estimates.csv"))
readr::write_csv(common_band_summary, file.path(analysis_dir, "common_band_summary.csv"))
readr::write_csv(validity_balance, file.path(analysis_dir, "validity_balance_by_curtain_state.csv"))
readr::write_csv(validity_by_state_band, file.path(analysis_dir, "validity_by_room_state_band.csv"))
readr::write_csv(receiver_level_convergence, file.path(analysis_dir, "receiver_level_convergence.csv"))

results <- c(
  "# A21 results (post-review sensitivity checks)",
  "",
  "## Common-band receiver estimates (500 Hz-8 kHz)",
  "",
  sprintf(
    "- Room 1: %d/%d positive, %.4f to %.4f s. Room 2: %d/%d positive, %.4f to %.4f s.",
    common_band_summary$n_positive[1], common_band_summary$n_receivers[1],
    common_band_summary$minimum_s[1], common_band_summary$maximum_s[1],
    common_band_summary$n_positive[2], common_band_summary$n_receivers[2],
    common_band_summary$minimum_s[2], common_band_summary$maximum_s[2]
  ),
  sprintf(
    "- Maximum absolute shift from the locked all-available-band estimates: %.4f s (Room 1) and %.4f s (Room 2).",
    common_band_summary$maximum_abs_shift_s[1], common_band_summary$maximum_abs_shift_s[2]
  ),
  "",
  "## Strict-validity balance at 125/250 Hz (receiver cells with a strict value)",
  "",
  paste0(
    "- ",
    sprintf(
      "%g Hz %s: %d/%d",
      validity_balance$centre_frequency_hz, validity_balance$curtain_state,
      validity_balance$n_strict_valid, validity_balance$n_cells
    ),
    collapse = "\n"
  ),
  "",
  "## Receiver-level CWT / octave convergence",
  "",
  sprintf(
    "- %s: cell-level rho = %.3f (n = %d, reproduces A19); receiver-level rho = %.3f (n = %d profiles, minimum %d states each).",
    receiver_level_convergence$comparison_id,
    receiver_level_convergence$spearman_rho_cell_level,
    receiver_level_convergence$n_cell_pairs,
    receiver_level_convergence$spearman_rho_receiver_level,
    receiver_level_convergence$n_receiver_profiles,
    receiver_level_convergence$minimum_states_per_receiver
  )
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c(
    "receiver_band_metrics.csv", "low_frequency_cwt_receiver.csv",
    "writing-latex/output/data_lock/fig3_receivers.csv",
    "exploration/a19_low_frequency_cwt/octave_decay_convergence.csv"
  ),
  outputs = c(
    "common_band_receiver_estimates.csv", "common_band_summary.csv",
    "validity_balance_by_curtain_state.csv", "validity_by_room_state_band.csv",
    "receiver_level_convergence.csv", "results.md", "session_info.txt", "run.log"
  ),
  checks = c(
    "20/20 receivers complete at 500 Hz-8 kHz",
    "cell-level Spearman reproduces frozen A19 values",
    "descriptive only; no new hypothesis tests"
  )
)
