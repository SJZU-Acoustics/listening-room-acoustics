source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A19"
analysis_dir <- analysis_output_dir("a19_low_frequency_cwt")
low_frequency <- read_extension_csv("low_frequency_cwt_receiver.csv")
synthetic_validation <- read_extension_csv("low_frequency_synthetic_validation.csv")
receiver_metrics <- read_frozen_csv("receiver_band_metrics.csv")
modes <- read_frozen_csv("room1_modes_to_200hz.csv")

if (!all(synthetic_validation$frequency_check_pass) ||
    !all(synthetic_validation$dynamic_range_check_pass)) {
  stop("Synthetic low-frequency validation is not clean.", call. = FALSE)
}

mode_targets <- tribble(
  ~mode_id, ~geometry_frequency_hz, ~analysis_centre_hz,
  "mode_42", 42.11172498, 42,
  "mode_60", 59.73528387, 60
)

profile_group <- c(
  "receiver_cell_id", "room_id", "condition_id", "condition_order",
  "source_id", "receiver_id", "receiver_role", "physical_position_id"
)

valid_s1 <- low_frequency %>%
  filter(source_id == "S1", feature_valid) %>%
  mutate(centre_frequency_hz = as.numeric(centre_frequency_hz))

make_modal_excess <- function(centre_hz, mode_id, geometry_frequency_hz) {
  valid_s1 %>%
    group_by(across(all_of(profile_group))) %>%
    summarise(
      target_n = sum(centre_frequency_hz >= centre_hz - 1 & centre_frequency_hz <= centre_hz + 1),
      flank_n = sum(
        (centre_frequency_hz >= centre_hz - 10 & centre_frequency_hz <= centre_hz - 6) |
          (centre_frequency_hz >= centre_hz + 6 & centre_frequency_hz <= centre_hz + 10)
      ),
      target_late_to_early_db = mean(
        late_to_early_db[centre_frequency_hz >= centre_hz - 1 & centre_frequency_hz <= centre_hz + 1]
      ),
      flank_late_to_early_db = mean(
        late_to_early_db[
          (centre_frequency_hz >= centre_hz - 10 & centre_frequency_hz <= centre_hz - 6) |
            (centre_frequency_hz >= centre_hz + 6 & centre_frequency_hz <= centre_hz + 10)
        ]
      ),
      .groups = "drop"
    ) %>%
    filter(target_n >= 2, flank_n >= 8) %>%
    mutate(
      mode_id = mode_id,
      geometry_frequency_hz = geometry_frequency_hz,
      centre_frequency_hz = centre_hz,
      modal_excess_db = target_late_to_early_db - flank_late_to_early_db
    )
}

modal_excess <- bind_rows(lapply(seq_len(nrow(mode_targets)), function(i) {
  make_modal_excess(
    mode_targets$analysis_centre_hz[[i]],
    mode_targets$mode_id[[i]],
    mode_targets$geometry_frequency_hz[[i]]
  )
}))

receiver_mode_profiles <- modal_excess %>%
  group_by(
    room_id, receiver_id, receiver_role, physical_position_id,
    mode_id, geometry_frequency_hz, centre_frequency_hz
  ) %>%
  summarise(
    n_conditions = n_distinct(condition_id),
    modal_excess_db = mean(modal_excess_db),
    .groups = "drop"
  )

receiver_mode_profiles <- receiver_mode_profiles %>%
  mutate(inference_eligible = n_conditions == 4)
receiver_mode_profiles_complete <- receiver_mode_profiles %>%
  filter(inference_eligible)

mode_groups <- receiver_mode_profiles_complete %>%
  distinct(room_id, mode_id, geometry_frequency_hz, centre_frequency_hz) %>%
  arrange(room_id, centre_frequency_hz)
mode_summary_rows <- vector("list", nrow(mode_groups))
for (i in seq_len(nrow(mode_groups))) {
  group <- mode_groups[i, ]
  values <- receiver_mode_profiles_complete %>%
    filter(room_id == group$room_id, mode_id == group$mode_id) %>%
    arrange(receiver_id) %>%
    pull(modal_excess_db)
  theta <- mean(values)
  set.seed(BOOT_SEED + 190L + i)
  boot <- replicate(BOOT_N, mean(sample(values, size = length(values), replace = TRUE)))
  jack <- vapply(seq_along(values), function(j) mean(values[-j]), numeric(1))
  limits <- bca_limits(theta, boot, jack)
  sign_test <- block_signflip(values / length(values), max_permutations = 65536)
  mode_summary_rows[[i]] <- bind_cols(
    group,
    tibble(
      n_receivers = length(values),
      estimate_modal_excess_db = theta,
      receiver_sd_db = sd(values)
    ),
    tibble::as_tibble_row(limits),
    sign_test
  )
}

mode_summary <- bind_rows(mode_summary_rows) %>%
  group_by(room_id) %>%
  mutate(
    p_holm_two_targets = if_else(
      room_id == 1,
      p.adjust(sign_symmetry_p_two_sided, method = "holm"),
      NA_real_
    )
  ) %>%
  ungroup()

room1_factorial_data <- modal_excess %>%
  filter(room_id == 1) %>%
  transmute(
    room_id, receiver_id, centre_frequency_hz, condition_id,
    modal_excess_db
  ) %>%
  mutate(
    condition_id = factor(condition_id, levels = CONDITION_LEVELS),
    receiver_block = interaction(room_id, receiver_id, drop = TRUE)
  )

furnishing_effects <- prepare_factorial_effects(
  room1_factorial_data,
  "modal_excess_db",
  "CWT modal excess",
  improvement = "lower",
  scale = "difference"
)
furnishing_observed <- estimate_factorial(furnishing_effects, level = "band")
furnishing_bootstrap <- bootstrap_factorial(
  furnishing_effects, level = "band", R = BOOT_N, seed = BOOT_SEED + 191L
)
furnishing_jackknife <- jackknife_factorial(furnishing_effects, level = "band")
furnishing_intervals <- factorial_intervals(
  furnishing_observed, furnishing_bootstrap, furnishing_jackknife,
  keys = c("metric", "scale", "effect_family", "centre_frequency_hz")
)
furnishing_contributions <- factorial_contributions(furnishing_effects, level = "band")
furnishing_signflip <- signflip_factorial(
  furnishing_contributions,
  keys = c("metric", "scale", "effect_family", "centre_frequency_hz"),
  max_permutations = 65536,
  seed = BOOT_SEED + 192L
)
furnishing_summary <- furnishing_intervals %>%
  left_join(
    furnishing_signflip,
    by = c("metric", "scale", "effect_family", "centre_frequency_hz")
  ) %>%
  mutate(q_bh_six_furnishing_tests = p.adjust(sign_symmetry_p_two_sided, method = "BH")) %>%
  arrange(effect_family, centre_frequency_hz)

octave_pairs <- tribble(
  ~cwt_frequency_hz, ~octave_frequency_hz, ~comparison_id,
  39, 31.5, "CWT39_vs_octave31.5",
  42, 31.5, "CWT42_vs_octave31.5",
  60, 63, "CWT60_vs_octave63"
)
convergence_rows <- vector("list", nrow(octave_pairs))
for (i in seq_len(nrow(octave_pairs))) {
  pair <- octave_pairs[i, ]
  cwt_rows <- valid_s1 %>%
    filter(centre_frequency_hz == pair$cwt_frequency_hz) %>%
    select(receiver_cell_id, room_id, cwt_late_to_early_db = late_to_early_db)
  octave_rows <- receiver_metrics %>%
    filter(
      source_id == "S1", band_scheme == "octave",
      centre_frequency_hz == pair$octave_frequency_hz
    ) %>%
    select(receiver_cell_id, t20_s, t20_s_strict)
  joined <- inner_join(cwt_rows, octave_rows, by = "receiver_cell_id")
  strict <- joined %>% filter(is.finite(t20_s_strict))
  convergence_rows[[i]] <- tibble(
    comparison_id = pair$comparison_id,
    cwt_frequency_hz = pair$cwt_frequency_hz,
    octave_frequency_hz = pair$octave_frequency_hz,
    n_unrestricted = sum(is.finite(joined$t20_s)),
    spearman_rho_unrestricted = cor(
      joined$cwt_late_to_early_db, joined$t20_s,
      method = "spearman", use = "complete.obs"
    ),
    n_strict = nrow(strict),
    spearman_rho_strict = cor(
      strict$cwt_late_to_early_db, strict$t20_s_strict,
      method = "spearman", use = "complete.obs"
    )
  )
}
octave_convergence <- bind_rows(convergence_rows)

listening_position <- modal_excess %>%
  group_by(room_id, condition_id, mode_id) %>%
  mutate(
    n_receivers = n(),
    receiver_percentile = 100 * rank(modal_excess_db, ties.method = "average") / n()
  ) %>%
  filter(receiver_role == "listening_position") %>%
  transmute(
    room_id, condition_id, mode_id, n_receivers,
    listening_excess_db = modal_excess_db,
    listening_percentile = receiver_percentile
  ) %>%
  ungroup()
listening_summary <- listening_position %>%
  group_by(room_id, mode_id) %>%
  summarise(
    n_conditions = n_distinct(condition_id),
    median_listening_excess_db = median(listening_excess_db),
    median_listening_percentile = median(listening_percentile),
    minimum_listening_percentile = min(listening_percentile),
    maximum_listening_percentile = max(listening_percentile),
    .groups = "drop"
  )

room_receiver_profiles <- valid_s1 %>%
  group_by(room_id, receiver_id, centre_frequency_hz) %>%
  summarise(
    n_conditions = n_distinct(condition_id),
    late_to_early_db = mean(late_to_early_db),
    .groups = "drop"
  ) %>%
  filter(n_conditions == 4)

make_local_screen <- function(centre_hz) {
  room_receiver_profiles %>%
    group_by(room_id, receiver_id) %>%
    summarise(
      target_n = sum(centre_frequency_hz >= centre_hz - 1 & centre_frequency_hz <= centre_hz + 1),
      flank_n = sum(
        (centre_frequency_hz >= centre_hz - 10 & centre_frequency_hz <= centre_hz - 6) |
          (centre_frequency_hz >= centre_hz + 6 & centre_frequency_hz <= centre_hz + 10)
      ),
      target_db = mean(
        late_to_early_db[centre_frequency_hz >= centre_hz - 1 & centre_frequency_hz <= centre_hz + 1]
      ),
      flank_db = mean(
        late_to_early_db[
          (centre_frequency_hz >= centre_hz - 10 & centre_frequency_hz <= centre_hz - 6) |
            (centre_frequency_hz >= centre_hz + 6 & centre_frequency_hz <= centre_hz + 10)
        ]
      ),
      .groups = "drop"
    ) %>%
    filter(target_n >= 2, flank_n >= 8) %>%
    mutate(
      centre_frequency_hz = centre_hz,
      local_excess_db = target_db - flank_db
    )
}

local_receiver_screen <- bind_rows(lapply(35:110, make_local_screen))
local_screen_groups <- local_receiver_screen %>% distinct(room_id, centre_frequency_hz)
local_summary_rows <- vector("list", nrow(local_screen_groups))
for (i in seq_len(nrow(local_screen_groups))) {
  group <- local_screen_groups[i, ]
  values <- local_receiver_screen %>%
    filter(room_id == group$room_id, centre_frequency_hz == group$centre_frequency_hz) %>%
    pull(local_excess_db)
  set.seed(BOOT_SEED + 193L + i)
  boot <- replicate(BOOT_N, mean(sample(values, size = length(values), replace = TRUE)))
  local_summary_rows[[i]] <- bind_cols(
    group,
    tibble(
      n_receivers = length(values),
      mean_local_excess_db = mean(values),
      receiver_sd_db = sd(values),
      percentile_low = quantile(boot, 0.025, names = FALSE),
      percentile_high = quantile(boot, 0.975, names = FALSE)
    )
  )
}
local_screen_summary <- bind_rows(local_summary_rows)
top_local_features <- local_screen_summary %>%
  group_by(room_id) %>%
  slice_max(mean_local_excess_db, n = 1, with_ties = FALSE) %>%
  ungroup()

nearest_modes <- top_local_features %>%
  filter(room_id == 1) %>%
  rowwise() %>%
  mutate(
    nearest_mode_frequency_hz = modes$frequency_hz[which.min(abs(modes$frequency_hz - centre_frequency_hz))],
    nearest_mode_type = modes$mode_type[which.min(abs(modes$frequency_hz - centre_frequency_hz))],
    nearest_mode_p = modes$p[which.min(abs(modes$frequency_hz - centre_frequency_hz))],
    nearest_mode_q = modes$q[which.min(abs(modes$frequency_hz - centre_frequency_hz))],
    nearest_mode_r = modes$r[which.min(abs(modes$frequency_hz - centre_frequency_hz))],
    peak_to_mode_gap_hz = abs(nearest_mode_frequency_hz - centre_frequency_hz)
  ) %>%
  ungroup()

plot_data <- local_screen_summary %>%
  mutate(room_label = factor(room_id, levels = c(1, 2), labels = c("R1", "R2")))
end_labels <- plot_data %>%
  group_by(room_label) %>%
  slice_max(centre_frequency_hz, n = 1, with_ties = FALSE) %>%
  ungroup()

local_plot <- ggplot(
  plot_data,
  aes(x = centre_frequency_hz, y = mean_local_excess_db, colour = room_label, fill = room_label)
) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_vline(
    xintercept = mode_targets$geometry_frequency_hz,
    linewidth = 0.35, linetype = "dashed", colour = "#777777"
  ) +
  geom_ribbon(
    aes(ymin = percentile_low, ymax = percentile_high),
    alpha = 0.12, colour = NA
  ) +
  geom_line(linewidth = 0.65) +
  geom_text(
    data = end_labels,
    aes(label = room_label),
    hjust = -0.08, size = 3, show.legend = FALSE
  ) +
  scale_colour_manual(values = c("R1" = "#0072B2", "R2" = "#D55E00")) +
  scale_fill_manual(values = c("R1" = "#0072B2", "R2" = "#D55E00")) +
  scale_x_continuous(breaks = c(40, 60, 80, 100), limits = c(35, 115), expand = expansion(mult = c(0, 0))) +
  labs(
    x = "CWT centre frequency (Hz)",
    y = "Local persistence excess (dB)"
  ) +
  coord_cartesian(clip = "off") +
  theme_classic(base_family = "Arial", base_size = 9) %+replace%
  theme(
    axis.line = element_line(colour = "black", linewidth = 0.45),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length = grid::unit(0.10, "cm"),
    axis.title = element_text(size = 10, colour = "black"),
    axis.text = element_text(size = 9, colour = "black"),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_blank(),
    plot.margin = margin(5.5, 14, 5.5, 5.5),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path(analysis_dir, "local_persistence_frequency_screen.png"), local_plot,
  width = 85, height = 85, units = "mm", dpi = 600, bg = "white"
)

readr::write_csv(modal_excess, file.path(analysis_dir, "condition_receiver_modal_excess.csv"))
readr::write_csv(receiver_mode_profiles, file.path(analysis_dir, "receiver_modal_excess_profiles.csv"))
readr::write_csv(mode_summary, file.path(analysis_dir, "fixed_room_modal_excess_summary.csv"))
readr::write_csv(furnishing_summary, file.path(analysis_dir, "room1_furnishing_modal_effects.csv"))
readr::write_csv(octave_convergence, file.path(analysis_dir, "octave_decay_convergence.csv"))
readr::write_csv(listening_position, file.path(analysis_dir, "listening_position_modal_percentiles.csv"))
readr::write_csv(listening_summary, file.path(analysis_dir, "listening_position_modal_summary.csv"))
readr::write_csv(local_receiver_screen, file.path(analysis_dir, "receiver_local_frequency_screen.csv"))
readr::write_csv(local_screen_summary, file.path(analysis_dir, "local_frequency_screen_summary.csv"))
readr::write_csv(top_local_features, file.path(analysis_dir, "top_local_features_by_room.csv"))
readr::write_csv(nearest_modes, file.path(analysis_dir, "room1_top_feature_nearest_mode.csv"))

room1_42 <- mode_summary %>% filter(room_id == 1, mode_id == "mode_42")
room1_60 <- mode_summary %>% filter(room_id == 1, mode_id == "mode_60")
room2_42 <- mode_summary %>% filter(room_id == 2, mode_id == "mode_42")
room2_60 <- mode_summary %>% filter(room_id == 2, mode_id == "mode_60")
corr_42 <- octave_convergence %>% filter(comparison_id == "CWT42_vs_octave31.5")
corr_60 <- octave_convergence %>% filter(comparison_id == "CWT60_vs_octave63")
listener_42 <- listening_summary %>% filter(room_id == 1, mode_id == "mode_42")
listener_60 <- listening_summary %>% filter(room_id == 1, mode_id == "mode_60")
room1_top <- nearest_modes %>% slice(1)
curtain_modes <- furnishing_summary %>% filter(effect_family == "curtain")

supported_42 <- room1_42$bca_low > 0 && room1_42$p_holm_two_targets < 0.05
supported_60 <- room1_60$bca_low > 0 && room1_60$p_holm_two_targets < 0.05
verdict <- if (supported_42 && !supported_60) {
  "The raw IRs independently support a broad approximately 40-42 Hz persistence feature in Room 1, but not the inherited 60 Hz persistence claim. The modal diagnosis is therefore narrowed rather than globally upgraded."
} else if (supported_42 && supported_60) {
  "The raw IRs independently support persistence at both pre-specified Room-1 modal targets; this upgrades the bounded modal diagnosis but does not validate treatment."
} else {
  "Neither pre-specified target meets the full upgrade rule; the inherited 40/60 Hz modal claim remains contextual only."
}

results <- c(
  "# A19 results",
  "",
  paste0("**Verdict:** ", verdict),
  "",
  "## Outcome",
  "",
  sprintf("- Room 1's 42 Hz target has local persistence excess %.3f dB (BCa 95%% CI %.3f to %.3f; Holm p = %.4g; %d complete receiver profiles); the 60 Hz target is %.3f dB (%.3f to %.3f; Holm p = %.4g; %d profiles).", room1_42$estimate_modal_excess_db, room1_42$bca_low, room1_42$bca_high, room1_42$p_holm_two_targets, room1_42$n_receivers, room1_60$estimate_modal_excess_db, room1_60$bca_low, room1_60$bca_high, room1_60$p_holm_two_targets, room1_60$n_receivers),
  sprintf("- The descriptive Room-1 screen peaks at %s Hz (%.3f dB), %.2f Hz from the nearest %s mode at %.2f Hz; it is a screened frequency and carries no confirmatory p-value.", frequency_label(room1_top$centre_frequency_hz), room1_top$mean_local_excess_db, room1_top$peak_to_mode_gap_hz, room1_top$nearest_mode_type, room1_top$nearest_mode_frequency_hz),
  sprintf("- Room 2 shows target excess %.3f dB at 42 Hz and %.3f dB at 60 Hz, confirming that fixed-room patterns differ.", room2_42$estimate_modal_excess_db, room2_60$estimate_modal_excess_db),
  sprintf("- CWT persistence converges with octave T20: strict Spearman rho = %.3f for 42 Hz versus the 31.5-Hz octave (n = %d) and %.3f for 60 Hz versus the 63-Hz octave (n = %d).", corr_42$spearman_rho_strict, corr_42$n_strict, corr_60$spearman_rho_strict, corr_60$n_strict),
  sprintf("- Room 1's listening position is not solely responsible: its median percentile is %.1f at 42 Hz and %.1f at 60 Hz across conditions.", listener_42$median_listening_percentile, listener_60$median_listening_percentile),
  sprintf("- Curtain effects on modal excess range from %.3f to %.3f dB across the two targets; %d/2 pass the six-test furnishing FDR screen.", min(curtain_modes$estimate), max(curtain_modes$estimate), sum(curtain_modes$q_bh_six_furnishing_tests < 0.05)),
  "",
  "## Decision",
  "",
  verdict,
  "",
  "## Assumptions and limitations",
  "",
  "The CWT late/early ratio is a relative persistence feature, not ISO decay time, calibrated level or modal Q. Symmetric local flanks remove the broad frequency trend but their 6-10 Hz offset is an estimator choice fixed before the target tests. The two rooms remain fixed cases, and furnishing states were sequential. The full frequency screen is descriptive and cannot select a new confirmatory mode. No result predicts the unmeasured D60W trap performance.",
  "",
  "The 25-120 Hz wavelet features are read from the deposited low-frequency layer of the Mendeley workbook."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = c(
    "low_frequency_cwt_receiver.csv", "low_frequency_synthetic_validation.csv",
    "receiver_band_metrics.csv", "room1_modes_to_200hz.csv"
  ),
  outputs = c(
    "condition_receiver_modal_excess.csv", "receiver_modal_excess_profiles.csv",
    "fixed_room_modal_excess_summary.csv", "room1_furnishing_modal_effects.csv",
    "octave_decay_convergence.csv", "listening_position_modal_percentiles.csv",
    "listening_position_modal_summary.csv", "receiver_local_frequency_screen.csv",
    "local_frequency_screen_summary.csv", "top_local_features_by_room.csv",
    "room1_top_feature_nearest_mode.csv", "local_persistence_frequency_screen.png",
    "results.md", "session_info.txt", "run.log"
  ),
  checks = c(
    "two pre-specified Room-1 targets Holm-adjusted",
    "full frequency screen descriptive only",
    "synthetic frequency and dynamic-range checks passed"
  )
)
