source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A17"
analysis_dir <- analysis_output_dir("a17_third_octave_structure")

THIRD_OCTAVE_BANDS <- c(
  100, 125, 160, 200, 250, 315, 400, 500, 630, 800, 1000,
  1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000, 10000
)
FULL_SUPPORT_BANDS <- THIRD_OCTAVE_BANDS[THIRD_OCTAVE_BANDS >= 630]

receiver <- read_frozen_csv("receiver_band_metrics.csv")
third_octave <- receiver %>%
  filter(
    source_id == "S1",
    band_scheme == "third_octave",
    centre_frequency_hz %in% THIRD_OCTAVE_BANDS
  ) %>%
  mutate(
    condition_id = factor(condition_id, levels = CONDITION_LEVELS),
    room_id = factor(room_id, levels = c(1, 2)),
    receiver_block = interaction(room_id, receiver_id, drop = TRUE)
  )

effects <- prepare_factorial_effects(
  third_octave, "t20_s_strict", "T20", improvement = "lower", scale = "difference"
)

support <- factorial_support(effects) %>%
  group_by(metric, scale, centre_frequency_hz) %>%
  summarise(
    n_complete_four_receivers = sum(n_complete_four_receivers),
    n_rooms = n_distinct(room_id),
    .groups = "drop"
  ) %>%
  mutate(
    support_class = if_else(
      centre_frequency_hz %in% FULL_SUPPORT_BANDS & n_complete_four_receivers == 20,
      "full_support", "diagnostic_incomplete"
    )
  )

if (!all(support$n_complete_four_receivers[support$centre_frequency_hz %in% FULL_SUPPORT_BANDS] == 20)) {
  stop("The declared full-support third-octave range is no longer complete.", call. = FALSE)
}

observed <- estimate_factorial(effects, level = "band")
bootstrap <- bootstrap_factorial(effects, level = "band", R = BOOT_N, seed = BOOT_SEED + 170L)
jackknife <- jackknife_factorial(effects, level = "band")
intervals <- factorial_intervals(
  observed, bootstrap, jackknife,
  keys = c("metric", "scale", "effect_family", "centre_frequency_hz")
)

contributions <- factorial_contributions(effects, level = "band")
signflip <- signflip_factorial(
  contributions,
  keys = c("metric", "scale", "effect_family", "centre_frequency_hz"),
  max_permutations = 65536,
  seed = BOOT_SEED + 1700L
)

band_estimates <- intervals %>%
  left_join(
    signflip,
    by = c("metric", "scale", "effect_family", "centre_frequency_hz")
  ) %>%
  group_by(metric, scale, effect_family) %>%
  mutate(q_bh_within_21_band_profile = p.adjust(sign_symmetry_p_two_sided, method = "BH")) %>%
  ungroup() %>%
  left_join(support, by = c("metric", "scale", "centre_frequency_hz")) %>%
  arrange(effect_family, centre_frequency_hz)

receiver_effects <- effects %>%
  group_by(effect_family, room_id, receiver_id, receiver_block, centre_frequency_hz) %>%
  summarise(effect_value = mean(effect_value), .groups = "drop")

receiver_slopes <- receiver_effects %>%
  filter(centre_frequency_hz %in% FULL_SUPPORT_BANDS) %>%
  group_by(effect_family, room_id, receiver_id, receiver_block) %>%
  group_modify(~ {
    fit <- lm(effect_value ~ log2(centre_frequency_hz / 1000), data = .x)
    tibble(
      n_bands = nrow(.x),
      intercept_at_1khz_s = unname(coef(fit)[[1]]),
      slope_s_per_octave = unname(coef(fit)[[2]]),
      slope_r_squared = summary(fit)$r.squared
    )
  }) %>%
  ungroup()

if (!all(receiver_slopes$n_bands == length(FULL_SUPPORT_BANDS))) {
  stop("A receiver slope does not contain every full-support band.", call. = FALSE)
}

summarise_slopes <- function(data) {
  data %>%
    group_by(effect_family, room_id) %>%
    summarise(estimate = mean(slope_s_per_octave), .groups = "drop") %>%
    group_by(effect_family) %>%
    summarise(estimate = mean(estimate), .groups = "drop")
}

observed_slopes <- summarise_slopes(receiver_slopes)
blocks <- receiver_slopes %>% distinct(room_id, receiver_id)
set.seed(BOOT_SEED + 171L)
slope_bootstrap <- vector("list", BOOT_N)
for (b in seq_len(BOOT_N)) {
  sampled <- blocks %>%
    group_by(room_id) %>%
    reframe(receiver_id = sample(receiver_id, size = n(), replace = TRUE))
  slope_bootstrap[[b]] <- receiver_slopes %>%
    inner_join(sampled, by = c("room_id", "receiver_id"), relationship = "many-to-many") %>%
    summarise_slopes() %>%
    mutate(bootstrap_id = b, .before = 1)
}
slope_bootstrap <- bind_rows(slope_bootstrap)

slope_jackknife <- vector("list", nrow(blocks))
for (i in seq_len(nrow(blocks))) {
  slope_jackknife[[i]] <- receiver_slopes %>%
    filter(!(room_id == blocks$room_id[[i]] & receiver_id == blocks$receiver_id[[i]])) %>%
    summarise_slopes() %>%
    mutate(
      jackknife_id = i,
      omitted_room_id = blocks$room_id[[i]],
      omitted_receiver_id = blocks$receiver_id[[i]],
      .before = 1
    )
}
slope_jackknife <- bind_rows(slope_jackknife)

slope_contributions <- receiver_slopes %>%
  group_by(effect_family, room_id) %>%
  mutate(contribution = slope_s_per_octave / (2 * n())) %>%
  ungroup()

slope_signflip <- slope_contributions %>%
  group_by(effect_family) %>%
  group_modify(~ block_signflip(.x$contribution, max_permutations = 65536, seed = BOOT_SEED + 172L)) %>%
  ungroup()

slope_rows <- vector("list", nrow(observed_slopes))
for (i in seq_len(nrow(observed_slopes))) {
  family <- observed_slopes$effect_family[[i]]
  limits <- bca_limits(
    observed_slopes$estimate[[i]],
    slope_bootstrap$estimate[slope_bootstrap$effect_family == family],
    slope_jackknife$estimate[slope_jackknife$effect_family == family]
  )
  slope_rows[[i]] <- bind_cols(observed_slopes[i, , drop = FALSE], tibble::as_tibble_row(limits))
}

slope_summary <- bind_rows(slope_rows) %>%
  left_join(slope_signflip, by = "effect_family") %>%
  mutate(p_holm_three_slopes = p.adjust(sign_symmetry_p_two_sided, method = "holm")) %>%
  arrange(effect_family)

room_profiles <- estimate_factorial(effects, level = "room_band") %>%
  filter(centre_frequency_hz %in% FULL_SUPPORT_BANDS) %>%
  select(effect_family, centre_frequency_hz, room_id, estimate) %>%
  pivot_wider(names_from = room_id, values_from = estimate, names_prefix = "room_")

room_concordance <- room_profiles %>%
  group_by(effect_family) %>%
  summarise(
    n_full_support_bands = n(),
    pearson_r = cor(room_1, room_2, method = "pearson"),
    spearman_rho = cor(room_1, room_2, method = "spearman"),
    n_same_direction = sum(sign(room_1) == sign(room_2)),
    median_absolute_room_difference_s = median(abs(room_1 - room_2)),
    .groups = "drop"
  )

profile_structure <- band_estimates %>%
  filter(centre_frequency_hz %in% FULL_SUPPORT_BANDS) %>%
  group_by(effect_family) %>%
  summarise(
    n_bands = n(),
    n_positive = sum(estimate > 0),
    n_negative = sum(estimate < 0),
    n_q_below_005 = sum(q_bh_within_21_band_profile < 0.05),
    minimum_estimate_s = min(estimate),
    minimum_frequency_hz = centre_frequency_hz[which.min(estimate)],
    maximum_estimate_s = max(estimate),
    maximum_frequency_hz = centre_frequency_hz[which.max(estimate)],
    .groups = "drop"
  )

theme_a17 <- function(legend_position, legend_justification) {
  theme_classic(base_family = "Arial", base_size = 9) %+replace%
    theme(
      axis.line = element_line(colour = "black", linewidth = 0.45),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length = grid::unit(0.10, "cm"),
      axis.title = element_text(size = 10, colour = "black"),
      axis.text = element_text(size = 9, colour = "black"),
      legend.position = "inside",
      legend.position.inside = legend_position,
      legend.justification = legend_justification,
      legend.title = element_blank(),
      legend.background = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

make_profile_plot <- function(family, colour, y_label, legend_position, legend_justification) {
  plot_data <- band_estimates %>% filter(effect_family == family)
  ggplot(plot_data, aes(x = centre_frequency_hz, y = estimate)) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
    geom_errorbar(aes(ymin = bca_low, ymax = bca_high), width = 0, linewidth = 0.35, colour = colour) +
    geom_line(linewidth = 0.55, colour = colour) +
    geom_point(aes(shape = support_class), size = 1.9, colour = colour, fill = "white") +
    scale_shape_manual(
      values = c(diagnostic_incomplete = 1, full_support = 16),
      labels = c(diagnostic_incomplete = "Incomplete support", full_support = "20 receivers")
    ) +
    scale_x_log10(
      breaks = c(100, 250, 500, 1000, 2000, 4000, 10000),
      labels = frequency_label
    ) +
    labs(x = "Third-octave centre frequency (Hz)", y = y_label) +
    theme_a17(legend_position, legend_justification)
}

curtain_plot <- make_profile_plot(
  "curtain", "#0072B2", "Curtain T20 reduction (s)", c(0.97, 0.97), c(1, 1)
)
carpet_plot <- make_profile_plot(
  "carpet", "#D55E00", "Carpet T20 reduction (s)", c(0.97, 0.03), c(1, 0)
)

ggsave(
  file.path(analysis_dir, "curtain_third_octave_profile.png"), curtain_plot,
  width = 85, height = 85, units = "mm", dpi = 600, bg = "white"
)
ggsave(
  file.path(analysis_dir, "carpet_third_octave_profile.png"), carpet_plot,
  width = 85, height = 85, units = "mm", dpi = 600, bg = "white"
)

readr::write_csv(support, file.path(analysis_dir, "third_octave_support.csv"))
readr::write_csv(band_estimates, file.path(analysis_dir, "third_octave_factorial_estimates.csv"))
readr::write_csv(receiver_slopes, file.path(analysis_dir, "receiver_log_frequency_slopes.csv"))
readr::write_csv(slope_summary, file.path(analysis_dir, "log_frequency_slope_summary.csv"))
readr::write_csv(room_profiles, file.path(analysis_dir, "fixed_room_third_octave_profiles.csv"))
readr::write_csv(room_concordance, file.path(analysis_dir, "fixed_room_profile_concordance.csv"))
readr::write_csv(profile_structure, file.path(analysis_dir, "full_support_profile_structure.csv"))

curtain_structure <- profile_structure %>% filter(effect_family == "curtain")
carpet_structure <- profile_structure %>% filter(effect_family == "carpet")
carpet_slope <- slope_summary %>% filter(effect_family == "carpet")
carpet_rooms <- room_concordance %>% filter(effect_family == "carpet")
report_frequency <- function(value) {
  if (value >= 1000) sprintf("%g kHz", value / 1000) else sprintf("%g Hz", value)
}

results <- c(
  "# A17 results",
  "",
  "**Verdict:** Third-octave resolution confirms the curtain effect across the fully supported spectrum, but does not support a common cross-room carpet-frequency mechanism; the carpet reversal remains a qualification, not a storyline.",
  "",
  "## Outcome",
  "",
  sprintf("- Complete-four support rises from %d receivers at 100 Hz to all 20 receivers from 630 Hz through 10 kHz; 100-500 Hz estimates remain diagnostic.", min(support$n_complete_four_receivers)),
  sprintf("- Across the 13 full-support bands, the curtain effect is positive in %d/%d bands and ranges from %.3f s at %s to %.3f s at %s.", curtain_structure$n_positive, curtain_structure$n_bands, curtain_structure$minimum_estimate_s, report_frequency(curtain_structure$minimum_frequency_hz), curtain_structure$maximum_estimate_s, report_frequency(curtain_structure$maximum_frequency_hz)),
  sprintf("- The carpet profile is positive in %d/%d and negative in %d/%d full-support bands. Its log-frequency gradient is %.4f s per octave (BCa 95%% CI %.4f to %.4f; Holm-adjusted sign-symmetry p = %.4g).", carpet_structure$n_positive, carpet_structure$n_bands, carpet_structure$n_negative, carpet_structure$n_bands, carpet_slope$estimate, carpet_slope$bca_low, carpet_slope$bca_high, carpet_slope$p_holm_three_slopes),
  sprintf("- Room-specific carpet profiles have Pearson r = %.3f, Spearman rho = %.3f, and the same direction in %d/%d full-support bands.", carpet_rooms$pearson_r, carpet_rooms$spearman_rho, carpet_rooms$n_same_direction, carpet_rooms$n_full_support_bands),
  "",
  "## Decision",
  "",
  "The third-octave analysis is retained only if the carpet reversal forms a coherent full-support gradient and is not driven by one room. The curtain result remains the principal measured finding; low-frequency third-octave estimates do not upgrade the bounded modal claim.",
  "",
  "## Assumptions and limitations",
  "",
  "The 21 bandwise sign-symmetry tests per effect form an exploratory BH family and are not randomisation tests. Frequency-slope inference is restricted to 630 Hz-10 kHz, where all receivers are complete. Room-profile correlations are descriptive because bands share receiver profiles and the rooms are fixed cases."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "third_octave_support.csv", "third_octave_factorial_estimates.csv",
    "receiver_log_frequency_slopes.csv", "log_frequency_slope_summary.csv",
    "fixed_room_third_octave_profiles.csv", "fixed_room_profile_concordance.csv",
    "full_support_profile_structure.csv", "curtain_third_octave_profile.png",
    "carpet_third_octave_profile.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c(
    "BH within each 21-band profile", "Holm across three full-support slopes",
    "100-500 Hz diagnostic only", "630 Hz-10 kHz complete for all 20 receivers"
  )
)
