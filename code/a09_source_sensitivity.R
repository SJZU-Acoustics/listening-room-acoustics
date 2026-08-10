source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A09"
analysis_dir <- analysis_output_dir("a09_source_sensitivity")

receiver <- read_frozen_csv("receiver_band_metrics.csv")

stable <- receiver %>%
  filter(
    source_id %in% c("S1", "S2"),
    band_scheme == "octave",
    centre_frequency_hz %in% CORE_OCTAVE_BANDS,
    !(room_id == 1 & receiver_id == 10),
    !(room_id == 2 & receiver_id == 4)
  ) %>%
  select(
    room_id, condition_id, source_id, receiver_id, centre_frequency_hz,
    T20 = t20_s_strict, EDT = edt_s_strict, C80 = c80_db_strict
  ) %>%
  pivot_longer(cols = c(T20, EDT, C80), names_to = "metric", values_to = "value")

source_pairs <- stable %>%
  pivot_wider(names_from = source_id, values_from = value) %>%
  filter(is.finite(S1), is.finite(S2)) %>%
  mutate(source_difference_s2_minus_s1 = S2 - S1)

source_difference_summary <- source_pairs %>%
  group_by(metric, room_id, condition_id, centre_frequency_hz) %>%
  group_modify(~ {
    n_pairs <- nrow(.x)
    estimate <- mean(.x$source_difference_s2_minus_s1)
    standard_error <- sd(.x$source_difference_s2_minus_s1) / sqrt(n_pairs)
    signflip <- block_signflip(
      .x$source_difference_s2_minus_s1 / n_pairs,
      max_permutations = 512,
      seed = BOOT_SEED + n_pairs
    )
    tibble(
      n_stable_pairs = n_pairs,
      mean_s2_minus_s1 = estimate,
      sd_source_difference = sd(.x$source_difference_s2_minus_s1),
      ci_low = estimate - qt(0.975, df = n_pairs - 1) * standard_error,
      ci_high = estimate + qt(0.975, df = n_pairs - 1) * standard_error,
      n_sign_patterns = signflip$n_permutations,
      sign_symmetry_p_two_sided = signflip$sign_symmetry_p_two_sided
    )
  }) %>%
  group_by(metric, room_id, condition_id) %>%
  mutate(p_bh_seven_bands = p.adjust(sign_symmetry_p_two_sided, method = "BH")) %>%
  ungroup()

room2_wide <- stable %>%
  filter(
    room_id == 2,
    condition_id %in% c("open_no_carpet", "closed_no_carpet", "closed_carpet")
  ) %>%
  mutate(source_condition = paste(source_id, condition_id, sep = "__")) %>%
  select(room_id, receiver_id, centre_frequency_hz, metric, source_condition, value) %>%
  pivot_wider(names_from = source_condition, values_from = value)

curtain_columns <- c(
  "S1__open_no_carpet", "S1__closed_no_carpet",
  "S2__open_no_carpet", "S2__closed_no_carpet"
)
carpet_columns <- c(
  "S1__closed_no_carpet", "S1__closed_carpet",
  "S2__closed_no_carpet", "S2__closed_carpet"
)

curtain_effects <- room2_wide %>%
  filter(if_all(all_of(curtain_columns), is.finite)) %>%
  mutate(
    contrast_id = "curtain_no_carpet",
    effect_s1 = if_else(
      metric == "C80",
      S1__closed_no_carpet - S1__open_no_carpet,
      S1__open_no_carpet - S1__closed_no_carpet
    ),
    effect_s2 = if_else(
      metric == "C80",
      S2__closed_no_carpet - S2__open_no_carpet,
      S2__open_no_carpet - S2__closed_no_carpet
    )
  )

carpet_effects <- room2_wide %>%
  filter(if_all(all_of(carpet_columns), is.finite)) %>%
  mutate(
    contrast_id = "carpet_closed",
    effect_s1 = if_else(
      metric == "C80",
      S1__closed_carpet - S1__closed_no_carpet,
      S1__closed_no_carpet - S1__closed_carpet
    ),
    effect_s2 = if_else(
      metric == "C80",
      S2__closed_carpet - S2__closed_no_carpet,
      S2__closed_no_carpet - S2__closed_carpet
    )
  )

condition_effects <- bind_rows(curtain_effects, carpet_effects) %>%
  transmute(
    room_id, receiver_id, centre_frequency_hz, metric, contrast_id,
    effect_s1, effect_s2,
    source_by_condition_difference = effect_s2 - effect_s1
  )

condition_effect_band_summary <- condition_effects %>%
  group_by(metric, contrast_id, centre_frequency_hz) %>%
  group_modify(~ {
    n_pairs <- nrow(.x)
    source_difference <- mean(.x$source_by_condition_difference)
    standard_error <- sd(.x$source_by_condition_difference) / sqrt(n_pairs)
    signflip <- block_signflip(
      .x$source_by_condition_difference / n_pairs,
      max_permutations = 512,
      seed = BOOT_SEED + 900L + n_pairs
    )
    tibble(
      n_stable_receivers = n_pairs,
      effect_s1 = mean(.x$effect_s1),
      effect_s2 = mean(.x$effect_s2),
      source_by_condition_difference = source_difference,
      ci_low = source_difference - qt(0.975, df = n_pairs - 1) * standard_error,
      ci_high = source_difference + qt(0.975, df = n_pairs - 1) * standard_error,
      sign_symmetry_p_two_sided = signflip$sign_symmetry_p_two_sided
    )
  }) %>%
  group_by(metric, contrast_id) %>%
  mutate(p_bh_seven_bands = p.adjust(sign_symmetry_p_two_sided, method = "BH")) %>%
  ungroup()

condition_effect_global <- condition_effect_band_summary %>%
  group_by(metric, contrast_id) %>%
  summarise(
    effect_s1 = mean(effect_s1),
    effect_s2 = mean(effect_s2),
    source_by_condition_difference = mean(source_by_condition_difference),
    n_bands = n(),
    .groups = "drop"
  )

plot_data <- condition_effect_band_summary %>%
  filter(metric == "T20", contrast_id == "curtain_no_carpet") %>%
  select(centre_frequency_hz, effect_s1, effect_s2) %>%
  pivot_longer(cols = c(effect_s1, effect_s2), names_to = "source", values_to = "effect") %>%
  mutate(
    source = recode(source, effect_s1 = "S1", effect_s2 = "S2"),
    frequency = factor(frequency_label(centre_frequency_hz), levels = frequency_label(CORE_OCTAVE_BANDS))
  )

source_plot <- ggplot(plot_data, aes(x = frequency, y = effect, colour = source, group = source)) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_line(linewidth = 0.65) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(S1 = "#2A6F97", S2 = "#D08C60"), name = "Source") +
  labs(
    title = "Room 2 curtain effect by source position",
    subtitle = "Nine stable receiver positions; no-carpet contrast",
    x = "Octave-band centre frequency (Hz)",
    y = "T20 reduction (s)"
  ) +
  theme_p31()

readr::write_csv(source_pairs, file.path(analysis_dir, "matched_source_pair_differences.csv"))
readr::write_csv(source_difference_summary, file.path(analysis_dir, "source_difference_band_summary.csv"))
readr::write_csv(condition_effects, file.path(analysis_dir, "room2_source_condition_effects.csv"))
readr::write_csv(condition_effect_band_summary, file.path(analysis_dir, "room2_source_condition_band_summary.csv"))
readr::write_csv(condition_effect_global, file.path(analysis_dir, "room2_source_condition_global_summary.csv"))
save_p31_plot(source_plot, file.path(analysis_dir, "room2_curtain_effect_by_source.png"), width_mm = 165, height_mm = 100)

t20_source <- source_difference_summary %>% filter(metric == "T20")
t20_curtain <- condition_effect_global %>% filter(metric == "T20", contrast_id == "curtain_no_carpet")
t20_carpet <- condition_effect_global %>% filter(metric == "T20", contrast_id == "carpet_closed")
t20_curtain_bands <- condition_effect_band_summary %>% filter(metric == "T20", contrast_id == "curtain_no_carpet")

results <- c(
  "# A09 results",
  "",
  "## Outcome",
  "",
  sprintf("Matched source comparisons have a frame of nine stable receiver positions per available room-condition cell, with strict-valid pair counts reported by endpoint and band. Across T20 cells, the median absolute S2-S1 band mean is %.3f s and the maximum is %.3f s.", median(abs(t20_source$mean_s2_minus_s1)), max(abs(t20_source$mean_s2_minus_s1))),
  "",
  sprintf("- Room 2 no-carpet curtain reduction: S1 %.3f s; S2 %.3f s; S2-minus-S1 effect difference %.3f s.", t20_curtain$effect_s1, t20_curtain$effect_s2, t20_curtain$source_by_condition_difference),
  sprintf("- Room 2 closed-curtain carpet effect: S1 %.3f s; S2 %.3f s; S2-minus-S1 effect difference %.3f s.", t20_carpet$effect_s1, t20_carpet$effect_s2, t20_carpet$source_by_condition_difference),
  sprintf("- Curtain direction is positive in %d/7 S1 bands and %d/7 S2 bands; %d/7 source-by-curtain band tests have BH-adjusted p < 0.05.", sum(t20_curtain_bands$effect_s1 > 0), sum(t20_curtain_bands$effect_s2 > 0), sum(t20_curtain_bands$p_bh_seven_bands < 0.05)),
  "",
  "## Decision",
  "",
  "S2 is retained as source-position sensitivity evidence only. The available Room-2 comparison tests whether the principal condition directions survive source relocation, but the missing S2 factorial cells prevent a balanced two-source factorial analysis.",
  "",
  "## Assumptions and limitations",
  "",
  "Receiver IDs at the two idle-source locations were excluded because they do not represent stable physical receiver positions across sources. Exact sign-pattern enumeration still relies on receiver-difference sign symmetry and is not a source-randomisation test."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "matched_source_pair_differences.csv", "source_difference_band_summary.csv",
    "room2_source_condition_effects.csv", "room2_source_condition_band_summary.csv",
    "room2_source_condition_global_summary.csv", "room2_curtain_effect_by_source.png",
    "results.md", "session_info.txt", "run.log"
  ),
  checks = c("stable receiver positions only", "S2 availability represented exactly", "S1 remains primary")
)
