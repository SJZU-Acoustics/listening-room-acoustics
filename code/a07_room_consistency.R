source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A07"
analysis_dir <- analysis_output_dir("a07_room_consistency")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver)
effects <- prepare_factorial_effects(
  primary, "t20_s_strict", "T20", improvement = "lower", scale = "difference"
)

observed <- estimate_factorial(effects, level = "room")
bootstrap <- bootstrap_factorial(effects, level = "room", R = BOOT_N, seed = BOOT_SEED + 70L)
jackknife_all <- jackknife_factorial(effects, level = "room")
jackknife_within_room <- jackknife_all %>% filter(room_id == omitted_room_id)

room_intervals <- factorial_intervals(
  observed, bootstrap, jackknife_within_room,
  keys = c("metric", "scale", "effect_family", "room_id")
)

contributions <- factorial_contributions(effects, level = "room")
signflip <- signflip_factorial(
  contributions,
  keys = c("metric", "scale", "effect_family", "room_id"),
  max_permutations = 1024,
  seed = BOOT_SEED + 700L
)

room_estimates <- room_intervals %>%
  left_join(signflip, by = c("metric", "scale", "effect_family", "room_id")) %>%
  arrange(effect_family, room_id)

observed_difference <- observed %>%
  select(metric, scale, effect_family, room_id, estimate) %>%
  pivot_wider(names_from = room_id, values_from = estimate, names_prefix = "room") %>%
  mutate(estimate = room2 - room1)

bootstrap_difference <- bootstrap %>%
  select(bootstrap_id, metric, scale, effect_family, room_id, estimate) %>%
  pivot_wider(names_from = room_id, values_from = estimate, names_prefix = "room") %>%
  mutate(estimate = room2 - room1)

jackknife_difference <- jackknife_all %>%
  select(jackknife_id, omitted_room_id, omitted_receiver_id, metric, scale, effect_family, room_id, estimate) %>%
  pivot_wider(names_from = room_id, values_from = estimate, names_prefix = "room") %>%
  mutate(estimate = room2 - room1)

difference_rows <- vector("list", nrow(observed_difference))
for (i in seq_len(nrow(observed_difference))) {
  family <- observed_difference$effect_family[[i]]
  boot_values <- bootstrap_difference %>%
    filter(effect_family == family) %>%
    pull(estimate)
  jack_values <- jackknife_difference %>%
    filter(effect_family == family) %>%
    pull(estimate)
  limits <- bca_limits(observed_difference$estimate[[i]], boot_values, jack_values)
  difference_rows[[i]] <- bind_cols(
    observed_difference[i, c("metric", "scale", "effect_family", "estimate")],
    tibble::as_tibble_row(limits)
  )
}
room_differences <- bind_rows(difference_rows) %>%
  mutate(contrast = "Room 2 minus Room 1", .after = effect_family)

plot_data <- room_estimates %>%
  mutate(
    room_label = paste("Room", room_id),
    effect_label = factor(
      effect_family,
      levels = c("curtain", "carpet", "interaction"),
      labels = c("Curtain", "Carpet", "Curtain x carpet")
    )
  )

room_plot <- ggplot(plot_data, aes(x = estimate, y = room_label, colour = room_label)) +
  geom_vline(xintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_errorbar(aes(xmin = bca_low, xmax = bca_high), orientation = "y", width = 0.14, linewidth = 0.45) +
  geom_point(size = 2) +
  facet_wrap(~ effect_label, ncol = 1, scales = "free_x") +
  scale_colour_manual(values = c("Room 1" = "#2A6F97", "Room 2" = "#D08C60"), guide = "none") +
  labs(
    title = "Factorial T20 effects in the two fixed rooms",
    subtitle = "Equal-band estimates with within-room BCa 95% intervals",
    x = "T20 reduction (s)",
    y = NULL
  ) +
  theme_p31()

readr::write_csv(room_estimates, file.path(analysis_dir, "room_specific_factorial_estimates.csv"))
readr::write_csv(room_differences, file.path(analysis_dir, "room_difference_estimates.csv"))
readr::write_csv(bootstrap, file.path(analysis_dir, "room_specific_bootstrap_draws.csv"))
readr::write_csv(jackknife_all, file.path(analysis_dir, "room_specific_jackknife.csv"))
readr::write_csv(contributions, file.path(analysis_dir, "room_specific_receiver_contributions.csv"))
save_p31_plot(room_plot, file.path(analysis_dir, "room_specific_factorial_t20.png"), width_mm = 165, height_mm = 150)

curtain_rooms <- room_estimates %>% filter(effect_family == "curtain")
curtain_difference <- room_differences %>% filter(effect_family == "curtain")
carpet_rooms <- room_estimates %>% filter(effect_family == "carpet")
interaction_rooms <- room_estimates %>% filter(effect_family == "interaction")

results <- c(
  "# A07 results",
  "",
  "## Outcome",
  "",
  sprintf("- Curtain reduction in Room 1: %.3f s (BCa 95%% CI %.3f to %.3f).", curtain_rooms$estimate[curtain_rooms$room_id == 1], curtain_rooms$bca_low[curtain_rooms$room_id == 1], curtain_rooms$bca_high[curtain_rooms$room_id == 1]),
  sprintf("- Curtain reduction in Room 2: %.3f s (BCa 95%% CI %.3f to %.3f).", curtain_rooms$estimate[curtain_rooms$room_id == 2], curtain_rooms$bca_low[curtain_rooms$room_id == 2], curtain_rooms$bca_high[curtain_rooms$room_id == 2]),
  sprintf("- Room 2 minus Room 1 curtain difference: %.3f s (BCa 95%% CI %.3f to %.3f).", curtain_difference$estimate, curtain_difference$bca_low, curtain_difference$bca_high),
  sprintf("- Carpet estimates by room: %s.", paste(sprintf("Room %d %.3f s", carpet_rooms$room_id, carpet_rooms$estimate), collapse = "; ")),
  sprintf("- Interaction estimates by room: %s.", paste(sprintf("Room %d %.3f s", interaction_rooms$room_id, interaction_rooms$estimate), collapse = "; ")),
  "",
  "## Decision",
  "",
  "The curtain effect is directionally consistent in both measured rooms. Magnitude differences remain visible and constrain any pooled interpretation; they do not justify general population heterogeneity claims.",
  "",
  "## Assumptions and limitations",
  "",
  "Within-room receiver resampling assumes the measured positions are exchangeable representations of each room's spatial field. Room-difference intervals do not include uncertainty from sampling rooms because no such sampling occurred."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "room_specific_factorial_estimates.csv", "room_difference_estimates.csv",
    "room_specific_bootstrap_draws.csv", "room_specific_jackknife.csv",
    "room_specific_receiver_contributions.csv", "room_specific_factorial_t20.png",
    "results.md", "session_info.txt", "run.log"
  ),
  checks = c("two fixed rooms", "equal band weights", "within-room receiver bootstrap")
)
