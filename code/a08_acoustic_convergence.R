source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A08"
analysis_dir <- analysis_output_dir("a08_acoustic_convergence")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver)

metric_specification <- tribble(
  ~metric, ~column, ~improvement, ~unit,
  "T20", "t20_s_strict", "lower", "s",
  "T30", "t30_s_strict", "lower", "s",
  "EDT", "edt_s_strict", "lower", "s",
  "C50", "c50_db_strict", "higher", "dB",
  "C80", "c80_db_strict", "higher", "dB"
)

effect_sets <- vector("list", nrow(metric_specification))
for (i in seq_len(nrow(metric_specification))) {
  effect_sets[[i]] <- prepare_factorial_effects(
    primary,
    metric_specification$column[[i]],
    metric_specification$metric[[i]],
    improvement = metric_specification$improvement[[i]],
    scale = "difference"
  )
}
effects <- bind_rows(effect_sets)

support <- factorial_support(effects) %>%
  left_join(select(metric_specification, metric, unit), by = "metric")

global_observed <- estimate_factorial(effects, level = "global")
global_bootstrap <- bootstrap_factorial(effects, level = "global", R = BOOT_N, seed = BOOT_SEED + 80L)
global_jackknife <- jackknife_factorial(effects, level = "global")
global_estimates <- factorial_intervals(
  global_observed, global_bootstrap, global_jackknife,
  keys = c("metric", "scale", "effect_family")
) %>%
  left_join(select(metric_specification, metric, unit), by = "metric")

band_observed <- estimate_factorial(effects, level = "band")
band_bootstrap <- bootstrap_factorial(effects, level = "band", R = BOOT_N, seed = BOOT_SEED + 81L)
band_jackknife <- jackknife_factorial(effects, level = "band")
band_intervals <- factorial_intervals(
  band_observed, band_bootstrap, band_jackknife,
  keys = c("metric", "scale", "effect_family", "centre_frequency_hz")
)

band_contributions <- factorial_contributions(effects, level = "band")
band_signflip <- signflip_factorial(
  band_contributions,
  keys = c("metric", "scale", "effect_family", "centre_frequency_hz"),
  max_permutations = 32768,
  seed = BOOT_SEED + 800L
)

band_estimates <- band_intervals %>%
  left_join(
    band_signflip,
    by = c("metric", "scale", "effect_family", "centre_frequency_hz")
  ) %>%
  group_by(metric, effect_family) %>%
  mutate(p_bh_within_metric_effect = p.adjust(sign_symmetry_p_two_sided, method = "BH")) %>%
  ungroup() %>%
  left_join(select(metric_specification, metric, unit), by = "metric")

room_band_estimates <- estimate_factorial(effects, level = "room_band")
t20_reference <- room_band_estimates %>%
  filter(metric == "T20") %>%
  select(room_id, centre_frequency_hz, effect_family, t20_improvement = estimate)

direction_concordance <- room_band_estimates %>%
  filter(metric != "T20") %>%
  left_join(
    t20_reference,
    by = c("room_id", "centre_frequency_hz", "effect_family")
  ) %>%
  mutate(same_direction_as_t20 = sign(estimate) == sign(t20_improvement))

direction_summary <- direction_concordance %>%
  group_by(metric, effect_family) %>%
  summarise(
    n_room_bands = n(),
    n_same_direction = sum(same_direction_as_t20),
    direction_concordance = mean(same_direction_as_t20),
    .groups = "drop"
  )

plot_data <- band_estimates %>%
  filter(effect_family == "curtain") %>%
  mutate(
    frequency = factor(frequency_label(centre_frequency_hz), levels = frequency_label(CORE_OCTAVE_BANDS)),
    metric = factor(metric, levels = c("T20", "T30", "EDT", "C50", "C80"))
  )

convergence_plot <- ggplot(plot_data, aes(x = frequency, y = estimate, group = 1)) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_errorbar(aes(ymin = bca_low, ymax = bca_high), width = 0.12, linewidth = 0.4, colour = "#2A6F97") +
  geom_line(linewidth = 0.6, colour = "#2A6F97") +
  geom_point(aes(fill = p_bh_within_metric_effect < 0.05), shape = 21, size = 2, colour = "#2A6F97") +
  facet_wrap(~ metric, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = "#2A9D8F", `FALSE` = "white"), guide = "none") +
  labs(
    title = "Convergent curtain effects across acoustic endpoints",
    subtitle = "Positive values denote shorter decay or greater clarity; BCa 95% intervals",
    x = "Octave-band centre frequency (Hz)",
    y = "Endpoint improvement (endpoint units)"
  ) +
  theme_p31()

readr::write_csv(support, file.path(analysis_dir, "endpoint_complete_four_support.csv"))
readr::write_csv(global_estimates, file.path(analysis_dir, "global_endpoint_factorial_estimates.csv"))
readr::write_csv(global_bootstrap, file.path(analysis_dir, "global_endpoint_bootstrap_draws.csv"))
readr::write_csv(global_jackknife, file.path(analysis_dir, "global_endpoint_jackknife.csv"))
readr::write_csv(band_estimates, file.path(analysis_dir, "bandwise_endpoint_factorial_estimates.csv"))
readr::write_csv(band_contributions, file.path(analysis_dir, "bandwise_endpoint_receiver_contributions.csv"))
readr::write_csv(direction_concordance, file.path(analysis_dir, "endpoint_room_band_direction_concordance.csv"))
readr::write_csv(direction_summary, file.path(analysis_dir, "endpoint_direction_summary.csv"))
save_p31_plot(convergence_plot, file.path(analysis_dir, "curtain_endpoint_convergence.png"), width_mm = 170, height_mm = 240)

global_curtain <- global_estimates %>% filter(effect_family == "curtain")
curtain_direction <- direction_summary %>% filter(effect_family == "curtain")
curtain_scan <- band_estimates %>%
  filter(effect_family == "curtain") %>%
  group_by(metric) %>%
  summarise(n_bh_below_005 = sum(p_bh_within_metric_effect < 0.05), .groups = "drop")

metric_line <- function(metric_name) {
  row <- global_curtain %>% filter(metric == metric_name)
  sprintf("%s %.3f %s (BCa 95%% CI %.3f to %.3f)", metric_name, row$estimate, row$unit, row$bca_low, row$bca_high)
}

results <- c(
  "# A08 results",
  "",
  "## Outcome",
  "",
  sprintf("Global curtain improvements are: %s; %s; %s; %s; %s.", metric_line("T20"), metric_line("T30"), metric_line("EDT"), metric_line("C50"), metric_line("C80")),
  "",
  sprintf("- Room-band curtain direction concordance with T20: %s.", paste(sprintf("%s %.1f%%", curtain_direction$metric, 100 * curtain_direction$direction_concordance), collapse = "; ")),
  sprintf("- Curtain bands with BH-adjusted sign-symmetry p < 0.05: %s.", paste(sprintf("%s %d/7", curtain_scan$metric, curtain_scan$n_bh_below_005), collapse = "; ")),
  sprintf("- Complete-four support ranges from %d to %d of 140 scheduled receiver-band profiles across endpoints.", min(support %>% group_by(metric) %>% summarise(n = sum(n_complete_four_receivers)) %>% pull(n)), max(support %>% group_by(metric) %>% summarise(n = sum(n_complete_four_receivers)) %>% pull(n))),
  "",
  "## Decision",
  "",
  "T30, EDT, C50, and C80 are retained as convergent sensitivity evidence. T20 remains primary. Any endpoint- or band-specific contradiction remains visible in the detailed direction and estimate tables.",
  "",
  "## Assumptions and limitations",
  "",
  "Endpoint agreement is not independent replication because all metrics derive from the same IRs. Each endpoint uses its own complete-four validity support; differences between endpoint magnitudes therefore combine physical metric differences and small support differences."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "endpoint_complete_four_support.csv", "global_endpoint_factorial_estimates.csv",
    "global_endpoint_bootstrap_draws.csv", "global_endpoint_jackknife.csv",
    "bandwise_endpoint_factorial_estimates.csv", "bandwise_endpoint_receiver_contributions.csv",
    "endpoint_room_band_direction_concordance.csv", "endpoint_direction_summary.csv",
    "curtain_endpoint_convergence.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c("endpoint-specific complete-four support", "positive means improvement", "BH within endpoint-effect bands")
)
