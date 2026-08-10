source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A10"
analysis_dir <- analysis_output_dir("a10_spatial_uniformity")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver)

long <- primary %>%
  select(
    room_id, condition_id, receiver_id, receiver_block, centre_frequency_hz,
    T20 = t20_s_strict, EDT = edt_s_strict, C80 = c80_db_strict
  ) %>%
  pivot_longer(cols = c(T20, EDT, C80), names_to = "metric", values_to = "value")

spatial_summary <- function(data) {
  data %>%
    group_by(metric, room_id, condition_id, centre_frequency_hz) %>%
    summarise(
      n_valid = sum(is.finite(value)),
      mean = safe_mean(value),
      median = if (any(is.finite(value))) median(value, na.rm = TRUE) else NA_real_,
      sd = safe_sd(value),
      cv = if (
        first(metric) %in% c("T20", "EDT") && is.finite(mean) && mean != 0
      ) sd / mean else NA_real_,
      iqr = if (sum(is.finite(value)) >= 2) IQR(value, na.rm = TRUE) else NA_real_,
      mad = if (sum(is.finite(value)) >= 2) mad(value, na.rm = TRUE) else NA_real_,
      range = if (sum(is.finite(value)) >= 2) diff(range(value, na.rm = TRUE)) else NA_real_,
      .groups = "drop"
    )
}

spatial_factorial <- function(data) {
  summary <- spatial_summary(data) %>%
    filter(n_valid >= 5) %>%
    select(metric, room_id, condition_id, centre_frequency_hz, sd, iqr) %>%
    pivot_longer(cols = c(sd, iqr), names_to = "statistic", values_to = "spread") %>%
    pivot_wider(names_from = condition_id, values_from = spread) %>%
    filter(if_all(all_of(CONDITION_LEVELS), is.finite))

  effects <- bind_rows(
    summary %>% transmute(
      metric, statistic, room_id, centre_frequency_hz,
      effect_family = "curtain",
      effect = ((open_no_carpet - closed_no_carpet) + (open_carpet - closed_carpet)) / 2
    ),
    summary %>% transmute(
      metric, statistic, room_id, centre_frequency_hz,
      effect_family = "carpet",
      effect = ((open_no_carpet - open_carpet) + (closed_no_carpet - closed_carpet)) / 2
    ),
    summary %>% transmute(
      metric, statistic, room_id, centre_frequency_hz,
      effect_family = "interaction",
      effect = (open_carpet - closed_carpet) - (open_no_carpet - closed_no_carpet)
    )
  )

  effects %>%
    group_by(metric, statistic, effect_family, room_id) %>%
    summarise(room_estimate = mean(effect), n_bands_used = n(), .groups = "drop") %>%
    group_by(metric, statistic, effect_family) %>%
    summarise(
      estimate = mean(room_estimate),
      minimum_room_bands = min(n_bands_used),
      .groups = "drop"
    )
}

resample_long_profiles <- function(data) {
  blocks <- data %>% distinct(room_id, receiver_id)
  sampled <- blocks %>%
    group_by(room_id) %>%
    reframe(
      receiver_id = sample(receiver_id, size = n(), replace = TRUE),
      bootstrap_receiver_id = seq_len(n())
    )
  data %>%
    inner_join(sampled, by = c("room_id", "receiver_id"), relationship = "many-to-many") %>%
    mutate(
      receiver_id = bootstrap_receiver_id,
      receiver_block = paste0("room", room_id, "_bootstrap", bootstrap_receiver_id)
    ) %>%
    select(-bootstrap_receiver_id)
}

condition_spatial <- spatial_summary(long)
observed <- spatial_factorial(long)

set.seed(BOOT_SEED + 100L)
bootstrap_rows <- vector("list", BOOT_N)
for (b in seq_len(BOOT_N)) {
  bootstrap_rows[[b]] <- spatial_factorial(resample_long_profiles(long)) %>%
    mutate(bootstrap_id = b, .before = 1)
}
bootstrap <- bind_rows(bootstrap_rows)

blocks <- long %>% distinct(room_id, receiver_id) %>% arrange(room_id, receiver_id)
jackknife_rows <- vector("list", nrow(blocks))
for (i in seq_len(nrow(blocks))) {
  omitted_room <- blocks$room_id[[i]]
  omitted_receiver <- blocks$receiver_id[[i]]
  jackknife_rows[[i]] <- long %>%
    filter(!(room_id == omitted_room & receiver_id == omitted_receiver)) %>%
    spatial_factorial() %>%
    mutate(
      jackknife_id = i,
      omitted_room_id = omitted_room,
      omitted_receiver_id = omitted_receiver,
      .before = 1
    )
}
jackknife <- bind_rows(jackknife_rows)

spread_estimates <- factorial_intervals(
  observed, bootstrap, jackknife,
  keys = c("metric", "statistic", "effect_family")
)

effect_specs <- tribble(
  ~metric, ~column, ~improvement,
  "T20", "t20_s_strict", "lower",
  "EDT", "edt_s_strict", "lower",
  "C80", "c80_db_strict", "higher"
)
receiver_effect_sets <- vector("list", nrow(effect_specs))
for (i in seq_len(nrow(effect_specs))) {
  receiver_effect_sets[[i]] <- prepare_factorial_effects(
    primary,
    effect_specs$column[[i]],
    effect_specs$metric[[i]],
    improvement = effect_specs$improvement[[i]],
    scale = "difference"
  )
}

receiver_curtain_effects <- bind_rows(receiver_effect_sets) %>%
  filter(effect_family == "curtain") %>%
  group_by(metric, room_id, receiver_id, receiver_block, centre_frequency_hz) %>%
  summarise(band_effect = mean(effect_value), .groups = "drop") %>%
  group_by(metric, room_id, receiver_id, receiver_block) %>%
  summarise(
    curtain_effect = mean(band_effect),
    n_complete_bands = n(),
    .groups = "drop"
  )

receiver_effect_summary <- receiver_curtain_effects %>%
  group_by(metric, room_id) %>%
  summarise(
    n_receivers = n(),
    n_positive = sum(curtain_effect > 0),
    minimum = min(curtain_effect),
    q1 = quantile(curtain_effect, 0.25),
    median = median(curtain_effect),
    q3 = quantile(curtain_effect, 0.75),
    maximum = max(curtain_effect),
    .groups = "drop"
  )

plot_data <- receiver_curtain_effects %>%
  filter(metric == "T20") %>%
  mutate(room_label = paste("Room", room_id))

spatial_plot <- ggplot(plot_data, aes(x = room_label, y = curtain_effect, colour = room_label)) +
  geom_hline(yintercept = 0, linewidth = 0.35, colour = "#666666") +
  geom_boxplot(width = 0.42, outlier.shape = NA, colour = "#555555", fill = "white") +
  geom_jitter(width = 0.09, height = 0, size = 1.8) +
  scale_colour_manual(values = c("Room 1" = "#2A6F97", "Room 2" = "#D08C60"), guide = "none") +
  labs(
    title = "Receiver-specific curtain effects on strict T20",
    subtitle = "Complete-four effects averaged over carpet states and available core bands",
    x = NULL,
    y = "T20 reduction (s)"
  ) +
  theme_p31()

readr::write_csv(condition_spatial, file.path(analysis_dir, "condition_spatial_summary.csv"))
readr::write_csv(spread_estimates, file.path(analysis_dir, "spatial_spread_factorial_estimates.csv"))
readr::write_csv(bootstrap, file.path(analysis_dir, "spatial_spread_bootstrap_draws.csv"))
readr::write_csv(jackknife, file.path(analysis_dir, "spatial_spread_jackknife.csv"))
readr::write_csv(receiver_curtain_effects, file.path(analysis_dir, "receiver_curtain_effects.csv"))
readr::write_csv(receiver_effect_summary, file.path(analysis_dir, "receiver_curtain_effect_summary.csv"))
save_p31_plot(spatial_plot, file.path(analysis_dir, "receiver_curtain_effect_distribution.png"), width_mm = 145, height_mm = 100)

t20_sd <- spread_estimates %>% filter(metric == "T20", statistic == "sd", effect_family == "curtain")
t20_iqr <- spread_estimates %>% filter(metric == "T20", statistic == "iqr", effect_family == "curtain")
c80_sd <- spread_estimates %>% filter(metric == "C80", statistic == "sd", effect_family == "curtain")
t20_receiver <- receiver_effect_summary %>% filter(metric == "T20")

results <- c(
  "# A10 results",
  "",
  "## Outcome",
  "",
  sprintf("- Curtain effect on spatial T20 SD: %.3f s (BCa 95%% CI %.3f to %.3f); positive means less spatial spread.", t20_sd$estimate, t20_sd$bca_low, t20_sd$bca_high),
  sprintf("- Curtain effect on spatial T20 IQR: %.3f s (BCa 95%% CI %.3f to %.3f).", t20_iqr$estimate, t20_iqr$bca_low, t20_iqr$bca_high),
  sprintf("- Curtain effect on spatial C80 SD: %.3f dB (BCa 95%% CI %.3f to %.3f).", c80_sd$estimate, c80_sd$bca_low, c80_sd$bca_high),
  sprintf("- Receiver-specific strict-T20 curtain effects are positive at %s.", paste(sprintf("%d/%d positions in Room %d", t20_receiver$n_positive, t20_receiver$n_receivers, t20_receiver$room_id), collapse = "; ")),
  sprintf("- Receiver-specific T20 effect ranges: %s.", paste(sprintf("Room %d %.3f to %.3f s", t20_receiver$room_id, t20_receiver$minimum, t20_receiver$maximum), collapse = "; ")),
  "",
  "## Decision",
  "",
  "The curtain benefit is directionally robust across measured positions, but the T20 SD and IQR intervals do not show a clear uniformity improvement. Mean reduction and spatial uniformity must therefore remain separate claims.",
  "",
  "## Assumptions and limitations",
  "",
  "Spread estimates use at least five valid receivers per cell; EDT therefore has fewer low-frequency cells than T20 or C80. Receiver-grid bootstrap intervals remain conditional on each measured room and do not represent listener-to-listener or room-population uncertainty."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "condition_spatial_summary.csv", "spatial_spread_factorial_estimates.csv",
    "spatial_spread_bootstrap_draws.csv", "spatial_spread_jackknife.csv",
    "receiver_curtain_effects.csv", "receiver_curtain_effect_summary.csv",
    "receiver_curtain_effect_distribution.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c("receiver profile resampling", "minimum five valid positions", "mean and spread separated")
)
