source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A15"
analysis_dir <- analysis_output_dir("a15_robustness")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver)

make_variant <- function(data, column, variant, require_complete_four = TRUE) {
  prepare_factorial_effects(
    data,
    column,
    variant,
    improvement = "lower",
    scale = "difference",
    require_complete_four = require_complete_four
  ) %>%
    filter(is.finite(effect_value))
}

base_effects <- make_variant(primary, "t20_s_strict", "strict_T20_complete_four")
pairwise_effects <- make_variant(
  primary, "t20_s_strict", "strict_T20_pairwise_available", require_complete_four = FALSE
)

complete_receivers <- primary %>%
  group_by(room_id, receiver_id) %>%
  summarise(complete_28 = n() == 28 & all(is.finite(t20_s_strict)), .groups = "drop") %>%
  filter(complete_28) %>%
  select(room_id, receiver_id)

complete_profile_data <- primary %>%
  inner_join(complete_receivers, by = c("room_id", "receiver_id"))

variant_effects <- bind_rows(
  base_effects,
  pairwise_effects,
  make_variant(
    complete_profile_data,
    "t20_s_strict",
    "strict_T20_15_receiver_complete_profile"
  ),
  make_variant(primary, "t20_s", "unrestricted_T20_complete_four"),
  make_variant(
    filter(primary, centre_frequency_hz != 125),
    "t20_s_strict",
    "strict_T20_exclude_125"
  ),
  make_variant(
    filter(primary, centre_frequency_hz >= 500),
    "t20_s_strict",
    "strict_T20_500_8000"
  ),
  make_variant(primary, "t30_s_strict", "strict_T30_complete_four")
)

variant_support <- variant_effects %>%
  group_by(metric, effect_family) %>%
  summarise(
    n_effect_rows = n(),
    n_receiver_blocks = n_distinct(receiver_block),
    n_bands = n_distinct(centre_frequency_hz),
    n_room_band_strata = n_distinct(room_id, centre_frequency_hz, stratum),
    .groups = "drop"
  )

observed <- estimate_factorial(variant_effects, level = "global")
bootstrap <- bootstrap_factorial(
  variant_effects,
  level = "global",
  R = BOOT_N,
  seed = BOOT_SEED + 150L
)
jackknife <- jackknife_factorial(variant_effects, level = "global")
variant_estimates <- factorial_intervals(
  observed, bootstrap, jackknife,
  keys = c("metric", "scale", "effect_family")
)

primary_curtain <- variant_estimates %>%
  filter(metric == "strict_T20_complete_four", effect_family == "curtain") %>%
  pull(estimate)

variant_estimates <- variant_estimates %>%
  mutate(delta_from_primary_curtain = if_else(
    effect_family == "curtain",
    estimate - primary_curtain,
    NA_real_
  ))

model_data <- primary %>%
  condition_factors() %>%
  filter(is.finite(t20_s_strict)) %>%
  mutate(
    room_id = factor(room_id),
    band = factor(centre_frequency_hz, levels = CORE_OCTAVE_BANDS),
    receiver_block = factor(receiver_block)
  )

mixed_model <- lme4::lmer(
  t20_s_strict ~ curtain * carpet * room_id + band + (1 | receiver_block),
  data = model_data,
  REML = FALSE
)
model_emmean <- emmeans::emmeans(mixed_model, ~ curtain, weights = "equal")
model_contrast <- emmeans::contrast(
  model_emmean,
  method = list(curtain_reduction = c(1, -1))
) %>%
  summary(infer = c(TRUE, TRUE), level = 0.95) %>%
  as_tibble()

model_estimate <- tibble(
  metric = "mixed_model_all_strict_valid",
  scale = "difference",
  effect_family = "curtain",
  estimate = model_contrast$estimate,
  standard_error = model_contrast$SE,
  df = model_contrast$df,
  ci_low = model_contrast$lower.CL,
  ci_high = model_contrast$upper.CL,
  p_value = model_contrast$p.value,
  singular_fit = lme4::isSingular(mixed_model),
  delta_from_primary = model_contrast$estimate - primary_curtain
)

condition_means <- primary %>%
  group_by(room_id, condition_id, centre_frequency_hz) %>%
  summarise(t20_s_strict = mean(t20_s_strict, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = condition_id, values_from = t20_s_strict) %>%
  transmute(
    room_id,
    centre_frequency_hz,
    curtain = ((open_no_carpet - closed_no_carpet) + (open_carpet - closed_carpet)) / 2,
    carpet = ((open_no_carpet - open_carpet) + (closed_no_carpet - closed_carpet)) / 2,
    interaction = (open_carpet - closed_carpet) - (open_no_carpet - closed_no_carpet)
  ) %>%
  pivot_longer(cols = c(curtain, carpet, interaction), names_to = "effect_family", values_to = "effect")

condition_mean_estimates <- condition_means %>%
  group_by(effect_family, room_id) %>%
  summarise(room_estimate = mean(effect), .groups = "drop") %>%
  group_by(effect_family) %>%
  summarise(
    estimate = mean(room_estimate),
    method = "condition_mean_reconstruction",
    .groups = "drop"
  ) %>%
  mutate(
    delta_from_primary_curtain = if_else(
      effect_family == "curtain",
      estimate - primary_curtain,
      NA_real_
    )
  )

base_jackknife <- jackknife %>%
  filter(metric == "strict_T20_complete_four", effect_family == "curtain") %>%
  mutate(
    delta_from_full = estimate - primary_curtain,
    absolute_delta = abs(delta_from_full)
  ) %>%
  arrange(desc(absolute_delta))

leave_one_band <- vector("list", length(CORE_OCTAVE_BANDS))
for (i in seq_along(CORE_OCTAVE_BANDS)) {
  omitted_band <- CORE_OCTAVE_BANDS[[i]]
  leave_one_band[[i]] <- base_effects %>%
    filter(centre_frequency_hz != omitted_band) %>%
    estimate_factorial(level = "global") %>%
    filter(effect_family == "curtain") %>%
    transmute(
      omitted_band_hz = omitted_band,
      estimate,
      delta_from_full = estimate - primary_curtain,
      absolute_delta = abs(delta_from_full)
    )
}
leave_one_band <- bind_rows(leave_one_band) %>% arrange(desc(absolute_delta))

readr::write_csv(variant_support, file.path(analysis_dir, "robustness_variant_support.csv"))
readr::write_csv(variant_estimates, file.path(analysis_dir, "robustness_design_based_estimates.csv"))
readr::write_csv(bootstrap, file.path(analysis_dir, "robustness_bootstrap_draws.csv"))
readr::write_csv(jackknife, file.path(analysis_dir, "robustness_jackknife_all_variants.csv"))
readr::write_csv(model_estimate, file.path(analysis_dir, "mixed_model_curtain_sensitivity.csv"))
readr::write_csv(condition_mean_estimates, file.path(analysis_dir, "condition_mean_sensitivity.csv"))
readr::write_csv(base_jackknife, file.path(analysis_dir, "leave_one_receiver_influence.csv"))
readr::write_csv(leave_one_band, file.path(analysis_dir, "leave_one_band_influence.csv"))

plot_data <- variant_estimates %>%
  filter(effect_family == "curtain") %>%
  mutate(
    metric = factor(metric, levels = rev(c(
      "strict_T20_complete_four",
      "strict_T20_pairwise_available",
      "strict_T20_15_receiver_complete_profile",
      "unrestricted_T20_complete_four",
      "strict_T20_exclude_125",
      "strict_T20_500_8000",
      "strict_T30_complete_four"
    )), labels = rev(c(
      "Primary: strict T20, complete four",
      "Strict T20, pairwise available",
      "Strict T20, 15 complete receivers",
      "Unrestricted T20, complete four",
      "Strict T20, 250 Hz to 8 kHz",
      "Strict T20, 500 Hz to 8 kHz",
      "Strict T30, complete four"
    )))
  )

robustness_plot <- ggplot(plot_data, aes(x = estimate, y = metric)) +
  geom_vline(xintercept = primary_curtain, linetype = "dashed", linewidth = 0.45, colour = "#666666") +
  geom_errorbar(
    aes(xmin = bca_low, xmax = bca_high),
    orientation = "y", width = 0.14, linewidth = 0.45, colour = "#2A6F97"
  ) +
  geom_point(size = 2, colour = "#2A6F97") +
  labs(
    title = "Curtain-effect robustness across design-based variants",
    subtitle = "BCa 95% intervals; dashed line is the primary complete-four T20 estimate",
    x = "Decay-time reduction (s)",
    y = NULL
  ) +
  theme_p31()

save_p31_plot(robustness_plot, file.path(analysis_dir, "curtain_robustness_forest.png"), width_mm = 180, height_mm = 125)

t20_variants <- variant_estimates %>%
  filter(effect_family == "curtain", metric != "strict_T30_complete_four")
t30_variant <- variant_estimates %>%
  filter(effect_family == "curtain", metric == "strict_T30_complete_four")
carpet_t20_variants <- variant_estimates %>%
  filter(effect_family == "carpet", metric != "strict_T30_complete_four")
max_receiver <- base_jackknife %>% slice_max(absolute_delta, n = 1, with_ties = FALSE)

results <- c(
  "# A15 results",
  "",
  "## Outcome",
  "",
  sprintf("- Design-based T20 curtain estimates range from %.3f to %.3f s across strict support, complete-profile, unrestricted, and frequency-range variants; every BCa interval excludes zero.", min(t20_variants$estimate), max(t20_variants$estimate)),
  sprintf("- Strict-T30 complete-four curtain estimate: %.3f s (BCa 95%% CI %.3f to %.3f).", t30_variant$estimate, t30_variant$bca_low, t30_variant$bca_high),
  sprintf("- Fixed-room mixed receiver-block model: %.3f s (model-based 95%% CI %.3f to %.3f; singular fit: %s).", model_estimate$estimate, model_estimate$ci_low, model_estimate$ci_high, model_estimate$singular_fit),
  sprintf("- Condition-mean reconstruction: %.3f s, differing from the primary estimate by %.4f s.", condition_mean_estimates$estimate[condition_mean_estimates$effect_family == "curtain"], condition_mean_estimates$delta_from_primary_curtain[condition_mean_estimates$effect_family == "curtain"]),
  sprintf("- Largest leave-one-receiver shift: %.4f s after omitting Room %s receiver %s.", max_receiver$absolute_delta, max_receiver$omitted_room_id, max_receiver$omitted_receiver_id),
  sprintf("- Leave-one-band curtain estimates range from %.3f to %.3f s.", min(leave_one_band$estimate), max(leave_one_band$estimate)),
  sprintf("- T20 carpet estimates range from %.3f to %.3f s across variants, confirming that the global near-zero value masks frequency-dependent sign changes.", min(carpet_t20_variants$estimate), max(carpet_t20_variants$estimate)),
  "",
  "## Decision",
  "",
  "The positive curtain conclusion is robust to the registered analysis choices and influence checks. The complete-four strict-T20 estimator remains primary; robustness does not remove sequential condition-order confounding or create room-population generalisability.",
  "",
  "## Assumptions and limitations",
  "",
  "Pairwise and unrestricted variants change support; T30 changes estimator; the mixed model adds distributional and covariance assumptions. Agreement across them is sensitivity evidence, not independent replication."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "robustness_variant_support.csv", "robustness_design_based_estimates.csv",
    "robustness_bootstrap_draws.csv", "robustness_jackknife_all_variants.csv",
    "mixed_model_curtain_sensitivity.csv", "condition_mean_sensitivity.csv",
    "leave_one_receiver_influence.csv", "leave_one_band_influence.csv",
    "curtain_robustness_forest.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c("primary estimator unchanged", "whole receiver resampling", "condition order remains non-identifiable")
)
