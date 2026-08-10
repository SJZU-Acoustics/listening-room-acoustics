source(file.path("code", "helpers.R"))
source(file.path("code", "factorial_helpers.R"))

analysis_id <- "A05"
analysis_dir <- analysis_output_dir("a05_factorial_t20")

receiver <- read_frozen_csv("receiver_band_metrics.csv")
primary <- strict_primary_rows(receiver)

effects_seconds <- prepare_factorial_effects(
  primary, "t20_s_strict", "T20", improvement = "lower", scale = "difference"
)
effects_log <- prepare_factorial_effects(
  primary, "t20_s_strict", "T20", improvement = "lower", scale = "log_ratio"
)
effects <- bind_rows(effects_seconds, effects_log)

support <- factorial_support(effects_seconds)
observed <- estimate_factorial(effects, level = "global")
bootstrap <- bootstrap_factorial(effects, level = "global", R = BOOT_N, seed = BOOT_SEED)
jackknife <- jackknife_factorial(effects, level = "global")
intervals <- factorial_intervals(
  observed, bootstrap, jackknife,
  keys = c("metric", "scale", "effect_family")
)

contributions <- factorial_contributions(effects, level = "global")
signflip <- signflip_factorial(
  contributions,
  keys = c("metric", "scale", "effect_family"),
  max_permutations = 2^20,
  seed = BOOT_SEED
)

estimates <- intervals %>%
  left_join(
    signflip,
    by = c("metric", "scale", "effect_family")
  ) %>%
  mutate(
    contribution_check_difference = estimate - estimate_from_contributions,
    percent_reduction = if_else(
      scale == "log_ratio" & effect_family != "interaction",
      100 * (1 - exp(-estimate)),
      NA_real_
    ),
    percent_reduction_bca_low = if_else(
      scale == "log_ratio" & effect_family != "interaction",
      100 * (1 - exp(-bca_low)),
      NA_real_
    ),
    percent_reduction_bca_high = if_else(
      scale == "log_ratio" & effect_family != "interaction",
      100 * (1 - exp(-bca_high)),
      NA_real_
    ),
    interaction_ratio_of_reduction_ratios = if_else(
      scale == "log_ratio" & effect_family == "interaction",
      exp(estimate),
      NA_real_
    )
  )

if (any(abs(estimates$contribution_check_difference) > 1e-12)) {
  stop("A05 weighted receiver contributions do not reproduce the nested estimand.")
}

complete_profile_summary <- primary %>%
  group_by(room_id, receiver_id) %>%
  summarise(
    n_strict_t20 = sum(is.finite(t20_s_strict)),
    fully_complete_28 = n_strict_t20 == 28,
    .groups = "drop"
  )

plot_data <- estimates %>%
  filter(scale == "difference") %>%
  mutate(
    effect_family = factor(
      effect_family,
      levels = c("curtain", "carpet", "interaction"),
      labels = c("Curtain", "Carpet", "Curtain x carpet")
    )
  )

effect_plot <- ggplot(plot_data, aes(x = estimate, y = effect_family)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = "#666666") +
  geom_errorbar(
    aes(xmin = bca_low, xmax = bca_high),
    orientation = "y", width = 0.16, linewidth = 0.5, colour = "#2A6F97"
  ) +
  geom_point(size = 2.2, colour = "#2A6F97") +
  labs(
    title = "Global factorial effects on strict T20",
    subtitle = "Equal-room, equal-band estimates with room-stratified BCa 95% intervals",
    x = "T20 reduction (s)",
    y = NULL
  ) +
  theme_p31()

readr::write_csv(support, file.path(analysis_dir, "factorial_complete_four_support.csv"))
readr::write_csv(complete_profile_summary, file.path(analysis_dir, "complete_receiver_profile_support.csv"))
readr::write_csv(estimates, file.path(analysis_dir, "global_factorial_estimates.csv"))
readr::write_csv(bootstrap, file.path(analysis_dir, "global_factorial_bootstrap_draws.csv"))
readr::write_csv(jackknife, file.path(analysis_dir, "global_factorial_jackknife.csv"))
readr::write_csv(contributions, file.path(analysis_dir, "global_receiver_contributions.csv"))
save_p31_plot(effect_plot, file.path(analysis_dir, "global_factorial_t20.png"), width_mm = 160, height_mm = 90)

seconds <- estimates %>% filter(scale == "difference")
curtain <- seconds %>% filter(effect_family == "curtain")
carpet <- seconds %>% filter(effect_family == "carpet")
interaction <- seconds %>% filter(effect_family == "interaction")
curtain_percent <- estimates %>% filter(scale == "log_ratio", effect_family == "curtain")
max_interval_shift <- max(
  abs(estimates$bca_low - estimates$percentile_low),
  abs(estimates$bca_high - estimates$percentile_high)
)

results <- c(
  "# A05 results",
  "",
  "## Outcome",
  "",
  sprintf("The common factorial support contains %d of 140 scheduled receiver-band profiles; cell counts range from %d to %d receivers per room-band.", sum(support$n_complete_four_receivers), min(support$n_complete_four_receivers), max(support$n_complete_four_receivers)),
  "",
  sprintf("- Curtain: %.3f s reduction (BCa 95%% CI %.3f to %.3f; two-sided sign-symmetry p = %.4g).", curtain$estimate, curtain$bca_low, curtain$bca_high, curtain$sign_symmetry_p_two_sided),
  sprintf("- Curtain geometric proportional reduction: %.1f%% (BCa 95%% CI %.1f%% to %.1f%%).", curtain_percent$percent_reduction, curtain_percent$percent_reduction_bca_low, curtain_percent$percent_reduction_bca_high),
  sprintf("- Carpet: %.3f s reduction (BCa 95%% CI %.3f to %.3f; two-sided sign-symmetry p = %.4g).", carpet$estimate, carpet$bca_low, carpet$bca_high, carpet$sign_symmetry_p_two_sided),
  sprintf("- Curtain-by-carpet interaction: %.3f s (BCa 95%% CI %.3f to %.3f; two-sided sign-symmetry p = %.4g).", interaction$estimate, interaction$bca_low, interaction$bca_high, interaction$sign_symmetry_p_two_sided),
  sprintf("- Maximum BCa-versus-percentile endpoint shift across reported scales/effects: %.4f in the corresponding scale.", max_interval_shift),
  "",
  "## Decision",
  "",
  "The curtain contrast is the primary global result. Carpet and interaction remain secondary, estimation-first results. Sign-symmetry p-values do not convert the sequential room-wide condition design into a randomised experiment.",
  "",
  "## Assumptions and limitations",
  "",
  "Strict-validity selection is assumed not to depend on the unobserved four-condition contrast within room-band. The later robustness analysis must compare pairwise-available, 15-profile complete-case, unrestricted-finite, T30, and leave-one-receiver-out estimates."
)
writeLines(results, file.path(analysis_dir, "results.md"), useBytes = TRUE)
write_session_info(file.path(analysis_dir, "session_info.txt"))
write_run_log(
  file.path(analysis_dir, "run.log"), analysis_id,
  inputs = "receiver_band_metrics.csv",
  outputs = c(
    "factorial_complete_four_support.csv", "complete_receiver_profile_support.csv",
    "global_factorial_estimates.csv", "global_factorial_bootstrap_draws.csv",
    "global_factorial_jackknife.csv", "global_receiver_contributions.csv",
    "global_factorial_t20.png", "results.md", "session_info.txt", "run.log"
  ),
  checks = c(
    "common complete-four support", "equal room and band weights",
    "bootstrap receiver profiles within room", "weighted contributions reproduce estimand"
  )
)
