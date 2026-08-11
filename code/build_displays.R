source(file.path("code", "style.R"))
source(file.path("code", "load_data.R"))

# This pipeline consumes only frozen processed data and completed Stage-2 outputs.
# It does not refit any inferential model.

# ----------------------------------------------------------------------------
# Figure 1: study design and evidence boundary
# ----------------------------------------------------------------------------
fig1_design <- tribble(
  ~item, ~value,
  "Room 1", paste("8.15", intToUtf8(215), "6.00", intToUtf8(215), "2.87 m; 139.176 m3; four chamfers"),
  "Room 2", paste("8.00", intToUtf8(215), "6.00", intToUtf8(215), "2.87 m; nominal dimensions"),
  "Primary grid", "S1 across four sequential furnishing states, ten receiver positions and two rooms",
  "Sensitivity grid", "S2 available in four source-condition sets",
  "Acquisition", "DIRAC 6.0; B&K 4292-L source and 4189 microphone; height 1.2 m",
  "Evidence", "130 deconvolved impulse responses; no installed renovation or post-treatment measurement"
)
lock_csv(fig1_design, "fig1_design.csv")

box <- function(xmin, xmax, ymin, ymax, fill, colour = "#333333", linewidth = 0.45) {
  annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
           fill = fill, colour = colour, linewidth = linewidth)
}
arrow_seg <- function(x, xend, y, yend, linetype = "solid", colour = "#444444") {
  annotate("segment", x = x, xend = xend, y = y, yend = yend,
           colour = colour, linewidth = 0.55, linetype = linetype,
           arrow = arrow(length = unit(0.11, "inches"), type = "closed"))
}

p1a <- ggplot() +
  box(0.5, 4.7, 6.7, 9.1, "#EAF3F8", COL["blue"]) +
  box(5.3, 9.5, 6.7, 9.1, "#FDF2E2", COL["orange"]) +
  annotate("text", x = 2.6, y = 8.65, label = "Room 1", fontface = "bold", size = 3.2) +
  annotate("text", x = 2.6, y = 8.05, label = paste("8.15", intToUtf8(215), "6.00", intToUtf8(215), "2.87 m"), size = 2.85) +
  annotate("text", x = 2.6, y = 7.48, label = "10 receivers\n4 chamfered corners", size = 2.85, lineheight = 0.9) +
  annotate("text", x = 7.4, y = 8.65, label = "Room 2", fontface = "bold", size = 3.2) +
  annotate("text", x = 7.4, y = 8.05, label = paste("8.00", intToUtf8(215), "6.00", intToUtf8(215), "2.87 m"), size = 2.85) +
  annotate("text", x = 7.4, y = 7.48, label = "10 receivers\nnominal dimensions", size = 2.85, lineheight = 0.9) +
  arrow_seg(5, 5, 6.35, 5.75) +
  box(0.55, 2.75, 3.75, 5.55, "white", COL["grey"]) +
  box(2.95, 5.15, 3.75, 5.55, "#DCECF4", COL["blue"]) +
  box(5.35, 7.55, 3.75, 5.55, "#F4F4F4", COL["grey"]) +
  box(7.75, 9.95, 3.75, 5.55, "#DCECF4", COL["blue"]) +
  annotate("text", x = 1.65, y = 4.65, label = "Open\nNo carpet", size = 2.85) +
  annotate("text", x = 4.05, y = 4.65, label = "Closed\nNo carpet", size = 2.85) +
  annotate("text", x = 6.45, y = 4.65, label = "Open\nCarpet", size = 2.85) +
  annotate("text", x = 8.85, y = 4.65, label = "Closed\nCarpet", size = 2.85) +
  arrow_seg(5, 5, 3.45, 2.8) +
  box(1.25, 8.75, 1.05, 2.65, "#F7F7F7") +
  annotate("text", x = 5, y = 2.15, label = "DIRAC 6.0 | S1 complete | S2 partial", fontface = "bold", size = 3.0) +
  annotate("text", x = 5, y = 1.55, label = "130 deconvolved impulse responses", size = 2.85) +
  coord_cartesian(xlim = c(0.2, 10.2), ylim = c(0.7, 9.5), clip = "off") +
  theme_schematic()

p1b <- ggplot() +
  box(1.0, 9.0, 7.2, 9.15, "#EAF3F8", COL["blue"]) +
  annotate("text", x = 5, y = 8.65, label = "Measured evidence", fontface = "bold", size = 3.2) +
  annotate("text", x = 5, y = 8.05,
           label = "italic(T)[20]~'|'~italic(T)[30]~'|'~plain('EDT')~'|'~italic(C)[50]~'|'~italic(C)[80]~'|'~plain('CWT')",
           parse = TRUE, size = 2.85) +
  annotate("text", x = 5, y = 7.55, label = "receiver-profile resampling", size = 2.85) +
  arrow_seg(5, 5, 7.0, 6.35) +
  box(1.0, 9.0, 4.45, 6.2, "#ECF6F1", COL["green"]) +
  annotate("text", x = 5, y = 5.75, label = "Measured diagnosis", fontface = "bold", size = 3.2) +
  annotate("text", x = 5, y = 5.15, label = "broadband control | low-frequency boundary", size = 2.85) +
  annotate("segment", x = 0.65, xend = 9.35, y = 3.95, yend = 3.95,
           linetype = "dashed", colour = COL["vermillion"], linewidth = 0.7) +
  annotate("text", x = 5, y = 4.2, label = "Evidence boundary", colour = COL["vermillion"], size = 2.85) +
  annotate("segment", x = 5, xend = 5, y = 3.78, yend = 3.36,
           colour = COL["vermillion"], linewidth = 0.55, linetype = "31") +
  arrow_seg(5, 5, 3.38, 3.15, colour = COL["vermillion"]) +
  box(1.0, 9.0, 0.65, 3.05, "#FCEBE6", COL["vermillion"]) +
  annotate("text", x = 5, y = 2.62, label = "Prospective product scenario", fontface = "bold", size = 3.2) +
  annotate("text", x = 5, y = 1.92, label = "certified coefficients\nSabine-equivalent calculation", size = 2.85, lineheight = 0.88) +
  annotate("text", x = 5, y = 0.92, label = "no installation | no post-treatment validation", colour = COL["vermillion"], size = 2.85) +
  coord_cartesian(xlim = c(0.2, 9.8), ylim = c(0.6, 9.5), clip = "off") +
  theme_schematic()

p1 <- (p1a | p1b) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
save_figure(p1, "fig1_design_boundary.png", "double_column", 88)

# ----------------------------------------------------------------------------
# Figure 2: principal curtain estimate and third-octave generality
# ----------------------------------------------------------------------------
global <- read_exp("a05_factorial_t20", "global_factorial_estimates.csv")
primary <- global %>% filter(scale == "difference", effect_family == "curtain")
primary_pct <- global %>% filter(scale == "log_ratio", effect_family == "curtain")
third <- read_exp("a17_third_octave_structure", "third_octave_factorial_estimates.csv") %>%
  filter(effect_family == "curtain")

fig2_primary <- primary %>%
  transmute(estimate, low = bca_low, high = bca_high,
            percent = primary_pct$percent_reduction,
            percent_low = primary_pct$percent_reduction_bca_low,
            percent_high = primary_pct$percent_reduction_bca_high)
lock_csv(fig2_primary, "fig2_primary.csv")
lock_csv(third, "fig2_third_octave.csv")

p2a <- ggplot(fig2_primary, aes(y = 1, x = estimate)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = COL["grey"]) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0, linewidth = 0.8, colour = COL["blue"]) +
  geom_point(size = 3.2, shape = 21, stroke = 0.6, fill = COL["blue"], colour = "black") +
  annotate("text", x = fig2_primary$estimate, y = 1.18,
           label = paste0(fmt(fig2_primary$estimate, 3), " s"), fontface = "bold", size = 3) +
  scale_x_continuous(limits = c(0, 0.14), breaks = c(0, 0.05, 0.10), expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(limits = c(0.62, 1.38), breaks = NULL) +
  labs(x = expression("Curtain reduction in " * italic(T)[20] * " (s)"), y = NULL) +
  theme_pub() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

p2b <- ggplot(third, aes(x = centre_frequency_hz, y = estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = COL["grey"]) +
  geom_ribbon(aes(ymin = bca_low, ymax = bca_high, fill = support_class), alpha = 0.16, colour = NA) +
  geom_line(aes(colour = support_class, linetype = support_class), linewidth = 0.7) +
  geom_point(aes(fill = support_class), shape = 21, colour = "black", stroke = 0.35, size = 2) +
  scale_x_log10(
    limits = c(100, 10000),
    breaks = c(100, 630, 2000, 10000),
    labels = c("100", "630", "2k", "10k"),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(breaks = c(0, 0.05, 0.10, 0.15, 0.20)) +
  scale_colour_manual(values = c(full_support = COL["blue"], diagnostic_incomplete = COL["grey"]),
                      breaks = c("full_support", "diagnostic_incomplete"),
                      labels = c(full_support = "Complete", diagnostic_incomplete = "Incomplete")) +
  scale_fill_manual(values = c(full_support = COL["blue"], diagnostic_incomplete = "white"),
                    breaks = c("full_support", "diagnostic_incomplete"),
                    labels = c(full_support = "Complete", diagnostic_incomplete = "Incomplete")) +
  scale_linetype_manual(values = c(full_support = "solid", diagnostic_incomplete = "22"),
                        breaks = c("full_support", "diagnostic_incomplete"),
                        labels = c(full_support = "Complete", diagnostic_incomplete = "Incomplete")) +
  guides(fill = "none", linetype = "none", colour = guide_legend(override.aes = list(linetype = c("solid", "22")))) +
  labs(x = "One-third-octave centre frequency (Hz)",
       y = expression("Curtain reduction in " * italic(T)[20] * " (s)")) +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(legend.position = "top", legend.direction = "horizontal",
        legend.justification = "left")

p2 <- (p2a | p2b) + plot_layout(widths = c(1, 1)) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
save_figure(p2, "fig2_curtain_spectrum.png", "double_column", 82)

# ----------------------------------------------------------------------------
# Figure 3: generality across rooms, receivers, sources, and listening positions
# ----------------------------------------------------------------------------
room_est <- read_exp("a07_room_consistency", "room_specific_factorial_estimates.csv") %>%
  filter(effect_family == "curtain") %>%
  mutate(room = factor(room_id, levels = c(2, 1), labels = c("Room 2", "Room 1")))
receiver_est <- read_exp("a10_spatial_uniformity", "receiver_curtain_effects.csv") %>%
  filter(metric == "T20") %>%
  mutate(room = factor(room_id, levels = c(1, 2), labels = c("Room 1", "Room 2")),
         role = case_when(room_id == 1 & receiver_id == 4 ~ "Listening position",
                          room_id == 2 & receiver_id == 5 ~ "Listening position",
                          TRUE ~ "Other receiver"))
source_est <- read_exp("a09_source_sensitivity", "room2_source_condition_global_summary.csv") %>%
  filter(metric == "T20", contrast_id == "curtain_no_carpet") %>%
  pivot_longer(c(effect_s1, effect_s2), names_to = "source", values_to = "estimate") %>%
  mutate(source = recode(source, effect_s1 = "S1", effect_s2 = "S2"))
listen_est <- read_exp("a11_listening_position", "listening_curtain_representativeness.csv") %>%
  filter(metric == "T20") %>%
  transmute(
    room = factor(room_id, levels = c(1, 2), labels = c("Room 1", "Room 2")),
    listening = listening_curtain_effect,
    other = other_receiver_mean_effect
  ) %>%
  pivot_longer(c(listening, other), names_to = "position", values_to = "estimate") %>%
  mutate(position = recode(position, listening = "Listening position", other = "Other receivers"))

lock_csv(room_est, "fig3_rooms.csv")
lock_csv(receiver_est, "fig3_receivers.csv")
lock_csv(source_est, "fig3_sources.csv")
lock_csv(listen_est, "fig3_listening.csv")

p3a <- ggplot(room_est, aes(y = room, x = estimate, colour = room, shape = room)) +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = COL["grey"]) +
  geom_errorbar(aes(xmin = bca_low, xmax = bca_high), orientation = "y", width = 0, linewidth = 0.7) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c("Room 1" = COL["blue"], "Room 2" = COL["orange"])) +
  scale_shape_manual(values = c("Room 1" = 16, "Room 2" = 17)) +
  scale_x_continuous(limits = c(0, 0.15), breaks = c(0, 0.05, 0.10, 0.15), expand = expansion(mult = c(0, 0))) +
  labs(x = expression("Curtain reduction in " * italic(T)[20] * " (s)"), y = NULL) +
  theme_pub() + theme(legend.position = "none", axis.ticks.y = element_blank())

p3b <- ggplot(receiver_est, aes(x = receiver_id, y = curtain_effect, colour = room, shape = room)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = COL["grey"]) +
  geom_point(size = 2.3, stroke = 0.6) +
  geom_point(data = filter(receiver_est, role == "Listening position"),
             shape = 5, colour = "black", size = 3.8, stroke = 0.65,
             show.legend = FALSE) +
  scale_colour_manual(values = c("Room 1" = COL["blue"], "Room 2" = COL["orange"])) +
  scale_shape_manual(values = c("Room 1" = 16, "Room 2" = 17)) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 10), limits = c(1, 10), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0, 0.17), breaks = c(0, 0.05, 0.10, 0.15), expand = expansion(mult = c(0, 0))) +
  labs(x = "Receiver position", y = expression("Curtain reduction in " * italic(T)[20] * " (s)")) +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(legend.position = "top", legend.direction = "horizontal",
        legend.justification = "left")

p3c <- ggplot(source_est, aes(x = source, y = estimate, fill = source)) +
  geom_col(width = 0.62, colour = "black", linewidth = 0.35) +
  geom_text(aes(label = fmt(estimate, 3)), vjust = -0.5, size = 2.8) +
  scale_fill_manual(values = c(S1 = COL["blue"], S2 = COL["sky"])) +
  scale_y_continuous(limits = c(0, 0.15), breaks = c(0, 0.05, 0.10, 0.15), expand = expansion(mult = c(0, 0))) +
  labs(x = "Room 2 source position", y = expression("Curtain reduction in " * italic(T)[20] * " (s)")) +
  theme_pub() + theme(legend.position = "none")

p3d <- ggplot(listen_est, aes(y = position, x = estimate, colour = room,
                             linetype = room, shape = room, group = room)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c("Room 1" = COL["blue"], "Room 2" = COL["orange"])) +
  scale_linetype_manual(values = c("Room 1" = "solid", "Room 2" = "22")) +
  scale_shape_manual(values = c("Room 1" = 16, "Room 2" = 17)) +
  scale_x_continuous(limits = c(0.06, 0.13), breaks = c(0.06, 0.09, 0.12), expand = expansion(mult = c(0, 0))) +
  labs(y = NULL, x = expression("Curtain reduction in " * italic(T)[20] * " (s)")) +
  theme_pub() +
  theme(legend.position = "top", legend.direction = "horizontal",
        legend.justification = "left", axis.ticks.y = element_blank())

p3 <- wrap_plots(p3a, p3b, p3c, p3d, ncol = 2) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
save_figure(p3, "fig3_generality.png", "double_column", 140)

# ----------------------------------------------------------------------------
# Figure 4: low-frequency persistence boundary
# ----------------------------------------------------------------------------
screen <- read_exp("a19_low_frequency_cwt", "local_frequency_screen_summary.csv") %>%
  mutate(room = factor(room_id, levels = c(1, 2), labels = c("Room 1", "Room 2")))
modal <- read_exp("a19_low_frequency_cwt", "fixed_room_modal_excess_summary.csv") %>%
  filter(room_id == 1) %>%
  mutate(target = factor(mode_id, levels = c("mode_60", "mode_42"), labels = c("60 Hz", "42 Hz")))
modal_receivers <- read_exp("a19_low_frequency_cwt", "receiver_modal_excess_profiles.csv") %>%
  filter(room_id == 1, inference_eligible) %>%
  mutate(target = factor(mode_id, levels = c("mode_60", "mode_42"), labels = c("60 Hz", "42 Hz")))
lock_csv(screen, "fig4_frequency_screen.csv")
lock_csv(modal, "fig4_modal_targets.csv")
lock_csv(modal_receivers, "fig4_modal_receivers.csv")

p4a <- ggplot(screen, aes(x = centre_frequency_hz, y = mean_local_excess_db,
                          colour = room, fill = room, linetype = room)) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = COL["grey"]) +
  geom_vline(xintercept = c(42.1117, 59.7353), linewidth = 0.4, linetype = "dashed", colour = "#555555") +
  geom_ribbon(aes(ymin = percentile_low, ymax = percentile_high), alpha = 0.12, colour = NA) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = c("Room 1" = COL["blue"], "Room 2" = COL["orange"])) +
  scale_fill_manual(values = c("Room 1" = COL["blue"], "Room 2" = COL["orange"])) +
  scale_linetype_manual(values = c("Room 1" = "solid", "Room 2" = "22")) +
  scale_x_continuous(limits = c(35, 110), breaks = c(40, 60, 80, 100), expand = expansion(mult = c(0, 0))) +
  labs(x = "CWT centre frequency (Hz)", y = "Local persistence excess (dB)") +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(legend.position = "inside", legend.position.inside = c(0.97, 0.97),
        legend.justification = c(1, 1)) +
  guides(fill = "none", colour = guide_legend(override.aes = list(fill = NA, alpha = 1)))

p4b <- ggplot() +
  geom_vline(xintercept = 0, linewidth = 0.4, colour = COL["grey"]) +
  geom_point(data = modal_receivers, aes(x = modal_excess_db, y = target),
             position = position_jitter(height = 0.07, width = 0), shape = 21,
             fill = "white", colour = COL["grey"], size = 1.7, stroke = 0.45) +
  geom_errorbar(data = modal, aes(xmin = bca_low, xmax = bca_high, y = target),
                orientation = "y", width = 0, linewidth = 0.8, colour = COL["blue"]) +
  geom_point(data = modal, aes(x = estimate_modal_excess_db, y = target),
             shape = 21, fill = COL["blue"], colour = "black", stroke = 0.5, size = 3) +
  scale_x_continuous(limits = c(-0.65, 1.20), breaks = c(-0.5, 0, 0.5, 1.0), expand = expansion(mult = c(0, 0))) +
  labs(x = "Persistence excess (dB)", y = NULL) +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(axis.ticks.y = element_blank())

p4 <- (p4a | p4b) + plot_layout(widths = c(1, 1)) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
# Panel b jitters the receiver points vertically; the jitter is drawn at render
# time, so the seed is set here to make the rendered figure reproducible.
set.seed(31082026L)
save_figure(p4, "fig4_low_frequency_boundary.png", "double_column", 82)

# ----------------------------------------------------------------------------
# Figure 5: measured target gap and prospective product scenarios
# ----------------------------------------------------------------------------
target_summary <- read_exp("a12_design_target_gap", "condition_target_gap_summary.csv") %>%
  filter(metric == "T20", target_id == "design_target") %>%
  mutate(
    room = factor(room_id, levels = c(1, 2), labels = c("Room 1", "Room 2")),
    condition = factor(
      condition_id,
      levels = c("open_no_carpet", "closed_no_carpet", "open_carpet", "closed_carpet"),
      labels = c("Open\nno carpet", "Closed\nno carpet", "Open\ncarpet", "Closed\ncarpet")
    )
  )
scenario_all <- read_exp("a18_product_design_envelope", "product_envelope_predictions.csv")
scenario <- scenario_all %>%
  filter(panel_id %in% c("p6_18_round", "workbook_6_18_assumption")) %>%
  transmute(
    centre_frequency_hz,
    profile = if_else(panel_id == "p6_18_round", "Closest-certified 6/18", "Design coefficients"),
    t20_s = predicted_t20_s
  )
baseline <- scenario_all %>%
  filter(panel_id == "p6_18_round") %>%
  distinct(centre_frequency_hz, measured_t20_s) %>%
  transmute(centre_frequency_hz, profile = "Measured baseline", t20_s = measured_t20_s)
scenario_plot <- bind_rows(baseline, scenario) %>%
  mutate(profile = factor(profile, levels = c(
    "Measured baseline",
    "Design coefficients",
    "Closest-certified 6/18"
  )))
lock_csv(target_summary, "fig5_measured_target_gap.csv")
lock_csv(scenario_plot, "fig5_design_scenarios.csv")

p5a <- ggplot(target_summary, aes(x = condition, y = equal_band_mean_s, colour = room,
                                  linetype = room, shape = room, group = room)) +
  geom_hline(yintercept = 0.4, linewidth = 0.55, linetype = "dashed", colour = COL["vermillion"]) +
  geom_line(linewidth = 0.65) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c("Room 1" = COL["blue"], "Room 2" = COL["orange"])) +
  scale_linetype_manual(values = c("Room 1" = "solid", "Room 2" = "22")) +
  scale_shape_manual(values = c("Room 1" = 16, "Room 2" = 17)) +
  scale_y_continuous(limits = c(0.38, 0.80), breaks = c(0.4, 0.5, 0.6, 0.7, 0.8), expand = expansion(mult = c(0, 0))) +
  labs(x = "Measured furnishing state", y = expression("Equal-band mean " * italic(T)[20] * " (s)")) +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(legend.position = "top", legend.direction = "horizontal",
        legend.justification = "left", axis.text.x = element_text(size = 8))

p5b <- ggplot(scenario_plot, aes(x = centre_frequency_hz, y = t20_s, colour = profile, linetype = profile, shape = profile)) +
  geom_hline(yintercept = 0.4, linewidth = 0.55, linetype = "dashed", colour = COL["vermillion"]) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 2.1, stroke = 0.5) +
  scale_colour_manual(values = c(
    "Measured baseline" = COL["black"],
    "Design coefficients" = COL["grey"],
    "Closest-certified 6/18" = COL["blue"]
  )) +
  scale_linetype_manual(values = c(
    "Measured baseline" = "solid",
    "Design coefficients" = "22",
    "Closest-certified 6/18" = "solid"
  )) +
  scale_shape_manual(values = c(
    "Measured baseline" = 16,
    "Design coefficients" = 1,
    "Closest-certified 6/18" = 17
  )) +
  scale_x_log10(breaks = c(125, 250, 500, 1000, 2000, 4000),
                labels = c("125", "250", "500", "1k", "2k", "4k"),
                 limits = c(125, 4000), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(0.30, 0.70), breaks = c(0.3, 0.4, 0.5, 0.6, 0.7), expand = expansion(mult = c(0, 0))) +
  labs(x = "Octave-band centre frequency (Hz)", y = expression(italic(T)[20] * " (s)")) +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(legend.position = "top", legend.justification = "left",
        legend.direction = "vertical", legend.text = element_text(size = 8))

p5 <- (p5a | p5b) + plot_layout(widths = c(1, 1)) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
save_figure(p5, "fig5_target_gap.png", "double_column", 88)

# ----------------------------------------------------------------------------
# Main Table 1
# ----------------------------------------------------------------------------
endpoint <- read_exp("a08_acoustic_convergence", "global_endpoint_factorial_estimates.csv") %>%
  filter(effect_family == "curtain", metric != "T20")
robust <- read_exp("a15_robustness", "robustness_design_based_estimates.csv") %>%
  filter(effect_family == "curtain")
mixed <- read_exp("a15_robustness", "mixed_model_curtain_sensitivity.csv")

table1 <- bind_rows(
  tibble(group = "Primary factorial estimates", analysis = c("Curtain, strict T20", "Curtain, geometric reduction", "Carpet, strict T20", "Curtain x carpet interaction"),
         unit = c("s", "%", "s", "s"),
         estimate = c(primary$estimate, primary_pct$percent_reduction,
                      global %>% filter(scale == "difference", effect_family == "carpet") %>% pull(estimate),
                      global %>% filter(scale == "difference", effect_family == "interaction") %>% pull(estimate)),
         low = c(primary$bca_low, primary_pct$percent_reduction_bca_low,
                 global %>% filter(scale == "difference", effect_family == "carpet") %>% pull(bca_low),
                 global %>% filter(scale == "difference", effect_family == "interaction") %>% pull(bca_low)),
         high = c(primary$bca_high, primary_pct$percent_reduction_bca_high,
                  global %>% filter(scale == "difference", effect_family == "carpet") %>% pull(bca_high),
                  global %>% filter(scale == "difference", effect_family == "interaction") %>% pull(bca_high)),
         support = c("134/140 complete profiles", "134/140 complete profiles", "134/140 complete profiles", "134/140 complete profiles")),
  endpoint %>% transmute(
    group = "Convergent endpoints",
    analysis = recode(metric, T30 = "Curtain, strict T30", EDT = "Curtain, strict EDT",
                      C50 = "Curtain, C50", C80 = "Curtain, C80"),
    unit, estimate, low = bca_low, high = bca_high,
    support = recode(metric, T30 = "103 complete profiles", EDT = "115 complete profiles",
                     C50 = "140 complete profiles", C80 = "140 complete profiles")
  ),
  robust %>% filter(metric != "strict_T20_complete_four") %>% transmute(
    group = "Sensitivity analyses",
    analysis = recode(metric,
      strict_T20_15_receiver_complete_profile = "Fifteen fully complete receivers",
      strict_T20_500_8000 = "500 Hz-8 kHz",
      strict_T20_exclude_125 = "Excluding 125 Hz",
      strict_T20_pairwise_available = "Pairwise-available strict T20",
      strict_T30_complete_four = "Strict T30 replication",
      unrestricted_T20_complete_four = "Unrestricted finite T20"
    ),
    unit = "s", estimate, low = bca_low, high = bca_high,
    support = "Whole-receiver bootstrap"
  ),
  mixed %>% transmute(group = "Sensitivity analyses", analysis = "Mixed receiver-block model",
                      unit = "s", estimate, low = ci_low, high = ci_high,
                      support = "Model-based 95% CI")
)
lock_csv(table1, "table1_global_estimates.csv")

table1_lines <- c(
  "\\begin{tabular}{@{}L{0.17\\textwidth}L{0.25\\textwidth}C{0.06\\textwidth}R{0.10\\textwidth}C{0.18\\textwidth}L{0.18\\textwidth}@{}}",
  "\\toprule",
  "Family & Analysis & Unit & Estimate & 95\\% CI & Support \\\\ ",
  "\\midrule"
)
last_group <- ""
for (i in seq_len(nrow(table1))) {
  g <- if (table1$group[i] == last_group) "" else table1$group[i]
  last_group <- table1$group[i]
  digits <- if (table1$unit[i] == "%") 1 else 3
  interval <- paste0(fmt(table1$low[i], digits), " to ", fmt(table1$high[i], digits))
  table1_lines <- c(table1_lines, paste(
    tex_escape(g), tex_acoustic_label(table1$analysis[i]), tex_escape(table1$unit[i]),
    fmt(table1$estimate[i], digits), interval, tex_escape(table1$support[i]),
    sep = " & ") |> paste0(" \\\\"))
}
table1_lines <- c(table1_lines, "\\bottomrule", "\\end{tabular}")
writeLines(table1_lines, file.path(TABLE_DIR, "table1_global_estimates.tex"), useBytes = TRUE)

# ----------------------------------------------------------------------------
# Supplementary tables
# ----------------------------------------------------------------------------

# S1: case record and source-condition grid
source_grid <- tribble(
  ~room, ~condition, ~S1, ~S2,
  "Room 1", "Curtain open, no carpet", "10 receivers", "10 receivers",
  "Room 1", "Curtain closed, no carpet", "10 receivers", "Not measured",
  "Room 1", "Curtain open, carpet laid", "10 receivers", "Not measured",
  "Room 1", "Curtain closed, carpet laid", "10 receivers", "Not measured",
  "Room 2", "Curtain open, no carpet", "10 receivers", "10 receivers",
  "Room 2", "Curtain closed, no carpet", "10 receivers", "10 receivers",
  "Room 2", "Curtain open, carpet laid", "10 receivers", "Not measured",
  "Room 2", "Curtain closed, carpet laid", "10 receivers", "10 receivers"
)
s1_rows <- list(
  c("\\multicolumn{4}{l}{\\textbf{Case record}}"),
  c("Room 1", "Exact DXF footprint", "48.476 m$^2$", "139.176 m$^3$"),
  c("Room 1", "Maximum dimensions", "8.15 $\\times$ 6.00 m", "Height 2.87 m"),
  c("Room 2", "Documented dimensions", "8.00 $\\times$ 6.00 m", "Height 2.87 m"),
  c("Campaign", "Measurement date", "10 November 2020", "Sequential room-wide states"),
  c("Grid", "Files and cells", "130 IR files", "120 receiver cells"),
  c("Repeats", "Room 2 listening position", "Five cells with three takes", "Averaged within receiver"),
  c("\\addlinespace\\multicolumn{4}{l}{\\textbf{Observed source--condition grid}}"),
  c("\\textbf{Room}", "\\textbf{Furnishing state}", "\\textbf{S1}", "\\textbf{S2}")
)
s1_rows <- c(s1_rows, lapply(seq_len(nrow(source_grid)), function(i) {
  c(source_grid$room[i], tex_escape(source_grid$condition[i]), source_grid$S1[i], source_grid$S2[i])
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS1_design.tex"),
  "Case record and observed source--condition grid. S1 is the only source present in all eight room--condition combinations.",
  "tab:s1-design", "L{0.13\\linewidth}L{0.34\\linewidth}L{0.20\\linewidth}L{0.20\\linewidth}",
  c("Case", "Item", "Value", "Additional detail"), s1_rows)

# S2: processing and validity
validity_rows <- list(
  c("$T_{30}$", "$-5$ to $-35$ dB", "45 dB", "$R^2 \\geq 0.95$; $BT>16$", "Sensitivity endpoint"),
  c("$T_{20}$", "$-5$ to $-25$ dB", "35 dB", "$R^2 \\geq 0.95$; $BT>16$", "Primary, 125~Hz--8~kHz"),
  c("EDT", "$-0.1$ to $-10.1$ dB", "20 dB", "$R^2 \\geq 0.95$; $BT>16$", "Convergence check"),
  c("$C_{50}$", "Early/late split at 50 ms", "--", "Finite; Lundeby intersection after 80 ms", "Convergence check"),
  c("$C_{80}$", "Early/late split at 80 ms", "--", "Finite; Lundeby intersection after 80 ms", "Convergence check")
)
take_validity <- read_frozen_csv("ir_band_metrics.csv") %>%
  filter(band_scheme == "octave", centre_frequency_hz %in% c(125, 250, 500, 1000, 2000, 4000, 8000)) %>%
  group_by(centre_frequency_hz) %>%
  summarise(n = n(), n_valid = sum(t20_strict_valid), percent = 100 * mean(t20_strict_valid), .groups = "drop")
coverage_row <- function(frequency) {
  r <- take_validity %>% filter(centre_frequency_hz == frequency)
  c("$T_{20}$ coverage", paste0(fmt(frequency, 0), " Hz"), "--",
    paste0(r$n_valid, "/", r$n, " takes (", fmt(r$percent, 1), "\\%)"), "Primary")
}
high_coverage <- take_validity %>% filter(centre_frequency_hz >= 500)
validity_rows <- c(validity_rows, list(
  c("\\addlinespace\\multicolumn{5}{l}{\\textbf{Strict-$T_{20}$ take-level coverage and endpoint role}}"),
  coverage_row(125),
  coverage_row(250),
  c("$T_{20}$ coverage", "500 Hz--8 kHz", "--",
    paste0(sum(high_coverage$n_valid), "/", sum(high_coverage$n), " takes (100.0\\%)"), "Primary"),
  c("$T_{20}$", "31.5 and 63 Hz", "--", "Outside the primary frequency range", "Diagnostic only")
))
validation_summary <- read_frozen_csv("validation_summary.csv") %>%
  filter(validation_scope == "core_125_8000_hz", target_family == "dirac_export_average")
validity_rows <- c(validity_rows, list(c("\\addlinespace\\multicolumn{5}{l}{\\textbf{Independent comparison with the DIRAC average export}}")))
validity_rows <- c(validity_rows, lapply(seq_len(nrow(validation_summary)), function(i) {
  r <- validation_summary[i, ]
  c(tex_metric(r$metric), paste0(r$n_pass, "/", r$n, " checks"), "--",
    paste0("MAE ", fmt(r$mae, ifelse(r$metric %in% c("C50", "C80"), 3, ifelse(r$metric == "T30", 5, 4))), ifelse(r$metric %in% c("C50", "C80"), " dB", " s")),
    paste0("Maximum |difference| ", fmt(r$max_abs_difference, 3)))
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS2_processing.tex"),
  "Endpoint definitions, strict-validity rules and core-band validation. Finite values that failed a strict rule were retained for comparison but excluded from analyses requiring strict validity.",
  "tab:s2-processing", "C{0.10\\linewidth}L{0.20\\linewidth}C{0.11\\linewidth}L{0.28\\linewidth}L{0.20\\linewidth}",
  c("Endpoint", "Decay or split range", "Peak SNR", "Additional strict rule / validation", "Role"), validity_rows)

# S3: repeatability and endpoint agreement
repeatability <- read_exp("a04_technical_repeatability", "technical_repeatability_summary.csv")
agreement <- read_exp("a03_endpoint_convergence", "t20_t30_agreement_overall.csv")
direction_summary <- read_exp("a08_acoustic_convergence", "endpoint_direction_summary.csv") %>%
  filter(effect_family == "curtain")
s3_rows <- lapply(seq_len(nrow(repeatability)), function(i) {
  r <- repeatability[i, ]
  c(tex_metric(r$metric), as.character(r$n_cell_bands), fmt(r$median_within_sd, 4), fmt(r$p95_within_sd, 4),
    fmt(r$max_within_range, 4), ifelse(is.na(r$median_cv), "--", fmt(r$median_cv, 3)))
})
s3_rows <- c(s3_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{$T_{20}$--$T_{30}$ agreement across matched receiver cells meeting the strict criteria}}"),
  c("\\textbf{Comparison}", "\\textbf{$n$ receiver cells}", "\\textbf{Mean $T_{20}$ (s)}", "\\textbf{Mean $T_{30}$ (s)}", "\\textbf{Mean difference (s)}", "\\textbf{Pearson $r$}"),
  c("$T_{20}$ vs $T_{30}$", as.character(agreement$n_cells), fmt(agreement$mean_t20_s, 3), fmt(agreement$mean_t30_s, 3),
    fmt(agreement$mean_difference_s, 3), fmt(agreement$pearson_r, 3))
))
s3_rows <- c(s3_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Curtain-effect direction relative to strict $T_{20}$ across room--band cells}}"),
  c("\\textbf{Endpoint}", "\\textbf{$n$ room--bands}", "\\textbf{Same direction}", "\\textbf{Concordance (\\%)}", "--", "--")
))
s3_rows <- c(s3_rows, lapply(seq_len(nrow(direction_summary)), function(i) {
  r <- direction_summary[i, ]
  c(tex_metric(r$metric), as.character(r$n_room_bands), as.character(r$n_same_direction),
    fmt(100 * r$direction_concordance, 1), "--", "--")
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS3_repeatability.tex"),
  "Technical repeatability in the five three-take Room 2 listening-position cells, agreement between strict $T_{20}$ and $T_{30}$, and room--band direction concordance for the curtain effect. SD and ranges retain each endpoint's native unit (seconds for decay, decibels for clarity).",
  "tab:s3-repeatability", "L{0.14\\linewidth}C{0.10\\linewidth}R{0.15\\linewidth}R{0.15\\linewidth}R{0.15\\linewidth}R{0.12\\linewidth}", c("Metric", "$n$ cell-bands", "Median within SD", "95th percentile SD", "Maximum range", "Median CV"), s3_rows)

# S4: global and room-specific factorial estimates
global_diff <- global %>% filter(scale == "difference")
s4_rows <- lapply(seq_len(nrow(global_diff)), function(i) {
  r <- global_diff[i, ]
  c("Pooled fixed cases", tex_escape(str_to_title(r$effect_family)), fmt(r$estimate, 4), fmt(r$bca_low, 4), fmt(r$bca_high, 4), fmt_p(r$sign_symmetry_p_two_sided))
})
room_full <- read_exp("a07_room_consistency", "room_specific_factorial_estimates.csv")
s4_rows <- c(s4_rows, lapply(seq_len(nrow(room_full)), function(i) {
  r <- room_full[i, ]
  c(paste0("Room ", r$room_id), tex_escape(str_to_title(r$effect_family)), fmt(r$estimate, 4), fmt(r$bca_low, 4), fmt(r$bca_high, 4), fmt_p(r$sign_symmetry_p_two_sided))
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS4_factorial.tex"),
  "Factorial estimates for strict $T_{20}$ profiles complete across all four furnishing states. Positive main effects denote shorter decay when the furnishing is present; a positive interaction denotes a larger curtain reduction when carpet is laid. Intervals are bias-corrected and accelerated (BCa) 95\\% confidence intervals. Sign-symmetry values are two-sided sensitivity $p$ values, not randomisation-test $p$ values.",
  "tab:s4-factorial", "L{0.22\\linewidth}L{0.18\\linewidth}R{0.13\\linewidth}R{0.13\\linewidth}R{0.13\\linewidth}R{0.10\\linewidth}", c("Scope", "Effect", "Estimate (s)", "BCa low", "BCa high", "$p$"), s4_rows)

# S5: complete third-octave profile
s5_rows <- lapply(seq_len(nrow(read_exp("a17_third_octave_structure", "third_octave_factorial_estimates.csv"))), function(i) {
  r <- read_exp("a17_third_octave_structure", "third_octave_factorial_estimates.csv")[i, ]
  c(tex_escape(str_to_title(r$effect_family)), fmt(r$centre_frequency_hz, 0), as.character(r$n_complete_four_receivers),
    tex_escape(ifelse(r$support_class == "full_support", "Full", "Incomplete")),
    fmt(r$estimate, 4), fmt(r$bca_low, 4), fmt(r$bca_high, 4), fmt_p(r$sign_symmetry_p_two_sided), fmt_p(r$q_bh_within_21_band_profile))
})
room_concordance <- read_exp("a17_third_octave_structure", "fixed_room_profile_concordance.csv") %>%
  filter(effect_family == "carpet")
s5_rows <- c(s5_rows, list(
  c("\\addlinespace\\multicolumn{9}{l}{\\textbf{Room-specific direction agreement across the 13 fully supported bands}}"),
  c("Carpet", "630--10000", "20", "Full", paste0(room_concordance$n_same_direction, "/", room_concordance$n_full_support_bands, " bands"),
    "--", "--", "--", "--")
))
write_longtable(file.path(SI_TABLE_DIR, "tableS5_frequency.tex"),
  "One-third-octave factorial profiles for strict $T_{20}$. Positive main effects denote shorter decay when the furnishing is present; a positive interaction denotes a larger curtain reduction when carpet is laid. Intervals are bias-corrected and accelerated (BCa) 95\\% confidence intervals. Benjamini--Hochberg $q$ values were calculated within each 21-band effect profile. The 100--500~Hz rows have incomplete receiver support and are diagnostic.",
  "tab:s5-frequency", "L{0.14\\linewidth}C{0.08\\linewidth}C{0.10\\linewidth}C{0.10\\linewidth}R{0.10\\linewidth}R{0.10\\linewidth}R{0.10\\linewidth}R{0.10\\linewidth}R{0.10\\linewidth}", c("Effect", "Hz", "$n$ receivers", "Support", "Estimate", "BCa low", "BCa high", "$p$", "$q$"), s5_rows)

# S6: receiver, source, spatial-spread and listening-position results
s6_rows <- lapply(seq_len(nrow(receiver_est)), function(i) {
  r <- receiver_est[i, ]
  c(paste0("Room ", r$room_id), paste0("R", r$receiver_id), tex_escape(r$role), as.character(r$n_complete_bands), fmt(r$curtain_effect, 4), "--")
})
s6_rows <- c(s6_rows, list(c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Room 2 source-position sensitivity without carpet}}")))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(source_est)), function(i) {
  r <- source_est[i, ]
  c("Room 2", r$source, "Source-position sensitivity", "7", fmt(r$estimate, 4), "All seven bands positive")
}))
source_band <- read_exp("a09_source_sensitivity", "room2_source_condition_band_summary.csv") %>%
  filter(metric == "T20", contrast_id == "curtain_no_carpet")
s6_rows <- c(s6_rows, list(c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Room 2 source-by-curtain comparison by band}}")))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(source_band)), function(i) {
  r <- source_band[i, ]
  c("Room 2", paste0(fmt(r$centre_frequency_hz, 0), " Hz"), "S1 / S2 curtain reduction",
    paste0(r$n_stable_receivers, " receivers"), paste0(fmt(r$effect_s1, 4), " / ", fmt(r$effect_s2, 4)),
    paste0("BH $q$ = ", fmt_p(r$p_bh_seven_bands)))
}))
spread <- read_exp("a10_spatial_uniformity", "spatial_spread_factorial_estimates.csv") %>%
  filter(metric == "T20", effect_family == "curtain")
s6_rows <- c(s6_rows, list(c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Spatial spread and designated listening positions}}")))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(spread)), function(i) {
  r <- spread[i, ]
  c("Both rooms", str_to_upper(r$statistic), "Change in spatial spread", as.character(r$minimum_room_bands), fmt(r$estimate, 4), paste0("BCa ", fmt(r$bca_low, 4), " to ", fmt(r$bca_high, 4)))
}))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(listen_est)), function(i) {
  r <- listen_est[i, ]
  c(as.character(r$room), tex_escape(r$position), "Listening-position comparison", "7", fmt(r$estimate, 4), "--")
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS6_generality.tex"),
  "Receiver-level curtain reductions, the limited Room 2 source-position sensitivity comparison and spatial boundaries. Positive estimates denote shorter decay with the curtain closed. Receiver estimates average each position's available core bands complete across all four furnishing states; the listening positions are R4 in Room 1 and R5 in Room 2. Spatial-spread intervals are bias-corrected and accelerated (BCa) 95\\% confidence intervals.",
  "tab:s6-generality", "L{0.13\\linewidth}L{0.15\\linewidth}L{0.25\\linewidth}C{0.08\\linewidth}R{0.13\\linewidth}L{0.18\\linewidth}",
  c("Room", "Position / statistic", "Role", "Bands / $n$", "Estimate (s)", "Qualification"), s6_rows)

# S7: sensitivity analyses
s7_data <- robust %>% filter(metric != "strict_T20_complete_four")
s7_rows <- lapply(seq_len(nrow(s7_data)), function(i) {
  r <- s7_data[i, ]
  variant <- recode(r$metric,
    strict_T20_15_receiver_complete_profile = "Fifteen fully complete receivers",
    strict_T20_500_8000 = "500 Hz-8 kHz",
    strict_T20_complete_four = "Primary strict T20, all four states",
    strict_T20_exclude_125 = "Excluding 125 Hz",
    strict_T20_pairwise_available = "Pairwise-available strict T20",
    strict_T30_complete_four = "Strict T30 replication",
    unrestricted_T20_complete_four = "Unrestricted finite T20"
  )
  c(tex_acoustic_label(variant), tex_escape(str_to_title(r$effect_family)), fmt(r$estimate, 4), fmt(r$bca_low, 4), fmt(r$bca_high, 4),
    ifelse(is.na(r$delta_from_primary_curtain), "--", fmt(r$delta_from_primary_curtain, 4)))
})
s7_rows <- c(s7_rows, list(c("Mixed receiver-block model", "Curtain", fmt(mixed$estimate, 4), fmt(mixed$ci_low, 4), fmt(mixed$ci_high, 4), fmt(mixed$delta_from_primary, 4))))
leave_receiver <- read_exp("a15_robustness", "leave_one_receiver_influence.csv")
leave_band <- read_exp("a15_robustness", "leave_one_band_influence.csv")
s7_rows <- c(s7_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Leave-one-unit influence}}"),
  c("Leave-one-receiver range (20 omissions)", "Curtain",
    paste0(fmt(min(leave_receiver$estimate), 4), " to ", fmt(max(leave_receiver$estimate), 4)), "--", "--",
    paste0("max $|\\Delta|$ ", fmt(max(leave_receiver$absolute_delta), 4))),
  c("Leave-one-band range (7 omissions)", "Curtain",
    paste0(fmt(min(leave_band$estimate), 4), " to ", fmt(max(leave_band$estimate), 4)), "--", "--",
    paste0("max $|\\Delta|$ ", fmt(max(leave_band$absolute_delta), 4)))
))
write_longtable(file.path(SI_TABLE_DIR, "tableS7_robustness.tex"),
  "Curtain sensitivity analyses. Bias-corrected and accelerated (BCa) 95\\% confidence intervals use 1,000 whole-receiver bootstrap resamples except for the model-based interval. Estimates and changes are in seconds; $\\Delta$ is relative to the primary 0.09746-s estimate. The final two rows report estimate ranges rather than confidence intervals.",
  "tab:s7-robustness", "L{0.32\\linewidth}L{0.14\\linewidth}R{0.12\\linewidth}R{0.12\\linewidth}R{0.12\\linewidth}R{0.12\\linewidth}", c("Variant", "Effect", "Estimate", "95\\% low", "95\\% high", "$\\Delta$"), s7_rows)

# S8: modal context and fixed-room CWT targets
nearest <- read_exp("a13_low_frequency_modal", "nearest_modes_to_legacy_frequencies.csv")
critical <- read_exp("a13_low_frequency_modal", "critical_frequency_mode_counts.csv")
low_decay <- read_exp("a13_low_frequency_modal", "low_frequency_decay_excess.csv") %>%
  filter(room_id == 1, metric == "T20")
s8_rows <- lapply(seq_len(nrow(nearest)), function(i) {
  r <- nearest[i, ]
  c("Bounding-box mode", fmt(r$target_frequency_hz, 2), fmt(r$nearest_mode_hz, 2), tex_escape(r$mode_type),
    paste0("(", r$p, ",", r$q, ",", r$r, ")"), "--")
})
s8_rows <- c(s8_rows, lapply(seq_len(nrow(modal)), function(i) {
  r <- modal[i, ]
  c("Room 1 CWT target", fmt(r$centre_frequency_hz, 0), fmt(r$geometry_frequency_hz, 2), "Target vs flanks",
    as.character(r$n_receivers), paste0(fmt(r$estimate_modal_excess_db, 3), " dB; Holm p = ", fmt(r$p_holm_two_targets, 3)))
}))
s8_rows <- c(s8_rows, list(c(
  "Room 1 diagnostic decay", "31.5/63", "--", "$T_{20}$ low/mid ratio",
  paste0(nrow(low_decay), " states"), paste0(fmt(min(low_decay$low_to_mid_ratio), 2), " to ", fmt(max(low_decay$low_to_mid_ratio), 2))
)))
s8_rows <- c(s8_rows, lapply(seq_len(nrow(critical)), function(i) {
  r <- critical[i, ]
  criterion <- recode(r$scenario,
    design_target = "0.4-s design target",
    legacy_recommended = "EBU/ITU 0.28-s nominal",
    measured_open_no_carpet_s1_midband = "Measured 500/1000-Hz mean $T_{30}$, Room 1 open/no-carpet S1"
  )
  c("Critical-frequency scenario", fmt(r$critical_frequency_hz, 2), "--", criterion,
    as.character(r$n_modes_at_or_below), paste0(r$n_axial_modes_at_or_below, " axial modes"))
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS8_modal.tex"),
  "Room 1 bounding-box modal context, critical-frequency scenarios and CWT target results. Target-versus-flank excess compares each target with its symmetric 6--10-Hz flanks. Bounding calculations ignore chamfers, furnishings and impedance and do not identify a unique eigenmode.",
  "tab:s8-modal", "L{0.17\\linewidth}C{0.12\\linewidth}C{0.12\\linewidth}L{0.22\\linewidth}C{0.09\\linewidth}L{0.17\\linewidth}", c("Layer", "Target / $f_c$ (Hz)", "Nearest mode (Hz)", "Type / criterion", "Index / count", "Result"), s8_rows)

# S9: receiver CWT target profiles and furnishing contrasts
s9_rows <- lapply(seq_len(nrow(modal_receivers)), function(i) {
  r <- modal_receivers[i, ]
  role <- recode(r$receiver_role,
    spatial_receiver = "Other receiver",
    listening_position = "Listening position",
    idle_source_position = "Inactive source position"
  )
  c(paste0("R", r$receiver_id), tex_escape(role), fmt(r$centre_frequency_hz, 0), as.character(r$n_conditions), fmt(r$modal_excess_db, 4), "--", "--", "--")
})
furnishing <- read_exp("a19_low_frequency_cwt", "room1_furnishing_modal_effects.csv")
s9_rows <- c(s9_rows, list(c("\\addlinespace\\multicolumn{8}{l}{\\textbf{Room 1 furnishing contrasts in persistence excess}}")))
s9_rows <- c(s9_rows, lapply(seq_len(nrow(furnishing)), function(i) {
  r <- furnishing[i, ]
  c("Both", tex_escape(str_to_title(r$effect_family)), fmt(r$centre_frequency_hz, 0), as.character(r$n_blocks), fmt(r$estimate, 4), fmt(r$bca_low, 4), fmt(r$bca_high, 4), fmt_p(r$q_bh_six_furnishing_tests))
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS9_cwt.tex"),
  "Room 1 receiver-level CWT target-versus-flank profiles and furnishing contrasts. Positive values indicate greater persistence at the target than at its symmetric 6--10-Hz flanks. Furnishing intervals are bias-corrected and accelerated (BCa) 95\\% confidence intervals. The six furnishing tests form one Benjamini--Hochberg family; no $q$ value is below 0.05.",
  "tab:s9-cwt", "C{0.09\\linewidth}L{0.18\\linewidth}C{0.06\\linewidth}C{0.06\\linewidth}R{0.13\\linewidth}R{0.13\\linewidth}R{0.13\\linewidth}R{0.08\\linewidth}", c("Receiver", "Role / effect", "Hz", "$n$", "Estimate (dB)", "BCa low", "BCa high", "$q$"), s9_rows)

# S10: measured target gaps
s10_data <- target_summary
s10_rows <- lapply(seq_len(nrow(s10_data)), function(i) {
  r <- s10_data[i, ]
  c(as.character(r$room), tex_escape(as.character(r$condition)), as.character(r$n_bands), fmt(r$equal_band_mean_s, 4),
    fmt(r$equal_band_residual_gap_s, 4), fmt(100 * r$mean_band_proportion_above_target, 0))
})
write_longtable(file.path(SI_TABLE_DIR, "tableS10_target_gap.tex"),
  "Measured strict-$T_{20}$ gap to the 0.4-s design target. Equal-band means cover 125~Hz--8~kHz and every receiver-level band summary remained above the target.",
  "tab:s10-target", "L{0.13\\linewidth}L{0.27\\linewidth}C{0.08\\linewidth}R{0.15\\linewidth}R{0.15\\linewidth}R{0.15\\linewidth}", c("Room", "Furnishing state", "Bands", "Mean $T_{20}$ (s)", "Residual gap (s)", "Bands above (\\%)"), s10_rows)

# S11: closest-certified scenario
certified <- scenario_all %>% filter(panel_id == "p6_18_round")
s11_rows <- lapply(seq_len(nrow(certified)), function(i) {
  r <- certified[i, ]
  c(fmt(r$centre_frequency_hz, 0), fmt(r$measured_t20_s, 3), fmt(r$ceiling_alpha, 2), fmt(r$eq50q_alpha, 2),
    fmt(r$total_net_added_absorption_m2, 2), fmt(r$predicted_t20_s, 3), ifelse(r$at_or_below_0_4_s, "Yes", "No"))
})
write_longtable(file.path(SI_TABLE_DIR, "tableS11_scenario.tex"),
  "Replacement-adjusted Sabine-equivalent scenario based on the measured Room 1 closed-curtain/carpet profile, the proposed EQ50Q wall area and the closest documented ceiling coefficients for 6-mm holes at 18-mm centres. $\\alpha$ is the absorption coefficient and $A$ is equivalent absorption area. The certified cavity differs from the proposed assembly, and values are deterministic scenarios rather than installed performance.",
  "tab:s11-scenario", "C{0.08\\linewidth}R{0.14\\linewidth}R{0.12\\linewidth}R{0.12\\linewidth}R{0.15\\linewidth}R{0.14\\linewidth}C{0.11\\linewidth}", c("Hz", "Measured $T_{20}$ (s)", "Ceiling $\\alpha$", "EQ50Q $\\alpha$", "Net added $A$ (m$^2$)", "Scenario $T_{20}$ (s)", "$\\leq 0.4$ s"), s11_rows)

# S12: certified-library envelope and the design's own full-band calculation
ranking <- read_exp("a18_product_design_envelope", "product_envelope_ranking.csv")
s12_rows <- lapply(seq_len(nrow(ranking)), function(i) {
  r <- ranking[i, ]
  panel <- recode(r$panel_id,
    p12_25_square = "12/25 large square holes (A06-11-15)",
    p8_15_20_var_glass = "8/15/20 variable pattern with glass wool (ZQT170002-4)",
    p12_25_var_glass = "12/25 variable pattern with glass wool (ZQT170151)",
    irregular_rectangle = "Irregular rectangular holes (A19-037)",
    p8_18_square = "8/18 square holes (A19-036)",
    p8_12_50_round = "8/12/50 regular round holes (A15-239)",
    p6_18_round = "6/18 round holes, closest certificate (A15-238)",
    p8_18_round = "8/18 round holes (SX-10038)",
    p12_25_round = "12/25 large round holes (A15-241)",
    designpanel_micro_square = "3-by-3-mm micro-square holes (A09-02-24)",
    p8_15_20_irregular_round = "8/15/20 irregular round holes (A15-240)",
    workbook_6_18_assumption = "Design 6/18 assumption"
  )
  relationship <- recode(r$relationship_to_proposal,
    alternative_certified_system = "Alternative certified system",
    exact_perforation_nearest_documented_cavity = "Closest certified same-perforation system",
    exact_design_workbook_assumption = "Design assumption as proposed"
  )
  c(as.character(r$design_envelope_rank), tex_escape(panel), tex_escape(relationship),
    as.character(r$n_bands_at_or_below_0_4_s), fmt(r$maximum_predicted_t20_s, 3), fmt(r$mean_predicted_t20_s, 3))
})
audit <- read_exp("a18_product_design_envelope", "legacy_workbook_claim_audit.csv")
s12_rows <- c(s12_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{The design's own full-band calculation}}"),
  c("\\textbf{Rank}", "\\textbf{Band}", "\\textbf{Calculation}", "\\textbf{Target status}", "\\textbf{Residual absorption (m$^2$)}", "")
))
s12_rows <- c(s12_rows, lapply(seq_len(nrow(audit)), function(i) {
  r <- audit[i, ]
  c("--", paste0(fmt(r$centre_frequency_hz, 0), " Hz"), "Design arithmetic",
    ifelse(r$target_not_closed_under_workbook, "Miss", "Reach"), fmt(r$reconstructed_remaining_absorption_m2, 3), "")
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS12_product_audit.tex"),
  "Certified-product envelope and the design's own full-band calculation. Alternative systems are same-area sensitivity scenarios, not recommendations. With the design's own coefficients, the gross absorption leaves positive residual absorption in four of six bands.",
  "tab:s12-product", "C{0.07\\linewidth}L{0.20\\linewidth}L{0.27\\linewidth}C{0.14\\linewidth}R{0.14\\linewidth}R{0.12\\linewidth}", c("Rank", "Certified system", "Relationship", "Bands $\\leq 0.4$ s", "Maximum scenario $T_{20}$ (s)", "Mean scenario $T_{20}$ (s)"), s12_rows, landscape = TRUE)

# ----------------------------------------------------------------------------
# S13-S15: sensitivity analyses specified after the primary analyses (A21-A23)
# ----------------------------------------------------------------------------
common_band <- read_exp("a21_post_review_broadband_checks", "common_band_receiver_estimates.csv")
lock_csv(common_band, "tableS13_common_band.csv")
s13_rows <- lapply(seq_len(nrow(common_band)), function(i) {
  r <- common_band[i, ]
  c(paste("Room", r$room_id), paste0("R", r$receiver_id), fmt(r$curtain_effect_common_s, 4),
    fmt(r$curtain_effect_locked_s, 4), as.character(r$n_bands_locked), fmt(r$shift_s, 4))
})
write_longtable(file.path(SI_TABLE_DIR, "tableS13_common_band.tex"),
  "Common-band receiver curtain estimates. The common-band column averages the five octave bands with complete strict support at every receiver (500~Hz--8~kHz); the all-band column repeats the five-to-seven-band estimates of Fig.~3b for comparison.",
  "tab:s13-common", "L{0.13\\linewidth}C{0.11\\linewidth}R{0.20\\linewidth}R{0.20\\linewidth}C{0.12\\linewidth}R{0.14\\linewidth}", c("Room", "Receiver", "Common-band estimate (s)", "All-band estimate (s)", "All-band $n$", "Shift (s)"), s13_rows)

cwt_grid <- read_frozen_csv("cwt_parameter_grid.csv")
cwt_bandwidth <- read_frozen_csv("effective_bandwidth.csv")
lock_csv(cwt_grid, "tableS14_cwt_grid.csv")
lock_csv(cwt_bandwidth, "tableS14_bandwidth.csv")
variant_label <- function(x) {
  recode(x,
    "base" = "Base analysis",
    "early_window_0.10-0.40_s" = "Early window 0.10--0.40 s",
    "early_window_0.20-0.50_s" = "Early window 0.20--0.50 s",
    "late_window_0.55-1.15_s" = "Late window 0.55--1.15 s",
    "late_window_0.75-1.35_s" = "Late window 0.75--1.35 s",
    "flanks_5-9_Hz" = "Flanks 5--9 Hz",
    "flanks_7-11_Hz" = "Flanks 7--11 Hz",
    "target_plus_minus_2_Hz" = "Target $\\pm$2 Hz",
    "wavelet_cmor0.5-1.0" = "Wavelet cmor0.5-1.0",
    "wavelet_cmor2.0-1.0" = "Wavelet cmor2.0-1.0",
    "wavelet_cmor4.0-1.0" = "Wavelet cmor4.0-1.0"
  )
}
s14_rows <- list()
for (target in c(42, 60)) {
  s14_rows <- c(s14_rows, list(c(paste0(
    "\\addlinespace\\multicolumn{5}{l}{\\textbf{", target, " Hz target}}"
  ))))
  block <- cwt_grid %>% filter(target_hz == target)
  s14_rows <- c(s14_rows, lapply(seq_len(nrow(block)), function(i) {
    r <- block[i, ]
    c(variant_label(r$variant), fmt(r$mean_excess_db, 3), fmt(r$receiver_sd_db, 3),
      paste0(r$n_positive_receivers, "/", r$n_receivers), "--")
  }))
}
s14_rows <- c(s14_rows, list(c(
  "\\addlinespace\\multicolumn{5}{l}{\\textbf{Effective spectral resolution (power-spectrum FWHM)}}"
)))
s14_rows <- c(s14_rows, lapply(seq_len(nrow(cwt_bandwidth)), function(i) {
  r <- cwt_bandwidth[i, ]
  c(paste0(r$wavelet, " at ", fmt(r$frequency_hz, 0), " Hz"),
    "--", "--", "--",
    paste0(fmt(r$analytic_power_fwhm_hz, 1), " Hz (measured ", fmt(r$measured_power_fwhm_hz, 0), " Hz)"))
}))
write_longtable(file.path(SI_TABLE_DIR, "tableS14_cwt_grid.tex"),
  "Wavelet parameter sensitivity for the Room 1 persistence targets (source S1, ten receivers). Each variant changes one analysis choice relative to the base configuration (cmor1.0-1.0, early window 0.15--0.45 s, late window 0.65--1.25 s, target $\\pm$1 Hz, flanks 6--10 Hz). Values are receiver-profile means of the target-versus-flank excess; the base rows reproduce the frozen primary estimates.",
  "tab:s14-cwt-grid", "L{0.31\\linewidth}R{0.14\\linewidth}R{0.12\\linewidth}C{0.12\\linewidth}L{0.25\\linewidth}", c("Variant", "Mean excess (dB)", "Receiver SD (dB)", "Positive receivers", "Resolution"), s14_rows)

envelope <- read_exp("a23_post_review_scenario_envelope", "scenario_envelope.csv")
lock_csv(envelope, "tableS15_envelope.csv")
envelope_label <- c(
  sabine_base = "Sabine, base coefficients",
  sabine_conservative = "Sabine, conservative ($-$10\\%, $+$0.02)",
  sabine_optimistic = "Sabine, optimistic ($+$10\\%, $-$0.02)",
  eyring_base = "Eyring, base coefficients",
  eyring_conservative = "Eyring, conservative ($-$10\\%, $+$0.02)",
  eyring_optimistic = "Eyring, optimistic ($+$10\\%, $-$0.02)"
)
envelope_wide <- envelope %>%
  select(variant_id, centre_frequency_hz, scenario_t20_s) %>%
  pivot_wider(names_from = variant_id, values_from = scenario_t20_s)
measured_by_band <- envelope %>% distinct(centre_frequency_hz, measured_t20_s)
s15_rows <- lapply(seq_len(nrow(envelope_wide)), function(i) {
  r <- envelope_wide[i, ]
  m <- measured_by_band$measured_t20_s[measured_by_band$centre_frequency_hz == r$centre_frequency_hz]
  c(fmt(r$centre_frequency_hz, 0), fmt(m, 3),
    fmt(r$sabine_base, 3), fmt(r$sabine_conservative, 3), fmt(r$sabine_optimistic, 3),
    fmt(r$eyring_base, 3), fmt(r$eyring_conservative, 3), fmt(r$eyring_optimistic, 3))
})
band_counts <- envelope %>%
  group_by(variant_id) %>%
  summarise(n_met = sum(at_or_below_0_4_s), .groups = "drop")
count_of <- function(id) as.character(band_counts$n_met[band_counts$variant_id == id])
s15_rows <- c(s15_rows, list(c(
  "\\addlinespace Bands $\\leq$ 0.4 s", "0",
  count_of("sabine_base"), count_of("sabine_conservative"), count_of("sabine_optimistic"),
  count_of("eyring_base"), count_of("eyring_conservative"), count_of("eyring_optimistic")
)))
write_longtable(file.path(SI_TABLE_DIR, "tableS15_envelope.tex"),
  "Deterministic uncertainty envelope for the closest-certified replacement-adjusted scenario. Conservative and optimistic variants scale the certified and EQ50Q coefficients by $\\mp$10\\% and shift the existing-surface coefficients by $\\pm$0.02 in opposing directions. The Eyring form uses the 175.30-m$^2$ interior surface from the Room 1 model. All values are prospective calculations, not installed performance.",
  "tab:s15-envelope", "L{0.12\\linewidth}R{0.10\\linewidth}R{0.11\\linewidth}R{0.12\\linewidth}R{0.11\\linewidth}R{0.11\\linewidth}R{0.12\\linewidth}R{0.11\\linewidth}", c("Hz", "Measured (s)", "Sabine base", "Sabine cons.", "Sabine opt.", "Eyring base", "Eyring cons.", "Eyring opt."), s15_rows)

# ----------------------------------------------------------------------------
# S16: source-receiver distances from direct-sound arrival delays (A24)
# ----------------------------------------------------------------------------
distances <- read_frozen_csv("source_receiver_distances.csv")
lock_csv(distances, "tableS16_distances.csv")
dist_wide <- distances %>%
  pivot_wider(id_cols = c(room_id, receiver_id), names_from = source_id,
              values_from = c(mean_m, min_m, max_m))
dist_cell <- function(mean_v, min_v, max_v) {
  if (is.na(mean_v)) return("--")
  if ((max_v - min_v) < 0.005) return(fmt(mean_v, 2))
  paste0(fmt(mean_v, 2), " (", fmt(min_v, 2), "--", fmt(max_v, 2), ")")
}
s16_rows <- lapply(seq_len(nrow(dist_wide)), function(i) {
  r <- dist_wide[i, ]
  role <- case_when(
    r$room_id == 1 & r$receiver_id == 4 ~ "Listening position",
    r$room_id == 2 & r$receiver_id == 5 ~ "Listening position",
    r$room_id == 1 & r$receiver_id == 10 ~ "Idle source position",
    r$room_id == 2 & r$receiver_id == 4 ~ "Idle source position",
    TRUE ~ "Spatial receiver"
  )
  c(paste("Room", r$room_id), paste0("R", r$receiver_id), tex_escape(role),
    dist_cell(r$mean_m_S1, r$min_m_S1, r$max_m_S1),
    dist_cell(r$mean_m_S2, r$min_m_S2, r$max_m_S2))
})
write_longtable(file.path(SI_TABLE_DIR, "tableS16_distances.tex"),
  "Source--receiver distances derived from direct-sound arrival delays. Values are means over takes and furnishing states, with ranges across states in parentheses where they differ by 0.005~m or more. The idle-source receivers stood at the inactive source position, so their location changed with the active source.",
  "tab:s16-distances", "L{0.12\\linewidth}C{0.10\\linewidth}L{0.22\\linewidth}R{0.23\\linewidth}R{0.23\\linewidth}", c("Room", "Receiver", "Role", "Distance to S1 (m)", "Distance to S2 (m)"), s16_rows)

# ----------------------------------------------------------------------------
# Build record
# ----------------------------------------------------------------------------
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "sessionInfo.txt"), useBytes = TRUE)
message("All P31 figures and tables built.")
