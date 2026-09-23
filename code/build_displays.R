source(file.path("code", "style.R"))
source(file.path("code", "load_data.R"))

# This pipeline consumes only frozen processed data and completed Stage-2 outputs.
# It does not refit any inferential model.

# ----------------------------------------------------------------------------
# Figure 1: room photographs and relative measurement layout
# ----------------------------------------------------------------------------
PHOTO_DIR <- file.path("data", "room_photographs")
fig1_photographs <- tibble(
  panel = c("a", "b", "c"),
  view = c("Room 1 front", "Room 1 rear", "Room 2 front"),
  file = c("room1_front.jpg", "room1_rear.jpg", "room2_front.jpg")
) %>%
  mutate(sha256 = vapply(file.path(PHOTO_DIR, file), digest::digest, character(1),
                         algo = "sha256", serialize = FALSE, file = TRUE, USE.NAMES = FALSE))
lock_csv(fig1_photographs, "fig1_photographs.csv")

photo_panel <- function(file) {
  img <- jpeg::readJPEG(file.path(PHOTO_DIR, file))
  aspect <- dim(img)[2] / dim(img)[1]
  ggplot() +
    annotation_raster(img, xmin = 0, xmax = aspect, ymin = 0, ymax = 1, interpolate = TRUE) +
    coord_fixed(xlim = c(0, aspect), ylim = c(0, 1), expand = FALSE) +
    theme_schematic() +
    theme(plot.margin = margin(1, 3, 1, 3))
}

# Receiver positions are triangulated in the source plane from the mean
# direct-sound distances to the two sources. The idle-source receiver stood at
# the inactive source, so its two distances give the source separation.
room_roles <- tibble(room_id = c(1, 2), idle_receiver_id = c(10, 4), listening_receiver_id = c(4, 5))
fig1_distances <- read_frozen_csv("source_receiver_distances.csv") %>%
  inner_join(room_roles, by = "room_id")
source_separation <- fig1_distances %>%
  filter(receiver_id == idle_receiver_id) %>%
  group_by(room_id) %>%
  summarise(source_separation_m = mean(mean_m), .groups = "drop")
fig1_layout <- fig1_distances %>%
  filter(receiver_id != idle_receiver_id) %>%
  select(room_id, receiver_id, listening_receiver_id, source_id, mean_m) %>%
  pivot_wider(names_from = source_id, values_from = mean_m, names_prefix = "distance_m_") %>%
  left_join(source_separation, by = "room_id") %>%
  mutate(
    along_source_line_m = (distance_m_S1^2 - distance_m_S2^2) / (2 * source_separation_m),
    depth_squared = distance_m_S1^2 - (along_source_line_m + source_separation_m / 2)^2,
    role = if_else(receiver_id == listening_receiver_id, "Listening position", "Other receiver")
  )
stopifnot(nrow(fig1_layout) == 18, all(fig1_layout$depth_squared > 0))
fig1_layout <- fig1_layout %>%
  mutate(depth_from_source_line_m = sqrt(depth_squared)) %>%
  select(room_id, receiver_id, role, distance_m_S1, distance_m_S2, source_separation_m,
         along_source_line_m, depth_from_source_line_m) %>%
  arrange(room_id, receiver_id)
lock_csv(fig1_layout, "fig1_layout.csv")

layout_panel <- function(room, colour) {
  z <- filter(fig1_layout, room_id == room)
  s <- z$source_separation_m[1]
  sources <- tibble(x = 0, y = c(-s / 2, s / 2), label = c("S1", "S2"))
  ggplot(z, aes(x = -depth_from_source_line_m, y = along_source_line_m)) +
    annotate("segment", x = 0, xend = 0, y = -s / 2, yend = s / 2,
             linetype = "22", linewidth = 0.35, colour = COL["grey"]) +
    annotate("text", x = 0.3, y = 0, label = paste0(fmt(s, 1), " m"), angle = 90,
             size = 8 / .pt, family = BASE_FAMILY) +
    geom_point(aes(shape = role), colour = colour, size = 1.8, stroke = 0.55) +
    scale_shape_manual(values = c("Listening position" = 16, "Other receiver" = 1), guide = "none") +
    geom_text(aes(label = paste0("R", receiver_id)), nudge_y = 0.3, size = 8 / .pt, family = BASE_FAMILY) +
    geom_point(data = sources, aes(x = x, y = y), inherit.aes = FALSE, shape = 17, size = 2.2) +
    geom_text(data = sources, aes(x = x + 0.2, y = y, label = label), inherit.aes = FALSE,
              hjust = 0, size = 8 / .pt, family = BASE_FAMILY) +
    annotate("segment", x = -5.6, xend = -4.6, y = -2.55, yend = -2.55, linewidth = 0.5) +
    annotate("text", x = -5.1, y = -2.7, label = "1 m", vjust = 1, size = 8 / .pt, family = BASE_FAMILY) +
    coord_fixed(xlim = c(-5.75, 0.95), ylim = c(-3.05, 2.45), expand = FALSE, clip = "off") +
    theme_schematic()
}

p1_photos <- wrap_plots(lapply(fig1_photographs$file, photo_panel), nrow = 1)
p1_layout <- layout_panel(1, COL["blue"]) | layout_panel(2, COL["orange"])
p1 <- (p1_photos / p1_layout) + plot_layout(heights = c(0.36, 1)) + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
save_figure(p1, "fig1_rooms_layout.png", "double_column", 112)

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
# Figure 5: four-state spectra and prospective product scenarios
# ----------------------------------------------------------------------------
STATE_LEVELS <- c("open_no_carpet", "closed_no_carpet", "open_carpet", "closed_carpet")
STATE_LABELS <- c("Open, no carpet", "Closed, no carpet", "Open, carpet", "Closed, carpet")
CORE_BANDS <- c(125, 250, 500, 1000, 2000, 4000, 8000)

# Each plotted band uses the receivers with a strict T20 value in all four
# states, so the curves compare the same receivers.
spectra_profiles <- read_frozen_csv("receiver_band_metrics.csv") %>%
  filter(source_id == "S1", band_scheme == "octave", centre_frequency_hz %in% CORE_BANDS) %>%
  select(room_id, receiver_id, centre_frequency_hz, condition_id, t20_s_strict) %>%
  pivot_wider(names_from = condition_id, values_from = t20_s_strict) %>%
  filter(if_all(all_of(STATE_LEVELS), is.finite))
stopifnot(nrow(spectra_profiles) == 134)
four_state_spectra <- spectra_profiles %>%
  pivot_longer(all_of(STATE_LEVELS), names_to = "condition_id", values_to = "t20_s") %>%
  group_by(room_id, centre_frequency_hz, condition_id) %>%
  summarise(n_receivers = n(), mean_t20_s = mean(t20_s), .groups = "drop") %>%
  mutate(state = factor(condition_id, levels = STATE_LEVELS, labels = STATE_LABELS)) %>%
  arrange(room_id, centre_frequency_hz, state)
lock_csv(four_state_spectra, "fig5_four_state_spectra.csv")

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
lock_csv(scenario_plot, "fig5_design_scenarios.csv")

band_axis <- scale_x_log10(breaks = CORE_BANDS, labels = c("125", "250", "500", "1k", "2k", "4k", "8k"),
                           limits = c(125, 8000), expand = expansion(mult = c(0.05, 0.05)))
t20_axis <- scale_y_continuous(limits = c(0.3, 1.0), breaks = c(0.4, 0.6, 0.8, 1.0),
                               expand = expansion(mult = c(0, 0)))
target_line <- geom_hline(yintercept = 0.4, linewidth = 0.4, linetype = "dotted", colour = COL["grey"])

spectrum_panel <- function(room) {
  ggplot(filter(four_state_spectra, room_id == room),
         aes(x = centre_frequency_hz, y = mean_t20_s, colour = state, shape = state,
             linetype = state, group = state)) +
    target_line +
    geom_line(linewidth = 0.5) +
    geom_point(size = 1.5, stroke = 0.5) +
    scale_colour_manual(values = setNames(COL[c("green", "purple", "green", "purple")], STATE_LABELS)) +
    scale_shape_manual(values = setNames(c(16, 17, 1, 2), STATE_LABELS)) +
    scale_linetype_manual(values = setNames(c("solid", "solid", "22", "22"), STATE_LABELS)) +
    band_axis + t20_axis +
    labs(x = if (room == 2) "Octave-band centre frequency (Hz)" else NULL,
         y = if (room == 1) expression(italic(T)[20] * " (s)") else NULL) +
    theme_pub(base_size = 8, axis_title_size = 9) +
    theme(aspect.ratio = 1, legend.position = "top", legend.key.width = unit(7, "mm"))
}

p5c <- ggplot(scenario_plot, aes(x = centre_frequency_hz, y = t20_s, colour = profile,
                                 linetype = profile, shape = profile)) +
  target_line +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.6, stroke = 0.5) +
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
  band_axis + t20_axis +
  labs(x = NULL, y = NULL) +
  theme_pub(base_size = 8, axis_title_size = 9) +
  theme(aspect.ratio = 1, legend.position = "top", legend.key.width = unit(7, "mm"))

p5 <- (spectrum_panel(1) | spectrum_panel(2) | p5c) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", theme = theme(legend.position = "top", legend.justification = "left",
                                                   legend.box = "vertical", legend.box.just = "left",
                                                   legend.spacing.y = unit(0.5, "mm"))) &
  theme(plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY))
save_figure(p5, "fig5_spectra_target_gap.png", "double_column", 76)

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
      strict_T20_500_8000 = "500 Hz--8 kHz",
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
  interval <- paste0(fmt_tex(table1$low[i], digits), " to ", fmt_tex(table1$high[i], digits))
  table1_lines <- c(table1_lines, paste(
    tex_escape(g), tex_acoustic_label(table1$analysis[i]), tex_escape(table1$unit[i]),
    fmt_tex(table1$estimate[i], digits), interval, tex_escape(table1$support[i]),
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
  c("Room 2", "Nominal dimensions", "8.00 $\\times$ 6.00 m", "Height 2.87 m"),
  c("Campaign", "Measurement date", "10 November 2020", "Sequential room-wide states"),
  c("Grid", "Files and cells", "130 IR files", "120 receiver cells"),
  c("Repeats", "Room 2 listening position", "Five cells with three takes", "Averaged within receiver"),
  c("\\addlinespace\\multicolumn{4}{l}{\\textbf{Observed source--condition grid}}"),
  c("\\textbf{Room}", "\\textbf{Furnishing state}", "\\textbf{S1}", "\\textbf{S2}")
)
s1_rows <- c(s1_rows, lapply(seq_len(nrow(source_grid)), function(i) {
  c(source_grid$room[i], tex_escape(source_grid$condition[i]), source_grid$S1[i], source_grid$S2[i])
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_case_record.tex"), s1_rows)

# S3: processing and validity
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
  c("$T_{20}$ coverage", paste0(fmt_tex(frequency, 0), " Hz"), "--",
    paste0(r$n_valid, "/", r$n, " takes (", fmt_tex(r$percent, 1), "\\%)"), "Primary")
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
    paste0("MAE ", fmt_tex(r$mae, ifelse(r$metric %in% c("C50", "C80"), 3, ifelse(r$metric == "T30", 5, 4))), ifelse(r$metric %in% c("C50", "C80"), " dB", " s")),
    paste0("Maximum |difference| ", fmt_tex(r$max_abs_difference, 3)))
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_processing.tex"), validity_rows)

# S4: repeatability and endpoint agreement
repeatability <- read_exp("a04_technical_repeatability", "technical_repeatability_summary.csv")
agreement <- read_exp("a03_endpoint_convergence", "t20_t30_agreement_overall.csv")
direction_summary <- read_exp("a08_acoustic_convergence", "endpoint_direction_summary.csv") %>%
  filter(effect_family == "curtain")
s3_rows <- lapply(seq_len(nrow(repeatability)), function(i) {
  r <- repeatability[i, ]
  c(tex_metric(r$metric), as.character(r$n_cell_bands), fmt_tex(r$median_within_sd, 4), fmt_tex(r$p95_within_sd, 4),
    fmt_tex(r$max_within_range, 4), ifelse(is.na(r$median_cv), "--", fmt_tex(r$median_cv, 3)))
})
s3_rows <- c(s3_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{$T_{20}$--$T_{30}$ agreement across matched receiver cells meeting the strict criteria}}"),
  c("\\textbf{Comparison}", "\\textbf{$n$ receiver cells}", "\\textbf{Mean $T_{20}$ (s)}", "\\textbf{Mean $T_{30}$ (s)}", "\\textbf{Mean difference (s)}", "\\textbf{Pearson $r$}"),
  c("$T_{20}$ vs $T_{30}$", as.character(agreement$n_cells), fmt_tex(agreement$mean_t20_s, 3), fmt_tex(agreement$mean_t30_s, 3),
    fmt_tex(agreement$mean_difference_s, 3), fmt_tex(agreement$pearson_r, 3))
))
s3_rows <- c(s3_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Curtain-effect direction relative to strict $T_{20}$ across room--band cells}}"),
  c("\\textbf{Endpoint}", "\\textbf{$n$ room--bands}", "\\textbf{Same direction}", "\\textbf{Concordance (\\%)}", "--", "--")
))
s3_rows <- c(s3_rows, lapply(seq_len(nrow(direction_summary)), function(i) {
  r <- direction_summary[i, ]
  c(tex_metric(r$metric), as.character(r$n_room_bands), as.character(r$n_same_direction),
    fmt_tex(100 * r$direction_concordance, 1), "--", "--")
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_repeatability.tex"), s3_rows)

# S5: global and room-specific factorial estimates
global_diff <- global %>% filter(scale == "difference")
s4_rows <- lapply(seq_len(nrow(global_diff)), function(i) {
  r <- global_diff[i, ]
  c("Pooled fixed cases", tex_escape(str_to_title(r$effect_family)), fmt_tex(r$estimate, 4), fmt_tex(r$bca_low, 4), fmt_tex(r$bca_high, 4), fmt_p(r$sign_symmetry_p_two_sided))
})
room_full <- read_exp("a07_room_consistency", "room_specific_factorial_estimates.csv")
s4_rows <- c(s4_rows, lapply(seq_len(nrow(room_full)), function(i) {
  r <- room_full[i, ]
  c(paste0("Room ", r$room_id), tex_escape(str_to_title(r$effect_family)), fmt_tex(r$estimate, 4), fmt_tex(r$bca_low, 4), fmt_tex(r$bca_high, 4), fmt_p(r$sign_symmetry_p_two_sided))
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_factorial.tex"), s4_rows)

# S6: complete third-octave profile
s5_rows <- lapply(seq_len(nrow(read_exp("a17_third_octave_structure", "third_octave_factorial_estimates.csv"))), function(i) {
  r <- read_exp("a17_third_octave_structure", "third_octave_factorial_estimates.csv")[i, ]
  c(tex_escape(str_to_title(r$effect_family)), fmt_tex(r$centre_frequency_hz, 0), as.character(r$n_complete_four_receivers),
    tex_escape(ifelse(r$support_class == "full_support", "Full", "Incomplete")),
    fmt_tex(r$estimate, 4), fmt_tex(r$bca_low, 4), fmt_tex(r$bca_high, 4), fmt_p(r$sign_symmetry_p_two_sided), fmt_p(r$q_bh_within_21_band_profile))
})
room_concordance <- read_exp("a17_third_octave_structure", "fixed_room_profile_concordance.csv") %>%
  filter(effect_family == "carpet")
s5_rows <- c(s5_rows, list(
  c("\\addlinespace\\multicolumn{9}{l}{\\textbf{Room-specific direction agreement across the 13 fully supported bands}}"),
  c("Carpet", "630--10000", "20", "Full", paste0(room_concordance$n_same_direction, "/", room_concordance$n_full_support_bands, " bands"),
    "--", "--", "--", "--")
))
write_si_rows(file.path(SI_TABLE_DIR, "si_third_octave.tex"), s5_rows)

# S8: receiver, source, spatial-spread and listening-position results
s6_rows <- lapply(seq_len(nrow(receiver_est)), function(i) {
  r <- receiver_est[i, ]
  c(paste0("Room ", r$room_id), paste0("R", r$receiver_id), tex_escape(r$role), as.character(r$n_complete_bands), fmt_tex(r$curtain_effect, 4), "--")
})
s6_rows <- c(s6_rows, list(c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Room 2 source-position sensitivity without carpet}}")))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(source_est)), function(i) {
  r <- source_est[i, ]
  c("Room 2", r$source, "Source-position sensitivity", "7", fmt_tex(r$estimate, 4), "All seven bands positive")
}))
source_band <- read_exp("a09_source_sensitivity", "room2_source_condition_band_summary.csv") %>%
  filter(metric == "T20", contrast_id == "curtain_no_carpet")
s6_rows <- c(s6_rows, list(c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Room 2 source-by-curtain comparison by band}}")))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(source_band)), function(i) {
  r <- source_band[i, ]
  c("Room 2", paste0(fmt_tex(r$centre_frequency_hz, 0), " Hz"), "S1 / S2 curtain reduction",
    paste0(r$n_stable_receivers, " receivers"), paste0(fmt_tex(r$effect_s1, 4), " / ", fmt_tex(r$effect_s2, 4)),
    paste0("BH $q$ = ", fmt_p(r$p_bh_seven_bands)))
}))
spread <- read_exp("a10_spatial_uniformity", "spatial_spread_factorial_estimates.csv") %>%
  filter(metric == "T20", effect_family == "curtain")
s6_rows <- c(s6_rows, list(c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Spatial spread and designated listening positions}}")))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(spread)), function(i) {
  r <- spread[i, ]
  c("Both rooms", str_to_upper(r$statistic), "Change in spatial spread", as.character(r$minimum_room_bands), fmt_tex(r$estimate, 4), paste0("BCa ", fmt_tex(r$bca_low, 4), " to ", fmt_tex(r$bca_high, 4)))
}))
s6_rows <- c(s6_rows, lapply(seq_len(nrow(listen_est)), function(i) {
  r <- listen_est[i, ]
  c(as.character(r$room), tex_escape(r$position), "Listening-position comparison", "7", fmt_tex(r$estimate, 4), "--")
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_generality.tex"), s6_rows)

# S9: sensitivity analyses
s7_data <- robust %>% filter(metric != "strict_T20_complete_four")
s7_rows <- lapply(seq_len(nrow(s7_data)), function(i) {
  r <- s7_data[i, ]
  variant <- recode(r$metric,
    strict_T20_15_receiver_complete_profile = "Fifteen fully complete receivers",
    strict_T20_500_8000 = "500 Hz--8 kHz",
    strict_T20_complete_four = "Primary strict T20, all four states",
    strict_T20_exclude_125 = "Excluding 125 Hz",
    strict_T20_pairwise_available = "Pairwise-available strict T20",
    strict_T30_complete_four = "Strict T30 replication",
    unrestricted_T20_complete_four = "Unrestricted finite T20"
  )
  c(tex_acoustic_label(variant), tex_escape(str_to_title(r$effect_family)), fmt_tex(r$estimate, 4), fmt_tex(r$bca_low, 4), fmt_tex(r$bca_high, 4),
    ifelse(is.na(r$delta_from_primary_curtain), "--", fmt_tex(r$delta_from_primary_curtain, 4)))
})
s7_rows <- c(s7_rows, list(c("Mixed receiver-block model", "Curtain", fmt_tex(mixed$estimate, 4), fmt_tex(mixed$ci_low, 4), fmt_tex(mixed$ci_high, 4), fmt_tex(mixed$delta_from_primary, 4))))
leave_receiver <- read_exp("a15_robustness", "leave_one_receiver_influence.csv")
leave_band <- read_exp("a15_robustness", "leave_one_band_influence.csv")
s7_rows <- c(s7_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{Leave-one-unit influence}}"),
  c("Leave-one-receiver range (20 omissions)", "Curtain",
    paste0(fmt_tex(min(leave_receiver$estimate), 4), " to ", fmt_tex(max(leave_receiver$estimate), 4)), "--", "--",
    paste0("max $|\\Delta|$ ", fmt_tex(max(leave_receiver$absolute_delta), 4))),
  c("Leave-one-band range (7 omissions)", "Curtain",
    paste0(fmt_tex(min(leave_band$estimate), 4), " to ", fmt_tex(max(leave_band$estimate), 4)), "--", "--",
    paste0("max $|\\Delta|$ ", fmt_tex(max(leave_band$absolute_delta), 4)))
))
write_si_rows(file.path(SI_TABLE_DIR, "si_robustness.tex"), s7_rows)

# S10: modal context and fixed-room CWT targets
nearest <- read_exp("a13_low_frequency_modal", "nearest_modes_to_legacy_frequencies.csv")
critical <- read_exp("a13_low_frequency_modal", "critical_frequency_mode_counts.csv")
low_decay <- read_exp("a13_low_frequency_modal", "low_frequency_decay_excess.csv") %>%
  filter(room_id == 1, metric == "T20")
s8_rows <- lapply(seq_len(nrow(nearest)), function(i) {
  r <- nearest[i, ]
  c("Bounding-box mode", fmt_tex(r$target_frequency_hz, 2), fmt_tex(r$nearest_mode_hz, 2), tex_escape(r$mode_type),
    paste0("(", r$p, ",", r$q, ",", r$r, ")"), "--")
})
s8_rows <- c(s8_rows, lapply(seq_len(nrow(modal)), function(i) {
  r <- modal[i, ]
  c("Room 1 CWT target", fmt_tex(r$centre_frequency_hz, 0), fmt_tex(r$geometry_frequency_hz, 2), "Target vs flanks",
    as.character(r$n_receivers), paste0(fmt_tex(r$estimate_modal_excess_db, 3), " dB; Holm p = ", fmt_tex(r$p_holm_two_targets, 3)))
}))
s8_rows <- c(s8_rows, list(c(
  "Room 1 diagnostic decay", "31.5/63", "--", "$T_{20}$ low/mid ratio",
  paste0(nrow(low_decay), " states"), paste0(fmt_tex(min(low_decay$low_to_mid_ratio), 2), " to ", fmt_tex(max(low_decay$low_to_mid_ratio), 2))
)))
s8_rows <- c(s8_rows, lapply(seq_len(nrow(critical)), function(i) {
  r <- critical[i, ]
  criterion <- recode(r$scenario,
    design_target = "0.4-s design target",
    legacy_recommended = "EBU/ITU 0.28-s nominal",
    measured_open_no_carpet_s1_midband = "Measured 500/1000-Hz mean $T_{30}$, Room 1 open/no-carpet S1"
  )
  c("Critical-frequency scenario", fmt_tex(r$critical_frequency_hz, 2), "--", criterion,
    as.character(r$n_modes_at_or_below), paste0(r$n_axial_modes_at_or_below, " axial modes"))
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_modal.tex"), s8_rows)

# S12: receiver CWT target profiles and furnishing contrasts
s9_rows <- lapply(seq_len(nrow(modal_receivers)), function(i) {
  r <- modal_receivers[i, ]
  role <- recode(r$receiver_role,
    spatial_receiver = "Other receiver",
    listening_position = "Listening position",
    idle_source_position = "Inactive source position"
  )
  c(paste0("R", r$receiver_id), tex_escape(role), fmt_tex(r$centre_frequency_hz, 0), as.character(r$n_conditions), fmt_tex(r$modal_excess_db, 4), "--", "--", "--")
})
furnishing <- read_exp("a19_low_frequency_cwt", "room1_furnishing_modal_effects.csv")
s9_rows <- c(s9_rows, list(c("\\addlinespace\\multicolumn{8}{l}{\\textbf{Room 1 furnishing contrasts in persistence excess}}")))
s9_rows <- c(s9_rows, lapply(seq_len(nrow(furnishing)), function(i) {
  r <- furnishing[i, ]
  c("Both", tex_escape(str_to_title(r$effect_family)), fmt_tex(r$centre_frequency_hz, 0), as.character(r$n_blocks), fmt_tex(r$estimate, 4), fmt_tex(r$bca_low, 4), fmt_tex(r$bca_high, 4), fmt_p(r$q_bh_six_furnishing_tests))
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_cwt_targets.tex"), s9_rows)

# S13: measured target gaps
target_summary <- read_exp("a12_design_target_gap", "condition_target_gap_summary.csv") %>%
  filter(metric == "T20", target_id == "design_target") %>%
  mutate(
    room = factor(room_id, levels = c(1, 2), labels = c("Room 1", "Room 2")),
    state = factor(condition_id, levels = STATE_LEVELS, labels = STATE_LABELS)
  ) %>%
  arrange(room, state)
lock_csv(target_summary, "si_target_gap.csv")
s10_data <- target_summary
s10_rows <- lapply(seq_len(nrow(s10_data)), function(i) {
  r <- s10_data[i, ]
  c(as.character(r$room), tex_escape(as.character(r$state)), as.character(r$n_bands), fmt_tex(r$equal_band_mean_s, 4),
    fmt_tex(r$equal_band_residual_gap_s, 4), fmt_tex(100 * r$mean_band_proportion_above_target, 0))
})
write_si_rows(file.path(SI_TABLE_DIR, "si_target_gap.tex"), s10_rows)

# S14: closest-certified scenario
certified <- scenario_all %>% filter(panel_id == "p6_18_round")
s11_rows <- lapply(seq_len(nrow(certified)), function(i) {
  r <- certified[i, ]
  c(fmt_tex(r$centre_frequency_hz, 0), fmt_tex(r$measured_t20_s, 3), fmt_tex(r$ceiling_alpha, 2), fmt_tex(r$eq50q_alpha, 2),
    fmt_tex(r$total_net_added_absorption_m2, 2), fmt_tex(r$predicted_t20_s, 3), ifelse(r$at_or_below_0_4_s, "Yes", "No"))
})
write_si_rows(file.path(SI_TABLE_DIR, "si_scenario.tex"), s11_rows)

# S16: certified-library envelope and the design's own full-band calculation
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
    as.character(r$n_bands_at_or_below_0_4_s), fmt_tex(r$maximum_predicted_t20_s, 3), fmt_tex(r$mean_predicted_t20_s, 3))
})
audit <- read_exp("a18_product_design_envelope", "legacy_workbook_claim_audit.csv")
s12_rows <- c(s12_rows, list(
  c("\\addlinespace\\multicolumn{6}{l}{\\textbf{The design's own full-band calculation}}"),
  c("\\textbf{Rank}", "\\textbf{Band}", "\\textbf{Calculation}", "\\textbf{Target status}", "\\textbf{Residual absorption (m$^2$)}", "")
))
s12_rows <- c(s12_rows, lapply(seq_len(nrow(audit)), function(i) {
  r <- audit[i, ]
  c("--", paste0(fmt_tex(r$centre_frequency_hz, 0), " Hz"), "Design arithmetic",
    ifelse(r$target_not_closed_under_workbook, "Miss", "Reach"), fmt_tex(r$reconstructed_remaining_absorption_m2, 3), "")
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_product_audit.tex"), s12_rows)

# ----------------------------------------------------------------------------
# S7, S11 and S15: sensitivity analyses specified after the primary analyses (A21-A23)
# ----------------------------------------------------------------------------
common_band <- read_exp("a21_post_review_broadband_checks", "common_band_receiver_estimates.csv")
lock_csv(common_band, "si_common_band.csv")
s13_rows <- lapply(seq_len(nrow(common_band)), function(i) {
  r <- common_band[i, ]
  c(paste("Room", r$room_id), paste0("R", r$receiver_id), fmt_tex(r$curtain_effect_common_s, 4),
    fmt_tex(r$curtain_effect_locked_s, 4), as.character(r$n_bands_locked), fmt_tex(r$shift_s, 4))
})
write_si_rows(file.path(SI_TABLE_DIR, "si_common_band.tex"), s13_rows)

cwt_grid <- read_frozen_csv("cwt_parameter_grid.csv")
cwt_bandwidth <- read_frozen_csv("effective_bandwidth.csv")
lock_csv(cwt_grid, "si_cwt_grid.csv")
lock_csv(cwt_bandwidth, "si_cwt_bandwidth.csv")
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
    "wavelet_cmor0.5-1.0" = "Wavelet bandwidth $B$ = 0.5",
    "wavelet_cmor2.0-1.0" = "Wavelet bandwidth $B$ = 2.0",
    "wavelet_cmor4.0-1.0" = "Wavelet bandwidth $B$ = 4.0"
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
    c(variant_label(r$variant), fmt_tex(r$mean_excess_db, 3), fmt_tex(r$receiver_sd_db, 3),
      paste0(r$n_positive_receivers, "/", r$n_receivers), "--")
  }))
}
s14_rows <- c(s14_rows, list(c(
  "\\addlinespace\\multicolumn{5}{l}{\\textbf{Effective spectral resolution (power-spectrum FWHM)}}"
)))
s14_rows <- c(s14_rows, lapply(seq_len(nrow(cwt_bandwidth)), function(i) {
  r <- cwt_bandwidth[i, ]
  c(paste0("$B$ = ", fmt_tex(r$bandwidth_parameter_b, 1), " at ", fmt_tex(r$frequency_hz, 0), " Hz"),
    "--", "--", "--",
    paste0(fmt_tex(r$analytic_power_fwhm_hz, 1), " Hz (measured ", fmt_tex(r$measured_power_fwhm_hz, 0), " Hz)"))
}))
write_si_rows(file.path(SI_TABLE_DIR, "si_cwt_grid.tex"), s14_rows)

envelope <- read_exp("a23_post_review_scenario_envelope", "scenario_envelope.csv")
lock_csv(envelope, "si_scenario_envelope.csv")
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
  c(fmt_tex(r$centre_frequency_hz, 0), fmt_tex(m, 3),
    fmt_tex(r$sabine_base, 3), fmt_tex(r$sabine_conservative, 3), fmt_tex(r$sabine_optimistic, 3),
    fmt_tex(r$eyring_base, 3), fmt_tex(r$eyring_conservative, 3), fmt_tex(r$eyring_optimistic, 3))
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
write_si_rows(file.path(SI_TABLE_DIR, "si_scenario_envelope.tex"), s15_rows)

# ----------------------------------------------------------------------------
# S2: source-receiver distances from direct-sound arrival delays (A24)
# ----------------------------------------------------------------------------
distances <- read_frozen_csv("source_receiver_distances.csv")
lock_csv(distances, "si_distances.csv")
dist_wide <- distances %>%
  pivot_wider(id_cols = c(room_id, receiver_id), names_from = source_id,
              values_from = c(mean_m, min_m, max_m))
dist_cell <- function(mean_v, min_v, max_v) {
  if (is.na(mean_v)) return("--")
  if ((max_v - min_v) < 0.005) return(fmt_tex(mean_v, 2))
  paste0(fmt_tex(mean_v, 2), " (", fmt_tex(min_v, 2), "--", fmt_tex(max_v, 2), ")")
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
write_si_rows(file.path(SI_TABLE_DIR, "si_distances.tex"), s16_rows)

# ----------------------------------------------------------------------------
# Build record
# ----------------------------------------------------------------------------
writeLines(capture.output(sessionInfo()), file.path(OUT_DIR, "sessionInfo.txt"), useBytes = TRUE)
message("All P31 figures and tables built.")
