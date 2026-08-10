# =============================================================================
# run_all.R — reproduce every figure and table of the listening-room case study
# from the Mendeley Data workbook.
#
#   Rscript run_all.R
#
# Reads   data/Listening_room_acoustic_decay_and_clarity_data.xlsx
# Writes  output/<analysis id>/   one folder per analysis module
#         output/figures/         Figures 1-5 (PNG, 600 dpi)
#         output/tables/          Table 1 and Supplementary Tables S1-S16
#         output/data_lock/       the plotted values behind every display item
# =============================================================================

options(warn = 1)
start_time <- Sys.time()

if (!file.exists(file.path("code", "run_all.R")) && !file.exists("run_all.R")) {
  stop("Run this script from the repository root: Rscript run_all.R", call. = FALSE)
}

# Analysis modules in dependency order. A21 reads A10's and A19's outputs and
# A23 reads A18's, so those four come last.
MODULES <- c(
  "a03_endpoint_convergence",
  "a04_technical_repeatability",
  "a05_factorial_t20",
  "a07_room_consistency",
  "a08_acoustic_convergence",
  "a09_source_sensitivity",
  "a10_spatial_uniformity",
  "a11_listening_position",
  "a12_design_target_gap",
  "a13_low_frequency_modal",
  "a15_robustness",
  "a17_third_octave_structure",
  "a18_product_design_envelope",
  "a19_low_frequency_cwt",
  "a21_post_review_broadband_checks",
  "a23_post_review_scenario_envelope"
)

for (module in MODULES) {
  message("\n=== ", module, " ===")
  module_start <- Sys.time()
  # Each module runs in its own environment so no object leaks between them.
  sys.source(file.path("code", paste0(module, ".R")), envir = new.env(parent = globalenv()))
  message("    done in ", round(as.numeric(difftime(Sys.time(), module_start, units = "secs")), 1), " s")
}

message("\n=== display items ===")
sys.source(file.path("code", "build_displays.R"), envir = new.env(parent = globalenv()))

message("\nComplete in ",
        round(as.numeric(difftime(Sys.time(), start_time, units = "mins")), 1),
        " min. Figures, tables and locked display values are in output/.")
