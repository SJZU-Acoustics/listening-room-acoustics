suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(tidyr)
})

# Single data entry: every table comes from the Mendeley workbook.
source(file.path("code", "load_data.R"))

CORE_OCTAVE_BANDS <- c(125, 250, 500, 1000, 2000, 4000, 8000)
DIAGNOSTIC_OCTAVE_BANDS <- c(31.5, 63)
BOOT_N <- 1000L
BOOT_SEED <- 31082026L

CONDITION_LEVELS <- c(
  "open_no_carpet",
  "closed_no_carpet",
  "open_carpet",
  "closed_carpet"
)

CONDITION_LABELS <- c(
  open_no_carpet = "Open, no carpet",
  open_carpet = "Open, carpet",
  closed_no_carpet = "Closed, no carpet",
  closed_carpet = "Closed, carpet"
)

P31_COLOURS <- c(
  open_no_carpet = "#5E6472",
  open_carpet = "#D08C60",
  closed_no_carpet = "#2A6F97",
  closed_carpet = "#2A9D8F"
)

# All analysis output goes under output/<analysis id>/.
OUTPUT_ROOT <- "output"

analysis_output_dir <- function(analysis) {
  path <- file.path(OUTPUT_ROOT, analysis)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

strict_primary_rows <- function(data) {
  data %>%
    filter(
      source_id == "S1",
      band_scheme == "octave",
      centre_frequency_hz %in% CORE_OCTAVE_BANDS
    ) %>%
    mutate(
      condition_id = factor(condition_id, levels = CONDITION_LEVELS),
      room_id = factor(room_id, levels = c(1, 2)),
      receiver_block = interaction(room_id, receiver_id, drop = TRUE)
    )
}

condition_factors <- function(data) {
  data %>%
    mutate(
      curtain = if_else(str_starts(as.character(condition_id), "closed"), "present", "absent"),
      carpet = if_else(str_ends(as.character(condition_id), "carpet") &
                         !str_ends(as.character(condition_id), "no_carpet"),
                       "present", "absent"),
      curtain = factor(curtain, levels = c("absent", "present")),
      carpet = factor(carpet, levels = c("absent", "present"))
    )
}

frequency_label <- function(x) {
  vapply(as.numeric(x), function(value) {
    if (value >= 1000) {
      paste0(format(value / 1000, trim = TRUE, scientific = FALSE), "k")
    } else {
      format(value, trim = TRUE, scientific = FALSE)
    }
  }, character(1))
}

theme_p31 <- function(base_size = 9) {
  theme_minimal(base_family = "Arial", base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(colour = "#222222"),
      axis.text = element_text(colour = "#333333"),
      plot.title = element_text(face = "bold", size = rel(1.08)),
      plot.subtitle = element_text(colour = "#4A4A4A"),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      plot.margin = margin(6, 8, 6, 6)
    )
}

save_p31_plot <- function(plot, filename, width_mm = 180, height_mm = 120) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    device = ragg::agg_png,
    width = width_mm,
    height = height_mm,
    units = "mm",
    dpi = 600,
    background = "white"
  )
}

write_session_info <- function(path) {
  writeLines(capture.output(sessionInfo()), path, useBytes = TRUE)
}

write_run_log <- function(path, analysis_id, inputs, outputs, checks = character()) {
  lines <- c(
    paste0("analysis_id: ", analysis_id),
    paste0("run_time: ", format(Sys.time(), tz = "Asia/Shanghai", usetz = TRUE)),
    paste0("data_source: ", XLSX_PATH),
    paste0("inputs: ", paste(inputs, collapse = "; ")),
    paste0("outputs: ", paste(outputs, collapse = "; ")),
    paste0("checks: ", if (length(checks)) paste(checks, collapse = "; ") else "none")
  )
  writeLines(lines, path, useBytes = TRUE)
}

safe_mean <- function(x) {
  if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (sum(is.finite(x)) < 2) NA_real_ else sd(x, na.rm = TRUE)
}

concordance_correlation <- function(x, y) {
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  if (length(x) < 3) return(NA_real_)
  2 * cov(x, y) / (var(x) + var(y) + (mean(x) - mean(y))^2)
}
