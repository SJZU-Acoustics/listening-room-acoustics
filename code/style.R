suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(ragg)
})

# Release layout: every analysis module writes into output/<analysis id>/, and
# the display items land beside them in output/figures, output/tables and
# output/data_lock.
OUT_DIR <- "output"
EXPLORE <- OUT_DIR
FIG_DIR <- file.path(OUT_DIR, "figures")
TABLE_DIR <- file.path(OUT_DIR, "tables")
SI_TABLE_DIR <- file.path(TABLE_DIR, "si")
LOCK_DIR <- file.path(OUT_DIR, "data_lock")

for (path in c(FIG_DIR, TABLE_DIR, SI_TABLE_DIR, LOCK_DIR, OUT_DIR)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

WIDTH_MM <- c(single_column = 85, double_column = 178)
mm_to_in <- function(x) x / 25.4
BASE_FAMILY <- "Helvetica"

COL <- c(
  blue = "#0072B2",
  orange = "#E69F00",
  green = "#009E73",
  vermillion = "#D55E00",
  sky = "#56B4E9",
  purple = "#CC79A7",
  yellow = "#F0E442",
  black = "#000000",
  grey = "#777777",
  light_grey = "#D0D0D0"
)
class(COL) <- c("p31_palette", "character")
`[.p31_palette` <- function(x, i, ...) unname(NextMethod("["))

theme_pub <- function(base_size = 9, axis_title_size = 10) {
  theme_classic(base_size = base_size, base_family = BASE_FAMILY) %+replace%
    theme(
      axis.line = element_line(colour = "black", linewidth = 0.45),
      axis.ticks = element_line(colour = "black", linewidth = 0.4),
      axis.ticks.length = unit(0.09, "cm"),
      axis.title = element_text(size = axis_title_size, colour = "black"),
      axis.title.x = element_text(margin = margin(t = 3)),
      axis.title.y = element_text(margin = margin(r = 3), angle = 90),
      axis.text = element_text(size = base_size, colour = "black"),
      legend.text = element_text(size = base_size, colour = "black"),
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.tag = element_text(size = 10, face = "bold", family = BASE_FAMILY),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      strip.background = element_blank(),
      strip.text = element_blank()
    )
}

theme_schematic <- function() {
  theme_void(base_family = BASE_FAMILY, base_size = 9) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(2, 2, 2, 2)
    )
}

save_figure <- function(plot, filename, width_mode = "double_column", height_mm) {
  width_mm <- unname(WIDTH_MM[[width_mode]])
  output <- file.path(FIG_DIR, filename)
  ggsave(
    output, plot,
    width = mm_to_in(width_mm), height = mm_to_in(height_mm), units = "in",
    dpi = 600, device = ragg::agg_png, bg = "white"
  )
  message("Wrote ", output, " (", width_mm, " x ", height_mm, " mm)")
  invisible(output)
}

read_exp <- function(analysis, filename) {
  readr::read_csv(file.path(EXPLORE, analysis, filename), show_col_types = FALSE)
}

lock_csv <- function(data, filename) {
  path <- file.path(LOCK_DIR, filename)
  readr::write_csv(data, path, na = "")
  invisible(path)
}

fmt <- function(x, digits = 3) {
  x[!is.na(x) & abs(x) < 0.5 * 10^(-digits)] <- 0
  formatC(x, digits = digits, format = "f")
}
fmt_p <- function(x) {
  ifelse(x < 0.001, "<0.001", formatC(x, digits = 3, format = "f"))
}
# Table cells: a negative value takes a math minus, never a text hyphen.
fmt_tex <- function(x, digits = 3) {
  sub("^-", "$-$", fmt(x, digits))
}

tex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  x <- gsub("\\^", "\\\\textasciicircum{}", x)
  x
}

tex_path <- function(x) paste0("\\path{", as.character(x), "}")

tex_acoustic_label <- function(x) {
  x <- tex_escape(x)
  x <- gsub("T20", "$T_{20}$", x, fixed = TRUE)
  x <- gsub("T30", "$T_{30}$", x, fixed = TRUE)
  x <- gsub("C50", "$C_{50}$", x, fixed = TRUE)
  x <- gsub("C80", "$C_{80}$", x, fixed = TRUE)
  x <- gsub(" x ", " $\\times$ ", x, fixed = TRUE)
  x
}

tex_metric <- function(x) {
  recode(as.character(x), T20 = "$T_{20}$", T30 = "$T_{30}$",
         C50 = "$C_{50}$", C80 = "$C_{80}$", .default = as.character(x))
}

# Supplementary table fragments hold the body rows only. The column
# specification, caption and header rows live in supplementary_information.tex.
write_si_rows <- function(path, rows) {
  lines <- vapply(rows, function(row) paste0(paste(row, collapse = " & "), " \\\\"), character(1))
  writeLines(lines, path, useBytes = TRUE)
}
