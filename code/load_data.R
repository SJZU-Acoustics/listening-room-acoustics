# =============================================================================
# load_data.R — single data entry for the listening-room-acoustics release.
#
# Data source: the Mendeley Data workbook
#   data/Listening_room_acoustic_decay_and_clarity_data.xlsx  (CC BY 4.0)
#
# Sheets are read with col_types = "text" at the XLSX boundary and re-typed
# through a CSV round-trip, so readr infers exactly the types the working
# pipeline's CSVs carried and both "NA" and "" map to NA.
#
# Every analysis module reads its input through read_frozen_csv() or
# read_extension_csv(), which name the original processed CSV; the mapping
# below resolves each name to its workbook sheet. Nothing else touches the
# workbook.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(readr)
})

XLSX_PATH <- file.path("data", "Listening_room_acoustic_decay_and_clarity_data.xlsx")
CACHE_DIR <- file.path("output", "_workbook_cache")

# processed-CSV name -> workbook sheet
SHEET_FOR_FILE <- c(
  "ir_manifest.csv"                        = "ir_manifest",
  "ir_band_metrics.csv"                    = "take_band_metrics",
  "receiver_band_metrics.csv"              = "receiver_metrics",
  "condition_band_summary.csv"             = "condition_summary",
  "prefreeze_condition_contrasts.csv"      = "condition_contrasts",
  "position_lookup.csv"                    = "position_lookup",
  "room1_geometry.csv"                     = "room1_geometry",
  "room1_modes_to_200hz.csv"               = "room1_modes",
  "room1_critical_frequency.csv"           = "critical_frequency",
  "background_noise_summary.csv"           = "background_noise",
  "validation_summary.csv"                 = "validation_summary",
  "source_assignment_audit.csv"            = "source_assignment",
  "low_frequency_cwt_ir.csv"               = "low_frequency_take",
  "low_frequency_cwt_receiver.csv"         = "low_frequency_receiver",
  "low_frequency_cwt_condition_summary.csv" = "low_frequency_condition",
  "low_frequency_synthetic_validation.csv" = "low_frequency_validation",
  "cwt_parameter_grid.csv"                 = "cwt_parameter_grid",
  "effective_bandwidth.csv"                = "cwt_bandwidth",
  "source_receiver_distances.csv"          = "source_receiver_distances"
)

read_workbook_sheet <- function(sheet) {
  if (!file.exists(XLSX_PATH)) {
    stop("Workbook not found at ", XLSX_PATH, "\n",
         "Download it from Mendeley Data and place it in data/ — see README.",
         call. = FALSE)
  }
  dir.create(CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  cached <- file.path(CACHE_DIR, paste0(sheet, ".csv"))
  if (!file.exists(cached)) {
    raw <- readxl::read_excel(XLSX_PATH, sheet = sheet, col_types = "text")
    readr::write_csv(raw, cached, na = "")
  }
  readr::read_csv(cached, show_col_types = FALSE, progress = FALSE)
}

#' Read a canonical processed table by its original file name.
read_frozen_csv <- function(filename, ...) {
  sheet <- SHEET_FOR_FILE[[filename]]
  if (is.null(sheet)) {
    stop("No workbook sheet is registered for ", filename, call. = FALSE)
  }
  read_workbook_sheet(sheet)
}

#' Read a table from the append-only low-frequency extension. Same source; the
#' separate name is kept so the modules read as they did against the freeze.
read_extension_csv <- function(filename) read_frozen_csv(filename)
