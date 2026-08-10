R code for reproducing the statistical analyses, figures and tables for the manuscript "Broadband decay control and low-frequency boundaries in two small listening rooms".

## Requirements

- R 4.5+ (developed and verified on R 4.5.3)

- CRAN packages: `tidyverse`, `readxl`, `patchwork`, `scales`, `ragg`, `lme4`, `lmerTest`, `boot`, `jsonlite`, `digest`

- Install:

  ```r
  install.packages(c("tidyverse", "readxl", "patchwork", "scales", "ragg",
                     "lme4", "lmerTest", "boot", "jsonlite", "digest"))
  ```

- No non-standard hardware is required. A complete run takes about two to three minutes on a normal desktop; the bootstrap and jackknife modules account for most of it.

## Data

The analysis reads a single input: the Mendeley Data workbook
`Listening_room_acoustic_decay_and_clarity_data.xlsx`.

1. Download it from Mendeley Data (CC BY 4.0) — see the manuscript's data-availability statement for the DOI.
2. Place the `.xlsx` file in the `data/` folder (see `data/README.md`).

The workbook holds the analysis-ready acoustic chain: the impulse-response manifest, take- and receiver-level octave and one-third-octave decay and clarity metrics, condition-level aggregates, the 25–120 Hz wavelet decay layer, source–receiver distances recovered from the impulse responses' arrival delays, Room 1 geometry and modal calculations, the summary-only background-noise spectrum, and the legacy-validation layers.

The 130 raw impulse-response WAVs are **not** part of the deposit. Two analyses read them directly — the wavelet-parameter robustness grid and the arrival-delay geometry — so their result tables are deposited in the workbook instead of being recomputed here.

Two small design inputs ship with this repository in `data/design_inputs/` because they are transcriptions of documents rather than measurements: the certified absorption library taken from the eleven perforated-panel test reports, and the six-band absorption assumptions of the design's own Sabine calculation. The renovation was never installed, so neither supports any validation claim.

## File structure

- `run_all.R` — master script: runs every analysis module in dependency order, then builds every manuscript display item.
- `code/load_data.R` — single data entry: maps each canonical processed-table name to its workbook sheet and reads it with `col_types = "text"` plus a CSV round-trip, so column types match the working pipeline exactly.
- `code/helpers.R`, `code/factorial_helpers.R` — shared design constants (core octave bands, condition levels, bootstrap N and seed) and the design-based factorial estimator with its BCa bootstrap and jackknife.
- `code/style.R` — shared publication plot styling and the LaTeX table writer.
- `code/a*.R` — the analysis modules, one per analysis of the paper: endpoint convergence (A03), technical repeatability (A04), the global T20 factorial (A05), room consistency (A07), cross-endpoint convergence (A08), source sensitivity (A09), spatial uniformity (A10), listening-position representativeness (A11), the design target gap (A12), low-frequency modal diagnosis (A13), robustness variants (A15), one-third-octave structure (A17), the certified-product envelope (A18), the low-frequency wavelet boundary (A19), and the two post-review checks — common-band broadband estimates (A21) and the scenario uncertainty envelope (A23).
- `code/build_displays.R` — Figures 1–5, Table 1 and Supplementary Tables S1–S16.

## Usage

From the repository root:

```bash
Rscript run_all.R
```

Outputs are written to:

- `output/figures/` — Figures 1–5 (PNG, 600 dpi, 178 mm)
- `output/tables/` — Table 1 and `output/tables/si/` Supplementary Tables S1–S16 (LaTeX fragments)
- `output/data_lock/` — the plotted values behind every figure panel and table (CSV)
- `output/<analysis id>/` — the full regenerated result tables, plot, `results.md`, `session_info.txt` and `run.log` of each analysis module

To keep the console log alongside the outputs:

```bash
Rscript run_all.R 2>&1 | tee output/run_log.txt
```

`output/` is produced at run time and is safe to delete. It also holds `_workbook_cache/`, the extracted workbook sheets; delete it to force a fresh read.

## Verification

Run against the deposited workbook, this pipeline reproduces the manuscript's display items **byte-identically**: all 5 figures, all 18 data-lock CSVs and all 17 LaTeX table fragments match the values in the paper exactly. Figure 4b jitters its receiver points, so the render is seeded to keep it reproducible.

## Notes

- Modules are run in dependency order by `run_all.R`: A21 reads A10's and A19's outputs and A23 reads A18's, so those come last. Each module runs in its own environment, so nothing leaks between them.
- Strict-valid T20 for S1 over octave bands 125 Hz–8 kHz is the primary decay endpoint throughout; T30 is retained for legacy comparability and sensitivity, and 31.5/63 Hz are diagnostic only. The modules never substitute one endpoint for another.
- The five Room 2 listening-position cells with three valid takes are averaged within receiver before the ten receivers are equally weighted; the shared helpers apply this once.
- The bootstrap is 1,000 resamples with a fixed seed, so interval endpoints reproduce exactly.

## License

Code in this repository is released under the MIT License (see `LICENSE`). The input data are archived separately under CC BY 4.0 at Mendeley Data.
