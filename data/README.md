# Data folder

## The Mendeley Data workbook (required)

Download `Listening_room_acoustic_decay_and_clarity_data.xlsx` from Mendeley Data
(CC BY 4.0; the DOI is given in the manuscript's data-availability statement) and
place it in **this folder**:

```
data/Listening_room_acoustic_decay_and_clarity_data.xlsx
```

`run_all.R` reads nothing else from the deposit. The workbook is not tracked in this
repository — it is archived at Mendeley Data, which is its citable home.

Sheets used by the analysis:

| Sheet | What it holds |
|---|---|
| `ir_manifest` | one row per impulse-response take: room, condition, source, receiver, roles, raw-file hash |
| `take_band_metrics` | take-level T30/T20/EDT/C50/C80 per octave and one-third-octave band, with strict-validity flags and processing diagnostics |
| `receiver_metrics` | the same metrics averaged within receiver (the five three-take cells are averaged here) |
| `condition_summary` | equal-receiver condition means and equal-source sensitivity summaries |
| `condition_contrasts` | the pre-freeze curtain/carpet direction check |
| `position_lookup` | receiver roles by room and active source |
| `room1_geometry`, `room1_modes`, `critical_frequency` | Room 1 dimensions and volume, modal frequencies to 200 Hz, Schroeder critical frequencies |
| `background_noise` | the summary-only Room 1 background-noise spectrum |
| `validation_summary`, `source_assignment` | recomputed-versus-legacy checks and the S2 identification of the unlabelled DIRAC export |
| `low_frequency_take`, `low_frequency_receiver`, `low_frequency_condition`, `low_frequency_validation` | the 25–120 Hz wavelet decay layer and its synthetic-signal validation |
| `cwt_parameter_grid`, `cwt_bandwidth` | wavelet-parameter robustness for the 42/60 Hz modal-excess estimates |
| `source_receiver_distances` | source–receiver distances recovered from impulse-response arrival delays |

The last three groups are computed from the raw impulse-response WAVs, which are not
deposited; they are archived as data so this pipeline needs nothing but the workbook.

## `design_inputs/` (shipped here)

Two transcription tables, not measurements, so they travel with the code:

- `certified_absorption_library.csv` — absorption coefficients from the eleven
  certified perforated-panel test reports, one row per panel system. `report_id`
  identifies the report; `relationship_to_proposal` records how close each system is
  to the construction the design proposed.
- `legacy_design_workbook_inputs.csv` — the six-band absorption coefficients, surface
  areas and required-absorption figures of the design's own Sabine calculation,
  transcribed from the collaborator's client-facing calculation workbook (not public).
  `code/a18_product_design_envelope.R` re-derives that workbook's own arithmetic from
  these values and stops if the reconstruction does not close to 1e-8.

Neither supports a validation claim: the renovation was never installed and no
post-treatment measurement or simulation exists.

## `room_photographs/` (shipped here)

The three site photographs of Figure 1a–c, byte-identical to the originals; their
SHA-256 digests are written to `output/data_lock/fig1_photographs.csv` at run time.

- `room1_front.jpg` — Room 1, front (Figure 1a)
- `room1_rear.jpg` — Room 1, rear, with the curtains (Figure 1b)
- `room2_front.jpg` — Room 2, front, with the curtains (Figure 1c)

The photographs are undated. They show the rooms and their visible furnishings, not a
particular measured furnishing state.
