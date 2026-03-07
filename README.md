# MR-MAP Demand Forecast Model
## Replication of Ko et al. (2023)

## Overview

This model replicates the demand forecasting methodology from:

> Ko et al. (2023). "Estimating the future global dose demand for measles–rubella
> microarray patches." *Frontiers in Public Health.*
> https://doi.org/10.3389/fpubh.2022.1037157

The model estimates global programmatic dose requirements (PDR) for measles-rubella
microarray patches (MR-MAPs) from 2030 to 2040 across 194 countries, covering
Steps 1–3 of the published methodology.

---

## Repository Structure

```
.
├── data/
│   └── Supplemental_Annex.xlsx      # Input data (not included — see Data Sources)
├── outputs/                         # Generated plots and tables (created on run)
├── fun/                             # Folder storing all functions
    |── helperFunctions.R            # load_annex(), pivot_sheet()
    |── steps.R                      # calculate_step1/2/3(), compute_sia_schedule()
    |── plots.R                      # All plot functions
    |── sensitivity.R                # run_sensitivity_analysis()
    └── tables.R                     # table_sia_schedule(), table_pdr_summary()
├── run_model.R                      # Main entry point — run this
└── renv.lock                        # Package versions (managed by renv)
```
The code `qc_run_model.R` was kept to show the initial development version of these codes.

---

## Environment Setup

This project uses [`renv`](https://rstudio.github.io/renv/) for reproducible package
management. It was developed under **R 4.5.2** on Ubuntu 24.04 LTS.

**To set up the environment:**

1. Clone the repository in RStudio:
   - File → New Project → Version Control → Git
   - Paste the SSH address of this repository

2. Once in project, if `renv` is not installed, install it first:
   ```r
   install.packages("renv")
   ```

3. Restore all package versions used during development:
   ```r
   renv::restore()
   ```
   This will install the exact package versions recorded in `renv.lock`.

**Key packages and versions:**

| Package  | Version |
|----------|---------|
| R        | 4.5.2   |
| readxl   | 1.4.5   |
| dplyr    | 1.2.0   |
| tidyr    | 1.3.2   |
| purrr    | 1.2.1   |
| ggplot2  | 4.0.2   |
| gt       | 1.3.0   |

---

## How to Run

1. Place `Supplemental_Annex.xlsx` in the `./data/` folder
2. Open `run_model.R` in RStudio
3. Run the full script (source) or call (once all libraries and internal functions have been loaded/run):

```r
RESULTS <- run_pipeline(save_outputs = TRUE, output_dir = "./outputs")
```

**Accessing outputs:**

```r
# Plots
RESULTS$plots$ns_vs_map               # N&S vs MR-MAP stacked bar (Fig.3 paper)
RESULTS$plots$sensitivity             # Sensitivity analysis
RESULTS$plots$by_country_global       # PDR of MR_MAP by country group (Fig.4 paper)
RESULTS$plots$sia_freq                # SIA frequency vs MCV2 coverage for some example countries
RESULTS$plots$ts_country_demand_sia_mr# Country-level demand with SIA/MAP markers for some example countries

# Tables (render in RStudio Viewer)
RESULTS$tables$sia_schedule           # Countries with SIA per year
RESULTS$tables$pdr_by_group           # Cumulative PDR by MR MAP group
RESULTS$tables$pdr_by_who             # Cumulative PDR by WHO region
RESULTS$tables$pdr_key_countries      # Cumulative PDR by Country for Key Countries
```

All plots and tables are also saved to `./outputs/` when `save_outputs = TRUE`.

---

## Scope

**Steps implemented:**

- **Step 1**: Total MR PDR — routine immunization (MCV1 + MCV2) + SIAs + buffer stock
- **Step 2**: MR-MAP PDR — market penetration by country archetype
- **Step 3**: Additional PDR from hard-to-reach (HTR) and missed opportunity
  for vaccination (MOV) populations

**Steps not implemented:**

- **Step 4**: Use case split (UC1/UC2/UC3+4) — delivery location and provider
- **Step 5**: Scenario uncertainty — requires additional modelling assumptions

---

## Data Sources

| Data | File |
|------|------|
| Population projections (surviving infants, U5, age groups) | sheets 2a–2g |
| Population projections percentage (U2, 2-15) | sheets 2d-2f |
| MCV1/MCV2 coverage forecasts | sheets 4a–4b |
| HTR coverage assumptions | sheets 4c–4d |
| Routine wastage multipliers | sheet 5 |
| MAP adoption years | sheet 1 Scenario 1: Base|
| Hard-to-reach populations | sheets 3b–3e |


All data is sourced from `Supplemental_Annex.xlsx`, the spreadsheet shared through internal communication. This file is not included in the repository.

---

## Key Assumptions

| Assumption | Value | Justification |
|---|---|---|
| SIA coverage | 100% | Per Ko et al. (2023) |
| SIA wastage multiplier — 10-dose vial | 1.111 | Per Ko et al. (2023)|
| SIA wastage multiplier — 5-dose vial | 1.111 | Per Ko et al. (2023)|
| SIA wastage multiplier — 1-dose vial | 1.010 | Per Ko et al. (2023)|
| Market penetration Group A | 5% | Ko et al. (2023) Step 2 |
| Market penetration Group B | 30% | Ko et al. (2023) Step 2 |
| Market penetration Groups C/D | 80% | Ko et al. (2023) Step 2 |
| Key country archetype mapping | By WHO region + Income level for US and Brazil | No individual data available; see code |
| SIA start year | 2030 (all countries) | Historical SIA dates unavailable; see Known Limitations |

---

## Known Limitations

1. **SIA scheduling**: All countries are anchored to 2030 as the first SIA year
   due to unavailability of historical SIA dates. This causes an artificial concentration of SIA doses in
   2030 visible in the output plots. The published paper points to a sheet on the Annex spreadsheet with SIA frequency that I was not able to find. 
   The authors probably use some historical starting SIA year which causes a different SIA distribution across countries.
   
2. **Key country assumptions**: The 16 key countries in the paper received
   individual expert-derived assumptions. Here they are assigned to archetypes
   by WHO region as a proxy, as the `MR MAP Group` column was populated either with 1-4 or Key.

3. **Source data**: The supplementary Excel file used may differ from the exact
   version used in Ko et al. (2023), as the original file is not directly linked
   in the published paper. Results are directionally consistent with published
   figures but not identical.

---

## Results Summary

| Step | This model | Ko et al. (2023) | Accuracy |
|------|-----------|-----------------|---------|
| Step 1: Total MR PDR (2030–2040) | ~4.9b doses | ~4.05b doses | ~85% |
| Step 2: MR-MAP PDR (2030–2040) | ~1.98b doses | ~1.46b doses | ~90% |
| Step 3: HTR/MOV PDR (2030–2040) | ~0.25b doses | ~0.25b doses | 100% |

Differences are primarily attributable to the known limitations described above,
in particular the SIA scheduling assumption and the allocation to groups A-D of the key countries.

---

## Use of AI Assistance

This implementation was developed with the assistance of Claude (Anthropic, free version). AI assistance was used for:

- Code structure and refactoring suggestions
- Debugging data pipeline issues
- Documentation drafting

All methodological decisions, assumptions, and validation against the published
paper were made by the author. The AI was used as a coding and writing assistant.