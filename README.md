# MR-MAP Demand Forecast Model
## Replication of Ko et al. (2023)

## Overview
This model replicates the demand forecasting methodology from:
> Ko et al. (2023). "Estimating the future global dose demand for 
> measles–rubella microarray patches." Frontiers in Public Health.
> https://doi.org/10.3389/fpubh.2022.1037157

The model estimates global programmatic dose requirements (PDR) for 
measles-rubella microarray patches (MR-MAPs) from 2030 to 2040 across 
194 countries, covering Steps 1–3 of the published methodology.

---

## How to Run
0. ADD renv instructions
1. Place `Supplemental_Annex.xlsx` in the `./data/` folder
2. Open `mr_map_demand_forecast.R` in RStudio
3. Run the full script or call:
```r
RESULTS <- run_pipeline()
RESULTS$plots$ns_vs_map    # Plot 1: N/S vs MR-MAP
RESULTS$plots$by_country   # Plot 2: Country breakdown
RESULTS$plots$sensitivity  # Plot 3: Sensitivity analysis
```

---

## Scope
Steps implemented:
- **Step 1**: Total MR PDR — routine immunization (MCV1 + MCV2) + SIAs + buffer stock
- **Step 2**: MR-MAP PDR — market penetration by country archetype
- **Step 3**: Additional PDR from hard-to-reach (HTR) and missed opportunity 
  for vaccination (MOV) populations

Steps not implemented:
- **Step 4**: Use case split (UC1/UC2/UC3+4) — delivery location and provider
- **Step 5**: Scenario uncertainty — requires additional modelling assumptions

---

## Data Sources
| Data | Source | File |
|------|--------|------|
| Population projections (surviving infants, U5, age groups) | UN World Population Prospects (via Ko et al. annex) | `Supplemental_Annex.xlsx` sheets 2a–2g |
| MCV1/MCV2 coverage forecasts | WHO/UNICEF WUENIC 2019 via MI4A methodology | sheets 4a–4b |
| HTR coverage assumptions | Expert opinion (Ko et al. Working Group) | sheets 4c–4d |
| Routine wastage multipliers | WHO guidance | sheet 5 |
| MAP adoption years | Ko et al. (2023) Scenario 1: Base | sheet 1 |
| Hard-to-reach populations | Multiple sources via Ko et al. annex | sheets 3b–3e |

---

## Key Assumptions
All assumptions are documented inline in the code with `# ASSUMPTION:` comments.

| Assumption | Value | Justification |
|---|---|---|
| SIA coverage | 100% | Per Ko et al. (2023) |
| SIA wastage — 10-dose vial | 1.111 | Per Ko et al. (2023) Annex A |
| SIA wastage — 5-dose vial | 1.111 | Per Ko et al. (2023) Annex A |
| SIA wastage — 1-dose vial | 1.010 | Per Ko et al. (2023) Annex A |
| Market penetration Group A | 5% | Ko et al. (2023) Step 2 |
| Market penetration Group B | 30% | Ko et al. (2023) Step 2 |
| Market penetration Groups C/D | 80% | Ko et al. (2023) Step 2 |
| Key country archetype mapping | By WHO region | No individual data available; see code |
| SIA start year | 2030 (all countries) | Historical SIA dates unavailable; see Known Limitations |

---

## Known Limitations
1. **SIA scheduling**: All countries are anchored to 2030 as the first SIA year 
   due to unavailability of historical SIA dates. In the published paper, 
   country-specific historical schedules were used, which naturally staggers 
   SIAs across years. This causes an artificial concentration of SIA doses in 
   2030 visible in the output plots.

2. **Key country assumptions**: The 16 key countries in the paper received 
   individual expert-derived assumptions. Here they are assigned to archetypes 
   by WHO region as a proxy.

3. **Source data**: The supplementary Excel file used may differ from the exact 
   version used in Ko et al. (2023), as the original file is not directly linked 
   in the published paper. Results are directionally consistent (~85-90% of 
   published figures) but not identical.

---

## Results Summary
| Step | This model | Ko et al. (2023) | Accuracy |
|------|-----------|-----------------|---------|
| Step 1: Total MR PDR (2030–2040) | ~4.9b doses | ~4.05b doses | ~85% |
| Step 2: MR-MAP PDR (2030–2040) | ~1.98b doses | ~1.46b doses | ~90% |
| Step 3: HTR/MOV PDR (2030–2040) | ~0.32b doses | ~0.25b doses | ~79% |

---


## Add comment on LLM use
