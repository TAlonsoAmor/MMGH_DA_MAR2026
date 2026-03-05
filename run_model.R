# =============================================================================
# Ko et al. (2023) – MR-MAP Demand Forecast
# Reference: "Estimating the future global dose demand for measles–rubella
#             microarray patches", Frontiers in Public Health (2023)
# =============================================================================

# ---- 0. Dependencies --------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
## Source specific functions
source("./helperFunctions.R")
source("./steps.R")
source("./plots.R")
source("./sensitivity.R")

# ---- 1. Constants & Assumptions ---------------------------------------------
# All hardcoded assumptions are declared here for transparency and easy
# modification during sensitivity analysis

ANNEX_PATH   <- "./data/Supplemental_Annex.xlsx"
FORECAST_YEARS <- as.character(seq(2030, 2040))

# Market penetration rates by archetype (Source: Ko et al. 2023, Step 2)
MARKET_PENETRATION <- c(
  "1" = 0.05,   # Group A: MMR/MMRV countries, MAPs for special populations only
  "2" = 0.30,   # Group B: historical MR N/S share ~30% in MMR/MMRV countries
  "3" = 0.80,   # Group C: AFRO/EMRO MR countries, unlikely to fully switch to MAPs
  "4" = 0.80    # Group D: SEARO/WPRO MR countries, same logic as Group C
)

# ISO corrections (data quality fix: Dominica incorrectly coded as DOM)
ISO_CORRECTIONS <- tribble(
  ~Country,     ~ISO,
  "Dominica",   "DMA"
)

# =============================================================================
# MAIN PIPELINE
# =============================================================================

run_pipeline <- function(annex_path = ANNEX_PATH) {
  
  # ── Load data ───────────────────────────────────────────────────────────────
  DATA <- load_annex(
    annex_path,
    skip_exceptions = c("4c. %HTR coverage U2" = 3)
  )
  
  # ── Step 1: Total MR PDR ────────────────────────────────────────────────────
  message("Running Step 1: Routine + SIA demand...")
  STEP1 <- calculate_step1(
    sheet_si_pop  = DATA$`2a. SI Pop`,
    sheet_mcv1    = DATA$`4a. MCV1`,
    sheet_mcv2    = DATA$`4b. MCV2`,
    sheet_wastage = DATA$`5. Routine Wastage N&S`,
    sheet_u5_sia  = DATA$`2g. U5 pop for SIA`
  )
  message("  Step 1 total PDR: ", round(sum(STEP1$combined$demand_rutine_SIA, na.rm=TRUE) / 1e9, 2), "b doses")
  
  # ── Step 2: MR-MAP PDR ──────────────────────────────────────────────────────
  message("Running Step 2: MAP market penetration...")
  STEP2 <- calculate_step2(
    step1_combined = STEP1$combined,
    sheet_overview = DATA$`1. Overview`
  )
  message("  Step 2 MAP PDR: ", round(sum(STEP2$MR_PDR_step2, na.rm=TRUE) / 1e9, 2), "b doses")
  
  # MAP adoption lookup (needed for Step 3 gating)
  MAP_ADOPTION <- DATA$`1. Overview` |>
    select(Country, `Scenario 1: Base`) |>
    rename(MAP_start_year = `Scenario 1: Base`)
  
  # SIA schedule (needed for Step 3 2-15 gating)
  SIA_SCHEDULE <- STEP1$sia_mr |>
    select(ISO, year, sia_indicator) |>
    distinct()
  
  # ── Step 3: HTR/MOV additional PDR ─────────────────────────────────────────
  message("Running Step 3: HTR/MOV additional reach...")
  STEP3 <- calculate_step3(
    sheet_sec_comp     = DATA$`3b. Sec comp pop`,
    sheet_rural        = DATA$`3c. Rural Pop`,
    sheet_remote_rural = DATA$`3d. % remote rural`,
    sheet_urban_slums  = DATA$`3e. Slum Pop`,
    sheet_pct_u2       = DATA$`2d. U2 pop %`,
    sheet_pct_2_15     = DATA$`2f. 2-15 pop %`,
    sheet_htr_cov_u2   = DATA$`4c. %HTR coverage U2`,
    sheet_htr_cov_2_15 = DATA$`4d. %HTR Coverage 2-15`,
    sia_schedule       = SIA_SCHEDULE,
    map_adoption       = MAP_ADOPTION
  )
  message("  Step 3 HTR/MOV PDR: ",
          round((sum(STEP3$mr_map_mov_u2, na.rm=TRUE) +
                   sum(STEP3$mr_map_mov_2_15, na.rm=TRUE)) / 1e9, 2), "b doses")
  
  # ── Combine all steps ───────────────────────────────────────────────────────
  DEMAND_ALL <- STEP2 |>
    select(ISO, Country, `MR MAP Group`, year, demand_rutine_SIA, MR_PDR_step2) |>
    left_join(
      STEP3 |> select(ISO, Country, `MR MAP Group`, year, mr_map_mov_u2, mr_map_mov_2_15),
      by = c("ISO", "Country", "MR MAP Group", "year")
    ) |>
    mutate(demand_MR_all = MR_PDR_step2 + mr_map_mov_u2 + mr_map_mov_2_15)
  
  # ── Sensitivity analysis ────────────────────────────────────────────────────
  message("Running sensitivity analysis...")
  SENSITIVITY <- run_sensitivity_analysis(STEP1$combined, DATA$`1. Overview`)
  
  # ── Plots ───────────────────────────────────────────────────────────────────
  message("Generating plots...")
  P1 <- plot_ns_vs_map(DEMAND_ALL)
  P2 <- plot_country_archetype(DEMAND_ALL)
  P3 <- plot_sensitivity(SENSITIVITY)
  P4 <- plot_pdr_by_group(DEMAND_ALL)
  P5 <- plot_sia_coverage(STEP1$sia_mr, selected_iso = c("NGA", "BRA", "IND", "ETH"))
  
  return(list(
    data        = DATA,
    step1       = STEP1,
    step2       = STEP2,
    step3       = STEP3,
    demand_all  = DEMAND_ALL,
    sensitivity = SENSITIVITY,
    plots       = list(ns_vs_map   = P1,
                       by_country  = P2,
                       sensitivity = P3,
                       by_country_global = P4,
                       sia_freq = P5)
  ))
}

# ── Run ───────────────────────────────────────────────────────────────────────
RESULTS <- run_pipeline()
# RESULTS$plots$ns_vs_map
# RESULTS$plots$by_country
# RESULTS$plots$sensitivity
# RESULTS$sensitivity