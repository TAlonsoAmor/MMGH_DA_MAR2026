# =============================================================================
# Ko et al. (2023) – MR-MAP Demand Forecast
# STEP 1: Load annex.xls and propose algorithm structure
# =============================================================================
# Reference: "Estimating the future global dose demand for measles–rubella
#             microarray patches", Ko et al., Frontiers in Public Health (2023)
#
# ASSUMPTIONS & NOTES:
#   - The annex Excel file ("annex.xls") is the primary data source referenced
#     throughout the paper's supplementary materials.
#   - Rows 1–2 of every sheet are metadata headers → skipped (skip = 2).
#   - Sheet "TOC" is a table of contents → excluded.
#   - All sheet data is loaded into a named list (dataAll) for easy reference:
#       dataAll$`2a` , dataAll$`4a` , dataAll$`8a` , etc.
# =============================================================================

# ---- 0. Dependencies --------------------------------------------------------
# install.packages(c("readxl", "dplyr", "tidyr", "purrr"))
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)


# ---- 1. Helper: load_annex() ------------------------------------------------
# ── Lookup table: corrections to apply to ALL sheets ─────────────────────────
# Add any future fixes here as you discover them
ISO_CORRECTIONS <- tribble(
  ~Country,            ~ISO,
  "Dominica",          "DMA"
)

load_annex <- function(path,
                       skip           = 2,
                       exclude_sheets = "TOC") {
  
  all_sheets  <- excel_sheets(path)
  keep_sheets <- setdiff(all_sheets, exclude_sheets)
  
  message("Loading ", length(keep_sheets), " sheet(s) from: ", path)
  
  dataAll <- map(
    set_names(keep_sheets),
    ~ {
      df <- read_excel(path, sheet = .x, skip = skip,
                       col_names = TRUE, .name_repair = "unique") |>
        filter(if_any(everything(), ~ !is.na(.)))
      
      # ── Apply ISO corrections if both Country and ISO columns exist ────────
      if (all(c("Country", "ISO") %in% names(df))) {
        df <- df |>
          rows_update(ISO_CORRECTIONS, by = "Country", unmatched = "ignore")
      }
      
      df
    }
  )
  
  message("Sheets loaded: ", paste(names(dataAll), collapse = ", "))
  return(dataAll)
}


# ---- 2. Load data -----------------------------------------------------------
# UPDATE THIS PATH to point to your local copy of annex.xls / annex.xlsx
ANNEX_PATH <- "./data/Supplemental_Annex.xlsx"   # <-- change as needed

dataAll <- load_annex(ANNEX_PATH)

# Quick access examples (once loaded):
#   dataAll$`2a`           → surviving infants by country, 2030-2040
#   dataAll$`2b`           → 9-59 month-old children by country, 2030-2040
#   dataAll$`4a`           → MCV1 coverage forecasts
#   dataAll$`4b`           → MCV2 coverage forecasts
#   dataAll$`8a`           → SIA frequency / schedule data
#   dataAll$`5`            → buffer stock & wastage parameters


# =============================================================================
# PROPOSED ALGORITHM – Step 1: Estimate MR Programmatic Dose Requirement (PDR)
# for 2030–2040 (Routine Immunization + SIAs)
# =============================================================================
#
# The PDR formula (per country, per year) is:
#
#   PDR_total = PDR_routine + PDR_SIA
#
# where:
#   PDR_routine = doses for MCV1 + doses for MCV2 (+ buffer + wastage)
#   PDR_SIA     = doses for supplementary immunization activities (+ buffer + wastage)
#
# ── Sub-step A: Target Population ──────────────────────────────────────────
#   Source: sheets 2a–2g (UN WPP medium-variant projections)
#
#   surviving_infants[country, year]      → MCV1 eligible cohort
#   children_9_59m[country, year]         → SIA eligible cohort
#
# ── Sub-step B: Coverage Forecasts ─────────────────────────────────────────
#   Source: sheets 4a, 4b  (MI4A methodology applied to WUENIC 2019 estimates)
#
#   mcv1_cov[country, year]  ∈ [0,1]
#   mcv2_cov[country, year]  ∈ [0,1]
#
#   Method: linear extrapolation from 2019 WUENIC values to 2030,
#           then held constant (or trend-extended) to 2040.
#
# ── Sub-step C: Routine Immunization Doses ──────────────────────────────────
#
#   routine_doses_MCV1 = surviving_infants × mcv1_cov
#   routine_doses_MCV2 = surviving_infants × mcv2_cov
#                        (note: uses surviving infants as proxy for MCV2 cohort)
#
#   routine_doses_gross = (routine_doses_MCV1 + routine_doses_MCV2)
#                          / (1 - wastage_rate)   # adjust for open-vial wastage
#
# ── Sub-step D: SIA Frequency & Doses ──────────────────────────────────────
#   Source: sheets 8a–8b  (MI4A SIA forecasting rules)
#
#   Rule (from supplementary Annex A):
#     IF mcv2_cov < 60%  → SIA every 2 years
#     IF 60% ≤ mcv2_cov < 80% → SIA every 3 years
#     IF mcv2_cov ≥ 80%  → SIA every 4 years (or no SIA)
#
#   sia_indicator[country, year] ∈ {0, 1}   (1 = SIA scheduled that year)
#
#   sia_doses_gross = children_9_59m × sia_coverage × sia_indicator
#                     / (1 - wastage_rate)
#
# ── Sub-step E: Buffer Stock ────────────────────────────────────────────────
#   Source: sheet 5  (WHO guidance on buffer stock)
#
#   buffer = 0.25 × |PDR[year] − PDR[year-1]|
#            (25% of the change in demand from prior year)
#
#   Applied only when demand increases year-over-year.
#
# ── Sub-step F: Total PDR ───────────────────────────────────────────────────
#
#   PDR_total[country, year] =
#       routine_doses_gross + sia_doses_gross + buffer[country, year]
#
# =============================================================================


# ---- 3. Skeleton functions (to be filled once data is available) ------------

#' A: Extract target populations from sheets 2a/2b
get_target_populations <- function(dataAll) {
  # Returns long-format tibble: country | year | surviving_infants | ch_9_59m
  # dataAll$`2a` expected columns: Country, 2030, 2031, ..., 2040
  infants <- dataAll$`2a. SI Pop` |>
    pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "surviving_infants") |>
    #rename(country = 1) |>
    mutate(year = as.integer(year))
  
  pop_2yr <- dataAll$`2b. 2 yr pop` |>
    pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "pop_2yr") |>
    #rename(country = 1) |>
    mutate(year = as.integer(year))
  
  pop_u5 <- dataAll$`2g. U5 pop for SIA` |>
    pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "pop_u5") |>
    #rename(country = 1) |>
    mutate(year = as.integer(year))
  
  
  infants %>% 
    left_join(pop_2yr, by = c("ISO", "Country", "WHO", "WB Group (June 2020)", "MR MAP Group", "year")) %>% 
    left_join(pop_u5, by = c("ISO", "Country", "WHO", "WB Group (June 2020)", "MR MAP Group", "year")) 
}


#' B: Extract coverage forecasts from sheets 4a/4b
get_coverage_forecasts <- function(dataAll) {
  mcv1 <- dataAll$`4a. MCV1`|>
    pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "mcv1_cov") |>
    mutate(year = as.integer(year), mcv1_cov = mcv1_cov / 100)
  
  mcv2 <- dataAll$`4b. MCV2` |>
    pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "mcv2_cov") |>
    mutate(year = as.integer(year), mcv2_cov = mcv2_cov / 100)
  
  left_join(mcv1, mcv2, by = c("ISO", "Country", "WHO","WB Group (June 2020)", "MR MAP Group", "year"))
}


get_wastage <- function(dataAll) {
  wastage_df <- dataAll$`5. Routine Wastage N&S` |>
                pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "wastage_multiplier") |>
                mutate(year = as.integer(year))
}


#' C+D+E+F: Calculate PDR per country per year
#'
#' @param pop_df     Output of get_target_populations()
#' @param cov_df     Output of get_coverage_forecasts()
#' @param wastage_rate  Open-vial wastage rate (default = 0.10, i.e. 10%)
#'                      ASSUMPTION: 10% used as placeholder; replace with
#'                      sheet 5 values once available.
#' @param sia_coverage  Proportion of 9-59m children reached in SIA
#'                      ASSUMPTION: 0.85 (85%) as per common SIA planning norms
calculate_pdr <- function(pop_df,
                          cov_df,
                          wastage_df,
                          sia_coverage = 0.85) {
  
  df <- left_join(pop_df, cov_df, by = c("ISO", "Country","WHO" ,"WB Group (June 2020)", "MR MAP Group", "year")) |>
        left_join(wastage_df, by = c("ISO", "Country", "WHO", "WB Group (June 2020)", "MR MAP Group", "year")) %>% 
        arrange(ISO, year)
  
  df <- df |>
    mutate(
      # ── C: Routine doses (before buffer) ──────────────────────────────────
      routine_mcv1_net   = surviving_infants * mcv1_cov,
      routine_mcv2_net   = pop_2yr * mcv2_cov,          
      routine_net        = routine_mcv1_net + routine_mcv2_net,
      routine_gross      = routine_net /  wastage_multiplier,
      
      # ── D: SIA scheduling rule ────────────────────────────────────────────
      sia_freq = case_when(
        mcv2_cov < 0.60 ~ 2L,
        mcv2_cov < 0.80 ~ 3L,
        TRUE            ~ 4L
      ),
      sia_indicator = as.integer(year %% sia_freq == 0),
      sia_net       = pop_u5 * sia_coverage * sia_indicator,  
      sia_gross     = sia_net / wastage_multiplier,
      
      pdr_pre_buffer = routine_gross + sia_gross
    
    )
  
  # ── E: Buffer stock (requires prior-year demand) ──────────────────────────
  df |>
    group_by(ISO) |>
    mutate(
      pdr_lag          = lag(pdr_pre_buffer, default = first(pdr_pre_buffer)),
      demand_increase  = pmax(pdr_pre_buffer - pdr_lag, 0),
      buffer           = 0.25 * demand_increase,
      pdr_total        = pdr_pre_buffer + buffer
    ) |>
    ungroup()
  
  # df |> select(ISO, Country, year, surviving_infants, children_9_59m,
  #              mcv1_cov, mcv2_cov, routine_gross, sia_indicator,
  #              sia_gross, buffer, pdr_total)
}


# ---- 4. Main pipeline (runs once data is available) ------------------------
run_step1 <- function(annex_path = ANNEX_PATH) {
  
  dataAll <- load_annex(annex_path)
  
  pop_df <- get_target_populations(dataAll)
  cov_df <- get_coverage_forecasts(dataAll)
  wastage_df <- get_wastage(dataAll)
  
  pdr_df <- calculate_pdr(pop_df, cov_df, wastage_df)
  
  message("PDR calculated for ", n_distinct(pdr_df$Country),
          " countries across years ", min(pdr_df$year), "–", max(pdr_df$year))
  
  return(list(
    dataAll = dataAll,
    pdr     = pdr_df
  ))
}

# To run:  
results <- run_step1()
# Then:    View(results$pdr)