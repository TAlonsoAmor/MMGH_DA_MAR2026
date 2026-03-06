# =============================================================================
# STEP 1: Estimate MR PDR (Routine + SIA + Buffer)
# =============================================================================
# Methodology (Source: Ko et al. 2023, Supplemental Annex A, Step 1):
#   PDR_total = PDR_routine + PDR_SIA
#   Routine: surviving infants × (MCV1_cov + MCV2_cov) × wastage_multiplier + buffer
#   SIA:     U5 population × sia_wastage_multiplier, only in scheduled SIA years
#   Buffer:  25% of positive demand increase year-over-year (routine only)
#
# SIA scheduling rules (applied per country, anchored to 2030):
#   MCV2 < 60%  → SIA every 2 years
#   MCV2 60-80% → SIA every 3 years
#   MCV2 ≥ 80%  → SIA every 4 years
#   MCV2 ≥ 90% for 3 consecutive years → SIA stops permanently
#   Countries with MCV2 ≥ 90% in 2030 → no SIA at all


#' Compute SIA schedule indicator per country per year
#' @param df Data frame with columns ISO, Country, year, cov_mcv2
#' @return df with additional column sia_indicator (0/1)
compute_sia_schedule <- function(df) {
  df |>
    arrange(ISO, Country, year) |>
    group_by(ISO) |>
    mutate(
      sia_indicator = {
        n           <- n()
        ind         <- integer(n)
        consec_high <- 0L
        
        if (cov_mcv2[1] < 0.90) {
          # Force SIA in 2030, anchor clock there
          ind[1]   <- 1L
          last_sia <- year[1]
          
          for (i in 2:n) {
            # Track consecutive years at >=90%
            if (cov_mcv2[i] >= 0.90) consec_high <- consec_high + 1L
            else consec_high <- 0L
            
            # Stop SIAs after 3 consecutive years >=90%
            if (consec_high >= 3L) break
            
            freq <- case_when(
              cov_mcv2[i] < 0.60 ~ 2L,
              cov_mcv2[i] < 0.80 ~ 3L,
              TRUE               ~ 4L
            )
            
            if ((year[i] - last_sia) >= freq) {
              ind[i]   <- 1L
              last_sia <- year[i]
            }
          }
        }
        ind
      }
    ) |>
    ungroup()
}


#' Calculate Step 1: Total MR PDR (Routine + SIA + Buffer)
#'
#' @param sheet_si_pop      Sheet 2a: surviving infants
#' @param sheet_mcv1        Sheet 4a: MCV1 coverage forecasts
#' @param sheet_mcv2        Sheet 4b: MCV2 coverage forecasts
#' @param sheet_wastage     Sheet 5:  routine wastage multipliers
#' @param sheet_u5_sia      Sheet 2g: U5 population for SIA
#'
#' @return List with:
#'   $routine_mr:  long tibble with routine demand per country/year
#'   $sia_mr:      long tibble with SIA demand per country/year
#'   $combined:    joined tibble with both + demand_rutine_SIA total
calculate_step1 <- function(sheet_si_pop,
                            sheet_mcv1,
                            sheet_mcv2,
                            sheet_wastage,
                            sheet_u5_sia) {
  
  # ── A: Load and pivot input sheets ─────────────────────────────────────────
  target_pop_routine <- pivot_sheet(sheet_si_pop,  "surviving_infants")
  cov_mcv1           <- pivot_sheet(sheet_mcv1,    "cov_mcv1")
  cov_mcv2           <- pivot_sheet(sheet_mcv2,    "cov_mcv2")
  wastage            <- pivot_sheet(sheet_wastage,  "wastage")
  target_pop_sia     <- pivot_sheet(sheet_u5_sia,  "pop_SIA")
  
  # ── B: Routine demand ──────────────────────────────────────────────────────
  # Doses = surviving_infants × coverage × wastage_multiplier
  # Buffer = 25% of positive demand increase (routine only, per paper)
  routine_mr <- target_pop_routine |>
    left_join(cov_mcv1,  by = c("ISO", "Country", "WHO",
                                "WB Group (June 2020)", "MR MAP Group", "year")) |>
    left_join(cov_mcv2,  by = c("ISO", "Country", "WHO",
                                "WB Group (June 2020)", "MR MAP Group", "year")) |>
    left_join(wastage,   by = c("ISO", "Country", "WHO",
                                "WB Group (June 2020)", "MR MAP Group", "year")) |>
    pivot_longer(cols = c("cov_mcv1", "cov_mcv2"),
                 names_to = "vaccine_type", values_to = "coverage") |>
    mutate(
      demand = if_else(
        is.na(wastage) | wastage == 0 | is.na(surviving_infants),
        NA_real_,
        surviving_infants * coverage * wastage
      )
    ) |>
    group_by(ISO, vaccine_type) |>
    arrange(year) |>
    mutate(
      # ASSUMPTION: buffer applies to routine only 
      buffer       = case_when(demand - lag(demand) > 0 ~ 0.25 * (demand - lag(demand)),
                               TRUE ~ 0),
      demand_total = demand + buffer
    ) |>
    ungroup()
  
  # ── C: SIA demand ──────────────────────────────────────────────────────────
  # SIA wastage inferred from routine vial size (no SIA-specific data available)
  # ASSUMPTION: SIA coverage = 100% (per paper)
  sia_mr <- target_pop_sia |>
    left_join(cov_mcv2, by = c("ISO", "Country", "WHO",
                               "WB Group (June 2020)", "MR MAP Group", "year")) |>
    compute_sia_schedule() |>
    left_join(wastage, by = c("ISO", "Country", "WHO",
                              "WB Group (June 2020)", "MR MAP Group", "year")) |>
    mutate(
      # Convert routine wastage multiplier to SIA equivalent by vial size
      # Routine: 1-dose=5%, 5-dose=15%, 10-dose=40%  → multipliers 1/(1-rate)
      # SIA:     1-dose=1%, 5-dose=10%, 10-dose=10%  → multipliers 1/(1-rate)
      sia_wastage = case_when(
        abs(wastage - 1.666667) < 0.001 ~ 1.111,   # 10-dose vial
        abs(wastage - 1.176471) < 0.001 ~ 1.111,   # 5-dose vial
        abs(wastage - 1.050000) < 0.001 ~ 1.010,   # 1-dose vial
        TRUE                             ~ NA_real_
      ),
      demand = if_else(
        is.na(sia_wastage) | is.na(pop_SIA),
        NA_real_,
        sia_indicator * pop_SIA * sia_wastage
      )
    )
  
  # ── D: Combine routine + SIA ───────────────────────────────────────────────
  combined <- routine_mr |>
    group_by(ISO, Country, WHO, `WB Group (June 2020)`, `MR MAP Group`, year) |>
    summarise(pdr_routine = sum(demand_total, na.rm = TRUE), .groups = "drop") |>
    left_join(
      sia_mr |> select(ISO, year, demand, sia_indicator),
      by = c("ISO", "year")
    ) |>
    mutate(demand_rutine_SIA = pdr_routine + demand)
  
  return(list(
    routine_mr = routine_mr,
    sia_mr     = sia_mr,
    combined   = combined
  ))
}


# =============================================================================
# STEP 2: Estimate MR-MAP PDR (Market Penetration by Archetype)
# =============================================================================
# Methodology (Source: Ko et al. 2023, Step 2):
#   MR-MAP PDR = Total MR PDR × market_penetration_rate
#   Applied only after each country's MAP adoption year (Scenario 1: Base)
#
# Market penetration rates (Source: Ko et al. 2023, Step 2):
#   Group A (1): 5%  – MMR/MMRV countries, MAPs for special populations
#   Group B (2): 30% – historical MR N/S share in MMR/MMRV countries
#   Group C (3): 80% – AFRO/EMRO MR countries
#   Group D (4): 80% – SEARO/WPRO MR countries
#
# ASSUMPTION: 16 key countries mapped to archetypes by WHO region
#   (no individual country data available; see code comments for mapping)


#' Calculate Step 2: MR-MAP PDR via market penetration
#'
#' @param step1_combined  Output of calculate_step1()$combined
#' @param sheet_overview  Sheet 1: Overview with MAP start years and archetypes
#'
#' @return Tibble with MR_PDR_step2 column added
calculate_step2 <- function(step1_combined, sheet_overview) {
  
  # ── A: Build archetype + adoption year lookup ──────────────────────────────
  mr_map_market <- sheet_overview |>
    select(Country, WHO, `MR MAP Group`, `Scenario 1: Base`) |>
    rename(MAP_start_year = `Scenario 1: Base`) |>
    mutate(
      # ASSUMPTION: key countries assigned to nearest archetype by WHO region
      # AFRO/EMRO  → Group 3 (MR/M countries, Africa & Eastern Med)
      # SEARO/WPRO → Group 4 (MR/M countries, SE Asia & Western Pacific)
      # USA (PAHO) → Group 1 (exclusively MMR/MMRV)
      # Brazil (PAHO) → Group 2 (MMR/MMRV routine, MR for SIAs)
      MR_MAP_Group_New = case_when(
        `MR MAP Group` != "Key"                                      ~ `MR MAP Group`,
        WHO %in% c("AFRO", "EMRO")                                   ~ "3",
        WHO %in% c("SEARO", "WPRO")                                  ~ "4",
        WHO == "PAHO" & Country == "United States of America"        ~ "1",
        WHO == "PAHO"                                                 ~ "2",
        TRUE ~ NA_character_
      ),
      MR_MAP_Group_New  = as.numeric(MR_MAP_Group_New),
      market_multiplier = MARKET_PENETRATION[as.character(MR_MAP_Group_New)]
    )
  
  # ── B: Apply market penetration after adoption year ────────────────────────
  step1_combined |>
    left_join(
      mr_map_market |> select(Country, MAP_start_year, MR_MAP_Group_New, market_multiplier),
      by = "Country"
    ) |>
    group_by(ISO, Country) |>
    mutate(
      MR_Flag      = year >= MAP_start_year,
      MR_PDR_step2 = if_else(MR_Flag, demand_rutine_SIA * market_multiplier, 0)
    ) |>
    ungroup()
}


# =============================================================================
# STEP 3: Additional PDR from Hard-to-Reach (HTR) and MOV Populations
# =============================================================================
# Methodology (Source: Ko et al. 2023, Supplemental Annex A, Step 3):
#   HTR populations: children in remote rural, urban slums, security-compromised
#   HTR pop = remote_rural_pop + urban_slum_pop + security_compromised_pop
#   Age split: HTR_U2 = HTR × %under2;  HTR_2_15 = HTR × %2_15
#   Coverage from sheets 4c/4d (encodes MAP adoption year, 0 before adoption)
#
#   Doses U2:   HTR_U2   × 2 doses × coverage_u2   × wastage (routine)
#   Doses 2-15: HTR_2_15 × 2 doses × coverage_2_15 × wastage (SIA years only)
#   Buffer: 25% of positive demand increase (U2 routine only)


#' Calculate Step 3: Additional HTR/MOV PDR
#'
#' @param sheet_sec_comp      Sheet 3b: security compromised population
#' @param sheet_rural         Sheet 3c: rural population
#' @param sheet_remote_rural  Sheet 3d: % remote rural
#' @param sheet_urban_slums   Sheet 3e: urban slum population
#' @param sheet_pct_u2        Sheet 2d: % under-2 of total population
#' @param sheet_pct_2_15      Sheet 2f: % 2-15 of total population
#' @param sheet_htr_cov_u2    Sheet 4c: HTR coverage U2 (0 before MAP adoption)
#' @param sheet_htr_cov_2_15  Sheet 4d: HTR coverage 2-15
#' @param sia_schedule        Output of compute_sia_schedule() (for 2-15 SIA gating)
#' @param map_adoption        Data frame with ISO and MAP_start_year
#'
#' @return Tibble with mr_map_mov_u2 and mr_map_mov_2_15 columns
calculate_step3 <- function(sheet_sec_comp,
                            sheet_rural,
                            sheet_remote_rural,
                            sheet_urban_slums,
                            sheet_pct_u2,
                            sheet_pct_2_15,
                            sheet_htr_cov_u2,
                            sheet_htr_cov_2_15,
                            sia_schedule,
                            map_adoption) {
  
  # ── A: Pivot all input sheets ───────────────────────────────────────────────
  sec_comp     <- pivot_sheet(sheet_sec_comp,     "sec_comp_pop")
  rural        <- pivot_sheet(sheet_rural,        "rural_pop")
  remote_rural <- pivot_sheet(sheet_remote_rural, "percent_remote_rural")
  slums        <- pivot_sheet(sheet_urban_slums,  "urban_slum_pop")
  pct_u2       <- pivot_sheet(sheet_pct_u2,       "percent_u2")
  pct_2_15     <- pivot_sheet(sheet_pct_2_15,     "percent_2_15")
  htr_cov_u2   <- pivot_sheet(sheet_htr_cov_u2,   "coverage_u2")
  htr_cov_2_15 <- pivot_sheet(sheet_htr_cov_2_15, "coverage_2_15")
  
  JOIN_KEYS <- c("ISO", "Country", "WHO", "WB Group (June 2020)", "MR MAP Group", "year")
  
  # ── B: Build HTR population by age group ───────────────────────────────────
  target_pop_mov <- sec_comp |>
    left_join(rural,        by = JOIN_KEYS) |>
    left_join(remote_rural, by = JOIN_KEYS) |>
    left_join(slums,        by = JOIN_KEYS) |>
    left_join(pct_u2,       by = JOIN_KEYS) |>
    left_join(pct_2_15,     by = JOIN_KEYS) |>
    left_join(htr_cov_u2,   by = JOIN_KEYS) |>
    left_join(htr_cov_2_15, by = JOIN_KEYS) |>
    mutate(
      remote_rural_pop = percent_remote_rural * rural_pop,
      total_htr_pop    = remote_rural_pop + urban_slum_pop + sec_comp_pop,
      htr_u2_pop       = total_htr_pop * percent_u2,
      htr_2_15_pop     = total_htr_pop * percent_2_15
    )
  
  # ── C: Calculate doses ─────────────────────────────────────────────────────
  # U2:   2 doses routine
  #       gated by coverage_u2 (sheet 4c already encodes 0 before MAP adoption)
  # 2-15: 2 doses SIA
  #       gated by sia_indicator AND MAP_start_year
  #       (coverage_2_15 in sheet 4d does NOT encode adoption year)
  add_mr_map_mov <- target_pop_mov |>
    left_join(
      map_adoption |> select(Country, MAP_start_year),
      by = "Country"
    ) |>
    left_join(
      sia_schedule |> select(ISO, year, sia_indicator) |> distinct(ISO, year, .keep_all = TRUE),
      by = c("ISO", "year")
    ) |>
    mutate(
      mr_map_mov_pre_buffer_u2 = htr_u2_pop * 2 * coverage_u2 * 1.010,
      mr_map_mov_2_15          = if_else(
        sia_indicator == 1 & year >= MAP_start_year,
        htr_2_15_pop * 2 * coverage_2_15 * 1.010,
        0
      )
    ) |>
    group_by(ISO, Country) |>
    arrange(year) |>
    mutate(
      buffer        = case_when(
        mr_map_mov_pre_buffer_u2 - lag(mr_map_mov_pre_buffer_u2) > 0 ~
          0.25 * (mr_map_mov_pre_buffer_u2 - lag(mr_map_mov_pre_buffer_u2)),
        TRUE ~ 0
      ),
      mr_map_mov_u2 = mr_map_mov_pre_buffer_u2 + buffer
    ) |>
    ungroup()
  
  return(add_mr_map_mov)
}