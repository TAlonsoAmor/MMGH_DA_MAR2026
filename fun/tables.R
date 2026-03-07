# =============================================================================
# TABLES
# =============================================================================

#' Create a gt table of SIA scheduled countries per year
#' @param sia_mr    Output of RESULTS$step1$sia_mr
table_sia_schedule <- function(sia_mr) {
  
  tbl <- sia_mr |>
    group_by(year) |>
    summarise(
      n_countries = sum(sia_indicator, na.rm = TRUE),
      .groups = "drop"
    ) |>
    gt() |>
    tab_header(
      title    = "Number of Countries with SIA Scheduled per Year",
      subtitle = "Based on MCV2 coverage-driven scheduling rules (Ko et al. 2023)"
    ) |>
    cols_label(
      year        = "Year",
      n_countries = "Countries with SIA"
    ) |>
    tab_footnote(
      footnote  = "SIA frequency: MCV2 <60% every 2yr | 60-80% every 3yr | 80-90% every 4yr | ≥90% no SIA",
      locations = cells_column_labels(columns = n_countries)
    ) |>
    opt_stylize(style = 1)
  
  return(tbl)
}

#' Create  gt table summary of PDR per step and per group_var
#' @param demand_all Dataframe with the information from all steps, created in run_pipeline() 
#' @param group_var Column name of variable to use for row split
table_pdr_summary <- function(demand_all,
                              group_var  = "MR MAP Group") {
  # group_var: any column name in demand_all to group by
  # Examples:
  #   group_var = "MR MAP Group"  → by archetype
  #   group_var = "WHO"           → by WHO region
  #   group_var = "Country"       → per country (e.g. after filtering to key countries)
  
  # Readable labels for MR MAP Group (applied only when grouping by that variable)
  map_group_labels <- c(
    "Key" = "16 Key Countries",
    "1"   = "Group A: MMR/MMRV only",
    "2"   = "Group B: MMR/MMRV + MR SIAs",
    "3"   = "Group C: AFRO/EMRO MR",
    "4"   = "Group D: SEARO/WPRO MR"
  )
  
  summary_data <- demand_all |>
    group_by(across(all_of(group_var))) |>
    summarise(
      step1_pdr = sum(demand_rutine_SIA,                  na.rm = TRUE),
      step2_pdr = sum(MR_PDR_step2,                       na.rm = TRUE),
      step3_pdr = sum(mr_map_mov_u2 + mr_map_mov_2_15,    na.rm = TRUE),
      .groups   = "drop"
    ) |>
    mutate(
      total_map   = step2_pdr + step3_pdr,
      group_label = if (group_var == "MR MAP Group") {
        recode(.data[[group_var]], !!!map_group_labels)
      } else {
        .data[[group_var]]
      },
      # Round to millions BEFORE computing total row
      # so displayed numbers and total are consistent
      across(c(step1_pdr, step2_pdr, step3_pdr, total_map),
             ~ round(. / 1e6) * 1e6)
    ) |>
    select(group_label, step1_pdr, step2_pdr, step3_pdr, total_map) |>
    arrange(group_label)
  
  # Add total row
  total_row <- summary_data |>
    summarise(
      group_label = "TOTAL",
      step1_pdr   = sum(step1_pdr),
      step2_pdr   = sum(step2_pdr),
      step3_pdr   = sum(step3_pdr),
      total_map   = sum(total_map)
    )
  
  summary_data <- bind_rows(summary_data, total_row)
  
  # Column label for the grouping variable
  group_col_label <- case_when(
    group_var == "MR MAP Group" ~ "Country Group",
    group_var == "WHO"          ~ "WHO Region",
    group_var == "Country"      ~ "Country",
    TRUE                        ~ group_var
  )
  
  tbl <- summary_data |>
    gt() |>
    tab_header(
      title    = "Cumulative MR-MAP Programmatic Dose Requirements 2030–2040",
      subtitle = "Millions of doses"
    ) |>
    cols_label(
      group_label = group_col_label,
      step1_pdr   = "Step 1: Total MR PDR",
      step2_pdr   = "Step 2: MR-MAP (Market Penetration)",
      step3_pdr   = "Step 3: MR-MAP (HTR/MOV)",
      total_map   = "Total MR-MAP PDR"
    ) |>
    fmt_number(
      columns  = c(step1_pdr, step2_pdr, step3_pdr, total_map),
      scale_by = 1e-6,
      decimals = 0
    ) |>
    tab_style(
      style     = list(cell_fill(color = "#9E6B7E"),
                       cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows = group_label == "TOTAL")
    ) |>
    tab_style(
      style     = cell_borders(sides = "left", color = "#9E6B7E", weight = px(2)),
      locations = cells_body(columns = total_map)
    ) |>
    tab_footnote(
      footnote  = "Step 1: Routine + SIA + buffer | Step 2: Market penetration by archetype | Step 3: Hard-to-reach and missed opportunity populations",
      locations = cells_column_labels(columns = step1_pdr)
    ) |>
    opt_stylize(style = 1)

  return(tbl)
}