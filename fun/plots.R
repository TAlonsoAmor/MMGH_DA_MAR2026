# =============================================================================
# PLOTS
# =============================================================================

#' Global MR PDR split by N/S vs MR-MAP (2030-2040)
#' Mirrors Figure 3 in Ko et al. (2023)
plot_ns_vs_map <- function(demand_all) {
  plot_data <- demand_all |>
    group_by(year) |>
    summarise(
      `Needle & Syringe` = sum(demand_rutine_SIA - MR_PDR_step2, na.rm = TRUE),
      `Microarray Patch`  = sum(demand_MR_MAP_all,                    na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(cols = c(`Needle & Syringe`, `Microarray Patch`),
                 names_to = "presentation", values_to = "doses")
  
  ggplot(plot_data, aes(x = factor(year), y = doses / 1e6, fill = factor(presentation, levels = c("Needle & Syringe", "Microarray Patch")))) +
    geom_col() +
    geom_text(aes(label = round(doses / 1e6)),
              position = position_stack(vjust = 0.5), size = 3) +
    scale_fill_manual(values = c(
      "Needle & Syringe" = "#D4B8C7",
      "Microarray Patch"  = "#9E6B7E"
    )) +
    labs(
      title    = "Global MR Programmatic Dose Requirements 2030–2040",
      subtitle = "Split by vaccine presentation (Scenario 1: Base)",
      x        = "Year",
      y        = "Millions of doses",
      fill     = "",
      caption = "Assumption: All deliveries through SIA start in 2030."
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


#' MR-MAP PDR by country archetype (2030-2040)
#' Time series of PDR per year, per country
plot_country_archetype <- function(demand_all,
                                   sia_schedule,
                                   map_adoption,
                                   selected_iso = c("BRA")) {
  
  plot_data <- demand_all |>
    filter(ISO %in% selected_iso) |>
    group_by(ISO, Country, year) |>
    summarise(doses = sum(demand_MR_MAP_all, na.rm = TRUE), .groups = "drop")
  
  # SIA deployment years for selected countries
  sia_lines <- sia_schedule |>
    filter(ISO %in% selected_iso, sia_indicator == 1) |>
    left_join(demand_all |> distinct(ISO, Country), by = "ISO")
  
  # MAP adoption year per selected country
  map_lines <- map_adoption |>
    left_join(demand_all |> distinct(ISO, Country), by = "Country") |>
    filter(ISO %in% selected_iso)
  
  ggplot(plot_data, aes(x = year, y = doses / 1e6)) +
    # Facet per country
    facet_wrap(~Country, scales = "free_y") +
    # SIA deployment lines
    geom_vline(
      data        = sia_lines,
      aes(xintercept = year),
      linetype    = "dashed",
      linewidth   = 0.5,
      alpha       = 0.6,
      color = "firebrick3"
    ) +
    # MAP adoption lines
    geom_vline(
      data        = map_lines,
      aes(xintercept = MAP_start_year),
      linetype    = "solid",
      linewidth   = 0.8,
      alpha       = 0.9
    ) +
    geom_line(linewidth = 1, color = "steelblue") +
    geom_point(color = "steelblue") +
    labs(
      title    = "MR-MAP PDR by Selected Country 2030–2040",
      subtitle = "Dashed = SIA deployment | Solid = MAP adoption year",
      x        = "Year",
      y        = "Millions of doses"
    ) +
    scale_x_continuous(breaks = seq(2030, 2040, 2)) +
    theme_minimal()
}

#' MR Global demand filled by country key 
#' Mirrors Figure 4 in Ko et al. (2023)
plot_pdr_by_group <- function(demand_all) {
  demand_all |>
    group_by(`MR MAP Group`, year) |>
    summarise(doses = sum(demand_MR_MAP_all, na.rm = TRUE), .groups = "drop") |>
    mutate(
      group_label = case_when(
        `MR MAP Group` == "Key" ~ "16 Key Countries",
        `MR MAP Group` == "1"   ~ "Group A: MMR/MMRV only",
        `MR MAP Group` == "2"   ~ "Group B: MMR/MMRV + MR SIAs",
        `MR MAP Group` == "3"   ~ "Group C: AFRO/EMRO MR",
        `MR MAP Group` == "4"   ~ "Group D: SEARO/WPRO MR",
        TRUE ~ paste("Group", `MR MAP Group`)
      )
    ) |>
    ggplot(aes(x = year, y = doses / 1e6, fill = group_label)) +
    geom_col() +
    scale_fill_brewer(palette = "RdPu") +
    guides(fill = guide_legend(nrow = 2)) + 
    labs(
      title    = "Estimated Global MR-MAP PDR by Country Group 2030–2040",
      subtitle = "16 key countries modelled individually; remainder by archetype",
      x        = "Year",
      y        = "Millions of doses",
      fill     = "Country Group"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


#' SIA frequency and MCV2 coverage (2030-2040)
#' Figure shows dynamic SIA deployment year in relation to MCV2 coverage
plot_sia_coverage <- function(sia_mr, selected_iso = c("NGA", "IND", "ETH", 
                                                       "BRA", "PAK", "IDN")) {
  sia_mr |>
    filter(ISO %in% selected_iso) |>
    mutate(
      sia_fired = factor(sia_indicator, levels = c(0, 1),
                         labels = c("No SIA", "SIA"))
    ) |>
    ggplot(aes(x = year)) +
    # MCV2 coverage line
    geom_line(aes(y = cov_mcv2), colour = "steelblue", linewidth = 1) +
    geom_point(aes(y = cov_mcv2), colour = "steelblue", size = 2) +
    # SIA indicator as points on the coverage line
    geom_point(
      data = ~ filter(., sia_indicator == 1),
      aes(y = cov_mcv2),
      shape = 25, size = 4, fill = "firebrick3", colour = "firebrick3"
    ) +
    # 90% threshold line
    geom_hline(yintercept = 0.90, linetype = "dashed", colour = "slategray4", alpha = 0.5) +
    geom_hline(yintercept = 0.80, linetype = "dashed", colour = "slategray4", alpha = 0.5) +
    geom_hline(yintercept = 0.60, linetype = "dashed", colour = "slategray4", alpha = 0.5) +
    facet_wrap(~ Country, ncol = 2) +
    scale_y_continuous(labels = scales::percent, breaks = c(0, 0.6, 0.8, 0.9)) +
    scale_x_continuous(breaks = seq(2030, 2040, 2)) +
    labs(
      title    = "MCV2 Coverage and SIA Schedule by Country 2030–2040",
      subtitle = "Triangles = SIA year | Dashed lines = frequency thresholds (60%, 80%, 90%)",
      x        = "Year",
      y        = "MCV2 Coverage",
      caption  = "SIA frequency: <60% every 2yr | 60-80% every 3yr | 80-90% every 4yr | ≥90% no SIA"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
}