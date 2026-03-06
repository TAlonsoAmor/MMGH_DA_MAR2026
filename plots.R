# =============================================================================
# PLOTS
# =============================================================================

#' Plot 1: Global MR PDR split by N/S vs MR-MAP (2030-2040)
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


#' Plot 2: MR-MAP PDR by country archetype (2030-2040)
#' Shows ramp-up pattern across representative countries
plot_country_archetype <- function(demand_all,
                                   selected_iso = c("NGA", "IND", "BRA",
                                                    "USA", "ETH", "IDN")) {
  demand_all |>
    filter(ISO %in% selected_iso) |>
    group_by(ISO, Country, year) |>
    summarise(doses = sum(demand_MR_MAP_all, na.rm = TRUE), .groups = "drop") |>
    ggplot(aes(x = year, y = doses / 1e6, colour = Country)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title    = "MR-MAP PDR by Selected Country 2030–2040",
      subtitle = "Covering key countries across all archetypes",
      x        = "Year",
      y        = "Millions of doses",
      colour   = "Country"
    ) +
    theme_minimal()
}


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
    geom_line(aes(y = cov_mcv2), colour = "#2196F3", linewidth = 1) +
    geom_point(aes(y = cov_mcv2), colour = "#2196F3", size = 2) +
    # SIA indicator as points on the coverage line
    geom_point(
      data = ~ filter(., sia_indicator == 1),
      aes(y = cov_mcv2),
      shape = 25, size = 4, fill = "#FF9800", colour = "#FF9800"
    ) +
    # 90% threshold line
    geom_hline(yintercept = 0.90, linetype = "dashed", colour = "red", alpha = 0.5) +
    geom_hline(yintercept = 0.80, linetype = "dashed", colour = "orange", alpha = 0.5) +
    geom_hline(yintercept = 0.60, linetype = "dashed", colour = "yellow3", alpha = 0.5) +
    facet_wrap(~ Country, ncol = 2) +
    scale_y_continuous(labels = scales::percent, breaks = c(0, 0.6, 0.8, 0.9)) +
    labs(
      title    = "MCV2 Coverage and SIA Schedule by Country 2030–2040",
      subtitle = "Orange triangles = SIA year | Dashed lines = frequency thresholds (60%, 80%, 90%)",
      x        = "Year",
      y        = "MCV2 Coverage",
      caption  = "SIA frequency: <60% every 2yr | 60-80% every 3yr | 80-90% every 4yr | ≥90% no SIA"
    ) +
    theme_minimal() +
    theme(strip.text = element_text(face = "bold"))
}