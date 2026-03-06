# =============================================================================
# SENSITIVITY ANALYSIS
# =============================================================================

#' Sensitivity analysis: vary Group C/D market penetration rate ±20%
#' (Source: Ko et al. 2023 – paper applies ±20% to key uncertain variables)
#'
#' Group C/D penetration (0.80) is the most impactful assumption:
#'   - Covers the majority of global MAP demand (AFRO/EMRO/SEARO/WPRO countries)
#'   - Explicitly flagged as assumption-based in the paper
#'   - No empirical market research was available at time of publication
run_sensitivity_analysis <- function(step1_combined, sheet_overview) {
  
  scenarios <- list(
    low    = 0.80 * 0.80,   # -20%: 64% penetration for Groups C/D
    base   = 0.80,           # base: 80% as per paper
    high   = 0.80 * 1.20    # +20%: 96% penetration for Groups C/D
  )
  
  results <- map_dfr(
    names(scenarios),
    ~ {
      # Temporarily override Group C/D penetration
      MARKET_PENETRATION["3"] <<- scenarios[[.x]]
      MARKET_PENETRATION["4"] <<- scenarios[[.x]]
      
      step2 <- calculate_step2(step1_combined, sheet_overview)
      
      step2 |>
        summarise(
          total_map_pdr = sum(MR_PDR_step2, na.rm = TRUE),
          .groups = "drop"
        ) |>
        mutate(scenario = .x,
               penetration_rate = scenarios[[.x]])
    }
  )
  
  # Reset to base
  MARKET_PENETRATION["3"] <<- 0.80
  MARKET_PENETRATION["4"] <<- 0.80
  
  return(results)
}


#' Plot sensitivity analysis results
plot_sensitivity <- function(sensitivity_results) {
  sensitivity_results |>
    mutate(
      scenario = factor(scenario, levels = c("low", "base", "high")),
      label    = paste0(round(penetration_rate * 100), "%\n(",
                        round(total_map_pdr / 1e9, 2), "b doses)")
    ) |>
    ggplot(aes(x = scenario, y = total_map_pdr / 1e9, fill = scenario)) +
    geom_col(width = 0.5) +
    geom_text(aes(label = label), vjust = -0.3, size = 3.5) +
    scale_fill_manual(values = c("low" = "#D4B8C7", "base" = "#9E6B7E", "high" = "#6B3E5E")) +
    scale_x_discrete(labels = c("low" = "-20% (64%)",
                                "base" = "Base (80%)",
                                "high" = "+20% (96%)")) +
    labs(
      title    = "Sensitivity Analysis: Group C/D Market Penetration Rate",
      subtitle = "±20% variation in MAP market penetration for AFRO/EMRO/SEARO/WPRO countries",
      x        = "Scenario",
      y        = "Global MR-MAP PDR 2030–2040 (step 2, billions)",
      caption  = "Base assumption: 80% market penetration (Ko et al. 2023)\nVariation method: ±20% as per paper sensitivity analysis methodology"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}
