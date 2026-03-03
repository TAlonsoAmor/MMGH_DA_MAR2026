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


######
target_pop_rutine <- dataAll$`2a. SI Pop` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "surviving_infants") |>
  mutate(year = as.integer(year))

cov_rutine_mcv1 <- dataAll$`4a. MCV1` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "cov_mcv1") |>
  mutate(year = as.integer(year))

cov_rutine_mcv2 <- dataAll$`4b. MCV2` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "cov_mcv2") |>
  mutate(year = as.integer(year))

wastage <- dataAll$`5. Routine Wastage N&S` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "wastage") |>
  mutate(year = as.integer(year))

# get all together and calculate the yearly demand per country
rutine_MR <- target_pop_rutine %>% 
  left_join(cov_rutine_mcv1) %>%
  left_join(cov_rutine_mcv2) %>%
  left_join(wastage) %>% 
  pivot_longer(cols = c("cov_mcv1", "cov_mcv2"), names_to = "VaccineCovType", values_to = "coverage") %>% 
  mutate(demand = surviving_infants*coverage*wastage) %>% 
  group_by(ISO, VaccineCovType) %>%
  arrange(year) %>% 
  mutate(buffer = case_when(demand - lag(demand) > 0 ~ 0.25*(demand - lag(demand)),
                            TRUE ~ 0)) %>% 
  mutate(demand_total = demand + buffer)

# demand for SIA
target_pop_SIA <- dataAll$`2g. U5 pop for SIA` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "pop_SIA") |>
  mutate(year = as.integer(year))

SIA_MR <- target_pop_SIA %>% 
  #get coverage of mcv2 to estimate frequency
  left_join(cov_rutine_mcv2) %>% 
  #assume that SIA freq is based on the coverage of 2030
  mutate(sia_freq = case_when(cov_mcv2 < 0.6 ~ 2,
                              0.6 <= cov_mcv2 <= 0.8 ~ 3,
                              0.8 < cov_mcv2 < 0.9 ~ 4.5,
                              cov_mcv2 > 0.9 ~ 0))