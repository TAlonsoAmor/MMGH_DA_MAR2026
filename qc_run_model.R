# ---- 0. Dependencies --------------------------------------------------------
# install.packages(c("readxl", "dplyr", "tidyr", "purrr"))
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)


# ---- 1. Helper: load_annex() ------------------------------------------------
# ── Lookup table: corrections to apply to ALL sheets ─────────────────────────
# Add any future fixes here as you discover them
ISO_CORRECTIONS <- tribble(
  ~Country,            ~ISO,
  "Dominica",          "DMA"
)

load_annex <- function(path,
                       skip            = 2,
                       exclude_sheets  = "TOC",
                       skip_exceptions = c()) {
  # skip_exceptions: named vector of sheet-specific skip values
  # e.g. skip_exceptions = c("3a. % MOV" = 3)
  
  all_sheets  <- excel_sheets(path)
  keep_sheets <- setdiff(all_sheets, exclude_sheets)
  
  message("Loading ", length(keep_sheets), " sheet(s) from: ", path)
  
  dataAll <- map(
    set_names(keep_sheets),
    ~ {
      # Use sheet-specific skip if defined, otherwise use default
      sheet_skip <- if (.x %in% names(skip_exceptions)) skip_exceptions[[.x]] else skip
      
      df <- read_excel(path, sheet = .x, skip = sheet_skip,
                       col_names = TRUE, .name_repair = "unique") |>
        filter(if_any(everything(), ~ !is.na(.)))
      
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

dataAll <- load_annex(
  ANNEX_PATH,
  skip_exceptions = c("4c. %HTR coverage U2" = 3)   # skipping 3 rows for that particular sheet
)


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

#### demand for SIA
target_pop_SIA <- dataAll$`2g. U5 pop for SIA` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "pop_SIA") |>
  mutate(year = as.integer(year))

compute_sia_schedule <- function(df) {
  df |>
    arrange(ISO, Country, year) |>
    group_by(ISO) |>
    mutate(
      sia_indicator = {
        n           <- n()
        ind         <- integer(n)
        last_sia    <- 2029L
        consec_high <- 0L
        
        # If already at >=90% in 2030, leave all zeros and skip loop
        if (cov_mcv2[1] < 0.90) {
          
          # Force SIA in 2030 (first year) and start clock there
          ind[1]   <- 1L
          last_sia <- year[1]   # anchor to 2030, not 2029
          
          for (i in 2:n) {     # start loop from year 2 (2031)
            
            if (cov_mcv2[i] >= 0.90) {
              consec_high <- consec_high + 1L
            } else {
              consec_high <- 0L
            }
            
            if (consec_high >= 3L) {
              break   # all remaining ind stay 0
            }
            
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

SIA_MR <- target_pop_SIA %>% 
  #get coverage of mcv2 to estimate frequency
  left_join(cov_rutine_mcv2)
  
freq_SIA <- compute_sia_schedule(SIA_MR) 

demand_SIA <- freq_SIA %>%  
  left_join(wastage) %>% 
  # convert rutine wastage to SIA wastage
  mutate(
    sia_wastage_multiplier = case_when(
      round(wastage, 2) == 1.67 ~ 1.111,   # 10-dose vial
      round(wastage, 2) == 1.18 ~ 1.111,   # 5-dose vial
      round(wastage, 2) == 1.05 ~ 1.010,   # 1-dose vial
      TRUE                           ~ 0
    )
  ) %>% 
  mutate(demand = sia_indicator*pop_SIA*sia_wastage_multiplier)


### join everything
demand_mr_all <- rutine_MR %>%
  dplyr::group_by(ISO, Country, WHO, `WB Group (June 2020)`, `MR MAP Group`, year) %>% 
  dplyr::summarize(pdr_rutine = sum(demand_total)) %>% 
  left_join(demand_SIA) %>% 
  mutate(demand_rutine_SIA = demand + pdr_rutine)

## demand all
print("STEP 1 TOTAL:")
print(sum(demand_mr_all$demand_rutine_SIA))

# STEP 2 ------------------------------------------------------------------
# I'm gonna use the Overview sheet for this
# Archetypes: Group A -> 1; group B -> 2; Group C -> 3; Group 4 -> 4. 
# 16 Key countrties, those marked with key.
# Assumption of remaping of key countries into archetypes
# AFRO: DRC, Ethiopia, Mozambique, Nigeria, Chad, Tanzania, Uganda, South Africa → Group 3
# EMRO: Afghanistan, Pakistan → Group 3
# SEARO: Bangladesh, India, Indonesia → Group 4
# WPRO: Philippines → Group 4
# PAHO: Brazil → Group 2 (uses MMR/MMRV but may use MR for SIAs)
# PAHO: USA → Group 1 (exclusively MMR/MMRV)


mr_map_startYear <- dataAll$`1. Overview` %>% 
  select(Country, WHO, `MR MAP Group`, `Scenario 1: Base`) %>% 
  rename(MAP_start_year = `Scenario 1: Base`)

# add market penetration multiplier
mr_map_market <- mr_map_startYear %>% 
  # add new group keys to consider the 16 key countries
  mutate(MR_MAP_Group_New = case_when(`MR MAP Group` != "Key" ~ `MR MAP Group`,
                                      # ASSUMPTION: key countries assigned to nearest archetype by WHO region
                                      WHO %in% c("AFRO", "EMRO")  ~ "3",  # MR/M countries in Africa & Eastern Med
                                      WHO %in% c("SEARO", "WPRO") ~ "4",  # MR/M countries in SE Asia & Western Pacific
                                      WHO == "PAHO" & Country == "United States of America" ~ "1",  # exclusively MMR/MMRV
                                      WHO == "PAHO"               ~ "2",  # Brazil: MMR/MMRV routine but MR for SIAs,
                                      TRUE ~ NA_character_
                                      )) %>% 
  mutate(MR_MAP_Group_New = as.numeric(MR_MAP_Group_New)) %>% 
  mutate(market_multiplier = case_when(MR_MAP_Group_New == 1 ~ 0.05, # Group A: MMR/MMRV countries, MAPs only for special populations
                                       MR_MAP_Group_New == 2 ~ 0.3,  # Group B: historical MR N/S share in MMR/MMRV countries
                                       MR_MAP_Group_New == 3 ~ 0.8, # Group C: AFRO/EMRO MR countries, unlikely to switch fully to MAPs
                                       MR_MAP_Group_New == 4 ~ 0.8, # Group D: SEARO/WPRO MR countries, same logic as Group C
                                       TRUE ~ 0))

# the PDR for MAP
mr_map_demand_setp2 <- demand_mr_all %>% 
  left_join(mr_map_market) %>% 
  # create flag of MAP, according to starting year
  group_by(ISO, Country) %>% 
  mutate(MR_Flag = ifelse(year>= MAP_start_year, TRUE, FALSE)) %>% 
  mutate(MR_PDR_step2 = ifelse(MR_Flag, demand_rutine_SIA*market_multiplier, 0))

# Total number of PDR for MAP
print("STEP 2 TOTAL:")
print(sum(mr_map_demand_setp2$MR_PDR_step2))


# STEP 3 ------------------------------------------------------------------
sec_comp <- dataAll$`3b. Sec comp pop` %>% 
  # remove additional columns that we don´t need
  select(-(starts_with('202')|starts_with("2019"))) %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "sec_comp_pop") |>
  mutate(year = as.integer(year))

rural <- dataAll$`3c. Rural Pop` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "rural_pop") |>
  mutate(year = as.integer(year))

perc_remote_rural <- dataAll$`3d. % remote rural` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "percent_remote_rural") |>
  mutate(year = as.integer(year))

urban_slums <- dataAll$`3e. Slum Pop` %>% 
  # remove additional columns that we don´t need
  select(-(starts_with('202')|starts_with("2014"))) %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "urban_slum_pop") |>
  mutate(year = as.integer(year))

mov_u2_per <- dataAll$`2d. U2 pop %` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "percent_u2") |>
  mutate(year = as.integer(year))

mov_2_15_per <- dataAll$`2f. 2-15 pop %` %>% 
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "percent_2_15") |>
  mutate(year = as.integer(year))

htr_coverage_u2 <- dataAll$`4c. %HTR coverage U2` %>%
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "coverage_u2") %>%
  mutate(year = as.integer(year))

htr_coverage_2_15 <- dataAll$`4d. %HTR Coverage 2-15` %>%
  pivot_longer(cols = as.character(seq(2030,2040)), names_to = "year", values_to = "coverage_2_15") %>%
  mutate(year = as.integer(year))


target_pop_mov <- sec_comp %>% 
  left_join(rural) %>% 
  left_join(perc_remote_rural) %>% 
  left_join(urban_slums) %>% 
  left_join(mov_u2_per) %>% 
  left_join(mov_2_15_per) %>% 
  left_join(htr_coverage_u2) %>% 
  left_join(htr_coverage_2_15) %>% 
  mutate(remote_rural_pop = percent_remote_rural*rural_pop,
         total_mov_pop = remote_rural_pop + urban_slum_pop + sec_comp_pop,
         mov_u2_pop = total_mov_pop*percent_u2,
         mov_2_15_pop = total_mov_pop*percent_2_15)
  
add_mr_map_mov <- target_pop_mov %>% 
  #add SIA schedule for 2_15
  left_join(freq_SIA) %>% 
  mutate(mr_map_mov_pre_buffer_u2 = mov_u2_pop*2*coverage_u2*1.010,
         mr_map_mov_2_15 = mov_2_15_pop*2*coverage_2_15*1.010*sia_indicator) %>% 
  dplyr::group_by(ISO, Country) %>% 
  mutate(buffer = case_when(mr_map_mov_pre_buffer_u2 - lag(mr_map_mov_pre_buffer_u2) > 0 ~ 0.25*(mr_map_mov_pre_buffer_u2 - lag(mr_map_mov_pre_buffer_u2)),
                            TRUE ~ 0)) %>% 
  mutate(mr_map_mov_u2 = mr_map_mov_pre_buffer_u2+buffer)

# Total MOV doses
print("STEP 3 TOTAL:")
print(sum(add_mr_map_mov$mr_map_mov_2_15, na.rm = TRUE) + sum(add_mr_map_mov$mr_map_mov_u2, na.rm = TRUE))


# PLOTS -------------------------------------------------------------------
demand_all <- demand_mr_all %>% 
  select(ISO, Country, `MR MAP Group`, year, demand_rutine_SIA) %>% 
  left_join(mr_map_demand_setp2  %>% select(ISO, Country, `MR MAP Group`, year, MR_PDR_step2)) %>% 
  left_join(add_mr_map_mov %>% select(ISO, Country, `MR MAP Group`, year,mr_map_mov_2_15, mr_map_mov_u2)) %>% 
  mutate(demand_MR_all = MR_PDR_step2 + mr_map_mov_2_15 + mr_map_mov_u2)


# plot, all MR
demand_all %>% 
  group_by(year) %>% 
  summarize(doses = sum(demand_rutine_SIA)) %>% 
  ggplot(aes(x = year, y = doses))+
  geom_col()

demand_all %>% 
  group_by(year) %>% 
  summarize(doses = sum(demand_MR_all, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = doses))+
  geom_col()
