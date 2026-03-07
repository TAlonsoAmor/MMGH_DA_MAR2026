# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Pivot a sheet from wide (year columns) to long format and clean
#' @param df         Data frame (one sheet from dataAll)
#' @param value_name Name to give the values column
#' @param years      Character vector of year columns to pivot (default: FORECAST_YEARS)
pivot_sheet <- function(df, value_name, years = FORECAST_YEARS) {
  df |>
    pivot_longer(cols = all_of(years), names_to = "year", values_to = value_name) |>
    mutate(year = as.integer(year)) |>
    filter(!is.na(ISO), !is.na(year)) |>
    distinct(ISO, year, .keep_all = TRUE)
}


# =============================================================================
# DATA LOADING
# =============================================================================

#' Load all sheets from the annex Excel file into a named list of data frames.
#' Applies ISO corrections to all sheets that contain Country + ISO columns.
#'
#' @param path            File path to the annex Excel file
#' @param skip            Default number of header rows to skip (default = 2)
#' @param exclude_sheets  Sheet names to exclude (default = "TOC")
#' @param skip_exceptions Named vector of sheet-specific skip values
#'                        e.g. c("4c. %HTR coverage U2" = 3)
#'
#' @return Named list of tibbles, one per sheet.
#'         Access: dataAll$`2a. SI Pop`$Country
load_annex <- function(path,
                       skip            = 2,
                       exclude_sheets  = "TOC",
                       skip_exceptions = c()) {
  
  all_sheets  <- excel_sheets(path)
  keep_sheets <- setdiff(all_sheets, exclude_sheets)
  
  message("Loading ", length(keep_sheets), " sheet(s) from: ", path)
  
  dataAll <- map(
    set_names(keep_sheets),
    ~ {
      sheet_skip <- if (.x %in% names(skip_exceptions)) skip_exceptions[[.x]] else skip
      
      df <- read_excel(path, sheet = .x, skip = sheet_skip,
                       col_names = TRUE, .name_repair = "unique") |>
        filter(if_any(everything(), ~ !is.na(.)))
      
      # Drop year columns outside the forecast period (e.g. 2014, 2019, 2025)
      df <- df |>
        select(-any_of(
          setdiff(
            grep("^[0-9]{4}$", names(df), value = TRUE),  # all 4-digit year cols
            FORECAST_YEARS                                  # keep only 2030-2040
          )
        ))
      
      # Apply ISO corrections where applicable
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