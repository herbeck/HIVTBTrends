# who_tb_to_unaids_like.R
# Convert WHO_TB_datasheets.xlsx -> UNAIDS-like wide .xlsx with a single Estimate column per indicator
# Output: WHO_TB_to_UNAIDS_like.xlsx

# ---- User paths ----
in_xlsx  <- "input_data/WHO_TB_datasheets.xlsx"     # path to your WHO file
sheet_nm <- "TB - All (WHO)"             # long table sheet: source, indicator, geography, year, value
out_xlsx <- "WHO_TB_to_UNAIDS_like.xlsx" # output file

# ---- Libraries ----
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(openxlsx)
})

# ---- Read WHO long table ----
sheets <- readxl::excel_sheets(in_xlsx)
if (!(sheet_nm %in% sheets)) {
  stop(sprintf("Sheet '%s' not found. Available sheets: %s", sheet_nm, paste(sheets, collapse = ", ")))
}

raw <- readxl::read_excel(in_xlsx, sheet = sheet_nm, col_names = TRUE)

# Expect columns: source, indicator, geography, year, value
need <- c("source", "indicator", "geography", "year", "value")
if (!all(need %in% names(raw))) {
  stop(sprintf(
    "Expected columns not found.\nHave: %s\nNeed: %s",
    paste(names(raw), collapse = ", "),
    paste(need, collapse = ", ")
  ))
}

# ---- Clean & standardize ----
df <- raw %>%
  rename(
    Location = geography,
    Year     = year,
    Estimate = value
  ) %>%
  mutate(
    Year = suppressWarnings(as.integer(Year))
  ) %>%
  filter(!is.na(Year), !is.na(Location), !is.na(indicator))

# ---- Pivot to wide: one column per indicator (Estimate only) ----
wide <- df %>%
  select(Year, Location, indicator, Estimate) %>%
  pivot_wider(
    id_cols = c(Year, Location),
    names_from = indicator,
    values_from = Estimate,
    values_fn = list(Estimate = ~ dplyr::first(.x))  # in case of accidental duplicates
  ) %>%
  arrange(Year, Location)

# Insert blank ISO column after Year to mirror UNAIDS leading columns
wide <- wide %>%
  mutate(ISO = NA_character_) %>%
  relocate(Year, ISO, Location)

# Collect indicator columns in the order they appear
indicators <- setdiff(names(wide), c("Year", "ISO", "Location"))

# ---- Write Excel with two-row header ----
wb <- createWorkbook()
addWorksheet(wb, "WHO_TB_to_UNAIDS_like")

# Header rows:
# Row 1: "", "", "" then indicator names (each over a single column)
# Row 2: "Year","ISO","Location" then "Estimate" under each indicator

n_cols <- ncol(wide)

# Row 1 blanks for the first three columns
writeData(wb, 1, x = t(c("", "", "")), startRow = 1, startCol = 1, colNames = FALSE)

# Row 1 indicator names
if (length(indicators) > 0) {
  writeData(wb, 1, x = t(indicators), startRow = 1, startCol = 4, colNames = FALSE)
}

# Row 2 labels for first three columns
writeData(wb, 1, x = c("Year", "ISO", "Location"), startRow = 2, startCol = 1, colNames = FALSE)

# Row 2 "Estimate" under each indicator
if (length(indicators) > 0) {
  writeData(wb, 1, x = t(rep("Estimate", length(indicators))), startRow = 2, startCol = 4, colNames = FALSE)
}

# Data starting row 3
writeData(wb, 1, x = wide, startRow = 3, startCol = 1, colNames = FALSE)

# Styling
hdr_bold_center <- createStyle(textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE)
addStyle(wb, 1, hdr_bold_center, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
addStyle(wb, 1, hdr_bold_center, rows = 2, cols = 1:n_cols, gridExpand = TRUE)
setRowHeights(wb, 1, rows = 1, heights = 22)
setRowHeights(wb, 1, rows = 2, heights = 18)

# Freeze panes at first data cell
freezePane(wb, 1, firstActiveRow = 3, firstActiveCol = 4)

# Autosize columns
setColWidths(wb, 1, cols = 1:n_cols, widths = "auto")

# Save
saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Wrote: ", out_xlsx)

# ---- Optional: ISO join (uncomment & supply a lookup if desired) ----
# iso_lkp <- read.csv("location_to_iso.csv") # columns: Location, ISO
# wide_iso <- wide %>% select(Year, Location) %>% distinct() %>% left_join(iso_lkp, by = "Location")
# # ... then merge ISO into 'wide' before writing
