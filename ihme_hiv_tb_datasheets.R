# ihme_to_unaids_like.R
# Converts IHME_GBD_2021_HIV_TB.csv to an .xlsx shaped like the UNAIDS subset:
#   - Two-row header: indicator merged over (Estimate, Low, High)
#   - First columns: Year, ISO, Location (ISO blank)
#   - IHME values => "Estimate"; Low/High = NA
#   - Append GBD metric (Number/Rate/Percentage) to indicator name

# ---- user paths ----
ihme_csv <- "input_data/IHME_GBD_2021_HIV_TB.csv"
out_xlsx <- "IHME_to_UNAIDS_like.xlsx"

# ---- packages ----
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(openxlsx)
})

# ---- read IHME ----
ihme <- readr::read_csv(ihme_csv, show_col_types = FALSE)

req <- c("measure","location","sex","age","cause","metric","year","val")
if (!all(req %in% names(ihme))) {
  stop("IHME file must contain columns: ", paste(req, collapse=", "))
}

# ---- tidy labels ----
# Normalize metric names; map Percent -> Percentage (per request)
metric_label <- function(x) {
  y <- str_to_lower(x)
  dplyr::case_when(
    y == "number"   ~ "Number",
    y == "rate"     ~ "Rate",
    y %in% c("percent","percentage") ~ "Percentage",
    TRUE ~ str_to_title(y)
  )
}

# Build indicator name.
# Keep it explicit but concise: "<Cause> - <Measure> - <Sex> - <Age> [<Metric>]"
ihme <- ihme %>%
  mutate(
    MetricLabel = metric_label(metric),
    Indicator = paste0(cause, " - ", measure, " - ", sex, " - ", age, " [", MetricLabel, "]")
  )

# ---- pivot to wide (Estimate only) ----
wide <- ihme %>%
  select(year, location, Indicator, val) %>%
  mutate(year = as.integer(year)) %>%
  pivot_wider(
    id_cols = c(year, location),
    names_from = Indicator,
    values_from = val
  ) %>%
  arrange(year, location)

# ---- add ISO and Low/High placeholders ----
wide <- wide %>%
  rename(Year = year, Location = location) %>%
  mutate(ISO = NA_character_) %>%
  relocate(Year, ISO, Location)

# Create triplets: for every indicator column, add Low/High (NA)
indicators <- setdiff(names(wide), c("Year","ISO","Location"))

for (ind in indicators) {
  wide[[paste0(ind, " - Low")]]  <- NA_real_
  wide[[paste0(ind, " - High")]] <- NA_real_
}

# Reorder columns so each indicator appears as [Estimate, Low, High] triplet
triplet_order <- c()
for (ind in indicators) {
  triplet_order <- c(triplet_order, ind, paste0(ind, " - Low"), paste0(ind, " - High"))
}
wide <- wide %>%
  select(Year, ISO, Location, all_of(triplet_order))

# ---- write Excel with two-row header and merged top cells ----
wb <- createWorkbook()
addWorksheet(wb, "IHME_to_UNAIDS_like")

n_rows <- nrow(wide)
n_cols <- ncol(wide)

# Row 1 & 2 headers
# For Year/ISO/Location: leave row1 blank (to mirror UNAIDS merged look) and put labels in row2
writeData(wb, "IHME_to_UNAIDS_like", x = t(c("", "", "")), startRow = 1, startCol = 1, colNames = FALSE, rowNames = FALSE)
writeData(wb, "IHME_to_UNAIDS_like", x = c("Year","ISO","Location"), startRow = 2, startCol = 1, colNames = FALSE, rowNames = FALSE)

# Build and write indicator headers
# Each indicator spans 3 columns; row2 labels are Estimate/Low/High
start_col <- 4
for (ind in indicators) {
  # The three subcolumns
  sublabels <- c("Estimate","Low","High")
  writeData(wb, "IHME_to_UNAIDS_like", x = t(sublabels), startRow = 2, startCol = start_col, colNames = FALSE)
  # Top merged header with indicator name
  mergeCells(wb, "IHME_to_UNAIDS_like", rows = 1, cols = start_col:(start_col+2))
  writeData(wb, "IHME_to_UNAIDS_like", x = ind, startRow = 1, startCol = start_col, colNames = FALSE)
  start_col <- start_col + 3
}

# Write body (data start at row 3)
writeData(wb, "IHME_to_UNAIDS_like", x = wide, startRow = 3, startCol = 1, colNames = FALSE, rowNames = FALSE)

# Basic styling (optional)
hdr1 <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
hdr2 <- createStyle(textDecoration = "bold", halign = "center")
addStyle(wb, "IHME_to_UNAIDS_like", hdr1, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
addStyle(wb, "IHME_to_UNAIDS_like", hdr2, rows = 2, cols = 1:n_cols, gridExpand = TRUE)
setRowHeights(wb, "IHME_to_UNAIDS_like", rows = 1, heights = 22)
setRowHeights(wb, "IHME_to_UNAIDS_like", rows = 2, heights = 18)
freezePane(wb, "IHME_to_UNAIDS_like", firstActiveRow = 3, firstActiveCol = 4)

# Autosize columns (can be slow with many columns; comment out if needed)
setColWidths(wb, "IHME_to_UNAIDS_like", cols = 1:n_cols, widths = "auto")

saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Wrote: ", out_xlsx)
