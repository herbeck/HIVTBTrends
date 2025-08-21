# ============================================
# merge_three_sources.R
# Inputs:
#   data/WHO_TB_datasheets.xlsx
#   data/IHME_GBD_2021.xlsx
#   data/UNAIDS_subset_7geos.xlsx
# Outputs:
#   outputs/merged_tidy.csv
#   outputs/merged_wide.xlsx  (sheet: merged)
# ============================================

# ---- Packages ----
pkgs <- c("readxl","dplyr","tidyr","stringr","readr","openxlsx","purrr")
new <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ---- Config ----
keep_geos <- c("Global","South Africa","Zimbabwe","Malawi","Nigeria","Kenya","Mozambique")

path_who   <- "data/WHO_TB_datasheets.xlsx"
path_gbd   <- "data/IHME_GBD_2021.xlsx"
path_unaid <- "data/UNAIDS_subset_7geos.xlsx"

stopifnot(file.exists(path_who), file.exists(path_gbd), file.exists(path_unaid))

# ============================================
# WHO TB (already tidy from your earlier script)
# ============================================
who_long <- readxl::read_excel(path_who, sheet = "TB - All (WHO)") %>%
  mutate(
    geography = as.character(geography),
    year = as.integer(year),
    source_indicator = paste0(source, "_", indicator)
  ) %>%
  select(year, geography, source_indicator, value) %>%
  filter(geography %in% keep_geos)

# ============================================
# IHME GBD (country + global; use Both/All Ages)
# ============================================
gbd_sheet <- readxl::excel_sheets(path_gbd)[1]
gbd_raw <- readxl::read_excel(path_gbd, sheet = gbd_sheet)

# Try to normalize common column names
nm <- tolower(names(gbd_raw))
names(gbd_raw) <- nm

expected_cols <- c("year","location","cause","measure","metric","val")
missing <- setdiff(expected_cols, names(gbd_raw))
if (length(missing)) {
  stop("IHME GBD file is missing columns: ", paste(missing, collapse = ", "),
       "\nFound: ", paste(names(gbd_raw), collapse = ", "))
}

# If present, filter Both/All Ages to avoid duplicates
if (all(c("sex","age") %in% names(gbd_raw))) {
  gbd_raw <- gbd_raw %>% filter(sex %in% c("Both","both"),
                                age %in% c("All Ages","all ages"))
}

gbd_long <- gbd_raw %>%
  transmute(
    year = as.integer(year),
    geography = as.character(location),
    source_indicator = paste0(
      "IHME GBD_", cause, " ", measure,
      ifelse(metric %in% c("Rate","rate"), " rate", ""),
      " (", metric, ")"
    ),
    value = val
  ) %>%
  filter(geography %in% keep_geos)

# ============================================
# UNAIDS (already subset to 7 geographies; tidy)
# Required columns: year, geography, source_indicator, value
# ============================================
unaids_long <- readxl::read_excel(path_unaid, sheet = 1)
req_u <- c("year","geography","source_indicator","value")
if (!all(req_u %in% names(unaids_long))) {
  stop("UNAIDS subset file must contain columns: ", paste(req_u, collapse = ", "),
       "\nFound: ", paste(names(unaids_long), collapse = ", "))
}
unaids_long <- unaids_long %>%
  mutate(year = as.integer(year),
         geography = as.character(geography)) %>%
  filter(geography %in% keep_geos)

# ============================================
# Merge (stack) → tidy, then pivot → wide
# ============================================
merged_tidy <- bind_rows(
  who_long,
  gbd_long,
  unaids_long
) %>%
  arrange(geography, year, source_indicator)

# Save tidy CSV (helpful for QC)
readr::write_csv(merged_tidy, "outputs/merged_tidy.csv")

# Wide (years × geography, columns = source_indicator)
merged_wide <- merged_tidy %>%
  tidyr::pivot_wider(
    id_cols = c(year, geography),
    names_from = source_indicator,
    values_from = value
  ) %>%
  arrange(geography, year)

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "merged")
openxlsx::writeData(wb, "merged", merged_wide)
openxlsx::freezePane(wb, "merged", firstActiveRow = 2, firstActiveCol = 3)
openxlsx::addFilter(wb, "merged", row = 1, cols = 1:ncol(merged_wide))
openxlsx::saveWorkbook(wb, "outputs/merged_wide.xlsx", overwrite = TRUE)

message("Wrote:\n  outputs/merged_tidy.csv\n  outputs/merged_wide.xlsx")
