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

path_who   <- "input_data/WHO_TB_datasheets.xlsx"
path_gbd   <- "input_data/IHME_GBD_2021.xlsx"
path_unaid <- "input_data/UNAIDS_subset_7geos.xlsx"

stopifnot(file.exists(path_who), file.exists(path_gbd), file.exists(path_unaid))

# ---- WHO TB (already tidy) ----
who_long <- readxl::read_excel(path_who, sheet = "TB - All (WHO)") %>%
  mutate(
    geography = as.character(geography),
    year = as.integer(year),
    source_indicator = paste0(source, "_", indicator),
    value = as.numeric(value)               # ensure numeric (already numeric, but safe)
  ) %>%
  select(year, geography, source_indicator, value) %>%
  filter(geography %in% keep_geos)

# ============================================
# IHME GBD (read FIRST sheet; map common names)
# ============================================
gbd_sheet <- readxl::excel_sheets(path_gbd)[1]
gbd_raw <- readxl::read_excel(path_gbd, sheet = gbd_sheet)

# normalize names
names(gbd_raw) <- gsub("\\s+","_", tolower(names(gbd_raw)))

pick_col <- function(df, candidates) {
  cand <- intersect(candidates, names(df))
  if (length(cand) == 0) NA_character_ else cand[[1]]
}

col_year    <- pick_col(gbd_raw, c("year","year_id"))
col_loc     <- pick_col(gbd_raw, c("location_name","location","loc_name"))
col_cause   <- pick_col(gbd_raw, c("cause_name","cause"))
col_measure <- pick_col(gbd_raw, c("measure_name","measure"))
col_metric  <- pick_col(gbd_raw, c("metric_name","metric"))
col_val     <- pick_col(gbd_raw, c("val","value","mean","mean_value","estimate"))

col_sex     <- pick_col(gbd_raw, c("sex","sex_name"))
col_age     <- pick_col(gbd_raw, c("age","age_name"))

needed <- c(col_year, col_loc, col_cause, col_measure, col_metric, col_val)
if (any(is.na(needed))) {
  stop(
    "IHME GBD: required columns not found on first sheet.\n",
    "Have: ", paste(names(gbd_raw), collapse = ", "), "\n",
    "Need any of:\n",
    "  year/year_id; location_name/location/loc_name; cause_name/cause;\n",
    "  measure_name/measure; metric_name/metric; val/value/mean/estimate"
  )
}

gbd_norm <- gbd_raw

# Only filter Both / All Ages if those columns exist
if (!is.na(col_sex)) gbd_norm <- dplyr::filter(gbd_norm, .data[[col_sex]] %in% c("Both","both"))
if (!is.na(col_age)) gbd_norm <- dplyr::filter(gbd_norm, .data[[col_age]] %in% c("All Ages","all ages"))

# Build tidy frame
gbd_long <- gbd_norm %>%
  dplyr::transmute(
    year   = as.integer(.data[[col_year]]),
    geography = trimws(as.character(.data[[col_loc]])),
    source_indicator = paste0(
      "IHME GBD_", .data[[col_cause]], " ", .data[[col_measure]],
      ifelse(tolower(.data[[col_metric]]) == "rate", " rate", ""),
      " (", .data[[col_metric]], ")"
    ),
    value  = suppressWarnings(as.numeric(.data[[col_val]]))
  ) %>%
  dplyr::filter(!is.na(geography), nzchar(geography)) %>%
  dplyr::filter(geography %in% keep_geos)

# Diagnostics you’ll see in the console
message("IHME rows kept (first sheet): ", nrow(gbd_long))
if (nrow(gbd_long) == 0) {
  ug <- unique(trimws(as.character(gbd_norm[[col_loc]])))
  message("First 20 geographies actually present on sheet:\n  ",
          paste(utils::head(ug, 20), collapse = " | "))
}


# ---- UNAIDS (subset tidy) ----
parse_unaids_value <- function(x) {
  s <- tolower(trimws(as.character(x)))
  # normalize spaces (incl. non-breaking)
  s <- gsub("\u00A0", " ", s, fixed = TRUE)
  
  # placeholders -> NA
  s[s %in% c("", "na", "n/a", "-", "—", "–", "…", "...")] <- NA
  
  # detect suffix multipliers
  mult <- ifelse(grepl("\\bm\\b", s), 1e6,
                 ifelse(grepl("\\bk\\b", s), 1e3, 1))
  
  # remove suffix for parsing
  s_stripped <- gsub("\\b(m|k)\\b", "", s)
  
  # parse numbers (handles '<0.1' -> 0.1, '430 000' -> 430000)
  val <- readr::parse_number(
    s_stripped,
    locale = readr::locale(grouping_mark = " ", decimal_mark = "."),
    na = c("", "na")
  )
  
  as.numeric(val) * mult
}

unaids_long_raw <- readxl::read_excel(path_unaid, sheet = 1) %>%
  mutate(
    year = as.integer(year),
    geography = as.character(geography)
  ) %>%
  filter(geography %in% keep_geos)

# Parse with robust function
unaids_long <- unaids_long_raw %>%
  mutate(value_num = parse_unaids_value(value)) %>%
  select(year, geography, source_indicator, value = value_num)

# Optional: log any rows still NA after parsing (for QC)
unaids_parse_issues <- unaids_long_raw %>%
  mutate(value_num = parse_unaids_value(value)) %>%
  filter(!is.na(value) & is.na(value_num))

if (nrow(unaids_parse_issues) > 0) {
  readr::write_csv(unaids_parse_issues, "outputs/unaids_parse_issues.csv")
  message("UNAIDS: ", nrow(unaids_parse_issues),
          " rows still non-numeric; logged to outputs/unaids_parse_issues.csv")
}

# Optional: quick sanity check for NAs introduced by parsing
na_rows <- unaids_long %>% filter(is.na(value))
if (nrow(na_rows) > 0) {
  message("UNAIDS: ", nrow(na_rows), " rows have NA after numeric parsing (likely blanks).")
}


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
