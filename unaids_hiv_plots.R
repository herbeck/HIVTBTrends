# ============================================================
# Read UNAIDS combined Excel -> write CSV -> make plots
# - Auto-detect two header rows (Indicator over Estimate/Low/High)
# - Auto-detect Year and Name columns in the data
# - Keeps Global, South Africa (ZAF), Zimbabwe (ZWE), Malawi (MWI)
# - Outputs:
#    1) datasheets/UNAIDS/UNAIDS_HIV_4geos_long.csv  (tidy, NA as "NA")
#    2) datasheets/UNAIDS/plots_R/*.pdf + *.png      (plots)
# ============================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(readr);  library(ggplot2); library(purrr)
})

# ---------- Config ----------
# Pass a path as the first arg to override the default below.
in_xlsx <- file.path("datasheets_UNAIDS","UNAIDS_subset_combined.xlsx")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) in_xlsx <- args[1]

# Where to write the tidy CSV and plots
out_csv   <- file.path("datasheets","UNAIDS","UNAIDS_HIV_4geos_long.csv")
plots_dir <- file.path("datasheets","UNAIDS","plots_R")
dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Geos (Name -> code)
targets_names <- c("Global","South Africa","Zimbabwe","Malawi")
name_to_iso   <- c("South Africa"="ZAF","Zimbabwe"="ZWE","Malawi"="MWI","Global"="Global")
targets_codes <- c("Global","ZAF","ZWE","MWI")

# ---------- Helpers ----------
fill_forward <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  if (length(x) > 1) for (i in 2:length(x)) if (x[i]=="" && x[i-1]!="") x[i] <- x[i-1]
  x
}
norm_stat <- function(s) {
  s <- tolower(str_squish(as.character(s)))
  s <- ifelse(grepl("est", s), "estimate", s)
  s <- ifelse(grepl("low|lower|lo", s), "low", s)
  s <- ifelse(grepl("high|upper|hi|up", s), "high", s)
  s
}
# Find consecutive rows r and r+1 such that row r+1 contains estimate/low/high
detect_header_rows <- function(raw, scan = 50) {
  n <- nrow(raw); lim <- max(1, min(scan, n-1))
  for (r in 1:lim) {
    r2 <- tolower(as.character(unlist(raw[r+1, , drop = TRUE])))
    if (any(grepl("estimate", r2)) &&
        any(grepl("low|lower|lo", r2)) &&
        any(grepl("high|upper|hi|up", r2))) return(c(r, r+1))
  }
  # fallback to first two rows if not detected
  c(1, 2)
}
score_year_col <- function(v) {
  s <- suppressWarnings(as.integer(as.character(v)))
  sum(!is.na(s) & s >= 1980 & s <= 2035)
}
score_name_col <- function(v) {
  s <- as.character(v)
  sum(!is.na(s) & !grepl("^\\s*[-]?[0-9][0-9,\\.]*\\s*$", s))
}

# ---------- Read Excel & pick sheet ----------
sheets <- tryCatch(readxl::excel_sheets(in_xlsx), error = function(e) character())
if (!length(sheets)) stop("No sheets found in: ", in_xlsx)

# Prefer your combined sheet name if present; otherwise first ByYear-like sheet
preferred_sheet <- "HIV_ByYear_Combined"
if (preferred_sheet %in% sheets) {
  sheet_to_use <- preferred_sheet
} else {
  byyear <- grep("ByYear", sheets, value = TRUE)
  sheet_to_use <- if (length(byyear)) byyear[1] else sheets[1]
}
message("Using sheet: ", sheet_to_use)

raw <- readxl::read_excel(in_xlsx, sheet = sheet_to_use, col_names = FALSE)

if (!is.data.frame(raw) || nrow(raw) < 3) stop("Sheet is too small to parse: ", sheet_to_use)

# ---------- Detect two header rows & data block ----------
hdr <- detect_header_rows(raw)
h1  <- fill_forward(unlist(raw[hdr[1], , drop = TRUE]))
h2  <- fill_forward(unlist(raw[hdr[2], , drop = TRUE]))
h2  <- norm_stat(h2)

data_start <- hdr[2] + 1
dat <- raw[data_start:nrow(raw), , drop = FALSE]

# Replace empty strings in DATA with NA
dat[] <- lapply(dat, function(col) { col <- as.character(col); col[col==""] <- NA_character_; col })

# ---------- Detect Year and Name columns from the data ----------
year_col <- which.max(vapply(dat, score_year_col, integer(1)))
name_col <- which.max(vapply(dat, score_name_col, integer(1)))

if (!length(year_col) || !length(name_col)) stop("Could not detect Year and/or Name column.")

# Filter to target locations by Name column
nm <- as.character(dat[[name_col]])
keep <- which(!is.na(nm) & nm %in% targets_names)
if (!length(keep)) stop("No rows found for the target locations in the selected sheet.")

dat <- dat[keep, , drop = FALSE]
nm  <- nm[keep]

years <- suppressWarnings(as.integer(as.character(dat[[year_col]])))

# ---------- Build a tidy long table from indicator columns ----------
records <- list()
for (c in seq_len(ncol(dat))) {
  stat <- h2[c]
  if (!(stat %in% c("estimate","low","high"))) next
  indicator <- str_squish(as.character(h1[c]))
  if (!nzchar(indicator)) next
  # skip Year/Name columns if they happen to carry a stat tag
  if (c == year_col || c == name_col) next
  
  vals_raw <- as.character(dat[[c]])
  vals_num <- readr::parse_number(vals_raw, locale = readr::locale(grouping_mark = ","))
  
  geo_name <- nm
  geo_code <- unname(name_to_iso[geo_name]); geo_code[is.na(geo_code)] <- geo_name
  
  records[[length(records)+1]] <- tibble(
    geo       = geo_code,
    geo_name  = geo_name,
    year      = years,
    indicator = indicator,
    stat      = stat,
    value     = vals_num
  )
}

tidy <- if (length(records)) bind_rows(records) else tibble(
  geo       = character(),
  geo_name  = character(),
  year      = integer(),
  indicator = character(),
  stat      = character(),
  value     = double()
)

# Keep year range and our geos
tidy <- tidy %>%
  filter(!is.na(year), year >= 1990, year <= 2025) %>%
  mutate(geo = if_else(geo %in% c("WLD","GLB","GLOBAL"), "Global", geo)) %>%
  filter(geo %in% targets_codes)

# ---------- Write the tidy CSV FIRST ----------
readr::write_csv(tidy %>% arrange(geo, indicator, year, stat), out_csv, na = "NA")
message("Wrote tidy CSV: ", normalizePath(out_csv))

# ---------- Plotting ----------
if (!nrow(tidy)) stop("Tidy CSV has no rows after filtering; check headers and target locations.")

df <- tidy %>%
  mutate(stat = factor(tolower(stat), levels = c("low","estimate","high"), ordered = TRUE),
         geo  = recode(geo, "GLOBAL" = "Global"))

present <- unique(df$indicator)
pick <- function(pat) {
  hits <- present[str_detect(tolower(present), tolower(pat))]
  if (length(hits)) hits[1] else NA_character_
}
selected <- c(
  pick("people\\s*living\\s*with\\s*hiv|\\bplhiv\\b"),
  pick("new\\s*hiv\\s*infect|incidence"),
  pick("aids[- ]?related\\s*deaths|hiv\\s*deaths"),
  pick("prevalence\\s*\\(%\\)|\\bprevalence\\b")
) %>% discard(is.na)

if (!length(selected)) {
  writeLines(sort(present), con = file.path(plots_dir, "available_indicators.txt"))
  stop("No indicators matched defaults. See 'available_indicators.txt' and adjust 'selected'.")
}

plot_one <- function(ind_name) {
  d <- df %>%
    filter(indicator == ind_name) %>%
    select(geo, year, stat, value) %>%
    pivot_wider(names_from = stat, values_from = value) %>%
    arrange(geo, year)
  
  ggplot(d, aes(x = year, y = estimate, color = geo)) +
    geom_ribbon(aes(ymin = low, ymax = high, fill = geo),
                alpha = 0.15, color = NA, inherit.aes = FALSE) +
    geom_line(size = 0.95) +
    labs(title = ind_name, x = "Year", y = "Value", color = "Location", fill = "Location") +
    theme_minimal(base_size = 12)
}

plots <- map(selected, plot_one)

# Save a multi-page PDF and individual PNGs
pdf_file <- file.path(plots_dir, "unaids_selected_indicators.pdf")
grDevices::pdf(pdf_file, width = 10, height = 6)
for (p in plots) print(p)
grDevices::dev.off()

for (i in seq_along(plots)) {
  ggplot2::ggsave(file.path(plots_dir, sprintf("plot_%02d.png", i)),
                  plots[[i]], width = 10, height = 6, dpi = 160)
}

message("Saved plots to: ", normalizePath(plots_dir))
