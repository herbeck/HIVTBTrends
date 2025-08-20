# ==========================================================
# HIV & TB Global/Country Data Sheets (IHME • UNAIDS • WHO)
# Locales: Global, South Africa, Zimbabwe, Malawi
# Output: outputs/HIV_TB_data_sheets.xlsx
# ==========================================================

# ---- 0) Packages ------------------------------------------------------------
pkgs <- c("httr", "jsonlite", "dplyr", "tidyr", "stringr",
          "readr", "purrr", "openxlsx", "tibble")
new <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

# UNAIDS helper (cleaned EDMS/AIDSinfo estimates)
if (!requireNamespace("mindthegap", quietly = TRUE)) {
  install.packages(c("mindthegap", "glitr"),
                   repos = c("https://usaid-oha-si.r-universe.dev", getOption("repos")))
}
library(mindthegap)

# ---- 1) Config --------------------------------------------------------------
locales <- tibble::tribble(
  ~locale,         ~iso3, ~who_name,      ~unaids_name,
  "Global",        NA,    "Global",       "Global",
  "South Africa",  "ZAF", "South Africa", "South Africa",
  "Zimbabwe",      "ZWE", "Zimbabwe",     "Zimbabwe",
  "Malawi",        "MWI", "Malawi",       "Malawi"
)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
out_file <- "outputs/HIV_TB_data_sheets.xlsx"

# ---- 2) Source: WHO TB (generateCSV) ---------------------------------------
# Docs/examples of this WHO TB CSV API: https://extranet.who.int/tme/generateCSV.asp (see public use & examples).
# WHO GHO OData API reference for general WHO data programmatic access. 
# (Metadata / OData examples: https://www.who.int/data/gho/info/gho-odata-api)

who_tb_fetch <- function(ds = c("estimates", "notifications")) {
  ds <- match.arg(ds)
  url <- paste0("https://extranet.who.int/tme/generateCSV.asp?ds=", ds)
  suppressMessages(readr::read_csv(url, show_col_types = FALSE, progress = FALSE))
}

# Pull TB estimates (core burden metrics)
tb_est <- who_tb_fetch("estimates")

# Keep only target geographies; WHO includes a "Global" row in `country`
tb_est_sel <- tb_est %>%
  filter(country %in% unique(locales$who_name)) %>%
  # standardize column names lower
  rename_with(~ gsub("\\s+", "_", tolower(.x)))

# Helpful label map for commonly used WHO TB variables (presence may vary by vintage)
tb_vars <- tibble::tribble(
  ~var,                         ~label,
  "e_inc_num",                  "TB incidence (number)",
  "e_inc_100k",                 "TB incidence rate (per 100,000)",
  "e_mort_exc_tbhiv_num",       "TB deaths (HIV-negative, number)",
  "e_mort_exc_tbhiv_100k",      "TB mortality rate (HIV-negative, per 100,000)",
  "e_mort_inc_tbhiv_num",       "TB deaths (incl. HIV, number)",
  "e_mort_inc_tbhiv_100k",      "TB mortality rate (incl. HIV, per 100,000)",
  "e_tbhiv_prct",               "Share of incident TB that are HIV-positive (%)"
)

# Reshape WHO TB (wide -> long), filter to label map
tb_est_long <- tb_est_sel %>%
  select(country, year, all_of(intersect(names(tb_est_sel), tb_vars$var))) %>%
  pivot_longer(-c(country, year), names_to = "var", values_to = "value") %>%
  inner_join(tb_vars, by = "var") %>%
  transmute(source = "WHO TB",
            indicator = label,
            geography = country,
            year = as.integer(year),
            value)

# ---- 3) Source: UNAIDS (mindthegap cleaned UNAIDS Estimates) ---------------
# mindthegap loads the current, cleaned UNAIDS EDMS/AIDSinfo release.
# Data dictionary (indicator names, structure): https://usaid-oha-si.github.io/mindthegap/articles/unaids-data-dictionary.html

# --- Robust UNAIDS loader that works without a GitHub token -------------------
safe_load_unaids <- function(pepfar_only = FALSE, verbose = TRUE) {
  if (!requireNamespace("mindthegap", quietly = TRUE)) {
    install.packages("mindthegap",
                     repos = c("https://usaid-oha-si.r-universe.dev", getOption("repos")))
  }
  if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  
  old_pat <- Sys.getenv("GITHUB_PAT", "")
  old_tok <- Sys.getenv("GITHUB_TOKEN", "")
  on.exit({
    if (nzchar(old_pat)) Sys.setenv(GITHUB_PAT = old_pat) else Sys.unsetenv("GITHUB_PAT")
    if (nzchar(old_tok)) Sys.setenv(GITHUB_TOKEN = old_tok) else Sys.unsetenv("GITHUB_TOKEN")
  }, add = TRUE)
  Sys.unsetenv("GITHUB_PAT"); Sys.unsetenv("GITHUB_TOKEN")
  
  try1 <- try(mindthegap::load_unaids(pepfar_only = pepfar_only), silent = TRUE)
  if (!inherits(try1, "try-error")) {
    attr(try1, "unaids_asset") <- NA_character_
    attr(try1, "unaids_url")   <- "mindthegap::load_unaids() (piggyback)"
    attr(try1, "retrieved_at") <- Sys.time()
    if (verbose) message("Loaded UNAIDS via mindthegap::load_unaids()")
    return(try1)
  }
  
  if (verbose) message("load_unaids() failed; discovering asset via Releases…")
  api <- "https://api.github.com/repos/USAID-OHA-SI/mindthegap/releases/latest"
  rel <- try(httr::GET(api, httr::user_agent("safe_load_unaids/1.3")), silent = TRUE)
  
  asset_name <- NULL; download_url <- NULL
  if (!inherits(rel, "try-error") && httr::status_code(rel) == 200L) {
    jj <- jsonlite::fromJSON(rawToChar(rel$content))
    nms  <- jj$assets$name
    urls <- jj$assets$browser_download_url
    pat  <- if (pepfar_only)
      "^UNAIDS_\\d{4}_Clean_Estimates_PEPFAR-only\\.rds$"
    else
      "^UNAIDS_\\d{4}_Clean_Estimates\\.rds$"
    idx <- which(grepl(pat, nms))
    if (length(idx)) {
      asset_name  <- nms[idx[1]]
      download_url <- urls[idx[1]]
    }
  }
  
  if (is.null(download_url)) {
    yrs <- as.integer(format(Sys.Date(), "%Y")):2019
    candidates <- if (pepfar_only)
      paste0("UNAIDS_", yrs, "_Clean_Estimates_PEPFAR-only.rds")
    else
      paste0("UNAIDS_", yrs, "_Clean_Estimates.rds")
    for (nm in candidates) {
      url  <- paste0("https://github.com/USAID-OHA-SI/mindthegap/releases/latest/download/", nm)
      head <- try(httr::HEAD(url, httr::user_agent("safe_load_unaids/1.3")), silent = TRUE)
      if (!inherits(head, "try-error") && httr::status_code(head) == 200L) {
        asset_name  <- nm
        download_url <- url
        break
      }
    }
  }
  
  if (is.null(download_url)) {
    stop("Unable to locate the UNAIDS release asset from GitHub (public). Try again later or set a fresh GitHub PAT.")
  }
  
  if (verbose) message("Downloading UNAIDS asset: ", asset_name)
  tf <- tempfile(fileext = ".rds")
  utils::download.file(download_url, tf, mode = "wb", quiet = !verbose)
  out <- readr::read_rds(tf)
  attr(out, "unaids_asset") <- asset_name
  attr(out, "unaids_url")   <- download_url
  attr(out, "retrieved_at") <- Sys.time()
  out
}



df_unaids <- safe_load_unaids(pepfar_only = FALSE)
#df_unaids <- mindthegap::load_unaids(pepfar_only = FALSE)

# Select common, high-value indicators (All ages, All sex) for consistent country/global sheets
hiv_indicators <- c(
  "Number PLHIV",
  "Number New HIV Infections",
  "Number AIDS Related Deaths",
  "Percent on ART of PLHIV",
  "Percent VLS of PLHIV",
  "Incidence (per 1,000)" # UNAIDS rate (per 1,000)
)

# Optional cascade of care indicators
#hiv_indicators <- c(hiv_indicators,
#"Percent of PLHIV who know their status",
#"Percent on ART of diagnosed PLHIV",
#"Percent VLS of those on ART")

hiv_unaids <- df_unaids %>%
  filter(country %in% unique(locales$unaids_name),
         indicator %in% hiv_indicators,
         age == "All", sex == "All") %>%
  transmute(source = "UNAIDS",
            indicator,
            geography = country,
            year,
            value = estimate)

# ---- 4) Source: IHME SDG API (HIV & TB incidence rates) --------------------
# IHME’s SDG API is key-based. Get a free key here and keep it private:
#   https://ghdx.healthdata.org/ihme-api  → https://api.healthdata.org/sdg (sign in)
# Basic usage & endpoints (GetIndicator, GetLocation, GetResultsByIndicator, etc.):
#   See examples and endpoint list here: https://bookdown.org/fede_gazzelloni/hmsidR/appendixA.html
#   API call pattern: GET https://api.healthdata.org/sdg/v1/GetResultsByIndicator?indicator_id=...&location_id=...&year=...
#   with header: Authorization: <YOUR-KEY>

ihme_base <- "https://api.healthdata.org/sdg/v1"
Sys.setenv(IHME_API_KEY = "p73i8m5wxsazxpbud5s9dob04swki16j")
ihme_key  <- Sys.getenv("IHME_API_KEY") # set this in your .Renviron for persistent use

ihme_get <- function(endpoint, query = list()) {
  if (ihme_key == "") {
    message("IHME_API_KEY not set. Skipping IHME pulls. To enable, set Sys.setenv(IHME_API_KEY='...') or add to .Renviron.")
    return(tibble())
  }
  url <- httr::modify_url(ihme_base, path = paste0("sdg/v1/", sub("^/+", "", endpoint)), query = query)
  # The bookdown example uses Authorization header directly with the base /sdg/v1 path:
  url <- paste0(ihme_base, "/", endpoint)
  res <- httr::GET(url, httr::add_headers(Authorization = ihme_key))
  stop_for_status(res)
  jj <- jsonlite::fromJSON(rawToChar(res$content), simplifyVector = TRUE)
  as_tibble(jj$results)
}

# Pull indicator catalog, then locate HIV/TB incidence-rate indicators by name
ind_catalog <- tryCatch(ihme_get("GetIndicator"), error = function(e) tibble())
loc_catalog <- tryCatch(ihme_get("GetLocation"),  error = function(e) tibble())

# Helper to find indicator IDs by a regex over the indicator name
find_indicator_id <- function(pattern) {
  if (nrow(ind_catalog) == 0) return(NA_character_)
  id <- ind_catalog %>%
    mutate(nm = ifelse(!is.na(indicator_name), indicator_name, "")) %>%
    filter(grepl(pattern, nm, ignore.case = TRUE)) %>%
    arrange(indicator_id) %>%
    slice(1) %>%
    pull(indicator_id)
  as.character(id %||% NA_character_)
}

# Common SDG indicators for HIV & TB incidence rates (wording may vary; choose best match)
ihme_hiv_inc_id <- find_indicator_id("HIV.*incidence")
ihme_tb_inc_id  <- find_indicator_id("tuberculosis.*incidence|TB.*incidence")

# Map our locales to IHME location IDs
get_loc_id <- function(name) {
  if (nrow(loc_catalog) == 0) return(NA_character_)
  
  # Try exact name (case-insensitive), then "contains", then special-case Global
  hits <- loc_catalog %>%
    dplyr::filter(tolower(location_name) == tolower(name))
  
  if (nrow(hits) == 0) {
    hits <- loc_catalog %>%
      dplyr::filter(grepl(name, location_name, ignore.case = TRUE))
  }
  
  if (nrow(hits) == 0 && grepl("^global$", name, ignore.case = TRUE)) {
    hits <- loc_catalog %>%
      dplyr::filter(grepl("^global", location_name, ignore.case = TRUE)) %>%
      dplyr::arrange(nchar(location_name))  # prefer the shortest "Global..." label
  }
  
  if (nrow(hits) == 0) return(NA_character_)
  as.character(hits$location_id[[1]])
}


ihme_loc_map <- locales %>%
  dplyr::rowwise() %>%
  dplyr::mutate(ihme_location_id = get_loc_id(locale)) %>%
  dplyr::ungroup()

# (Optional) see which locales didn't match IHME
missing <- ihme_loc_map %>% dplyr::filter(is.na(ihme_location_id))
if (nrow(missing) > 0) message("IHME location not found for: ",
                               paste(missing$locale, collapse = ", "))

# Years: pull full time series if available; otherwise most recent
years_to_pull <- NULL  # NULL = let API return all

pull_ihme_series <- function(ind_id, loc_id, years = years_to_pull) {
  if (is.na(ind_id) || is.na(loc_id) || ind_id == "" || loc_id == "") return(tibble())
  q <- list(indicator_id = ind_id, location_id = loc_id)
  if (!is.null(years)) q$year <- paste(years, collapse = ",")
  df <- tryCatch(ihme_get(paste0("GetResultsByIndicator?indicator_id=", ind_id,
                                 "&location_id=", loc_id,
                                 if (!is.null(years)) paste0("&year=", paste(years, collapse = ",")) else "")),
                 error = function(e) tibble())
  if (nrow(df) == 0) return(df)
  df %>%
    transmute(source = "IHME SDG",
              indicator = indicator_name,
              geography = location_name,
              year = as.integer(year),
              value = mean_estimate)
}

ihme_series <- list()

if (!is.na(ihme_hiv_inc_id)) {
  ihme_series[["HIV"]] <- ihme_loc_map %>%
    mutate(dat = purrr::map(ihme_location_id, ~ pull_ihme_series(ihme_hiv_inc_id, .x))) %>%
    pull(dat) %>% bind_rows()
}

if (!is.na(ihme_tb_inc_id)) {
  ihme_series[["TB"]] <- ihme_loc_map %>%
    mutate(dat = purrr::map(ihme_location_id, ~ pull_ihme_series(ihme_tb_inc_id, .x))) %>%
    pull(dat) %>% bind_rows()
}

ihme_all <- bind_rows(ihme_series)

# ---- 5) Harmonize & build per-locale sheets --------------------------------
# Unified long format with (source, indicator, geography, year, value)
all_long <- bind_rows(
  hiv_unaids,
  ihme_all,
  tb_est_long
) %>%
  # Limit to our 4 geographies (IHME names might not exactly equal our aliases; align)
  mutate(geography = case_when(
    grepl("^Global$", geography, ignore.case = TRUE) ~ "Global",
    grepl("South Africa", geography, ignore.case = TRUE) ~ "South Africa",
    grepl("Zimbabwe", geography, ignore.case = TRUE) ~ "Zimbabwe",
    grepl("Malawi", geography, ignore.case = TRUE) ~ "Malawi",
    TRUE ~ geography
  )) %>%
  filter(geography %in% locales$locale)

# Convenience: indicator ordering
ind_order <- c(
  # UNAIDS
  "Number PLHIV",
  "Number New HIV Infections",
  "Number AIDS Related Deaths",
  "Percent on ART of PLHIV",
  "Percent VLS of PLHIV",
  "Incidence (per 1,000)",
  # IHME SDG (names returned by the API; keep original string)
  unique(all_long$indicator[all_long$source == "IHME SDG"]),
  # WHO TB labels
  tb_vars$label
) %>% unique() %>% .[!is.na(.)]

all_long <- all_long %>%
  mutate(indicator = factor(indicator, levels = ind_order))

# ---- 6) Write Excel workbook -----------------------------------------------
wb <- openxlsx::createWorkbook()

write_geo_sheet <- function(geo) {
  df <- all_long %>%
    filter(geography == geo) %>%
    arrange(indicator, year, source)
  
  # Pivot wide by indicator, keeping source as a suffix if duplicates
  # Build a composite name "indicator [source]" when needed
  ind_source_counts <- df %>% count(indicator, source)
  df2 <- df %>%
    mutate(ind_key = ifelse(duplicated(indicator) | duplicated(indicator, fromLast = TRUE),
                            paste0(as.character(indicator), " [", source, "]"),
                            as.character(indicator))) %>%
    select(year, ind_key, value) %>%
    pivot_wider(names_from = ind_key, values_from = value) %>%
    arrange(year)
  
  openxlsx::addWorksheet(wb, sheetName = paste0(ifelse(grepl("^HIV", geo), geo, ""), "")) # placeholder; we’ll set a proper name below
  sheet_name <- paste(ifelse(any(grepl("HIV", names(df2), ignore.case = TRUE) |
                                   grepl("UNAIDS", names(df2), ignore.case = TRUE)), "HIV", "TB"),
                      "-", geo)
  # Ensure unique and <= 31 chars
  sheet_name <- substr(sheet_name, 1, 31)
  if (sheet_name %in% openxlsx::sheets(wb)) {
    # make unique if collision
    suffix <- sum(openxlsx::sheets(wb) == sheet_name) + 1
    sheet_name <- substr(paste0(sheet_name, " ", suffix), 1, 31)
  }
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df2)
  # delete the placeholder sheet added at first call
  ph <- setdiff(openxlsx::sheets(wb), sheet_name)
  if (length(ph) == 1 && ph == "") openxlsx::removeWorksheet(wb, ph)
}

# We’ll create 8 sheets explicitly for clarity:
make_sheet <- function(title, filt_fun) {
  # Build the long data for this sheet
  df <- filt_fun() %>% dplyr::arrange(year, indicator, source)
  
  # Only add [source] when an indicator appears from >1 source
  df_w <- df %>%
    dplyr::group_by(indicator) %>%
    dplyr::mutate(ind_key = dplyr::if_else(dplyr::n_distinct(source) > 1,
                                           paste0(indicator, " [", source, "]"),
                                           as.character(indicator))) %>%
    dplyr::ungroup() %>%
    dplyr::select(year, ind_key, value) %>%
    tidyr::pivot_wider(names_from = ind_key, values_from = value) %>%
    dplyr::arrange(year)
  
  sheet_name <- substr(title, 1, 31)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, df_w)
  openxlsx::freezePane(wb, sheet_name, firstActiveRow = 2, firstActiveCol = 2)
  openxlsx::addFilter(wb, sheet_name, row = 1, cols = 1:ncol(df_w))
}


make_sheet("HIV - Global",        function() filter(hiv_unaids, geography == "Global") %>% bind_rows(ihme_all %>% filter(geography=="Global" & grepl("HIV", indicator, ignore.case=TRUE))))
make_sheet("HIV - South Africa",  function() filter(hiv_unaids, geography == "South Africa") %>% bind_rows(ihme_all %>% filter(geography=="South Africa" & grepl("HIV", indicator, ignore.case=TRUE))))
make_sheet("HIV - Zimbabwe",      function() filter(hiv_unaids, geography == "Zimbabwe") %>% bind_rows(ihme_all %>% filter(geography=="Zimbabwe" & grepl("HIV", indicator, ignore.case=TRUE))))
make_sheet("HIV - Malawi",        function() filter(hiv_unaids, geography == "Malawi") %>% bind_rows(ihme_all %>% filter(geography=="Malawi" & grepl("HIV", indicator, ignore.case=TRUE))))

make_sheet("TB - Global",         function() filter(tb_est_long, geography == "Global") %>% bind_rows(ihme_all %>% filter(geography=="Global" & grepl("tuberculosis|TB", indicator, ignore.case=TRUE))))
make_sheet("TB - South Africa",   function() filter(tb_est_long, geography == "South Africa") %>% bind_rows(ihme_all %>% filter(geography=="South Africa" & grepl("tuberculosis|TB", indicator, ignore.case=TRUE))))
make_sheet("TB - Zimbabwe",       function() filter(tb_est_long, geography == "Zimbabwe") %>% bind_rows(ihme_all %>% filter(geography=="Zimbabwe" & grepl("tuberculosis|TB", indicator, ignore.case=TRUE))))
make_sheet("TB - Malawi",         function() filter(tb_est_long, geography == "Malawi") %>% bind_rows(ihme_all %>% filter(geography=="Malawi" & grepl("tuberculosis|TB", indicator, ignore.case=TRUE))))

prov <- dplyr::tibble(
  source = c("UNAIDS", "UNAIDS", "WHO TB", "WHO TB", "IHME SDG", "IHME SDG", "Run"),
  field  = c("asset_name", "download_url", "estimates_endpoint", "notes",
             "indicator_id (HIV incidence)", "indicator_id (TB incidence)", "timestamp_utc"),
  value  = c(attr(df_unaids, "unaids_asset") %||% "mindthegap::load_unaids()",
             attr(df_unaids, "unaids_url")   %||% "piggyback (repo releases)",
             "https://extranet.who.int/tme/generateCSV.asp?ds=estimates",
             "WHO TB: public CSV API (burden estimates)",
             ihme_hiv_inc_id %||% "skipped (no IHME_API_KEY)",
             ihme_tb_inc_id  %||% "skipped (no IHME_API_KEY)",
             format(Sys.time(), tz = "UTC"))
)

openxlsx::addWorksheet(wb, "Provenance")
openxlsx::writeData(wb, "Provenance", prov)

openxlsx::saveWorkbook(wb, out_file, overwrite = TRUE)

message("Wrote: ", normalizePath(out_file))
# ==========================================================
