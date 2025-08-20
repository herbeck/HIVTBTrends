# ============================================================
# WHO TB datasheets (countries + Global)
# Geos: South Africa (ZAF), Zimbabwe (ZWE), Malawi (MWI), Global
# Output under datasheets/<ZAF|ZWE|MWI|Global>/WHO/
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(readr); library(dplyr); library(purrr); library(stringr)
})

countries <- c("ZAF" = "South Africa", "ZWE" = "Zimbabwe", "MWI" = "Malawi")
ISO3S <- names(countries)
GLOBAL_LABEL <- "Global"
name_to_iso <- setNames(names(countries), unname(countries)) # "South Africa"="ZAF", etc.

out_root <- "datasheets"
dir.create(out_root, showWarnings = FALSE)

write_out <- function(df, file) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, file)
}

message("Downloading WHO TB estimates ...")
tb_est_url <- "https://extranet.who.int/tme/generateCSV.asp?ds=estimates"
tb_raw <- {
  resp <- httr::GET(tb_est_url, httr::timeout(120))
  httr::stop_for_status(resp)
  readr::read_csv(httr::content(resp, "raw"), show_col_types = FALSE, progress = FALSE)
}

# Keep standard fields + region to detect global reliably
tb_keep <- c(
  "country","iso3","g_whoregion","year",
  "e_inc_num","e_inc_100k",
  "e_mort_num","e_mort_100k",
  "e_mort_tbhiv_num","e_mort_tbhiv_100k",
  "e_inc_tbhiv_num","e_inc_tbhiv_100k"
)

tb_df <- tb_raw %>%
  select(any_of(tb_keep)) %>%
  mutate(
    country      = trimws(country),
    iso3         = toupper(iso3),
    g_whoregion  = toupper(g_whoregion),
    country_uc   = toupper(country),
    geo = case_when(
      # various ways WHO encodes global rows
      country_uc %in% c("WORLD","GLOBAL") ~ GLOBAL_LABEL,
      iso3 %in% c("WLD","GLB","GLOBAL")   ~ GLOBAL_LABEL,
      g_whoregion == "GLOBAL"             ~ GLOBAL_LABEL,
      # our three countries
      iso3 %in% ISO3S                     ~ iso3,
      country %in% unname(countries)      ~ name_to_iso[country],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(geo %in% c(ISO3S, GLOBAL_LABEL)) %>%   # keep only our geos + Global
  arrange(geo, year) %>%
  select(-country_uc)

# Write TB by geo (countries + Global)
for (g in c(ISO3S, GLOBAL_LABEL)) {
  sub <- tb_df %>% filter(geo == g)
  if (nrow(sub) == 0) next
  #latest <- sub %>% filter(year == max(year, na.rm = TRUE))
  base   <- file.path(out_root, g, "WHO")
  write_out(sub,    file.path(base, "tb_timeseries.csv"))
  #write_out(latest, file.path(base, "tb_latest.csv"))
}

message("WHO TB files written under: ", normalizePath(out_root))
