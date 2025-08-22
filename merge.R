# merge_to_one_sheet_fixed.R
# Merge:
#   - UNAIDS_subset_combined.xlsx
#   - WHO_TB_to_UNAIDS_like.xlsx
#   - IHME_to_UNAIDS_like.xlsx
# into ONE sheet by Year + Location, with rock-solid handling for duplicate headers.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(openxlsx)
  library(purrr)
  library(rlang)
})

# ---------- Helpers ----------

# Make names unique, readable, and stable
make_names_unique <- function(x) {
  x <- gsub("\\s+", " ", x)        # squeeze whitespace
  x <- trimws(x)
  x <- ifelse(nzchar(x), x, "Unnamed")
  make.unique(x, sep = " __dup")
}

# Read a two-row header (row 1 = indicator name/group, row 2 = subheader),
# then data from row 3 onward. Build unique, source-prefixed names:
#  - First 3 columns forced to: Year, ISO_<SRC>, Location
#  - Others: "<SRC> | <Top> | <Sub>" (default sublabel 'Estimate' if blank)
read_two_row_header <- function(path, sheet = 1, src_prefix = "SRC") {
  # top & sub headers
  hdr1 <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = 1)
  hdr2 <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, skip = 1, n_max = 1)
  # data starts after two rows
  dat  <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, skip = 2)
  
  ncol_all <- max(ncol(hdr1), ncol(hdr2), ncol(dat))
  pad_cols <- function(df, n) { if (ncol(df) < n) df[, (ncol(df)+1):n] <- NA; df }
  hdr1 <- pad_cols(hdr1, ncol_all)
  hdr2 <- pad_cols(hdr2, ncol_all)
  dat  <- pad_cols(dat,  ncol_all)
  
  top <- as.character(hdr1[1, ])
  sub <- as.character(hdr2[1, ])
  top[is.na(top)] <- ""
  sub[is.na(sub)] <- ""
  
  nm <- character(ncol_all)
  nm[1:3] <- c("Year", paste0("ISO_", src_prefix), "Location")
  
  if (ncol_all > 3) {
    for (j in 4:ncol_all) {
      t <- str_squish(top[j]); s <- str_squish(sub[j])
      if (!nzchar(t) && !nzchar(s)) {
        nm[j] <- paste(src_prefix, "Unnamed", j, sep = " | ")
      } else if (nzchar(t) && nzchar(s)) {
        nm[j] <- paste(src_prefix, t, s, sep = " | ")
      } else if (nzchar(t)) {
        nm[j] <- paste(src_prefix, t, "Estimate", sep = " | ")
      } else { # only sub present
        nm[j] <- paste(src_prefix, s, "Estimate", sep = " | ")
      }
    }
  }
  
  # Enforce unique, stable names (avoids mutate() duplicate-name error)
  nm <- make_names_unique(nm)
  names(dat) <- nm
  
  # Coerce key columns; tolerate missing columns gracefully
  if ("Year" %in% names(dat)) dat$Year <- suppressWarnings(as.integer(dat$Year))
  if ("Location" %in% names(dat)) dat$Location <- as.character(dat$Location)
  iso_col <- paste0("ISO_", src_prefix)
  if (iso_col %in% names(dat)) dat[[iso_col]] <- as.character(dat[[iso_col]])
  
  # Drop columns that are entirely NA (except keys)
  keep <- c("Year", "Location", iso_col)
  drop_candidates <- setdiff(names(dat), keep)
  if (length(drop_candidates)) {
    all_na <- vapply(dat[drop_candidates], function(v) all(is.na(v)), logical(1))
    dat <- dat[, c(keep, drop_candidates[!all_na]), drop = FALSE]
  }
  
  # Deduplicate rows on keys if necessary
  dat <- dat %>% distinct(Year, Location, .keep_all = TRUE)
  
  dat
}

# ---------- Inputs / Outputs ----------
in_unaids <- "spreadsheets/UNAIDS_subset_combined.xlsx"
in_who    <- "spreadsheets/WHO_TB_to_UNAIDS_like.xlsx"
in_ihme   <- "spreadsheets/IHME_to_UNAIDS_like.xlsx"
out_xlsx  <- "spreadsheets/UNAIDS_WHO_IHME.xlsx"

stopifnot(file.exists(in_unaids), file.exists(in_who), file.exists(in_ihme))

# ---------- Load & normalize ----------
df_u <- read_two_row_header(in_unaids, sheet = 1, src_prefix = "UNAIDS")
df_w <- read_two_row_header(in_who,    sheet = 1, src_prefix = "WHO TB")
df_i <- read_two_row_header(in_ihme,   sheet = 1, src_prefix = "IHME")

# ---------- Merge by Year + Location ----------
merged <- df_u %>%
  full_join(df_w, by = c("Year", "Location")) %>%
  full_join(df_i, by = c("Year", "Location"))

# Build consolidated ISO (coalesce across any ISO_* columns)
iso_cols <- grep("^ISO_", names(merged), value = TRUE)
if (length(iso_cols)) {
  merged <- merged %>%
    mutate(ISO = coalesce(!!!syms(iso_cols))) %>%
    select(Year, ISO, Location, everything(), -all_of(iso_cols))
} else {
  merged <- merged %>% mutate(ISO = NA_character_) %>% relocate(ISO, .after = Year)
}

# Sort rows
merged <- merged %>% arrange(Year, Location)

# Optional: put columns in readable blocks (UNAIDS | …, WHO TB | …, IHME | …)
block_order <- c("UNAIDS \\|", "WHO TB \\|", "IHME \\|")
indicator_cols <- setdiff(names(merged), c("Year", "ISO", "Location"))
ordered_blocks <- unlist(lapply(block_order, function(p) grep(p, indicator_cols, value = TRUE)))
indicator_cols <- c(ordered_blocks, setdiff(indicator_cols, ordered_blocks))

merged <- merged %>% select(Year, ISO, Location, all_of(indicator_cols))

# ---------- Write one-sheet workbook ----------
wb <- createWorkbook()
addWorksheet(wb, "Merged")
writeData(wb, "Merged", merged, startRow = 1, startCol = 1, colNames = TRUE)
freezePane(wb, "Merged", firstActiveRow = 2, firstActiveCol = 4)
setColWidths(wb, "Merged", cols = 1:ncol(merged), widths = "auto")
saveWorkbook(wb, out_xlsx, overwrite = TRUE)
message("Wrote: ", out_xlsx)
