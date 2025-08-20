# ============================================================
# Subset UNAIDS Excel and CONCATENATE Test-&-Treat columns
# onto the Estimates tab (preserve two header rows)
# - Downloads from UNAIDS URL
# - Sheets used: "HIV2025Estimates_ByYear", "HIV-Test-&-Treat_ByYear"
# - Locations kept: Global, South Africa, Zimbabwe, Malawi
# - Data blanks -> "NA" (headers are kept as-is)
# - Output: datasheets/UNAIDS/UNAIDS_subset_combined.xlsx
#           (single sheet: "HIV_ByYear_Combined")
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(openxlsx)
})

# ---- Source & output
excel_url <- "https://www.unaids.org/sites/default/files/2025-07/HIV_estimates_from_1990-to-2025.xlsx"
out_xlsx  <- file.path("datasheets_UNAIDS","UNAIDS_subset_combined.xlsx")
dir.create(dirname(out_xlsx), recursive = TRUE, showWarnings = FALSE)

# ---- Sheets and locations to keep
sheet_est <- "HIV2025Estimates_ByYear"
sheet_tt  <- "HIV-Test-&-Treat_ByYear"
locations <- c("Global","South Africa","Zimbabwe","Malawi")

# ---- Known layout for this workbook:
# Two header rows at Excel rows 5 and 6 (1-based indexing).
# Data begins at row 7.
# The first three columns are: Year (col 1), Code (col 2), Name/Location (col 3).
header1_row <- 5
header2_row <- 6
year_col    <- 1
code_col    <- 2
name_col    <- 3
first_ind_col <- 4  # first indicator column

# ---- Download workbook
tmp_xlsx <- tempfile(fileext = ".xlsx")
tryCatch({
  utils::download.file(excel_url, tmp_xlsx, mode = "wb", quiet = TRUE)
}, error = function(e) {
  stop("Failed to download UNAIDS workbook from: ", excel_url, "\n", conditionMessage(e))
})

# ---- Helper: subset one sheet while preserving headers
subset_sheet <- function(path, sheet_name) {
  raw <- readxl::read_excel(path, sheet = sheet_name, col_names = FALSE)
  if (!is.data.frame(raw) || nrow(raw) < header2_row + 1) {
    stop("Sheet '", sheet_name, "' does not have the expected header rows.")
  }
  
  header_block <- raw[1:header2_row, , drop = FALSE]
  data_block   <- raw[(header2_row + 1):nrow(raw), , drop = FALSE]
  
  # Keep only requested locations
  nm <- as.character(data_block[[name_col]])
  keep_idx <- which(!is.na(nm) & nm %in% locations)
  
  # If nothing matches, still return header + empty data
  if (length(keep_idx) == 0) {
    return(list(
      header1 = as.character(unlist(header_block[header1_row, , drop = TRUE])),
      header2 = as.character(unlist(header_block[header2_row, , drop = TRUE])),
      data    = data_block[0, , drop = FALSE]
    ))
  }
  
  data_sub <- data_block[keep_idx, , drop = FALSE]
  
  # Replace blanks/NA in DATA rows with "NA" (headers left untouched)
  data_sub[] <- lapply(data_sub, function(x) {
    y <- as.character(x)
    y[is.na(y) | y == ""] <- "NA"
    y
  })
  
  list(
    header1 = as.character(unlist(header_block[header1_row, , drop = TRUE])),
    header2 = as.character(unlist(header_block[header2_row, , drop = TRUE])),
    data    = data_sub
  )
}

# ---- Read and subset both sheets
est <- subset_sheet(tmp_xlsx, sheet_est)
tt  <- subset_sheet(tmp_xlsx, sheet_tt)

# ---- Align Test-&-Treat rows to Estimates by (Year, Name)
# Build keys based on Year & Name from the DATA rows
key_est <- paste0(est$data[[year_col]], "||", est$data[[name_col]])
key_tt  <- if (nrow(tt$data)) paste0(tt$data[[year_col]], "||", tt$data[[name_col]]) else character(0)

# Reorder Test-&-Treat to match Estimates; fill missing with NA matrices if needed
if (nrow(tt$data)) {
  idx <- match(key_est, key_tt)
  tt_ind <- tt$data[, seq.int(first_ind_col, ncol(tt$data)), drop = FALSE]
  tt_aligned <- tt_ind[idx, , drop = FALSE]
  # rows not found in Test-&-Treat -> fill with "NA"
  na_rows <- which(is.na(idx))
  if (length(na_rows)) {
    tt_aligned[na_rows, ] <- "NA"
  }
} else {
  # No rows in TT for these locations; create all "NA" placeholder columns
  tt_aligned <- as.data.frame(matrix("NA", nrow = nrow(est$data),
                                     ncol = max(0, ncol(tt$data) - (first_ind_col - 1))))
}

# ---- Build combined headers: keep first 3 columns from Estimates; then append
# Estimates indicator columns, then Test-&-Treat indicator columns
h1_est <- est$header1; h2_est <- est$header2
h1_tt  <- tt$header1;  h2_tt  <- tt$header2

# Safety: pad header vectors to the same maximum width they appear in data frames
pad_to <- function(v, n) { c(v, rep("", max(0, n - length(v)))) }
h1_est <- pad_to(h1_est, ncol(est$data))
h2_est <- pad_to(h2_est, ncol(est$data))
h1_tt  <- pad_to(h1_tt,  ncol(tt$data))
h2_tt  <- pad_to(h2_tt,  ncol(tt$data))

header1_comb <- c(h1_est[seq_len(name_col)], h1_est[first_ind_col:ncol(est$data)],
                  if (ncol(tt$data) >= first_ind_col) h1_tt[first_ind_col:ncol(tt$data)] else NULL)
header2_comb <- c(h2_est[seq_len(name_col)], h2_est[first_ind_col:ncol(est$data)],
                  if (ncol(tt$data) >= first_ind_col) h2_tt[first_ind_col:ncol(tt$data)] else NULL)

# ---- Combine DATA: first 3 ID columns from Estimates, then both indicator blocks
est_ids   <- est$data[, seq_len(name_col), drop = FALSE]
est_ind   <- est$data[, seq.int(first_ind_col, ncol(est$data)), drop = FALSE]
comb_data <- cbind(est_ids, est_ind, tt_aligned)

# Ensure everything is character for faithful Excel writing
comb_data[] <- lapply(comb_data, as.character)

# ---- Assemble the final sheet: two header rows + data
final_sheet <- rbind(header1_comb, header2_comb, as.matrix(comb_data))
final_sheet <- apply(final_sheet, 2, as.character)  # keep as character matrix

# ---- Write single-sheet workbook
wb <- openxlsx::createWorkbook()
sheet_name <- "HIV_ByYear_Combined"  # change if you prefer the original tab name
openxlsx::addWorksheet(wb, sheet_name)
openxlsx::writeData(wb, sheet_name, final_sheet, colNames = FALSE, rowNames = FALSE)
openxlsx::freezePane(wb, sheet_name, firstActiveRow = header2_row + 1, firstActiveCol = first_ind_col)
openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol(final_sheet), widths = "auto")
openxlsx::saveWorkbook(wb, out_xlsx, overwrite = TRUE)

message("Wrote: ", normalizePath(out_xlsx))
