# ============================================================
# IHME GBD 2021 Data Processor
# Transform IHME data to match UNAIDS output format
# - Loads IHME_GBD_2021.xlsx 
# - Filters for target countries and HIV/TB data
# - Creates 2-row header structure like UNAIDS output
# - Uses "Estimate" for all values (no Low/High uncertainty)
# - Output: IHME_GBD_subset_combined.xlsx
# ============================================================

suppressPackageStartupMessages({
  library(readr)  # For reading CSV
  library(openxlsx)
  library(dplyr)
  library(tidyr)
})

# ---- Input and output files
input_file <- "input_data/IHME_GBD_2021_HIV_TB.csv"
output_file <- "IHME_GBD_subset_combined.xlsx"

# ---- Target countries (checking what's available in the data)
target_countries <- c("Global", "South Africa", "Zimbabwe", "Malawi", "Nigeria", "Kenya", "Mozambique")

cat("=== IHME GBD 2021 Data Processor ===\n")
cat("Input file:", input_file, "\n")
cat("Target countries:", paste(target_countries, collapse = ", "), "\n\n")

# ---- Read the IHME data
cat("Reading IHME GBD CSV data...\n")
ihme_data <- read.csv(input_file, stringsAsFactors = FALSE)

# Display data structure
cat("Data structure:\n")
cat("Columns:", paste(names(ihme_data), collapse = ", "), "\n")
cat("Total rows:", nrow(ihme_data), "\n")
cat("Unique locations:", paste(sort(unique(ihme_data$location)), collapse = ", "), "\n")
cat("Unique measures:", paste(unique(ihme_data$measure), collapse = ", "), "\n")
cat("Unique causes:", paste(unique(ihme_data$cause), collapse = ", "), "\n")
cat("Year range:", min(ihme_data$year), "to", max(ihme_data$year), "\n\n")

# Check which target countries are actually available
available_countries <- intersect(target_countries, unique(ihme_data$location))
missing_countries <- setdiff(target_countries, unique(ihme_data$location))

cat("Countries available:", paste(available_countries, collapse = ", "), "\n")
if (length(missing_countries) > 0) {
  cat("Countries missing:", paste(missing_countries, collapse = ", "), "\n")
}
cat("\n")

# ---- Filter for target countries and HIV/TB
cat("Filtering data...\n")
filtered_data <- ihme_data %>%
  filter(
    location %in% available_countries,  # Use only available countries
    cause %in% c("HIV/AIDS", "Tuberculosis"),
    year >= 1990 & year <= 2021  # IHME data typically goes to 2021
  ) %>%
  arrange(location, year, cause, measure, sex, age, metric)

cat("Filtered data:\n")
cat("Rows after filtering:", nrow(filtered_data), "\n")
cat("Countries found:", paste(unique(filtered_data$location), collapse = ", "), "\n")
cat("Years available:", min(filtered_data$year), "to", max(filtered_data$year), "\n\n")

# ---- Transform to wide format to match UNAIDS structure
cat("Transforming to wide format...\n")

# Create a combined indicator name from measure, cause, sex, age, and metric
filtered_data <- filtered_data %>%
  mutate(
    # Clean up indicator names
    clean_measure = case_when(
      measure == "Deaths" ~ "deaths",
      measure == "Prevalence" ~ "prevalence", 
      measure == "Incidence" ~ "incidence",
      TRUE ~ tolower(measure)
    ),
    clean_cause = case_when(
      cause == "HIV/AIDS" ~ "hiv",
      cause == "Tuberculosis" ~ "tb",
      TRUE ~ tolower(cause)
    ),
    clean_sex = case_when(
      sex == "Both" ~ "both_sexes",
      sex == "Male" ~ "male",
      sex == "Female" ~ "female", 
      TRUE ~ tolower(sex)
    ),
    clean_age = case_when(
      age == "All ages" ~ "all_ages",
      age == "15-49 years" ~ "15_49",
      age == "Under 5" ~ "under_5",
      TRUE ~ gsub("[^A-Za-z0-9]", "_", tolower(age))
    ),
    clean_metric = case_when(
      metric == "Number" ~ "number",
      metric == "Rate" ~ "rate",
      metric == "Percent" ~ "percent",
      TRUE ~ tolower(metric)
    ),
    # Create combined indicator name
    indicator = paste(clean_cause, clean_measure, clean_sex, clean_age, clean_metric, sep = "_")
  )

# Pivot to wide format
wide_data <- filtered_data %>%
  select(year, location, indicator, val) %>%
  pivot_wider(
    names_from = indicator,
    values_from = val,
    values_fn = mean, # In case of duplicates, take mean
    names_sort = TRUE
  ) %>%
  arrange(location, year)

cat("Wide format created:\n")
cat("Dimensions:", nrow(wide_data), "rows x", ncol(wide_data), "columns\n")
cat("Indicators created:", ncol(wide_data) - 2, "\n\n")

# ---- Create UNAIDS-style headers
cat("Creating UNAIDS-style headers...\n")

# Get indicator names (excluding year and location)
indicator_names <- names(wide_data)[3:ncol(wide_data)]

# Create two-row header structure
# Header 1: Group indicators by pathogen and measure
header1 <- c("", "", "", rep("", length(indicator_names)))
header2 <- c("", "", "", rep("Estimate", length(indicator_names)))

# Improve header1 by extracting meaningful groupings
for (i in seq_along(indicator_names)) {
  ind <- indicator_names[i]
  
  # Extract pathogen and measure
  parts <- strsplit(ind, "_")[[1]]
  pathogen <- toupper(parts[1])  # HIV or TB
  measure <- tools::toTitleCase(parts[2])  # Deaths, Prevalence, Incidence
  
  # Create a descriptive header
  if (length(parts) >= 5) {
    sex_part <- tools::toTitleCase(gsub("_", " ", parts[3]))
    age_part <- gsub("_", "-", parts[4])
    metric_part <- tools::toTitleCase(parts[5])
    
    header1[i + 3] <- paste0(pathogen, " ", measure, " (", sex_part, ", ", age_part, ", ", metric_part, ")")
  } else {
    header1[i + 3] <- paste0(pathogen, " ", measure)
  }
}

# Set the first three columns
header1[1] <- ""
header1[2] <- "" 
header1[3] <- ""
header2[1] <- ""
header2[2] <- ""
header2[3] <- ""

# ---- Prepare final data structure
cat("Preparing final data structure...\n")

# Add country codes (simplified mapping - will be updated based on actual countries)
country_codes <- list(
  "Global" = "GLOBAL",
  "South Africa" = "ZAF",
  "Zimbabwe" = "ZWE", 
  "Malawi" = "MWI",
  "Nigeria" = "NGA",
  "Kenya" = "KEN",
  "Mozambique" = "MOZ",
  "Zambia" = "ZMB"
)

# Use only codes for available countries
available_codes <- country_codes[names(country_codes) %in% available_countries]

# Prepare the data block with year, code, and country name
final_data <- wide_data %>%
  mutate(
    country_code = available_codes[location],
    .before = location
  ) %>%
  select(year, country_code, location, everything()) %>%
  rename(country_name = location)

# Convert to character matrix for writing
final_data_char <- final_data
final_data_char[] <- lapply(final_data_char, function(x) {
  x_char <- as.character(x)
  x_char[is.na(x_char)] <- "NA"
  return(x_char)
})

# Combine headers and data
final_sheet <- rbind(
  header1,
  header2, 
  as.matrix(final_data_char)
)

# Convert to character matrix
final_sheet <- apply(final_sheet, 2, as.character)

cat("Final structure:\n")
cat("Total rows (including headers):", nrow(final_sheet), "\n")
cat("Total columns:", ncol(final_sheet), "\n")
cat("Data rows:", nrow(final_data_char), "\n")
cat("Countries in final data:", paste(unique(final_data_char$country_name), collapse = ", "), "\n\n")

# ---- Write to Excel
cat("Writing to Excel...\n")

wb <- createWorkbook()
sheet_name <- "IHME_GBD_Combined"
addWorksheet(wb, sheet_name)

# Write the data
writeData(wb, sheet_name, final_sheet, colNames = FALSE, rowNames = FALSE)

# Format the worksheet
freezePane(wb, sheet_name, firstActiveRow = 3, firstActiveCol = 4)  # Freeze at data start
setColWidths(wb, sheet_name, cols = 1:ncol(final_sheet), widths = "auto")

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

# ---- Summary
cat("=== PROCESSING COMPLETE ===\n")
cat("Output file:", normalizePath(output_file), "\n")
cat("Final data summary:\n")
cat("- Countries:", length(unique(final_data_char$country_name)), "\n")
cat("- Years:", min(final_data_char$year), "to", max(final_data_char$year), "\n") 
cat("- Indicators:", ncol(final_data_char) - 3, "\n")
cat("- Total data rows:", nrow(final_data_char), "\n")

# Show sample of what was created
cat("\nSample of final data (first 5 rows, first 8 columns):\n")
print(final_data_char[1:min(5, nrow(final_data_char)), 1:min(8, ncol(final_data_char))])

cat("\nSample of indicators created:\n")
sample_indicators <- head(names(final_data_char)[4:ncol(final_data_char)], 10)
cat(paste(sample_indicators, collapse = "\n"), "\n")

message("\nSuccessfully created: ", output_file)
