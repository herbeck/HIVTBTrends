# =============================================================================
# UNAIDS HIV Data Cleaner Script
# Purpose: Clean UNAIDS HIV estimates file with multi-row headers
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# Function to clean UNAIDS HIV estimates data including ART cascade indicators
clean_unaids_hiv_data <- function(input_file = "HIV_estimates_from_1990to2025.xlsx", 
                                  output_file = "UNAIDS_HIV_cleaned.xlsx") {
  
  cat("Reading UNAIDS HIV estimates and ART cascade data...\n")
  
  # ============================================================================
  # PART 1: HIV ESTIMATES DATA
  # ============================================================================
  
  # Read the HIV estimates data starting from row 6
  estimates_data <- read_excel(input_file, 
                               sheet = "HIV2025Estimates_ByYear",
                               skip = 5,
                               col_names = FALSE)
  
  # Read headers for estimates data
  est_header4 <- read_excel(input_file, 
                            sheet = "HIV2025Estimates_ByYear",
                            skip = 3, n_max = 1, col_names = FALSE)
  
  est_header5 <- read_excel(input_file, 
                            sheet = "HIV2025Estimates_ByYear", 
                            skip = 4, n_max = 1, col_names = FALSE)
  
  # Create column names for estimates data
  estimates_columns <- create_column_names(est_header4, est_header5, "estimates")
  names(estimates_data) <- estimates_columns[1:ncol(estimates_data)]
  
  # ============================================================================
  # PART 2: ART CASCADE DATA
  # ============================================================================
  
  cat("Reading ART cascade indicators from Test & Treat sheet...\n")
  
  # Read the ART cascade data
  cascade_data <- read_excel(input_file, 
                             sheet = "HIV-Test-&-Treat_ByArea",
                             skip = 5,
                             col_names = FALSE)
  
  # Read headers for cascade data
  casc_header3 <- read_excel(input_file, 
                             sheet = "HIV-Test-&-Treat_ByArea",
                             skip = 2, n_max = 1, col_names = FALSE)
  
  casc_header4 <- read_excel(input_file, 
                             sheet = "HIV-Test-&-Treat_ByArea",
                             skip = 3, n_max = 1, col_names = FALSE)
  
  casc_header5 <- read_excel(input_file, 
                             sheet = "HIV-Test-&-Treat_ByArea", 
                             skip = 4, n_max = 1, col_names = FALSE)
  
  # Create column names for cascade data
  cascade_columns <- create_cascade_column_names(casc_header3, casc_header4, casc_header5)
  names(cascade_data) <- cascade_columns[1:ncol(cascade_data)]
  
  # ============================================================================
  # PART 3: FILTER AND COMBINE DATA
  # ============================================================================
  
  cat("Filtering for target countries...\n")
  
  # Define target countries
  target_countries <- c("Global", "Kenya", "Mozambique", "Malawi", "South Africa", "Nigeria")
  target_codes <- c("03M49WLD", "KEN", "MOZ", "MWI", "ZAF", "NGA")
  
  # Filter estimates data
  filtered_estimates <- estimates_data %>%
    filter(country_name %in% target_countries | country_code %in% target_codes) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year >= 1990 & year <= 2025) %>%
    arrange(country_name, year)
  
  # Filter cascade data
  filtered_cascade <- cascade_data %>%
    filter(country_name %in% target_countries | country_code %in% target_codes) %>%
    mutate(year = as.numeric(year)) %>%
    filter(year >= 1990 & year <= 2025) %>%
    arrange(country_name, year)
  
  # ============================================================================
  # PART 4: SELECT ESTIMATE COLUMNS ONLY
  # ============================================================================
  
  cat("Selecting estimate columns only...\n")
  
  # Get estimate columns from both datasets
  est_estimate_cols <- names(filtered_estimates)[str_detect(names(filtered_estimates), "_estimate$")]
  casc_estimate_cols <- names(filtered_cascade)[str_detect(names(filtered_cascade), "_estimate$")]
  
  # Create cleaned estimates dataset
  cleaned_estimates <- filtered_estimates %>%
    select(year, country_code, country_name, all_of(est_estimate_cols))
  
  # Create cleaned cascade dataset
  cleaned_cascade <- filtered_cascade %>%
    select(year, country_code, country_name, all_of(casc_estimate_cols))
  
  # Merge the datasets
  cat("Combining HIV estimates with ART cascade indicators...\n")
  
  combined_data <- cleaned_estimates %>%
    full_join(cleaned_cascade, by = c("year", "country_code", "country_name"))
  
  # Clean up column names (remove "_estimate" suffix)
  all_estimate_cols <- c(est_estimate_cols, casc_estimate_cols)
  clean_names <- str_replace(all_estimate_cols, "_estimate$", "")
  
  # Rename columns
  names(combined_data)[4:ncol(combined_data)] <- clean_names
  
  # Convert data columns to numeric
  for (col in clean_names) {
    combined_data[[col]] <- ifelse(
      str_detect(combined_data[[col]], "^<|^\\.\\.\\.$|^>"), 
      NA, # Convert "<0.1", "...", ">98" to NA for analysis
      suppressWarnings(as.numeric(as.character(combined_data[[col]])))
    )
  }
  
  # Check data availability
  check_data_availability(combined_data, target_countries)
  
  # Print summary
  print_data_summary(combined_data, clean_names)
  
  # Save cleaned data
  cat("\nSaving cleaned data to:", output_file, "\n")
  write_xlsx(combined_data, output_file)
  
  cat("✓ Data cleaning completed successfully!\n")
  
  return(combined_data)
}# Helper function to create column names for estimates data
create_column_names <- function(header4, header5, data_type) {
  header4 <- as.character(unlist(header4[1,]))
  header5 <- as.character(unlist(header5[1,]))
  
  column_names <- character(length(header4))
  column_names[1] <- "year"
  column_names[2] <- "country_code"
  column_names[3] <- "country_name"
  
  current_indicator <- ""
  for (i in 4:length(header4)) {
    # Safe check for header4
    if (i <= length(header4) && !is.na(header4[i]) && !is.null(header4[i]) && 
        nchar(as.character(header4[i])) > 0 && as.character(header4[i]) != "") {
      current_indicator <- as.character(header4[i])
    }
    
    # Safe check for header5
    estimate_type <- ""
    if (i <= length(header5) && !is.na(header5[i]) && !is.null(header5[i]) && 
        nchar(as.character(header5[i])) > 0 && as.character(header5[i]) != "") {
      estimate_type <- as.character(header5[i])
    }
    
    if (current_indicator != "" && estimate_type != "") {
      clean_indicator <- str_replace_all(current_indicator, "[^A-Za-z0-9 ]", "")
      clean_indicator <- str_replace_all(clean_indicator, "\\s+", "_")
      clean_indicator <- str_to_lower(clean_indicator)
      
      column_names[i] <- paste0(clean_indicator, "_", str_to_lower(estimate_type))
    } else {
      column_names[i] <- paste0("col_", i)
    }
  }
  
  return(column_names)
}

# Helper function to create column names for cascade data
create_cascade_column_names <- function(header3, header4, header5) {
  header3 <- as.character(unlist(header3[1,]))
  header4 <- as.character(unlist(header4[1,]))
  header5 <- as.character(unlist(header5[1,]))
  
  column_names <- character(length(header3))
  column_names[1] <- "year"
  column_names[2] <- "country_code"
  column_names[3] <- "country_name"
  
  current_indicator <- ""
  current_age_group <- ""
  
  for (i in 4:length(header3)) {
    # Safe check for main indicator (95-95-95 components)
    if (i <= length(header3) && !is.na(header3[i]) && !is.null(header3[i]) && 
        nchar(as.character(header3[i])) > 0 && as.character(header3[i]) != "") {
      current_indicator <- as.character(header3[i])
      # Clean up line breaks and long text
      current_indicator <- str_replace_all(current_indicator, "\\r\\n", " ")
      current_indicator <- str_replace_all(current_indicator, "\\n", " ")
      
      # Simplify common cascade indicators
      if (str_detect(current_indicator, "know their HIV status")) {
        current_indicator <- "know_status_95_1"
      } else if (str_detect(current_indicator, "on antiretroviral treatment") && 
                 str_detect(current_indicator, "know their status")) {
        current_indicator <- "on_treatment_95_2"
      } else if (str_detect(current_indicator, "on antiretroviral treatment") && 
                 !str_detect(current_indicator, "know their status")) {
        current_indicator <- "on_art_treatment"
      } else if (str_detect(current_indicator, "suppressed viral load") && 
                 str_detect(current_indicator, "on antiretroviral")) {
        current_indicator <- "viral_suppressed_95_3"
      } else if (str_detect(current_indicator, "suppressed viral load")) {
        current_indicator <- "viral_suppressed"
      } else if (str_detect(current_indicator, "pregnant women")) {
        current_indicator <- "pmtct"
      } else {
        # Generic cleaning for other indicators
        current_indicator <- str_replace_all(current_indicator, "[^A-Za-z0-9 ]", "")
        current_indicator <- str_replace_all(current_indicator, "\\s+", "_")
        current_indicator <- str_to_lower(current_indicator)
      }
    }
    
    # Safe check for age group
    if (i <= length(header4) && !is.na(header4[i]) && !is.null(header4[i]) && 
        nchar(as.character(header4[i])) > 0 && as.character(header4[i]) != "") {
      current_age_group <- str_replace_all(as.character(header4[i]), "[^A-Za-z0-9 ]", "")
      current_age_group <- str_replace_all(current_age_group, "\\s+", "_")
      current_age_group <- str_to_lower(current_age_group)
    }
    
    # Safe check for estimate type
    estimate_type <- ""
    if (i <= length(header5) && !is.na(header5[i]) && !is.null(header5[i]) && 
        nchar(as.character(header5[i])) > 0 && as.character(header5[i]) != "") {
      estimate_type <- as.character(header5[i])
    }
    
    if (current_indicator != "" && estimate_type != "") {
      # Combine indicator, age group, and estimate type
      if (current_age_group != "" && current_age_group != "all_ages") {
        column_names[i] <- paste0(current_indicator, "_", current_age_group, "_", str_to_lower(estimate_type))
      } else {
        column_names[i] <- paste0(current_indicator, "_", str_to_lower(estimate_type))
      }
    } else {
      column_names[i] <- paste0("col_", i)
    }
  }
  
  return(column_names)
}

# Helper function to check data availability
check_data_availability <- function(combined_data, target_countries) {
  found_countries <- unique(combined_data$country_name)
  missing_countries <- setdiff(target_countries, found_countries)
  
  if (length(missing_countries) > 0) {
    cat("Warning: The following countries were not found:\n")
    cat(paste(missing_countries, collapse = ", "), "\n")
  } else {
    cat("✓ All target countries found in the data\n")
  }
  
  cat("Countries in final dataset:\n")
  print(found_countries)
}

# Helper function to print data summary
print_data_summary <- function(combined_data, clean_names) {
  cat("\n=== CLEANED DATA SUMMARY ===\n")
  cat("Year range:", min(combined_data$year, na.rm = TRUE), "to", max(combined_data$year, na.rm = TRUE), "\n")
  cat("Data dimensions:", nrow(combined_data), "rows x", ncol(combined_data), "columns\n")
  
  # Categorize indicators
  hiv_indicators <- clean_names[str_detect(clean_names, "prevalence|incidence|deaths|living_with_hiv|newly_infected")]
  cascade_indicators <- clean_names[str_detect(clean_names, "know_status|on_treatment|on_art|viral_suppressed|95")]
  pmtct_indicators <- clean_names[str_detect(clean_names, "pmtct|pregnant")]
  
  cat("\nHIV Epidemiological Indicators (", length(hiv_indicators), "):\n")
  cat(paste(head(hiv_indicators, 5), collapse = ", "), ifelse(length(hiv_indicators) > 5, "...", ""), "\n")
  
  cat("\nART Cascade Indicators (", length(cascade_indicators), "):\n")
  cat(paste(head(cascade_indicators, 5), collapse = ", "), ifelse(length(cascade_indicators) > 5, "...", ""), "\n")
  
  if (length(pmtct_indicators) > 0) {
    cat("\nPMTCT Indicators (", length(pmtct_indicators), "):\n")
    cat(paste(pmtct_indicators, collapse = ", "), "\n")
  }
  
  cat("\nSample of cleaned data:\n")
  print(head(combined_data %>% select(1:6), 5))
}

# Function to create a comprehensive data dictionary
create_data_dictionary <- function(cleaned_data, output_file = "UNAIDS_data_dictionary.xlsx") {
  
  indicators <- names(cleaned_data)[4:ncol(cleaned_data)]
  
  # Create data dictionary with enhanced descriptions
  dictionary <- data.frame(
    column_name = names(cleaned_data),
    category = c("Metadata", "Metadata", "Metadata", rep("HIV Indicator", length(indicators))),
    description = c(
      "Year of estimate",
      "UNAIDS country/region code", 
      "Country or region name",
      rep("HIV/ART indicator estimate", length(indicators))
    ),
    data_type = c("Numeric", "Character", "Character", rep("Numeric", length(indicators))),
    source_sheet = c("N/A", "N/A", "N/A", rep("Multiple", length(indicators))),
    stringsAsFactors = FALSE
  )
  
  # Add specific descriptions for HIV epidemiological indicators
  for (i in 4:nrow(dictionary)) {
    col_name <- dictionary$column_name[i]
    
    # HIV Epidemiological Indicators
    if (str_detect(col_name, "adults.*prevalence")) {
      dictionary$description[i] <- "Adults (15-49) HIV prevalence (%)"
      dictionary$category[i] <- "HIV Epidemiology"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
    } else if (str_detect(col_name, "young_women.*prevalence")) {
      dictionary$description[i] <- "Young women (15-24) HIV prevalence (%)"
      dictionary$category[i] <- "HIV Epidemiology"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
    } else if (str_detect(col_name, "young_men.*prevalence")) {
      dictionary$description[i] <- "Young men (15-24) HIV prevalence (%)"
      dictionary$category[i] <- "HIV Epidemiology"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
    } else if (str_detect(col_name, "aids_related_deaths")) {
      dictionary$description[i] <- "AIDS-related deaths estimates"
      dictionary$category[i] <- "HIV Mortality"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
    } else if (str_detect(col_name, "living_with_hiv")) {
      dictionary$description[i] <- "People living with HIV estimates"
      dictionary$category[i] <- "HIV Prevalence"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
    } else if (str_detect(col_name, "newly_infected")) {
      dictionary$description[i] <- "New HIV infections estimates"
      dictionary$category[i] <- "HIV Incidence"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
    } else if (str_detect(col_name, "incidence")) {
      dictionary$description[i] <- "HIV incidence rate estimates"
      dictionary$category[i] <- "HIV Incidence"
      dictionary$source_sheet[i] <- "HIV2025Estimates_ByYear"
      
      # ART Cascade Indicators (95-95-95)
    } else if (str_detect(col_name, "know_status_95_1")) {
      dictionary$description[i] <- "% of people living with HIV who know their status (1st 95)"
      dictionary$category[i] <- "ART Cascade (95-95-95)"
      dictionary$source_sheet[i] <- "HIV-Test-&-Treat_ByArea"
    } else if (str_detect(col_name, "on_treatment_95_2")) {
      dictionary$description[i] <- "% of people who know status and are on ART (2nd 95)"
      dictionary$category[i] <- "ART Cascade (95-95-95)"
      dictionary$source_sheet[i] <- "HIV-Test-&-Treat_ByArea"
    } else if (str_detect(col_name, "viral_suppressed_95_3")) {
      dictionary$description[i] <- "% on ART with suppressed viral load (3rd 95)"
      dictionary$category[i] <- "ART Cascade (95-95-95)"
      dictionary$source_sheet[i] <- "HIV-Test-&-Treat_ByArea"
    } else if (str_detect(col_name, "on_art_treatment")) {
      dictionary$description[i] <- "% of people living with HIV on antiretroviral treatment"
      dictionary$category[i] <- "ART Treatment"
      dictionary$source_sheet[i] <- "HIV-Test-&-Treat_ByArea"
    } else if (str_detect(col_name, "viral_suppressed")) {
      dictionary$description[i] <- "% of people living with HIV with suppressed viral load"
      dictionary$category[i] <- "ART Treatment"
      dictionary$source_sheet[i] <- "HIV-Test-&-Treat_ByArea"
      
      # PMTCT Indicators
    } else if (str_detect(col_name, "pmtct|pregnant")) {
      dictionary$description[i] <- "Prevention of mother-to-child transmission indicator"
      dictionary$category[i] <- "PMTCT"
      dictionary$source_sheet[i] <- "HIV-Test-&-Treat_ByArea"
    }
    
    # Add age group info if present
    if (str_detect(col_name, "children")) {
      dictionary$description[i] <- paste(dictionary$description[i], "(Children 0-14)")
    } else if (str_detect(col_name, "adults_15")) {
      dictionary$description[i] <- paste(dictionary$description[i], "(Adults 15+)")
    } else if (str_detect(col_name, "women_15")) {
      dictionary$description[i] <- paste(dictionary$description[i], "(Women 15+)")
    } else if (str_detect(col_name, "men_15")) {
      dictionary$description[i] <- paste(dictionary$description[i], "(Men 15+)")
    }
  }
  
  # Create summary sheet
  summary_sheet <- dictionary %>%
    group_by(category) %>%
    summarise(
      count = n(),
      indicators = paste(column_name[category != "Metadata"], collapse = ", "),
      .groups = "drop"
    ) %>%
    filter(category != "Metadata")
  
  # Save both dictionary and summary
  excel_data <- list(
    "Data_Dictionary" = dictionary,
    "Summary_by_Category" = summary_sheet
  )
  
  write_xlsx(excel_data, output_file)
  cat("Comprehensive data dictionary saved to:", output_file, "\n")
  
  return(list(dictionary = dictionary, summary = summary_sheet))
}

# Main execution function
main <- function(input_file = "HIV_estimates_from_1990to2025.xlsx") {
  
  cat("=== UNAIDS HIV Data Cleaning Script (Enhanced with ART Cascade) ===\n\n")
  
  # Check if input file exists
  if (!file.exists(input_file)) {
    stop("Input file not found: ", input_file, "\nPlease make sure the file is in your working directory.")
  }
  
  # Clean the data (now includes both HIV estimates and ART cascade)
  cleaned_data <- clean_unaids_hiv_data(input_file)
  
  # Create comprehensive data dictionary
  dictionary_result <- create_data_dictionary(cleaned_data)
  
  cat("\n=== CLEANING COMPLETED ===\n")
  cat("Files created:\n")
  cat("- UNAIDS_HIV_cleaned.xlsx (cleaned data with HIV estimates + ART cascade)\n")
  cat("- UNAIDS_data_dictionary.xlsx (comprehensive data dictionary)\n\n")
  
  cat("Data includes:\n")
  cat("✓ HIV epidemiological indicators (prevalence, incidence, mortality)\n")
  cat("✓ ART cascade indicators (95-95-95 targets)\n")
  cat("✓ PMTCT indicators\n")
  cat("✓ Age and sex disaggregated data where available\n\n")
  
  return(list(
    data = cleaned_data, 
    dictionary = dictionary_result$dictionary,
    summary = dictionary_result$summary
  ))
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# # Run the complete cleaning process:
# result <- main("HIV_estimates_from_1990to2025.xlsx")
# 
# # Access the cleaned data:
# cleaned_data <- result$data
# 
# # View the 95-95-95 cascade indicators for a specific country:
# library(dplyr)
# kenya_cascade <- cleaned_data %>% 
#   filter(country_name == "Kenya") %>%
#   select(year, contains("95_")) %>%
#   filter(year >= 2015) # Cascade data typically starts around 2015
# 
# print(kenya_cascade)
# 
# # View HIV prevalence trends:
# prevalence_data <- cleaned_data %>%
#   filter(country_name %in% c("Kenya", "South Africa", "Malawi")) %>%
#   select(year, country_name, contains("prevalence")) %>%
#   filter(year >= 2000)
# 
# print(prevalence_data)
# 
# # View data availability by indicator category:
# print(result$summary)
# 
# # Create a simple visualization of the 95-95-95 cascade:
# library(ggplot2)
# 
# cascade_plot_data <- cleaned_data %>%
#   filter(country_name == "Global", year == 2024) %>%
#   select(contains("95_")) %>%
#   pivot_longer(everything(), names_to = "indicator", values_to = "percentage") %>%
#   mutate(
#     cascade_step = case_when(
#       str_detect(indicator, "95_1") ~ "1st 95: Know Status",
#       str_detect(indicator, "95_2") ~ "2nd 95: On Treatment", 
#       str_detect(indicator, "95_3") ~ "3rd 95: Viral Suppression",
#       TRUE ~ indicator
#     )
#   ) %>%
#   filter(!is.na(percentage))
# 
# ggplot(cascade_plot_data, aes(x = cascade_step, y = percentage)) +
#   geom_col(fill = "steelblue") +
#   geom_hline(yintercept = 95, linetype = "dashed", color = "red") +
#   labs(title = "Global HIV 95-95-95 Cascade (2024)",
#        x = "Cascade Step", y = "Percentage (%)",
#        subtitle = "Dashed line shows 95% target") +
#   theme_minimal() +
#   ylim(0, 100)

# =============================================================================
# RUN THE SCRIPT
# =============================================================================

# Execute the main function with the specified path
result <- main("input_data/HIV_estimates_from_1990to2025.xlsx")
