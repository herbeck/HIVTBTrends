# =============================================================================
# HIV and TB Data Analysis Scripts
# Author: [Your Name]
# Date: [Current Date]
# Purpose: Download, clean, and analyze HIV/TB data from IHME, WHO, and UNAIDS
# =============================================================================

# Load required libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(countrycode)
library(janitor)
library(lubridate)
library(scales)

# =============================================================================
# 1. DATA DOWNLOAD FUNCTIONS
# =============================================================================

# Function to download WHO TB data
download_who_tb_data <- function(output_dir = "who_data/") {
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # WHO TB data URLs
  urls <- list(
    burden_estimates = "https://extranet.who.int/tme/generateCSV.asp?ds=estimates",
    notifications = "https://extranet.who.int/tme/generateCSV.asp?ds=notifications",
    outcomes = "https://extranet.who.int/tme/generateCSV.asp?ds=outcomes",
    data_dictionary = "https://extranet.who.int/tme/generateCSV.asp?ds=dictionary"
  )
  
  # Download files
  for (name in names(urls)) {
    output_file <- file.path(output_dir, paste0("who_tb_", name, ".csv"))
    
    cat("Downloading WHO TB", name, "data...\n")
    
    tryCatch({
      download.file(urls[[name]], output_file, mode = "wb")
      cat("Successfully downloaded:", output_file, "\n")
    }, error = function(e) {
      cat("Error downloading", name, ":", e$message, "\n")
    })
  }
  
  return(file.path(output_dir, paste0("who_tb_", names(urls), ".csv")))
}

# Function to set up IHME data download (manual step required)
setup_ihme_download <- function() {
  cat("IHME GBD Data Download Instructions:\n")
  cat("1. Go to: https://vizhub.healthdata.org/gbd-results/\n")
  cat("2. Select the following settings:\n")
  cat("   - Measure: Deaths, DALYs, Incidence, Prevalence\n")
  cat("   - Location: Global, South Africa, Zimbabwe, Malawi, Nigeria, Mozambique, Kenya\n")
  cat("   - Age: All Ages\n")
  cat("   - Sex: Both\n")
  cat("   - Cause: HIV/AIDS, Tuberculosis\n")
  cat("   - Year: 2000-2021 (latest available)\n")
  cat("3. Download the CSV file and save as 'ihme_gbd_data.csv' in your data folder\n")
  cat("4. Run the cleaning functions below after downloading\n\n")
}

# Note: UNAIDS data typically requires manual download from aidsinfo.unaids.org
setup_unaids_download <- function() {
  cat("UNAIDS Data Download Instructions:\n")
  cat("1. Go to: https://aidsinfo.unaids.org/\n")
  cat("2. Navigate to 'Country factsheets' or 'Global data'\n")
  cat("3. Select your countries of interest:\n")
  cat("   - Global, South Africa, Zimbabwe, Malawi, Nigeria, Mozambique, Kenya\n")
  cat("4. Download HIV indicators for 2000-2024:\n")
  cat("   - People living with HIV\n")
  cat("   - New HIV infections\n")
  cat("   - AIDS-related deaths\n")
  cat("   - HIV prevalence\n")
  cat("5. Save as 'unaids_hiv_data.csv' in your data folder\n\n")
}

# =============================================================================
# 2. DATA CLEANING FUNCTIONS
# =============================================================================

# Clean WHO TB data
clean_who_tb_data <- function(data_dir = "data/") {
  
  # Read WHO TB burden estimates
  tb_burden_file <- file.path(data_dir, "who_tb_burden_estimates.csv")
  
  if (file.exists(tb_burden_file)) {
    tb_data <- read_csv(tb_burden_file, show_col_types = FALSE) %>%
      clean_names() %>%
      # Filter for our countries of interest
      filter(country %in% c("Global", "South Africa", "Zimbabwe", "Malawi", 
                            "Nigeria", "Mozambique", "Kenya") |
               iso3 %in% c("ZAF", "ZWE", "MWI", "NGA", "MOZ", "KEN")) %>%
      # Filter for years 2000-2024
      filter(year >= 2000 & year <= 2024) %>%
      # Select key indicators
      select(country, iso3, year, 
             # Incidence indicators
             e_inc_100k, e_inc_100k_lo, e_inc_100k_hi,
             e_inc_num, e_inc_num_lo, e_inc_num_hi,
             # Mortality indicators  
             e_mort_100k, e_mort_100k_lo, e_mort_100k_hi,
             e_mort_num, e_mort_num_lo, e_mort_num_hi,
             # HIV-positive TB
             e_inc_tbhiv_100k, e_inc_tbhiv_100k_lo, e_inc_tbhiv_100k_hi,
             e_mort_tbhiv_100k, e_mort_tbhiv_100k_lo, e_mort_tbhiv_100k_hi) %>%
      # Add data source
      mutate(source = "WHO")
    
    return(tb_data)
  } else {
    cat("WHO TB data file not found. Please download first.\n")
    return(NULL)
  }
}

# Clean IHME data
clean_ihme_data <- function(data_dir = "data/") {
  
  ihme_file <- file.path(data_dir, "ihme_gbd_data.csv")
  
  if (file.exists(ihme_file)) {
    ihme_data <- read_csv(ihme_file, show_col_types = FALSE) %>%
      clean_names() %>%
      # Filter for our locations
      filter(location_name %in% c("Global", "South Africa", "Zimbabwe", "Malawi",
                                  "Nigeria", "Mozambique", "Kenya")) %>%
      # Filter for HIV and TB
      filter(cause_name %in% c("HIV/AIDS", "Tuberculosis")) %>%
      # Filter years 2000-2021 (IHME latest)
      filter(year >= 2000 & year <= 2021) %>%
      # Standardize measure names
      mutate(
        measure_name = case_when(
          measure_name == "Deaths" ~ "mortality",
          measure_name == "Incidence" ~ "incidence", 
          measure_name == "Prevalence" ~ "prevalence",
          measure_name == "DALYs (Disability-Adjusted Life Years)" ~ "dalys",
          TRUE ~ tolower(measure_name)
        ),
        pathogen = case_when(
          cause_name == "HIV/AIDS" ~ "HIV",
          cause_name == "Tuberculosis" ~ "TB",
          TRUE ~ cause_name
        ),
        source = "IHME"
      ) %>%
      # Select key columns
      select(location_name, year, pathogen, measure_name, val, upper, lower, source) %>%
      rename(country = location_name, value = val)
    
    return(ihme_data)
  } else {
    cat("IHME data file not found. Please download from GBD Results Tool.\n")
    return(NULL)
  }
}

# Clean UNAIDS data  
clean_unaids_data <- function(data_dir = "data/") {
  
  unaids_file <- file.path(data_dir, "unaids_hiv_data.csv")
  
  if (file.exists(unaids_file)) {
    unaids_data <- read_csv(unaids_file, show_col_types = FALSE) %>%
      clean_names() %>%
      # This will depend on the exact format from UNAIDS
      # Common structure adjustments:
      filter(country %in% c("Global", "South Africa", "Zimbabwe", "Malawi",
                            "Nigeria", "Mozambique", "Kenya")) %>%
      filter(year >= 2000 & year <= 2024) %>%
      # Standardize indicator names (adjust based on actual UNAIDS format)
      mutate(
        pathogen = "HIV",
        source = "UNAIDS",
        measure_name = case_when(
          str_detect(tolower(indicator), "people living") ~ "prevalence",
          str_detect(tolower(indicator), "new infection") ~ "incidence", 
          str_detect(tolower(indicator), "death") ~ "mortality",
          TRUE ~ "other"
        )
      ) %>%
      # Select and rename columns (adjust based on actual UNAIDS format)
      select(country, year, pathogen, measure_name, estimate, lower_bound, upper_bound, source) %>%
      rename(value = estimate, lower = lower_bound, upper = upper_bound)
    
    return(unaids_data)
  } else {
    cat("UNAIDS data file not found. Please download from AIDSinfo.\n")
    return(NULL)
  }
}

# =============================================================================
# 3. DATA COMBINATION AND COMPARISON FUNCTIONS  
# =============================================================================

# Combine all data sources
combine_all_data <- function(data_dir = "data/") {
  
  # Clean individual datasets
  who_data <- clean_who_tb_data(data_dir)
  ihme_data <- clean_ihme_data(data_dir) 
  unaids_data <- clean_unaids_data(data_dir)
  
  # Combine IHME data (has both HIV and TB)
  combined_data <- ihme_data
  
  # Add WHO TB data if available
  if (!is.null(who_data)) {
    # Reshape WHO data to match IHME format
    who_long <- who_data %>%
      pivot_longer(
        cols = c(e_inc_100k, e_mort_100k, e_inc_tbhiv_100k, e_mort_tbhiv_100k),
        names_to = "measure_type",
        values_to = "value"
      ) %>%
      mutate(
        measure_name = case_when(
          str_detect(measure_type, "inc") ~ "incidence",
          str_detect(measure_type, "mort") ~ "mortality"
        ),
        pathogen = if_else(str_detect(measure_type, "tbhiv"), "TB-HIV", "TB"),
        source = "WHO"
      ) %>%
      select(country, year, pathogen, measure_name, value, source) %>%
      filter(!is.na(value))
    
    combined_data <- bind_rows(combined_data, who_long)
  }
  
  # Add UNAIDS HIV data if available  
  if (!is.null(unaids_data)) {
    combined_data <- bind_rows(combined_data, unaids_data)
  }
  
  # Clean and standardize
  combined_data <- combined_data %>%
    mutate(
      country = case_when(
        country == "Global" ~ "Global",
        TRUE ~ country
      )
    ) %>%
    arrange(country, pathogen, measure_name, year, source)
  
  return(combined_data)
}

# =============================================================================
# 4. VISUALIZATION FUNCTIONS
# =============================================================================

# Create comparison plots by country and pathogen
create_comparison_plots <- function(data, output_dir = "plots/") {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  countries <- unique(data$country)
  pathogens <- unique(data$pathogen)
  measures <- unique(data$measure_name)
  
  for (country_name in countries) {
    for (pathogen_name in pathogens) {
      for (measure_name in measures) {
        
        plot_data <- data %>%
          filter(country == country_name, 
                 pathogen == pathogen_name,
                 measure_name == measure_name) %>%
          filter(!is.na(value))
        
        if (nrow(plot_data) > 0) {
          
          p <- ggplot(plot_data, aes(x = year, y = value, color = source)) +
            geom_line(size = 1.2) +
            geom_point(size = 2) +
            # Add confidence intervals if available
            {if("upper" %in% names(plot_data)) 
              geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), 
                          alpha = 0.2, color = NA)} +
            labs(
              title = paste(pathogen_name, measure_name, "in", country_name),
              subtitle = "Comparison across data sources",
              x = "Year",
              y = case_when(
                measure_name == "incidence" ~ "Rate per 100,000",
                measure_name == "mortality" ~ "Rate per 100,000", 
                measure_name == "prevalence" ~ "Number of people",
                TRUE ~ "Value"
              ),
              color = "Data Source",
              fill = "Data Source"
            ) +
            theme_minimal() +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12),
              legend.position = "bottom"
            ) +
            scale_x_continuous(breaks = seq(2000, 2024, 5)) +
            scale_y_continuous(labels = comma_format()) +
            scale_color_brewer(type = "qual", palette = "Set1") +
            scale_fill_brewer(type = "qual", palette = "Set1")
          
          # Save plot
          filename <- paste0(tolower(gsub("[^A-Za-z0-9]", "_", 
                                          paste(country_name, pathogen_name, measure_name, sep = "_"))), 
                             ".png")
          
          ggsave(file.path(output_dir, filename), plot = p, 
                 width = 10, height = 6, dpi = 300)
          
          cat("Saved plot:", filename, "\n")
        }
      }
    }
  }
}

# Create summary comparison plot
create_summary_plot <- function(data, output_dir = "plots/") {
  
  # Create a summary plot showing data availability by source
  summary_data <- data %>%
    filter(!is.na(value)) %>%
    group_by(country, pathogen, source, year) %>%
    summarise(n_measures = n_distinct(measure_name), .groups = "drop")
  
  p <- ggplot(summary_data, aes(x = year, y = country, fill = source)) +
    geom_tile(alpha = 0.7) +
    facet_wrap(~pathogen, scales = "free") +
    labs(
      title = "Data Availability by Source, Country, and Pathogen",
      subtitle = "Years 2000-2024",
      x = "Year",
      y = "Country",
      fill = "Data Source"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_fill_brewer(type = "qual", palette = "Set1")
  
  ggsave(file.path(output_dir, "data_availability_summary.png"), plot = p,
         width = 12, height = 8, dpi = 300)
  
  return(p)
}

# =============================================================================
# 5. MAIN EXECUTION FUNCTIONS
# =============================================================================

# Run complete analysis
run_complete_analysis <- function(data_dir = "data/", output_dir = "plots/") {
  
  cat("=== HIV and TB Data Analysis Pipeline ===\n\n")
  
  # Step 1: Download instructions
  cat("Step 1: Download data files\n")
  cat("Run the following functions to get download instructions:\n")
  cat("- setup_ihme_download()\n")
  cat("- setup_unaids_download()\n")
  cat("- download_who_tb_data() # This will attempt automatic download\n\n")
  
  # Step 2: Try to download WHO data automatically
  cat("Step 2: Downloading WHO TB data...\n")
  who_files <- download_who_tb_data(data_dir)
  
  # Step 3: Clean and combine data
  cat("Step 3: Cleaning and combining data...\n")
  combined_data <- combine_all_data(data_dir)
  
  if (nrow(combined_data) > 0) {
    # Step 4: Create visualizations
    cat("Step 4: Creating comparison plots...\n")
    create_comparison_plots(combined_data, output_dir)
    summary_plot <- create_summary_plot(combined_data, output_dir)
    
    # Step 5: Save cleaned data
    write_csv(combined_data, file.path(data_dir, "combined_hiv_tb_data.csv"))
    cat("Saved combined dataset to:", file.path(data_dir, "combined_hiv_tb_data.csv"), "\n")
    
    # Step 6: Basic summary statistics
    cat("\n=== Data Summary ===\n")
    print(combined_data %>%
            group_by(source, pathogen) %>%
            summarise(
              countries = n_distinct(country),
              years = paste(min(year, na.rm = TRUE), "-", max(year, na.rm = TRUE)),
              measures = n_distinct(measure_name),
              observations = n(),
              .groups = "drop"
            ))
    
    return(combined_data)
  } else {
    cat("No data available. Please download data files first.\n")
    return(NULL)
  }
}

# =============================================================================
# USAGE EXAMPLES
# =============================================================================

# # To run the complete analysis:
# combined_data <- run_complete_analysis()
# 
# # To get download instructions:
# setup_ihme_download()
# setup_unaids_download()
# 
# # To download WHO data only:
# download_who_tb_data()
# 
# # To create plots from existing data:
# data <- combine_all_data()
# create_comparison_plots(data)
# create_summary_plot(data)
