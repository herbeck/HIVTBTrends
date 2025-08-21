# ============================================
# plot_examples.R
# Input: outputs/merged_tidy.csv (from merge_three_sources.R)
# Output: outputs/plots/*.png
# ============================================

pkgs <- c("dplyr","ggplot2","readr","stringr","tidyr")
new <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create("outputs/plots", showWarnings = FALSE, recursive = TRUE)

keep_geos <- c("Global","South Africa","Zimbabwe","Malawi","Nigeria","Kenya","Mozambique")

df <- readr::read_csv("outputs/merged_tidy.csv", show_col_types = FALSE) %>%
  filter(geography %in% keep_geos)

# --------------------------------------------
# TB incidence comparison (WHO TB vs IHME GBD)
# Both are typically "per 100,000" rates (check labels)
# --------------------------------------------
tb_df <- df %>%
  filter(
    # WHO TB rate
    grepl("^WHO TB_", source_indicator) & grepl("incidence rate", source_indicator, ignore.case = TRUE) |
      # IHME GBD rate for TB
      (grepl("^IHME GBD_", source_indicator) & grepl("^IHME GBD_.*Tuberculosis.*incidence.*\\(Rate\\)", source_indicator, ignore.case = TRUE))
  ) %>%
  mutate(source = dplyr::case_when(
    grepl("^WHO TB_", source_indicator) ~ "WHO TB (per 100,000)",
    grepl("^IHME GBD_", source_indicator) ~ "IHME GBD (per 100,000)",
    TRUE ~ "Other"
  ))

p_tb <- ggplot(tb_df, aes(x = year, y = as.numeric(value), color = source)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ geography, scales = "free_y") +
  labs(title = "TB incidence rate – WHO TB vs IHME GBD",
       subtitle = "Rates per 100,000 (as labeled)",
       x = NULL, y = "Incidence rate", color = "Source") +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/tb_incidence_comparison.png", p_tb, width = 12, height = 7, dpi = 150)

# --------------------------------------------
# HIV incidence comparison (UNAIDS vs IHME GBD)
# NOTE: Units differ:
#   - UNAIDS: per 1,000 uninfected population (All ages)
#   - IHME GBD: per 100,000 population
# --------------------------------------------

hiv_df <- df %>%
  filter(
    # UNAIDS all-ages incidence
    grepl("^UNAIDS_.*incidence.*(All ages|Adults and children)|^UNAIDS_All ages incidence", source_indicator, ignore.case = TRUE) |
      # IHME GBD HIV incidence rate (Rate)
      (grepl("^IHME GBD_.*HIV.*incidence.*\\(Rate\\)", source_indicator, ignore.case = TRUE))
  ) %>%
  mutate(source = dplyr::case_when(
    grepl("^UNAIDS_", source_indicator) ~ "UNAIDS (per 1,000 uninfected)",
    grepl("^IHME GBD_", source_indicator) ~ "IHME GBD (per 100,000)",
    TRUE ~ "Other"
  ))

p_hiv <- ggplot(hiv_df, aes(x = year, y = as.numeric(value), color = source)) +
  geom_line(na.rm = TRUE) +
  facet_wrap(~ geography, scales = "free_y") +
  labs(title = "HIV incidence – UNAIDS vs IHME GBD",
       subtitle = "Units differ (see legend): UNAIDS = per 1,000 uninfected; IHME GBD = per 100,000 population",
       x = NULL, y = "Incidence (see units in legend)", color = "Source") +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/hiv_incidence_comparison.png", p_hiv, width = 12, height = 7, dpi = 150)

message("Saved plots to outputs/plots/:",
        "\n  - tb_incidence_comparison.png",
        "\n  - hiv_incidence_comparison.png")
