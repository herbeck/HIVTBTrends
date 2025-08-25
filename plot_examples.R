# plot_counts_unaids15plus_vs_ihme1549.R
# Output: hiv_counts_unaids15plus_vs_ihme1549.png
# Compares:
#  - UNAIDS | Estimated adults (15+) living with HIV | Estimate   (count, 15+)
#  - IHME   | HIV/AIDS - Prevalence - Both - 15-49 years [Number] | Estimate (count, 15–49)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

# ---- settings ----
in_xlsx   <- "spreadsheets/UNAIDS_WHO_IHME.xlsx"
sheet_nm  <- "Merged"
keep_locs <- c("Global", "South Africa", "Mozambique")

unaids_col <- "UNAIDS | Estimated adults (15+) living with HIV | Estimate"
ihme_col   <- "IHME | HIV/AIDS - Prevalence - Both - 15-49 years [Number] | Estimate"

# ---- helpers ----
as_num <- function(x) {
  x <- gsub("[^0-9eE+\\-\\.]", "", as.character(x))  # strip commas, spaces, etc.
  suppressWarnings(as.numeric(x))
}

# ---- load & prep ----
stopifnot(file.exists(in_xlsx))
df <- read_excel(in_xlsx, sheet = sheet_nm)
stopifnot(all(c("Year","ISO","Location") %in% names(df)))
stopifnot(unaids_col %in% names(df), ihme_col %in% names(df))

df <- df %>%
  mutate(Year = as.integer(Year), Location = as.character(Location)) %>%
  filter(Location %in% keep_locs) %>%
  select(Year, Location,
         UNAIDS_15plus = all_of(unaids_col),
         IHME_15_49    = all_of(ihme_col)) %>%
  mutate(
    UNAIDS_15plus = as_num(UNAIDS_15plus),
    IHME_15_49    = as_num(IHME_15_49)
  ) %>%
  arrange(Location, Year) %>%
  filter(!(is.na(UNAIDS_15plus) & is.na(IHME_15_49)))  # drop rows where both are NA

long <- df %>%
  pivot_longer(c(UNAIDS_15plus, IHME_15_49),
               names_to = "Source", values_to = "Count")

# ---- plot ----
p <- ggplot(long, aes(Year, Count, color = Source)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  facet_wrap(~ Location, ncol = 3, scales = "free_y") +
  labs(
    title = "People Living with HIV (counts): UNAIDS 15+ vs IHME 15–49",
    subtitle = "Note: age groups differ (UNAIDS 15+, IHME 15–49)",
    x = "Year",
    y = "Count (people)"     # <-- clear y-axis label
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    strip.background = element_rect(fill = "white", color = "black"),
    legend.position  = "bottom",
    legend.background= element_rect(fill = "white", color = NA)
  )

ggsave("hiv_counts_unaids15plus_vs_ihme1549.png", p,
       width = 12, height = 5, dpi = 300, bg = "white")

message("Wrote: hiv_counts_unaids15plus_vs_ihme1549.png")
