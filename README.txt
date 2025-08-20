R scripts. 

"unaids_hiv_datasheets.R" downloads the Excel directly from "https://www.unaids.org/sites/default/files/2025-07/HIV_estimates_from_1990-to-2025.xlsx, parses the two-row headers (Indicator over Estimate/Low/High), keeps Global, South Africa (ZAF), Zimbabwe (ZWE), Malawi (MWI) for all years, converts blank cells to NA, and writes a single CSV. 
"unaids_hiv_plots.R" reads that CSV and makes a few time-series plots.
