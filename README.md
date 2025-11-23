# ESCB Macroeconomic Statistics Code

**Author:** Zlatko Čehulić

This repository demonstrates a complete R pipeline for macroeconomic statistics, highly relevant for the Economist-Statistician role at the ECB.

---

## Skills and Relevance

- **Programming:** R (Tidyverse, Eurostat, Quantmod, lubridate)
- **Data Standards:** SDMX (via Eurostat API)
- **Macroeconomic Stats:** Inflation/HICP, Unemployment, GDP, ECB Reference Rates
- **Statistical Production:** Automated cleaning, validation, indicator derivation, visualization
- **Econometrics:** OLS, VAR/VECM, IRF analysis

---

## Repository Structure

- `analysis_and_indicators/ESCB_Macro_Pipeline.R` – full macroeconomic data pipeline
- `analysis_and_indicators/visualization.R` – generation of facet plots (`TimeSeries_R.png`)
- `analysis_and_indicators/vecm_var_analysis.R` – VAR/VECM and IRF modeling
- `output/` – CSV data and charts
- `data_acquisition/` – optional scripts for automated data download
- `validation/` – optional scripts for data validation and stationarity tests

---

## How to Run

```R
# Install required packages
install.packages(c("eurostat","quantmod","tidyverse","lubridate","tidyr","vars","tseries","urca"))

# Run the main pipeline
source("analysis_and_indicators/ESCB_Macro_Pipeline.R")
