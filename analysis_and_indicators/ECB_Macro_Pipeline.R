# ESCB_Macro_Pipeline.R
# Full Macroeconomic Data Pipeline (Data Acquisition, Transformation, OLS Analysis)

# ==============================================================================
# 1. INSTALLATION AND LOADING PACKAGES
# ==============================================================================
if(!requireNamespace("eurostat", quietly = TRUE)) install.packages("eurostat")
if(!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod") 
if(!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if(!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate") 
if(!requireNamespace("tidyr", TRUE)) install.packages("tidyr") 
if(!requireNamespace("vars", quietly = TRUE)) install.packages("vars") # Added for potential VAR/VECM

library(eurostat)
library(quantmod)
library(tidyverse)
library(lubridate)
library(tidyr)
library(vars) 

# Set country filter
country_code <- "HR" # Croatia

# ==============================================================================
# 2. DATA ACQUISITION (Optimized and Relevant)
# ==============================================================================

## 2.1. INFLATION (Y): HICP Index - Eurostat (SDMX-based)
hicp_raw <- get_eurostat(
  "prc_hicp_midx", 
  time_format = "date",
  filters = list(geo = country_code, coicop = "CP00") # Total HICP
)

hicp_data <- hicp_raw %>%
  filter(!is.na(values)) %>%
  group_by(time) %>% 
  summarise(HICP_Index = mean(values, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = time) %>% 
  as_tibble()


## 2.2. UNEMPLOYMENT (Z1): Harmonized Unemployment Rate
hulc_raw <- get_eurostat(
  "une_rt_m", 
  time_format = "date",
  filters = list(geo = country_code) 
)

unemployment_data <- hulc_raw %>%
  filter(!is.na(values)) %>% 
  group_by(time) %>% 
  summarise(Unemployment_Rate = mean(values, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(date = time) %>% 
  as_tibble()


# REMOVE RAW OBJECTS TO SAVE MEMORY
rm(hicp_raw, hulc_raw)


## 2.3. ECB KEY RATE (X): Main Refinancing Operations Rate (FRED)
ecb_fred_id <- "ECBDFR" 
getSymbols(ecb_fred_id, src = "FRED", from = "2000-01-01")

ecb_rate_df <- get(ecb_fred_id) %>%
  data.frame(date = index(.), ECB_Ref_Rate_Raw = .) %>%
  as_tibble() %>%
  mutate(date = lubridate::floor_date(date, "month")) %>%
  group_by(date) %>%
  slice_tail(n = 1) %>% # Take the last observation of the month
  ungroup() %>%
  rename(ECB_Ref_Rate = ECBDFR) 

# Check and remove temporary xts variable if it exists
if(exists("ECBDFR")) rm(ECBDFR)


## 2.4. GDP GROWTH (Z2): Quarterly Data - Eurostat (Nominal GDP)
gdp_raw <- get_eurostat(
  "namq_10_gdp", 
  time_format = "date",
  filters = list(geo = country_code, unit = "CP_MNAC", s_adj = "SCA", na_item = "B1GQ") 
)

gdp_data <- gdp_raw %>%
  filter(!is.na(values)) %>%
  dplyr::select(date = time, GDP_Nominal = values) %>%
  as_tibble()

# REMOVE RAW OBJECT
rm(gdp_raw)


# ==============================================================================
# 3. PROCESSING, MERGING, AND TRANSFORMATION (Indicator Derivation)
# ==============================================================================


# 3.1. Calculate Inflation (Y/Y) and GDP Growth (Y/Y)
hicp_processed <- hicp_data %>%
  mutate(Inflation_YY = (HICP_Index / lag(HICP_Index, 12) - 1) * 100) %>% # Year-on-year
  dplyr::select(date, Inflation_YY) 


gdp_processed <- gdp_data %>%
  mutate(GDP_Growth_YY = (GDP_Nominal / lag(GDP_Nominal, 4) - 1) * 100) %>% # Year-on-year
  dplyr::select(date, GDP_Growth_YY)


# 3.2. Full JOIN all series
data_final <- hicp_processed %>%
  full_join(unemployment_data, by = "date") %>%
  full_join(ecb_rate_df, by = "date") %>%
  full_join(gdp_processed, by = "date") %>%
  
  # 3.3. Align to MONTHLY frequency and fill down/up for Quarterly GDP
  fill(GDP_Growth_YY, .direction = "downup") %>% 
  
  # 3.4. Select final variables and filter NA
  dplyr::select(date, Inflation_YY, Interest_Rate = ECB_Ref_Rate, Unemployment_Rate, GDP_Growth_YY) %>% 
  
  filter(date >= ymd("2001-01-01")) %>% 
  
  drop_na() 


# ==============================================================================
# 4. STATIONARITY PREPARATION (First Differences)
# ==============================================================================

# Variables already in Y/Y growth rates are likely stationary. 
# We difference I(1) variables (Interest Rate, Unemployment Rate) for OLS.

data_final_stac <- data_final %>%
  # 1. First Difference (D) for Interest Rate (i) - Month-on-Month
  dplyr::mutate(D_Interest_Rate = Interest_Rate - lag(Interest_Rate, 1)) %>%
  
  # 2. First Difference (D) for Unemployment Rate (u) - Month-on-Month
  dplyr::mutate(D_Unemployment_Rate = Unemployment_Rate - lag(Unemployment_Rate, 1)) %>%
  
  # 3. Select final variables for analysis
  dplyr::select(date, Inflation_YY, D_Interest_Rate, D_Unemployment_Rate, GDP_Growth_YY) %>%
  
  # 4. Remove NA values resulting from differencing
  tidyr::drop_na()

# ==============================================================================
# 5. OLS REGRESSION AND RESULTS (Demonstrating Econometric Skills)
# ==============================================================================

# OLS Regression on the STATIONARY DATA: 
# Model: Inflation_YY = f(Change in Interest Rate, Change in Unemployment, GDP Growth)
ols_model <- lm(Inflation_YY ~ D_Interest_Rate + D_Unemployment_Rate + GDP_Growth_YY, data = data_final_stac)

# Display results:
summary(ols_model)

# ==============================================================================
# 6. VISUALIZATION (Generating Time-Series Charts)
# ==============================================================================

# 1. Calculate the actual date range for the title
range_start <- format(min(data_final$date), "%Y")
range_end <- format(max(data_final$date), "%Y")
title_range <- paste0(range_start, "-", range_end)

# 2. Prepare data for ggplot (long format)
data_for_chart <- data_final %>%
  pivot_longer(
    cols = c(Inflation_YY, Interest_Rate, Unemployment_Rate, GDP_Growth_YY), 
    names_to = "Variable", 
    values_to = "Value"
  )

# 3. Create the chart (Facet Wrap - 2x2)
graf_timeseries <- ggplot(data_for_chart, aes(x = date, y = Value)) +
  geom_line(aes(color = Variable), linewidth = 0.8) + 
  
  facet_wrap(~ Variable, scales = "free_y", nrow = 2) + 
  
  labs(
    title = paste("Visual Check of Original Time Series (", title_range, ")", sep=""),
    subtitle = "Source: Eurostat and FRED",
    x = "Date",
    y = "Value (depending on series)"
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    legend.position = "none", 
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.subtitle = element_text(hjust = 0.5, size = 12),              
    strip.text = element_text(size = 12, face = "bold")               
  )

# 4. Save the chart 
ggsave("TimeSeries_R.png", plot = graf_timeseries, width = 10, height = 7, units = "in")
cat(paste("Chart TimeSeries_R.png (2x2) successfully generated.\n"))


# ==============================================================================
# 7. EXPORT
# ==============================================================================

# Print first 6 rows of the cleaned data
print(head(data_final))

# Export to CSV file
write_csv(data_final, "RH_Macro_Channel_Final.csv")
