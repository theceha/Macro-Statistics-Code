# vecm_var_analysis.R
# Advanced Time Series Analysis: Stationarity, Cointegration, VECM, and IRF

# This script assumes 'data_final' has been loaded or sourced from ESCB_Macro_Pipeline.R

# ==============================================================================
# 1. SETUP AND TIME SERIES OBJECT CREATION
# ==============================================================================
library(vars)
library(tseries)
library(urca)

# Create a time series object from the final merged data
ts_data <- ts(data.frame(
  Y_t = data_final$GDP_Growth_YY,        # GDP Growth (Q/Q or Y/Y)
  Pi_t = data_final$Inflation_YY,       # Inflation (HICP Y/Y)
  i_t = data_final$Interest_Rate        # ECB Key Interest Rate
), start = c(2001, 1), frequency = 12) # Monthly frequency


# ==============================================================================
# 2. UNIT ROOT TESTS (Stationarity)
# ==============================================================================

cat("--- ADF Tests on Series Levels ---\n")
print(adf.test(ts_data[, "Y_t"]))
print(adf.test(ts_data[, "Pi_t"]))
print(adf.test(ts_data[, "i_t"]))

# Create differenced series for I(1) variables
diff_ts_data <- diff(ts_data)

cat("--- ADF Tests on First Differences ---\n")
print(adf.test(diff_ts_data[, "Y_t"]))
print(adf.test(diff_ts_data[, "Pi_t"]))
print(adf.test(diff_ts_data[, "i_t"]))


# ==============================================================================
# 3. LAG ORDER SELECTION
# ==============================================================================
# Use differenced data for optimal lag selection (p)
select_p <- VARselect(diff_ts_data, lag.max = 12, type = "const")

cat("--- Optimal Lag Order Selection ---\n")
print(select_p$selection)

# Selecting optimal p based on AIC or HQ (e.g., p_opt = 12 based on AIC, or p_opt = HQ)
p_opt <- select_p$selection["HQ(n)"]  
cat(paste("\nSelected optimal lag order (HQ criterion) is:", p_opt, "\n"))


# ==============================================================================
# 4. JOHANSEN COINTEGRATION TEST
# ==============================================================================
# Run Johansen Test on the LEVELS of the series (ts_data)
jo_test <- ca.jo(ts_data, type="trace", K=p_opt, ecdet="const", spec="longrun")

cat("--- Johansen Cointegration Test (Trace Statistic) ---\n")
summary(jo_test)

# Display Cointegration Vector (Long-Run Relationship)
cat("--- Cointegration Vector (Normalized on GDP Growth) ---\n")
print(jo_test@V[, 1]) 


# ==============================================================================
# 5. VECM ESTIMATION AND IRF (Impulse Response Functions)
# ==============================================================================

# Transform VECM to VAR for IRF plotting (assuming r=1 cointegrating relationship)
vecm_model <- vec2var(jo_test, r = 1)


# VECM/VAR Model DIAGNOSTICS
cat("--- Diagnostics (Portmanteau Test for Serial Correlation) ---\n")
# Goal: p-value > 0.05
print(serial.test(vecm_model, lags.pt = 12, type = "PT.asymptotic"))


# SVAR (Cholesky) IRF - Addressing the Monetary Policy Channel
# Order: Y_t (GDP), Pi_t (Inflation), i_t (Interest Rate)
irf_results_vecm <- irf(vecm_model, 
                       impulse = "i_t",        # Shock in Interest Rate
                       response = "Pi_t",      # Response of Inflation
                       n.ahead = 24,           # 2 years ahead
                       boot = TRUE,            # Bootstrapping for confidence intervals
                       ci = 0.95,
                       runs = 500,
                       ortho = TRUE)           # SVECM identification

# Visualization of IRF
# plot(irf_results_vecm) # Uncomment to plot results
