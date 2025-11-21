# --- 0. Clear Environment and Load All Libraries ---
rm(list=ls())

library(quantmod)
library(tidyverse)
library(fpp3)
library(ggthemes)

# --- 1. Load Stock Data (quantmod) ---
# This creates the 'AZO' (xts) object
getSymbols("AZO", from = "2020-01-01", to = Sys.Date(), src = "yahoo")

# --- 2. Create the 'azo_data' (data.frame) object ---
# We use zoo::index() to prevent package conflicts
azo_data <- data.frame(
     date = zoo::index(AZO),
     Open = as.numeric(AZO$AZO.Open),
     High = as.numeric(AZO$AZO.High),
     Low = as.numeric(AZO$AZO.Low),
     Close = as.numeric(AZO$AZO.Close),
     Volume = as.numeric(AZO$AZO.Volume),
     Adjusted = as.numeric(AZO$AZO.Adjusted)
)

# --- 3. Create Revenue Data ---
# This creates 'revenue_long'
fiscal_year <- c(2025, 2025, 2025, 2025,
                 2024, 2024, 2024, 2024,
                 2023, 2023, 2023, 2023,
                 2022, 2022, 2022, 2022,
                 2021, 2021, 2021, 2021)
quarter <- rep(c("Q1", "Q2", "Q3", "Q4"), times = 5)
revenue <- c(
     3.95, 4.46, 4.50, 6.24,
     3.70, 3.95, 4.09, 5.69,
     3.70, 3.69, 4.09, 5.69,
     3.59, 3.69, 3.99, 5.39,
     3.39, 3.49, 3.79, 5.19
)
revenue_long <- data.frame(
     Fiscal_Year = fiscal_year,
     Quarter = quarter,
     Revenue = revenue
)

# --- 4. Revenue Forecast (ETS Model) ---

# Prepare the revenue tsibble
revenue_long %>% 
     mutate(Quarter_Date = yearquarter(paste(Fiscal_Year, Quarter))) %>% 
     arrange(Quarter_Date) %>% 
     as_tsibble(index = Quarter_Date) -> revenue_ts

# Fit the automated ETS model
revenue_ts %>% 
     model(ETS_Model = ETS(Revenue)) -> final_revenue_fit

# Report and Glance
report(final_revenue_fit)
glance(final_revenue_fit)

# Generate the final forecast (8 quarters)
final_revenue_fit %>% 
     forecast(h = 8) %>% 
     hilo(level = 90) -> final_revenue_fc

# Print the final forecast table
print(final_revenue_fc)

# --- 5. Revenue Forecast Plot (Manual ggplot) ---

# Extract the forecast into a simple data frame
revenue_fc_data <- as_tibble(final_revenue_fc)

# Manually split the '90%' hilo column
revenue_fc_data <- revenue_fc_data %>% 
     unpack_hilo('90%')

# Manually build the plot
ggplot(revenue_ts, aes(x = Quarter_Date)) +
     geom_line(aes(y = Revenue), color = "black") + # Original data
     geom_line(data = revenue_fc_data, aes(y = .mean), color = "blue", linetype = "dashed") + # Forecast mean
     geom_ribbon(data = revenue_fc_data, aes(ymin = `90%_lower`, ymax = `90%_upper`), 
                 fill = "blue", alpha = 0.2) + # Confidence interval
     labs(title = "AutoZone Revenue Forecast (ETS Model)",
          y = "Revenue (Billion USD)", x = "Quarter") +
     theme_classic()

# --- 6. Stock Price Forecast (Naive w/ Drift) ---

# Prepare the daily stock price tsibble (must fill gaps)
azo_data %>% 
     mutate(date = as_date(date)) %>% 
     as_tsibble(index = date) %>% 
     fill_gaps() -> azo_tsbl_daily

# Fit the Naive_Drift model
azo_tsbl_daily %>% 
     model(Naive_Drift = NAIVE(Adjusted ~ drift())) -> final_stock_fit

# Report and Glance
report(final_stock_fit)
glance(final_stock_fit)

# Generate the final forecast (e.g., "3 months")
final_stock_fit %>% 
     forecast(h = "3 months") %>% 
     hilo(level = 90) -> final_stock_fc

# Print the final forecast table
print(final_stock_fc)

# Filter for a cleaner plot
azo_tsbl_daily %>% 
     filter(year(date) >= 2024) -> azo_recent

# --- 7. Stock Price Plot (Manual ggplot) ---

# Extract the forecast into a simple data frame
stock_fc_data <- as_tibble(final_stock_fc)

# Manually split the '90%' hilo column
stock_fc_data <- stock_fc_data %>% 
     unpack_hilo('90%')

# Manually build the plot
ggplot(azo_recent, aes(x = date)) +
     geom_line(aes(y = Adjusted), color = "black") + # Original data
     geom_line(data = stock_fc_data, aes(y = .mean), color = "blue", linetype = "dashed") + # Forecast mean
     geom_ribbon(data = stock_fc_data, aes(ymin = `90%_lower`, ymax = `90%_upper`), 
                 fill = "blue", alpha = 0.2) + # Confidence interval
     labs(title = "AutoZone Stock Price Forecast (Naive with Drift)",
          y = "Price (USD)", x = "Date") +
     theme_classic()
