library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(gridExtra) # For arranging multiple plots

########################################################
# STEP 1: Data Wrangling & Merging

# 1. Configuration: Define file names
file_weather_county      <- "countytotals_extremeweather.csv"
file_unemployment_county <- "unemployment_wide.csv"
file_weather_state       <- "statetotals_extremeweather.csv"
file_unemployment_state  <- "unemployment_statetotals.csv"

# 2. Read Data
# Ensure your working directory is set to the folder containing these files
weather_county      <- read.csv(file_weather_county)
unemployment_county <- read.csv(file_unemployment_county)
weather_state       <- read.csv(file_weather_state)
unemployment_state  <- read.csv(file_unemployment_state)

# 3. Clean & Merge County Level Data
unemployment_county <- unemployment_county %>%
  rename(County = Area_Name)

county_full <- inner_join(weather_county, unemployment_county, by = "County")

county_full <- county_full %>%
  select(-Population.y, -State.y, -Census.Region.Code, -FIPS_Code) %>%
  rename(State = State.x) %>%
  rename(Population = Population.x)

# 4. Clean & Merge State Level Data
weather_state <- weather_state %>%
  filter(!is.na(State.Code))

unemployment_state <- unemployment_state %>%
  filter(FIPS_Code != 0) %>%
  select(-State) %>%
  rename(State = Area_Name)

state_full <- inner_join(weather_state, unemployment_state, by = "State")

state_full <- state_full %>% 
  select(-Population.x, -Census.Region.Code)

########################################################
#  Pre-Step 2 Check: Missing Data Proportion


# Helper function to calculate missing proportion
calc_missing_prop <- function(column) {
  n_missing <- sum(is.na(column) | as.character(column) == "Suppressed")
  prop <- n_missing / length(column)
  return(scales::percent(prop, accuracy = 0.1))
}

cat("--- Missing Data Check ---\n")
cat("Total Counties:", nrow(county_full), "\n")
cat("Heat Deaths Missing %:", calc_missing_prop(county_full$Heat.Deaths), "\n")
cat("Cold Deaths Missing %:", calc_missing_prop(county_full$Cold.Deaths), "\n")
cat("Median Income Missing %:", 
    scales::percent(sum(is.na(county_full$Median_Household_Income_2022)) / nrow(county_full), 0.1), "\n")

########################################################
# STEP 2: Data Cleaning & Imputation

# 1. Helper function: Convert "Suppressed" to NA and ensure numeric type
clean_numeric <- function(x) {
  if (is.character(x)) {
    x <- ifelse(str_trim(x) == "Suppressed", NA, x)
  }
  return(as.numeric(x))
}

# 2. Apply cleaning to death columns
county_full <- county_full %>%
  mutate(
    Heat.Deaths = clean_numeric(Heat.Deaths),
    Cold.Deaths = clean_numeric(Cold.Deaths)
  )

state_full <- state_full %>%
  mutate(
    Total.State.Heat.Deaths = clean_numeric(Total.State.Heat.Deaths),
    Total.State.Cold.Deaths = clean_numeric(Total.State.Cold.Deaths)
  )

# 3. Handle Suppressed State Totals
# Logic: If State Total is Suppressed (NA), assume 5 (midpoint of 1-9)
state_full <- state_full %>%
  mutate(
    Total.State.Heat.Deaths = ifelse(is.na(Total.State.Heat.Deaths), 5, Total.State.Heat.Deaths),
    Total.State.Cold.Deaths = ifelse(is.na(Total.State.Cold.Deaths), 5, Total.State.Cold.Deaths)
  )

# 4. Imputation Logic Function
impute_deaths <- function(county_df, state_df, death_col_county, death_col_state) {
  
  # A. Calculate sum of known counties per state
  known_sums <- county_df %>%
    group_by(State) %>%
    summarize(
      Known_Sum = sum(.data[[death_col_county]], na.rm = TRUE), 
      Suppressed_Count = sum(is.na(.data[[death_col_county]]))
    )
  
  # B. Calculate residual deaths to distribute
  imputation_logic <- state_df %>%
    select(State, Actual_State_Total = .data[[death_col_state]]) %>%
    left_join(known_sums, by = "State") %>%
    mutate(
      Residual = Actual_State_Total - Known_Sum,
      Residual = ifelse(Residual < 0, 0, Residual), # Prevent negative residuals
      Imputed_Value = ifelse(Suppressed_Count > 0, Residual / Suppressed_Count, 0)
    )
  
  # C. Fill NA values in county data
  county_df_imputed <- county_df %>%
    left_join(imputation_logic %>% select(State, Imputed_Value), by = "State") %>%
    mutate(
      !!death_col_county := ifelse(is.na(.data[[death_col_county]]), 
                                   Imputed_Value, 
                                   .data[[death_col_county]])
    ) %>%
    select(-Imputed_Value)
  
  return(county_df_imputed)
}

# 5. Run Imputation
county_full <- impute_deaths(county_full, state_full, "Heat.Deaths", "Total.State.Heat.Deaths")
county_full <- impute_deaths(county_full, state_full, "Cold.Deaths", "Total.State.Cold.Deaths")

# 6. Create final Outcome Variable
county_full <- county_full %>%
  mutate(Weather.Deaths = Heat.Deaths + Cold.Deaths)


########################################################
# STEP 3: Variable Preparation & EDA


# Variable Preparation 
# A. Calculate Mean Unemployment (2018-2023)
unemployment_cols <- grep("Unemployment_rate_20", names(county_full), value = TRUE)

if(length(unemployment_cols) > 0) {
  county_full$Unemployment_Mean <- rowMeans(county_full[, unemployment_cols], na.rm = TRUE)
  county_full$Unemployment_SD <- apply(county_full[, unemployment_cols], 1, sd, na.rm = TRUE)
}

# B. Rename Columns for clarity
if ("Median_Household_Income_2022" %in% names(county_full)) {
  county_full <- county_full %>% rename(Median_Income = Median_Household_Income_2022)
}
if ("Rural_Urban_Continuum_Code_2023" %in% names(county_full)) {
  county_full <- county_full %>% rename(Rural_Code = Rural_Urban_Continuum_Code_2023)
}

# C. Calculate Crude Death Rate (per 100k) for Visualization
if (is.character(county_full$Population)) {
  county_full$Population <- as.numeric(gsub(",", "", county_full$Population))
}

county_full <- county_full %>%
  mutate(Death_Rate_100k = (Weather.Deaths / Population) * 100000)


# Visualizations 

# Figure 1: Stability of Unemployment Rates
p1 <- ggplot(county_full, aes(x = Unemployment_SD)) +
  geom_histogram(binwidth = 0.2, fill = "#4E79A7", color = "white") +
  theme_minimal() +
  labs(title = "Fig 1: Stability of Unemployment Rates (2018-2023)",
       subtitle = "Lower SD indicates the rate is stable over time.",
       x = "Standard Deviation (SD)", y = "Count of Counties")

# Figure 2: Distribution of Weather Deaths
p2 <- ggplot(county_full, aes(x = Weather.Deaths)) +
  geom_histogram(binwidth = 1, fill = "#E15759", color = "white") +
  scale_x_continuous(limits = c(0, 50)) + 
  theme_minimal() +
  labs(title = "Fig 2: Distribution of Extreme Weather Deaths",
       subtitle = "Right-skewed distribution, suitable for Poisson regression.",
       x = "Weather Deaths (Imputed)", y = "Count of Counties")

# Figure 3: Median Income vs. Death Rate
p3 <- ggplot(county_full, aes(x = Median_Income, y = Death_Rate_100k)) +
  geom_point(alpha = 0.3, size = 1, color = "grey60") +
  geom_smooth(method = "gam", color = "blue", fill = "lightblue") +
  scale_x_log10(labels = scales::dollar) + 
  coord_cartesian(ylim = c(0, 5)) + 
  theme_minimal() +
  labs(title = "Fig 3: Median Household Income vs. Death Rate",
       subtitle = "Downward trend implies higher income is associated with lower mortality.",
       x = "Median Household Income 2022 (Log Scale)", y = "Deaths per 100k")

# Figure 4: Rural/Urban Differences
p4 <- ggplot(county_full %>% filter(!is.na(Rural_Code)), 
             aes(x = factor(Rural_Code), y = Death_Rate_100k)) +
  geom_boxplot(fill = "#59A14F", outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 3)) +
  theme_minimal() +
  labs(title = "Fig 4: Rural-Urban Code vs. Death Rate",
       subtitle = "1 = Metro/Urban ---> 9 = Total Rural",
       x = "USDA Rural-Urban Code (1-9)", y = "Deaths per 100k")

# Figure 5: Distribution of Mean Unemployment
p5 <- ggplot(county_full, aes(x = Unemployment_Mean)) +
  geom_histogram(binwidth = 0.5, fill = "#F28E2B", color = "white") +
  theme_minimal() +
  labs(title = "Fig 5: Distribution of Mean Unemployment (2018-2023)",
       subtitle = "Check for normality or skewness.",
       x = "Mean Unemployment Rate (%)", y = "Count of Counties")

# Figure 6: Distribution of Median Income
p6 <- ggplot(county_full, aes(x = Median_Income)) +
  geom_histogram(bins = 50, fill = "#76B7B2", color = "white") +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal() +
  labs(title = "Fig 6: Distribution of Median Household Income",
       subtitle = "Right-skewed data often benefits from Log transformation in models.",
       x = "Median Income ($)", y = "Count of Counties")

# Figure 7: Collinearity Check (Income vs Unemployment)
p7 <- ggplot(county_full, aes(x = Median_Income, y = Unemployment_Mean)) +
  geom_point(alpha = 0.3, color = "grey50") +
  geom_smooth(method = "lm", color = "red") +
  scale_x_continuous(labels = scales::dollar) +
  theme_minimal() +
  labs(title = "Fig 7: Collinearity Check: Income vs. Unemployment",
       subtitle = "Strong correlation suggests avoiding both in the same model.",
       x = "Median Income ($)", y = "Mean Unemployment Rate (%)")

# Print Plots
print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)

# Final Summary 
cat("\n--- Data Prep Summary ---\n")
cat("1. Final County Count:", nrow(county_full), "\n")
cat("2. Counties missing Income data (will be dropped in model):", sum(is.na(county_full$Median_Income)), "\n")
cat("3. Average Death Rate (per 100k):", round(mean(county_full$Death_Rate_100k, na.rm=TRUE), 2), "\n")