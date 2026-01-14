# Load required packages
library(tidyr)
library(plm)
library(dplyr)
library(readr)
library(countrycode)
library(lmtest)
library(car)
library(fixest)
library(lme4)
library(CRE)

# Load your data sets
aiddata <- read_csv("china_aid_flows_country_year.csv")
crs <- read_csv("multilateral_aid_flows_country_year.csv") 
wgi <- read_csv("wgidataset.csv") #all WGI indicators
debt_service <- read_csv("DebtServiceRatios.csv") #DSR: Net interest/Exports
gdp_growth <- read_csv("gdp_growth.csv") # GDP Growth Rate
inflation <- read_csv("inflation_data.csv") #Inflation rate
trade <-  read_csv("trade_data.csv") # Trade/GDP
debt_gni_source <- read_csv("debt_gni.csv") # Debt/GNI
total_reserves_source <- read_csv("total_reserves.csv") # World Bank total foreign reserves
gdp_nominal_source <- read_csv("gdp_nominal.csv") # Nominal GDP to scale aid flows
adjustment_factor = 299.17 / 261.58 # CPI conversion from 2021 to 2023 USD

# ------------------------------------------
wgi_wide <- wgi %>%
  select(`Country Code`, year, indicator, estimate) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate
  ) %>%
  filter(complete.cases(ge, cc, rl, va, rq, pv))  # Only keep complete cases for PCA

# Perform PCA on the six governance indicators
pca_result <- prcomp(wgi_wide[, c("ge", "cc", "rl", "va", "rq", "pv")], 
                     center = TRUE, scale. = TRUE)

# Get the first principal component (composite governance index)
wgi_wide$governance_composite <- pca_result$x[, 1]

# Reverse the sign if needed (so greater = better governance)
if(cor(wgi_wide$governance_composite, wgi_wide$ge, use = "complete.obs") < 0) {
  wgi_wide$governance_composite <- -wgi_wide$governance_composite
}

governance_long <- wgi_wide %>%
  select(`Country Code`, year, governance_composite) %>%
  rename(Year = year, governance_ind = governance_composite)
names(governance_long)[names(governance_long) == "estimate"] <- "governance_ind"
names(governance_long)[names(governance_long) == "year"] <- "Year"


# Reshape all data sets from wide to long format
# ------------------------------------------
aiddata_long <- aiddata %>%
  pivot_longer(
    cols = `2000`:`2022`,  # Adjust year range as needed
    names_to = "Year",
    values_to = "china_loans"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(china_loans = china_loans * adjustment_factor)

crs_long <- crs %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year", 
    values_to = "multilateral_loans"
  ) %>%
  mutate(Year = as.numeric(Year))

debt_service_long <- debt_service %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "debt_service_ratio"
  ) %>%
  mutate(Year = as.numeric(Year))

gdp_growth_long <- gdp_growth %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "gdp_growth_rate"
  ) %>%
  mutate(Year = as.numeric(Year))

inflation_long <- inflation %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "inflation_rate"
  ) %>%
  mutate(Year = as.numeric(Year))

trade_long <- trade %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "trade_gdp"
  ) %>%
  mutate(Year = as.numeric(Year))

debt_gni_long <- debt_gni_source %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "debt_gni"
  ) %>%
  mutate(Year = as.numeric(Year))

total_reserves_long <- total_reserves_source %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "total_reserves"
  ) %>%
  mutate(Year = as.numeric(Year))

gdp_nominal_long <- gdp_nominal_source %>%
  pivot_longer(
    cols = `2000`:`2023`,
    names_to = "Year",
    values_to = "gdp_nominal"
  ) %>%
  mutate(Year = as.numeric(Year))
gdp_nominal_long$gdp_nominal <- gdp_nominal_long$gdp_nominal / 1000000

# Merge all data sets 
# --------------------------------------
final_data <- debt_service_long %>%
  left_join(governance_long, by = c("Country Code", "Year")) %>%
  left_join(aiddata_long, by = c("Country Code", "Year")) %>%
  left_join(crs_long, by = c("Country Code", "Year")) %>%
  left_join(gdp_growth_long, by = c("Country Code", "Year")) %>%
  left_join(inflation_long, by = c("Country Code", "Year")) %>%
  left_join(trade_long, by = c("Country Code", "Year")) %>%
  left_join(debt_gni_long, by = c("Country Code", "Year")) %>%
  left_join(total_reserves_long, by = c("Country Code", "Year")) %>%
  left_join(gdp_nominal_long, by = c("Country Code", "Year")) %>%
  arrange(`Country Code`, Year)
final_data <- final_data %>%
  filter(`Country Code` != "CHN")

summary(final_data$gdp_nominal)

final_data <- final_data %>%
  mutate(
    # Convert to percentage of GDP
    china_loans_gdp = (china_loans / gdp_nominal) * 100,
    multilateral_loans_gdp = (multilateral_loans / gdp_nominal) * 100,
    
    # Handle division by zero and extreme values
    china_loans_gdp = ifelse(is.infinite(china_loans_gdp), NA, china_loans_gdp),
    multilateral_loans_gdp = ifelse(is.infinite(multilateral_loans_gdp), NA, multilateral_loans_gdp),
    
    # Lag the variables
    china_loans_gdp <- lag(china_loans_gdp, 1),
    multilateral_loans_gdp <- lag(multilateral_loans_gdp, 1)
  )

winsorize_variable <- function(x, probs = c(0.01, 0.99)) {
  lower_bound <- quantile(x, probs[1], na.rm = TRUE)
  upper_bound <- quantile(x, probs[2], na.rm = TRUE)
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

# ------------------------------------------
  
# Create lagged variables and interaction terms
final_data_nolag <- final_data %>%
  mutate(
    china_corruption_interaction = china_loans * governance_ind,
    multilateral_corruption_interaction = multilateral_loans * governance_ind,
    debt_service_ratio = winsorize_variable(debt_service_ratio),
    inflation = winsorize_variable(inflation_rate),
    gdp_growth = winsorize_variable(gdp_growth_rate),
    china_loans = winsorize_variable(china_loans),
    multilateral_loans = winsorize_variable(multilateral_loans),
    total_reserves = winsorize_variable(total_reserves),
    debt_gni = winsorize_variable(debt_gni),
    trade = winsorize_variable(trade_gdp)
  )

final_data <- final_data %>%
  group_by(`Country Code`) %>%
  mutate(
    china_loans_lag1 = lag(china_loans, 1),
    multilateral_loans_lag1 = lag(multilateral_loans, 1),
    china_corruption_interaction = china_loans_lag1 * governance_ind,
    multilateral_corruption_interaction = multilateral_loans_lag1 * governance_ind,
    gdp_growth_lag1 = lag(gdp_growth_rate, 1),
    inflation_lag1 = lag(inflation_rate, 1),
    trade_lag1 = lag(trade_gdp, 1),
    debt_gni_lag1 = lag(debt_gni, 1),
    total_reserves_lag1 = lag(total_reserves, 1)
  ) %>%
  ungroup()

final_data <- final_data %>%
  mutate(
    debt_service_ratio = winsorize_variable(debt_service_ratio),
    inflation_lag1 = winsorize_variable(inflation_lag1),
    gdp_growth_lag1 = winsorize_variable(gdp_growth_lag1),
    china_loans_lag1 = winsorize_variable(china_loans_lag1),
    multilateral_loans_lag1 = winsorize_variable(multilateral_loans_lag1),
    total_reserves_lag1 = winsorize_variable(total_reserves_lag1),
    debt_gni_lag1 = winsorize_variable(debt_gni_lag1),
    trade_lag1 = winsorize_variable(trade_lag1)
  )

final_data_pre_bri <- final_data %>% filter(Year < 2013)
final_data_post_bri <- final_data %>% filter(Year >= 2013)

# Set as panel data
final_data <- final_data %>% rename(Country_Code = `Country Code`)
panel_data <- pdata.frame(final_data, index = c("Country_Code", "Year"))

# FE Model and Hausman test
fe_model <- plm(
  debt_service_ratio ~
    china_loans_lag1 +
    multilateral_loans_lag1 +
    governance_ind +
    china_corruption_interaction +
    multilateral_corruption_interaction +
    gdp_growth_lag1 + inflation_lag1 + trade_lag1 +
    debt_gni_lag1 + total_reserves_lag1,
  data = final_data,
  model = "within",
  effect = "twoways"
)
fe_hc3 <- coeftest(fe_model, vcov. = vcovHC(fe_model, type = "HC3"))
print(fe_hc3)
# summary(fe_model)


# MODEL ROBUSTNESS CHECKS:
# ----------------------------------

# Pre vs. Post BRI (no change, very low N)
fe_bri <- plm(
  debt_service_ratio ~
    china_loans_lag1 +
    multilateral_loans_lag1 +
    governance_ind +
    china_corruption_interaction +
    multilateral_corruption_interaction +
    gdp_growth_lag1 + inflation_lag1 + trade_lag1 +
    debt_gni_lag1 + total_reserves_lag1,
  data = final_data_pre_bri, #or final_data_post_bri
  model = "within",
  effect = "twoways"
)
fe_bri_comp <- coeftest(fe_bri, vcov. = vcovHC(fe_bri, type = "HC3"))
print(fe_bri_comp) #Pre BRI

# Loans as % GDP
fe_gdp_model <- plm(
  debt_service_ratio ~
    china_loans_gdp +
    multilateral_loans_gdp +
    governance_ind +
    china_corruption_interaction +
    multilateral_corruption_interaction +
    gdp_growth_lag1 + inflation_lag1 + trade_lag1 +
    debt_gni_lag1 + total_reserves_lag1,
  data = final_data,
  model = "within",
  effect = "twoways"
)
fe_gdp_hc3 <- coeftest(fe_gdp_model, vcov. = vcovHC(fe_gdp_model, type = "HC3"))
print(fe_gdp_hc3)

# Two year lag instead of one
lagged_data <- final_data %>%
  group_by(`Country_Code`) %>%
  mutate(
    china_loans_lag1 = lag(china_loans, 2),
    multilateral_loans_lag1 = lag(multilateral_loans, 2),
    china_corruption_interaction = china_loans_lag1 * governance_ind,
    multilateral_corruption_interaction = multilateral_loans_lag1 * governance_ind,
    gdp_growth_lag1 = lag(gdp_growth_rate, 2),
    inflation_lag1 = lag(inflation_rate, 2),
    trade_lag1 = lag(trade_gdp, 2),
    debt_gni_lag1 = lag(debt_gni, 2),
    total_reserves_lag1 = lag(total_reserves, 2)
  ) %>%
  ungroup()

fe_extralag <- plm(
  debt_service_ratio ~
    china_loans_lag1 +
    multilateral_loans_lag1 +
    governance_ind +
    china_corruption_interaction +
    multilateral_corruption_interaction +
    gdp_growth_lag1 + inflation_lag1 + trade_lag1 +
    debt_gni_lag1 + total_reserves_lag1,
  data = lagged_data,
  model = "within",
  effect = "twoways"
)
fe_extralag_hc3 <- coeftest(fe_extralag, vcov. = vcovHC(fe_extralag, type = "HC3"))
print(fe_extralag_hc3)

# No lag
fe_nolag <- plm(
  debt_service_ratio ~
    china_loans +
    multilateral_loans +
    governance_ind +
    china_corruption_interaction +
    multilateral_corruption_interaction +
    gdp_growth + inflation + trade +
    debt_gni + total_reserves,
  data = final_data_nolag,
  model = "within",
  effect = "twoways"
)
fe_nolag_hc3 <- coeftest(fe_nolag, vcov. = vcovHC(fe_nolag, type = "HC3"))
print(fe_nolag_hc3)

# Random Effects regression
re_model <- plm(
  debt_service_ratio ~
    china_loans_lag1 +
    multilateral_loans_lag1 +
    governance_ind +
    china_corruption_interaction +
    multilateral_corruption_interaction +
    gdp_growth_lag1 + inflation_lag1 + trade_lag1 +
    debt_gni_lag1 + total_reserves_lag1,
  data = final_data,
  model = "random",
  effect = "twoways"
)
re_hc3 <- coeftest(re_model, vcov. = vcovHC(re_model, type = "HC3"))
print(re_hc3) 

# Hausman test 
hausman_test <- phtest(fe_model, re_model)
print(hausman_test)

# 2. Mundlak formulation (hybrid model) - tests RE assumptions
mundlak_data <- final_data %>%
  group_by(Country_Code) %>%
  mutate(
    across(c(china_loans_lag1, multilateral_loans_lag1, governance_ind, gdp_growth_lag1,
             inflation_lag1, trade_lag1, debt_gni_lag1, total_reserves_lag1),
           list(mean = ~mean(., na.rm = TRUE)),
           .names = "{.col}_mean")
  ) %>%
  ungroup()

# Mundlak model using plm (allows HC3)
mundlak_plm <- plm(
  debt_service_ratio ~
    china_loans_lag1 + multilateral_loans_lag1 +
    governance_ind + china_corruption_interaction +
    multilateral_corruption_interaction + gdp_growth_lag1 +
    inflation_lag1 + trade_lag1 + debt_gni_lag1 + total_reserves_lag1 +
    # Add group means
    governance_ind_mean + china_loans_lag1_mean + multilateral_loans_lag1_mean +
    gdp_growth_lag1_mean + inflation_lag1_mean +
    trade_lag1_mean + debt_gni_lag1_mean + total_reserves_lag1_mean,
  data = mundlak_data,
  model = "pooling"  # Manually adding fixed effects via group means
)
summary(mundlak_plm)
mundlak_hc3 <- coeftest(mundlak_plm, vcov. = vcovHC(mundlak_plm, type = "HC3"))


print(mundlak_hc3)

# Run between-effects model to see what RE is capturing (DOUBLE CHECK THIS - RE WITHOUT HC3 IS WAY OFF):
between_data <- final_data %>%
  group_by(Country_Code) %>%
  summarise(across(
    c(debt_service_ratio, china_loans_lag1, multilateral_loans_lag1,
      governance_ind, china_corruption_interaction, multilateral_corruption_interaction,
      gdp_growth_lag1, inflation_lag1, trade_lag1, debt_gni_lag1, total_reserves_lag1),
    ~mean(., na.rm = TRUE),
    .names = "mean_{.col}"
  ))
between_simple <- lm(
  mean_debt_service_ratio ~
    mean_china_loans_lag1 + mean_multilateral_loans_lag1 +
    mean_governance_ind + mean_china_corruption_interaction +
    mean_multilateral_corruption_interaction + mean_gdp_growth_lag1 +
    mean_inflation_lag1 + mean_trade_lag1 + mean_debt_gni_lag1 +
    mean_total_reserves_lag1,
  data = between_data
)

between_simple_hc3 <- coeftest(between_simple, vcov. = vcovHC(between_simple, type = "HC3"))
print(between_simple_hc3)


# Governance as a threshold - no extra significance
print("Threshold: 49% and 51%")
final_data <- final_data %>%
  mutate(
    corruption_tertile = ntile(governance_ind, 3),
    high_corruption = as.numeric(governance_ind < quantile(governance_ind, 0.49, na.rm = TRUE)),
    low_corruption = as.numeric(governance_ind > quantile(governance_ind, 0.51, na.rm = TRUE))
  )

fe_model_threshold <- plm(
  debt_service_ratio ~
    china_loans_lag1 * high_corruption +
    china_loans_lag1 * low_corruption +
    multilateral_loans_lag1 * high_corruption +
    multilateral_loans_lag1 * low_corruption +
    gdp_growth_lag1 + inflation_lag1 + trade_lag1 +
    debt_gni_lag1 + total_reserves_lag1,
  data = final_data,
  model = "within"
)
fe_governance_threshold_hc3 <- coeftest(fe_model_threshold, vcov. = vcovHC(fe_model_threshold, type = "HC3"))
print(fe_governance_threshold_hc3)

# Regional interaction term - nothing special here (limited extra significance can be attributed to chance):
region_data <- final_data %>%
  mutate(
    region = countrycode(Country_Code, "iso3c", "region"),
    subregion = countrycode(Country_Code, "iso3c", "un.regionsub.name")
  )

fe_model_region_interact <- plm(
  debt_service_ratio ~
    china_loans_lag1 * region +
    multilateral_loans_lag1 * region +
    governance_ind + china_corruption_interaction +
    multilateral_corruption_interaction + gdp_growth_lag1 +
    inflation_lag1 + trade_lag1 + debt_gni_lag1 + total_reserves_lag1,
  data = region_data,
  model = "within",
  effect = "twoways"
)
fe_region_hc3 <- coeftest(fe_model_region_interact, vcov. = vcovHC(fe_model_region_interact, type = "HC3"))
print(fe_region_hc3)

# VIF TESTS 
fe_model_lm <- lm(
  debt_service_ratio ~
    china_loans_lag1 + multilateral_loans_lag1 +
    governance_ind + china_corruption_interaction +
    multilateral_corruption_interaction + gdp_growth_lag1 +
    inflation_lag1 + trade_lag1 + debt_gni_lag1 +
    total_reserves_lag1,
  # Note: No fixed effects for VIF - we're testing multicollinearity in the predictors only
  data = final_data
)

vif_results_fe <- vif(fe_model_lm)
print("Variance Inflation Factors for RE Specification:")
print(vif_results_fe)

# Correlation matrix for FE variables
cor_vars <- c("debt_service_ratio", "china_loans_lag1", "multilateral_loans_lag1",
              "governance_ind", "china_corruption_interaction",
              "multilateral_corruption_interaction", "gdp_growth_lag1",
              "inflation_lag1", "trade_lag1", "debt_gni_lag1", "total_reserves_lag1")

# Create correlation matrix
cor_matrix_re <- cor(final_data[, cor_vars], use = "complete.obs")
print("Correlation Matrix for RE Specification:")
print(round(cor_matrix_re, 3))
# ---------------------------------
# Calculate effect sizes
calculate_effect_sizes <- function(model, data, dsr_var = "debt_service_ratio") {
  
  # Extract coefficients and variable names
  coefficients <- coef(model)
  var_names <- names(coefficients)
  
  # Calculate SD of dependent variable
  sd_dsr <- sd(data[[dsr_var]], na.rm = TRUE)
  
  # Initialize results dataframe
  results <- data.frame(
    variable = character(),
    coefficient = numeric(),
    sd = numeric(),
    effect_size = numeric(),
    percent_of_dsr_sd = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate effect for each variable
  for (var in var_names) {
    # Handle interaction terms and regular variables
    if (var %in% names(data)) {
      var_sd <- sd(data[[var]], na.rm = TRUE)
    } else {
      # For interaction terms, check if components exist
      var_sd <- NA
      warning(paste("Variable", var, "not found in dataset. Skipping SD calculation."))
    }
    
    # Calculate effect size
    effect <- coefficients[var] * var_sd
    
    # Add to results
    results <- rbind(results, data.frame(
      variable = var,
      coefficient = coefficients[var],
      sd = var_sd,
      effect_size = effect,
      percent_of_dsr_sd = 100 * abs(effect) / sd_dsr
    ))
  }
  
  # Sort by absolute effect size (largest first)
  results <- results[order(-abs(results$effect_size)), ]
  
  # Print formatted table
  cat("=== EFFECT SIZES: 1 STANDARD DEVIATION INCREASE ===\n\n")
  cat("Dependent Variable SD:", round(sd_dsr, 2), "\n\n")
  
  print(
    results %>%
      mutate(
        effect_size = round(effect_size, 4),
        percent_of_dsr_sd = round(percent_of_dsr_sd, 1),
        coefficient = round(coefficient, 6),
        sd = round(sd, 2)
      ) %>%
      rename(
        Variable = variable,
        Coefficient = coefficient,
        SD = sd,
        `Effect_Size` = effect_size,
        `%_of_DSR_SD` = percent_of_dsr_sd
      ),
    row.names = FALSE
  )
  
  # Return results invisibly for further use
  invisible(results)
}
calculate_effect_sizes(fe_model, final_data)


# PCA Loadings
# ----------------------
print(pca_result$rotation)

# Get summary with proportion of variance explained
summary(pca_result)

# Optional: Create a nice table of loadings
loadings_table <- pca_result$rotation[, 1]  # Loadings for first principal component
print("Loadings for PC1 (composite index):")
print(loadings_table)

# extreme_debt_countries <- final_data %>%
#   group_by(Country_Code) %>%
#   summarize(max_dsr = max(debt_service_ratio, na.rm = TRUE)) %>%
#   filter(max_dsr > quantile(max_dsr, 0.95, na.rm = TRUE)) %>%
#   pull(Country_Code)
# print(extreme_debt_countries)