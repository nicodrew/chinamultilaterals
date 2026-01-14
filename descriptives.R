# Load required packages
library(tidyr)
library(plm)
library(dplyr)
library(readr)
library(countrycode)
library(lmtest)
library(car)
library(ggplot2)
library(stargazer)
# MOVE CHINESE LOAN INFLATION FROM R TO PYTHON

winsorize_variable <- function(x, probs = c(0.01, 0.99)) {
  lower_bound <- quantile(x, probs[1], na.rm = TRUE)
  upper_bound <- quantile(x, probs[2], na.rm = TRUE)
  x[x < lower_bound] <- lower_bound
  x[x > upper_bound] <- upper_bound
  return(x)
}

african_iso3 <- countrycode::codelist$iso3c[countrycode::codelist$continent == "Africa"]
african_iso3 <- african_iso3[!is.na(african_iso3)]
print(african_iso3)

# Load your data sets
aiddata <- read_csv("china_aid_flows_country_year.csv")
crs <- read_csv("multilateral_aid_flows_country_year.csv") 
# governance <- read_csv("CorruptionIndex.csv") # World Bank Corruption index
wgi <- read_csv("wgidataset.csv") #all WGI indicators
debt_service <- read_csv("DebtServiceRatios.csv") #DSR: Net interest/Exports
gdp_growth <- read_csv("gdp_growth.csv") # GDP Growth Rate
inflation <- read_csv("inflation_data.csv") #Inflation rate
trade <-  read_csv("trade_data.csv") # Trade/GDP
debt_gni_source <- read_csv("debt_gni.csv") # Debt/GNI
total_reserves_source <- read_csv("total_reserves.csv") # World Bank total foreign reserves
gdp_nominal_source <- read_csv("gdp_nominal.csv") # Nominal GDP to scale aid flows
adjustment_factor = 299.17 / 261.58

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

# Reverse the sign if needed (so higher = better governance)
if(cor(wgi_wide$governance_composite, wgi_wide$ge, use = "complete.obs") < 0) {
  wgi_wide$governance_composite <- -wgi_wide$governance_composite
}

governance_long <- wgi_wide %>%
  select(`Country Code`, year, governance_composite) %>%
  rename(Year = year, governance_ind = governance_composite)
names(governance_long)[names(governance_long) == "estimate"] <- "governance_ind"
names(governance_long)[names(governance_long) == "year"] <- "Year"
# Reshape all data sets from wide to long format
# ------------------
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

# governance_long <- governance %>%
#   pivot_longer(
#     cols = `2002`:`2023`,
#     names_to = "Year",
#     values_to = "control_of_corruption"
#   ) %>%
#   mutate(Year = as.numeric(Year))

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
# --------------
panel_data <- debt_service_long %>%
  left_join(governance_long, by = c("Country Code", "Year")) %>%
  left_join(aiddata_long, by = c("Country Code", "Year")) %>%
  left_join(crs_long, by = c("Country Code", "Year")) %>%
  left_join(gdp_growth_long, by = c("Country Code", "Year")) %>%
  left_join(inflation_long, by = c("Country Code", "Year")) %>%
  left_join(trade_long, by = c("Country Code", "Year")) %>%
  left_join(debt_gni_long, by = c("Country Code", "Year")) %>%
  left_join(total_reserves_long, by = c("Country Code", "Year")) %>%
  arrange(`Country Code`, Year)%>%
  filter(`Country Code` != "CHN")

# Calculate aggregate lending by year
aggregate_lending <- panel_data %>%
  group_by(Year) %>%
  summarise(
    china_total = sum(china_loans, na.rm = TRUE),
    multilateral_total = sum(multilateral_loans, na.rm = TRUE)
  ) %>%
  filter(Year >= 2000 & Year <= 2022) %>%
  pivot_longer(
    cols = c(china_total, multilateral_total),
    names_to = "lender_type",
    values_to = "total_lending"
  ) %>%
  mutate(
    lender_type = case_when(
      lender_type == "china_total" ~ "China",
      lender_type == "multilateral_total" ~ "Multilateral"
    )
  )


aggregate_lending_billions <- aggregate_lending %>%
  mutate(total_lending_billions = total_lending / 1000)

panel_data <- panel_data %>%
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

panel_data <- panel_data %>%
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

# Calculate total aid per country over time
aid_totals <- panel_data %>%
  group_by(`Country Code`) %>%
  summarize(
    total_china_loans = sum(china_loans_lag1, na.rm = TRUE),
    total_multilateral_loans = sum(multilateral_loans_lag1, na.rm = TRUE),
    total_all_loans = total_china_loans + total_multilateral_loans,
    country_name = first(`Country Code`)  # Adjust if you have a name column
  ) %>%
  arrange(desc(total_china_loans))

total_flows <- panel_data %>%
  summarise(
    total_china = sum(china_loans_lag1, na.rm = TRUE),
    total_multilateral = sum(multilateral_loans_lag1, na.rm = TRUE),
    total_all = total_china + total_multilateral
  )

print(total_flows$total_china)

# View top recipients
print("Top 10 Aid Recipients:")
print(head(aid_totals, 10))

print("Top 10 Chinese Aid Recipients:")
print(head(aid_totals %>% arrange(desc(total_china_loans)), 10))

# Top 10 Multilateral aid recipients  
print("Top 10 Multilateral Aid Recipients:")
print(head(aid_totals %>% arrange(desc(total_multilateral_loans)), 10))

descriptive_vars <- c("debt_service_ratio", "china_loans_lag1", "multilateral_loans_lag1",
                      "governance_ind", "gdp_growth_lag1", "inflation_lag1", 
                      "trade_lag1", "debt_gni_lag1", "total_reserves_lag1")

descriptive_data <- final_data[descriptive_vars]

# Simple but effective
analysis_sample <- final_data %>%
  filter(!is.na(debt_service_ratio) & 
           !is.na(china_loans_lag1) & 
           !is.na(multilateral_loans_lag1) &
           !is.na(governance_ind))

cat("ACTUAL Analysis Sample:\n")
cat("Countries:", length(unique(analysis_sample$Country_Code)), "\n")
cat("Years:", length(unique(analysis_sample$Year)), "\n") 
cat("Total observations:", nrow(analysis_sample), "\n")

# Descriptives on actual analysis sample
stargazer(analysis_sample[descriptive_vars], 
          type = "text",
          title = "Descriptive Statistics (Analysis Sample)",
          digits = 2)

# Observations per country
obs_per_country <- final_data %>%
  count(Country_Code) %>%
  summarise(mean_obs = mean(n),
            min_obs = min(n), 
            max_obs = max(n))
print(obs_per_country)

regression_countries <- final_data %>%
  # Select only the variables used in your regression
  select(Country_Code, debt_service_ratio, china_loans_lag1, multilateral_loans_lag1,
         governance_ind, china_corruption_interaction, multilateral_corruption_interaction,
         gdp_growth_lag1, inflation_lag1, trade_lag1, debt_gni_lag1, total_reserves_lag1) %>%
  # Keep only complete cases (rows with no NAs in regression variables)
  na.omit() %>%
  # Get unique country codes
  distinct(Country_Code) %>%
  pull(Country_Code)

# Now get region info for these countries only
regression_countries_with_region <- final_data %>%
  filter(Country_Code %in% regression_countries) %>%
  mutate(
    region = countrycode(Country_Code, "iso3c", "region"),
    subregion = countrycode(Country_Code, "iso3c", "un.regionsub.name")
  ) %>%
  distinct(Country_Code, region, subregion)

# Print countries grouped by region that are in the regression
regression_countries_with_region %>%
  arrange(region, Country_Code) %>%
  group_by(region) %>%
  summarise(
    countries = paste(sort(unique(Country_Code)), collapse = ", "),
    count = n()
  ) %>%
  print(n = Inf)

# Also print the total count
cat("\nTotal countries in regression:", length(regression_countries), "\n")

governance_range <- panel_data %>%
  group_by(`Country Code`) %>%
  summarise(
    governance_range = max(governance_ind, na.rm = TRUE) - min(governance_ind, na.rm = TRUE),
    governance_min = min(governance_ind, na.rm = TRUE),
    governance_max = max(governance_ind, na.rm = TRUE),
    governance_mean = mean(governance_ind, na.rm = TRUE),
    n_years = sum(!is.na(governance_ind))
  ) %>%
  filter(n_years > 0) %>%  # Remove countries with no governance data
  arrange(desc(governance_range))

# Display top 10 countries with largest governance range (most variation)
cat("TOP 10 COUNTRIES WITH LARGEST GOVERNANCE RANGE (Most Variation):\n")
top_10_largest_range <- head(governance_range, 10)
print(top_10_largest_range, n = 10)

# Display bottom 10 countries with smallest governance range (most stable)
cat("\nBOTTOM 10 COUNTRIES WITH SMALLEST GOVERNANCE RANGE (Most Stable):\n")
bottom_10_smallest_range <- tail(governance_range %>% arrange(governance_range), 10)
print(bottom_10_smallest_range, n = 10)

# Summary statistics of governance ranges
cat("\nSUMMARY STATISTICS OF GOVERNANCE RANGES:\n")
summary_stats <- governance_range %>%
  summarise(
    mean_range = mean(governance_range, na.rm = TRUE),
    median_range = median(governance_range, na.rm = TRUE),
    sd_range = sd(governance_range, na.rm = TRUE),
    min_range = min(governance_range, na.rm = TRUE),
    max_range = max(governance_range, na.rm = TRUE),
    total_countries = n()
  )
print(summary_stats)

countries_range_gt_1 <- governance_range %>%
  filter(governance_range > 2.25) %>%
  # Get the years when min and max occurred to determine temporal direction
  left_join(
    panel_data %>%
      group_by(`Country Code`) %>%
      filter(governance_ind == max(governance_ind, na.rm = TRUE)) %>%
      summarise(max_year = first(Year)),
    by = "Country Code"
  ) %>%
  left_join(
    panel_data %>%
      group_by(`Country Code`) %>%
      filter(governance_ind == min(governance_ind, na.rm = TRUE)) %>%
      summarise(min_year = first(Year)),
    by = "Country Code"
  ) %>%
  mutate(
    temporal_direction = ifelse(max_year > min_year, "Increase", "Decrease"),
    direction_detail = ifelse(max_year > min_year, 
                              "Low to High (Improvement)", 
                              "High to Low (Deterioration)")
  ) %>%
  arrange(desc(governance_range))

cat("COUNTRIES WITH GOVERNANCE RANGE GREATER THAN 1 SD:\n")
print(countries_range_gt_1, n = nrow(countries_range_gt_1))

# Summary by direction
cat("\nSUMMARY BY TEMPORAL DIRECTION:\n")
countries_range_gt_1 %>%
  count(temporal_direction, direction_detail) %>%
  print()

gov_summary <- panel_data %>%
  summarize(
    median = median(governance_ind, na.rm = TRUE),
    min = min(governance_ind, na.rm = TRUE),
    max = max(governance_ind, na.rm = TRUE),
    range = max(governance_ind, na.rm = TRUE) - min(governance_ind, na.rm = TRUE)
  )

# Deciles
gov_deciles <- quantile(panel_data$governance_ind, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Print results
print(gov_summary)
print(gov_deciles)

# Create descriptive statistics table
desc_vars <- c("debt_service_ratio", "china_loans_lag1", "multilateral_loans_lag1",
               "governance_ind", "gdp_growth_lag1", "inflation_lag1", 
               "trade_lag1", "debt_gni_lag1", "total_reserves_lag1")

# Create empty data frame for results
descriptive_stats <- data.frame(
  Variable = desc_vars,
  N = numeric(length(desc_vars)),
  Mean = numeric(length(desc_vars)),
  SD = numeric(length(desc_vars)),
  Min = numeric(length(desc_vars)),
  Max = numeric(length(desc_vars)),
  Median = numeric(length(desc_vars)),
  stringsAsFactors = FALSE
)

# Calculate statistics for each variable
for (i in seq_along(desc_vars)) {
  var_name <- desc_vars[i]
  var_data <- panel_data[[var_name]]
  
  descriptive_stats$N[i] <- sum(!is.na(var_data))
  descriptive_stats$Mean[i] <- round(mean(var_data, na.rm = TRUE), 3)
  descriptive_stats$SD[i] <- round(sd(var_data, na.rm = TRUE), 3)
  descriptive_stats$Min[i] <- round(min(var_data, na.rm = TRUE), 3)
  descriptive_stats$Max[i] <- round(max(var_data, na.rm = TRUE), 3)
  descriptive_stats$Median[i] <- round(median(var_data, na.rm = TRUE), 3)
}

# Print the table
cat("\n=== DESCRIPTIVE STATISTICS TABLE ===\n")
print(descriptive_stats, row.names = FALSE)


# Aggregate Lending Graph
ggplot(aggregate_lending, aes(x = Year, y = total_lending, group = lender_type)) +
  geom_line(aes(color = lender_type), linewidth = 1.2) +
  geom_point(data = subset(aggregate_lending, lender_type == "China"),
             aes(color = lender_type), shape = 15, size = 2) +
  geom_point(data = subset(aggregate_lending, lender_type == "Multilateral"),
             aes(color = lender_type), shape = 19, size = 2) +
  geom_smooth(aes(color = lender_type), method = "lm", se = FALSE, 
              linewidth = 0.5, linetype = "dashed", alpha = 0.7) +
  scale_color_manual(
    values = c("China" = "#E41A1C", "Multilateral" = "#377EB8"),
    name = "Lender Type"
  ) +
  labs(
    title = "Aggregate Lending Over Time",
    subtitle = "China vs. Multilateral Sources (2000-2022)",
    x = "Year",
    y = "Total Lending (Millions USD)",
    caption = "Source: AidData and OECD CRS databases\nDashed lines show linear trends"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_y_continuous(labels = scales::comma)

# Optional: Save the plot
ggsave("aggregate_lending_trends.png", width = 10, height = 6, dpi = 300)

# Calculate Herfindahl-Hirschman Index for loan concentration
calculate_hhi <- function(loan_amounts) {
  # Remove NA values
  loan_amounts <- loan_amounts[!is.na(loan_amounts)]
  
  # Calculate market shares
  total_loans <- sum(loan_amounts)
  if (total_loans == 0) return(NA)
  
  market_shares <- loan_amounts / total_loans
  
  # Calculate HHI (sum of squared market shares, multiplied by 10,000)
  hhi <- sum(market_shares^2) * 10000
  
  return(hhi)
}

# Calculate HHI for Chinese loans
# Calculate Herfindahl-Hirschman Index for loan concentration
calculate_hhi <- function(loan_amounts) {
  # Remove NA values
  loan_amounts <- loan_amounts[!is.na(loan_amounts)]
  
  # Calculate market shares
  total_loans <- sum(loan_amounts)
  if (total_loans == 0) return(NA)
  
  market_shares <- loan_amounts / total_loans
  
  # Calculate HHI (sum of squared market shares, multiplied by 10,000)
  hhi <- sum(market_shares^2) * 10000
  
  return(hhi)
}

# HHI for chinese/multi loans
calculate_hhi <- function(loan_amounts) {
  # Remove NA values
  loan_amounts <- loan_amounts[!is.na(loan_amounts)]
  
  # Calculate market shares
  total_loans <- sum(loan_amounts)
  if (total_loans == 0) return(NA)
  
  market_shares <- loan_amounts / total_loans
  
  # Calculate HHI (sum of squared market shares, multiplied by 10,000)
  hhi <- sum(market_shares^2) * 10000
  
  return(hhi)
}

hhi_china <- panel_data %>%
  filter(!is.na(china_loans) & china_loans > 0) %>%
  group_by(`Country Code`) %>%
  summarise(total_china = sum(china_loans, na.rm = TRUE)) %>%
  pull(total_china) %>%
  calculate_hhi()

# Calculate HHI for multilateral loans  
hhi_multilateral <- panel_data %>%
  filter(!is.na(multilateral_loans) & multilateral_loans > 0) %>%
  group_by(`Country Code`) %>%
  summarise(total_multilateral = sum(multilateral_loans, na.rm = TRUE)) %>%
  pull(total_multilateral) %>%
  calculate_hhi()

# Print results
cat("Herfindahl-Hirschman Index (HHI) for Loan Concentration:\n")
cat("Chinese loans HHI:", round(hhi_china, 2), "\n")
cat("Multilateral loans HHI:", round(hhi_multilateral, 2), "\n\n")

# Interpretation
cat("HHI Interpretation:\n")
cat("Below 1,500: Unconcentrated\n")
cat("1,500-2,500: Moderately concentrated\n") 
cat("Above 2,500: Highly concentrated\n")
