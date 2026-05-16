############################################################
# Renewable Energy Adoption & Carbon Emission Analytics
# Dataset: Our World in Data CO2 and Energy Dataset
# Focus: EDA + Sustainability Analytics + Statistics + Regression
############################################################

# 1. Packages -------------------------------------------------------------

packages <- c(
  "tidyverse", "janitor", "skimr", "naniar",
  "corrplot", "rstatix", "broom", "scales", "zoo"
)

install.packages(setdiff(packages, rownames(installed.packages())))

library(tidyverse)
library(janitor)
library(skimr)
library(naniar)
library(corrplot)
library(rstatix)
library(broom)
library(scales)
library(zoo)


# 2. Create Folders -------------------------------------------------------

dir.create("outputs", showWarnings = FALSE)
dir.create("visuals", showWarnings = FALSE)


# 3. Import Data ----------------------------------------------------------

co2_raw <- read_csv("data/owid-co2-data.csv")

glimpse(co2_raw)
dim(co2_raw)
names(co2_raw)


# 4. Data Cleaning --------------------------------------------------------

co2_data <- co2_raw %>%
  clean_names() %>%
  filter(!is.na(country), !is.na(year)) %>%
  filter(!is.na(iso_code)) %>%
  filter(year >= 1990) %>%
  select(
    country,
    iso_code,
    year,
    population,
    gdp,
    co2,
    co2_per_capita,
    co2_per_gdp,
    methane,
    methane_per_capita,
    primary_energy_consumption,
    energy_per_capita,
    energy_per_gdp,
    coal_co2,
    oil_co2,
    gas_co2,
    cement_co2,
    temperature_change_from_co2,
    share_global_co2
  )

# Main analysis dataset
co2_clean <- co2_data %>%
  drop_na(
    co2,
    co2_per_capita,
    population,
    gdp,
    energy_per_capita
  ) %>%
  mutate(
    gdp_per_capita = gdp / population,
    co2_intensity = co2 / gdp,
    decade = floor(year / 10) * 10,
    emission_category = case_when(
      co2_per_capita >= quantile(co2_per_capita, 0.75, na.rm = TRUE) ~ "High emission",
      co2_per_capita <= quantile(co2_per_capita, 0.25, na.rm = TRUE) ~ "Low emission",
      TRUE ~ "Medium emission"
    )
  )

glimpse(co2_clean)
skim(co2_clean)


# 5. Missing Value Summary ------------------------------------------------

missing_summary <- co2_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_values"
  ) %>%
  arrange(desc(missing_values))

print(missing_summary)

write_csv(missing_summary, "outputs/missing_value_summary.csv")


# 6. Descriptive Statistics ----------------------------------------------

descriptive_stats <- co2_clean %>%
  summarise(
    observations = n(),
    countries = n_distinct(country),
    first_year = min(year),
    last_year = max(year),
    
    mean_co2 = mean(co2, na.rm = TRUE),
    median_co2 = median(co2, na.rm = TRUE),
    sd_co2 = sd(co2, na.rm = TRUE),
    iqr_co2 = IQR(co2, na.rm = TRUE),
    
    mean_co2_per_capita = mean(co2_per_capita, na.rm = TRUE),
    median_co2_per_capita = median(co2_per_capita, na.rm = TRUE),
    sd_co2_per_capita = sd(co2_per_capita, na.rm = TRUE),
    
    mean_energy_per_capita = mean(energy_per_capita, na.rm = TRUE),
    median_energy_per_capita = median(energy_per_capita, na.rm = TRUE),
    
    mean_gdp_per_capita = mean(gdp_per_capita, na.rm = TRUE),
    median_gdp_per_capita = median(gdp_per_capita, na.rm = TRUE)
  )

print(descriptive_stats)

write_csv(descriptive_stats, "outputs/descriptive_statistics.csv")


# 7. Global CO2 Trend -----------------------------------------------------

global_trend <- co2_clean %>%
  group_by(year) %>%
  summarise(
    total_co2 = sum(co2, na.rm = TRUE),
    mean_co2_per_capita = mean(co2_per_capita, na.rm = TRUE),
    mean_energy_per_capita = mean(energy_per_capita, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rolling_co2 = rollmean(total_co2, k = 5, fill = NA, align = "right")
  )

p_global_trend <- ggplot(global_trend, aes(x = year)) +
  geom_line(aes(y = total_co2), alpha = 0.45) +
  geom_line(aes(y = rolling_co2), linewidth = 1) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Global CO2 Emission Trend",
    subtitle = "Annual total CO2 emissions with 5-year rolling average",
    x = "Year",
    y = "Total CO2 emissions"
  ) +
  theme_minimal(base_size = 13)

print(p_global_trend)


# 8. Top Emitting Countries ----------------------------------------------

latest_year <- max(co2_clean$year, na.rm = TRUE)

top_emitters <- co2_clean %>%
  filter(year == latest_year) %>%
  arrange(desc(co2)) %>%
  slice_head(n = 15)

p_top_emitters <- ggplot(
  top_emitters,
  aes(x = reorder(country, co2), y = co2)
) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(
    title = paste("Top 15 CO2 Emitting Countries in", latest_year),
    x = "Country",
    y = "CO2 emissions"
  ) +
  theme_minimal(base_size = 13)

print(p_top_emitters)


# 9. CO2 Per Capita Comparison -------------------------------------------

top_per_capita <- co2_clean %>%
  filter(year == latest_year, population > 1000000) %>%
  arrange(desc(co2_per_capita)) %>%
  slice_head(n = 15)

p_per_capita <- ggplot(
  top_per_capita,
  aes(x = reorder(country, co2_per_capita), y = co2_per_capita)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Highest CO2 Emissions per Capita in", latest_year),
    subtitle = "Countries with population above 1 million",
    x = "Country",
    y = "CO2 per capita"
  ) +
  theme_minimal(base_size = 13)

print(p_per_capita)


# 10. Distribution Analysis ----------------------------------------------

p_distribution <- ggplot(co2_clean, aes(x = co2_per_capita)) +
  geom_histogram(bins = 50, color = "white") +
  labs(
    title = "Distribution of CO2 Emissions per Capita",
    x = "CO2 per capita",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13)

print(p_distribution)


# 11. Probability / Risk Analysis ----------------------------------------

high_threshold <- quantile(co2_clean$co2_per_capita, 0.75, na.rm = TRUE)

probability_high_emission <- co2_clean %>%
  summarise(
    threshold_75th_percentile = high_threshold,
    total_observations = n(),
    high_emission_observations = sum(co2_per_capita > high_threshold, na.rm = TRUE),
    probability_high_emission = mean(co2_per_capita > high_threshold, na.rm = TRUE)
  )

print(probability_high_emission)

risk_by_decade <- co2_clean %>%
  group_by(decade) %>%
  summarise(
    observations = n(),
    probability_high_emission = mean(co2_per_capita > high_threshold, na.rm = TRUE),
    mean_co2_per_capita = mean(co2_per_capita, na.rm = TRUE),
    .groups = "drop"
  )

print(risk_by_decade)

p_risk_decade <- ggplot(
  risk_by_decade,
  aes(x = decade, y = probability_high_emission)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Probability of High CO2-per-Capita Events by Decade",
    x = "Decade",
    y = "Probability"
  ) +
  theme_minimal(base_size = 13)

print(p_risk_decade)


# 12. Country Trend Comparison -------------------------------------------

selected_countries <- c(
  "Germany", "United States", "China", "India",
  "United Kingdom", "France", "Bangladesh"
)

country_trends <- co2_clean %>%
  filter(country %in% selected_countries)

p_country_trends <- ggplot(
  country_trends,
  aes(x = year, y = co2_per_capita, group = country)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "CO2 Emissions per Capita: Selected Countries",
    x = "Year",
    y = "CO2 per capita"
  ) +
  theme_minimal(base_size = 13)

print(p_country_trends)


# 13. Hypothesis Testing --------------------------------------------------

# Research question:
# Do high-income proxy countries have higher CO2 per capita than lower-income countries?
# Here GDP per capita median is used as a simple economic grouping.

median_gdp_pc <- median(co2_clean$gdp_per_capita, na.rm = TRUE)

income_groups <- co2_clean %>%
  mutate(
    income_group = case_when(
      gdp_per_capita >= median_gdp_pc ~ "Higher GDP per capita",
      gdp_per_capita < median_gdp_pc ~ "Lower GDP per capita"
    )
  ) %>%
  drop_na(income_group, co2_per_capita)

normality_test <- income_groups %>%
  group_by(income_group) %>%
  shapiro_test(co2_per_capita)

variance_test <- income_groups %>%
  levene_test(co2_per_capita ~ income_group)

wilcox_income_test <- wilcox.test(
  co2_per_capita ~ income_group,
  data = income_groups
)

effect_size_income <- income_groups %>%
  wilcox_effsize(co2_per_capita ~ income_group)

print(normality_test)
print(variance_test)
print(wilcox_income_test)
print(effect_size_income)


# 14. Correlation Analysis ------------------------------------------------

correlation_data <- co2_clean %>%
  select(
    co2,
    co2_per_capita,
    gdp_per_capita,
    energy_per_capita,
    population,
    methane,
    share_global_co2
  ) %>%
  drop_na()

correlation_matrix <- cor(
  correlation_data,
  method = "spearman",
  use = "complete.obs"
)

print(correlation_matrix)

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  number.cex = 0.7,
  tl.cex = 0.8
)


# 15. Regression Modeling -------------------------------------------------

# Log transformation is used because CO2 and GDP variables are highly skewed.

model_data <- co2_clean %>%
  select(
    co2,
    co2_per_capita,
    gdp_per_capita,
    energy_per_capita,
    population
  ) %>%
  drop_na() %>%
  filter(
    co2 > 0,
    gdp_per_capita > 0,
    energy_per_capita > 0,
    population > 0
  ) %>%
  mutate(
    log_co2 = log(co2),
    log_gdp_per_capita = log(gdp_per_capita),
    log_energy_per_capita = log(energy_per_capita),
    log_population = log(population)
  )

co2_model <- lm(
  log_co2 ~ log_gdp_per_capita + log_energy_per_capita + log_population,
  data = model_data
)

summary(co2_model)

model_coefficients <- tidy(co2_model)
model_fit <- glance(co2_model)

print(model_coefficients)
print(model_fit)


# 16. Regression Diagnostics ---------------------------------------------

par(mfrow = c(2, 2))
plot(co2_model)
par(mfrow = c(1, 1))


# 17. Model Visualization -------------------------------------------------

p_model_relationship <- ggplot(
  model_data,
  aes(x = log_energy_per_capita, y = log_co2)
) +
  geom_point(alpha = 0.35) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Energy Use and CO2 Emissions",
    subtitle = "Log-transformed relationship",
    x = "Log Energy per Capita",
    y = "Log CO2 emissions"
  ) +
  theme_minimal(base_size = 13)

print(p_model_relationship)


# 18. Save Outputs --------------------------------------------------------

write_csv(co2_clean, "outputs/clean_co2_energy_data.csv")
write_csv(descriptive_stats, "outputs/descriptive_statistics.csv")
write_csv(global_trend, "outputs/global_co2_trend.csv")
write_csv(top_emitters, "outputs/top_emitters.csv")
write_csv(top_per_capita, "outputs/top_per_capita_emitters.csv")
write_csv(probability_high_emission, "outputs/probability_high_emission.csv")
write_csv(risk_by_decade, "outputs/risk_by_decade.csv")
write_csv(model_coefficients, "outputs/regression_coefficients.csv")
write_csv(model_fit, "outputs/regression_model_fit.csv")

ggsave("visuals/global_co2_trend.png", p_global_trend, width = 9, height = 5)
ggsave("visuals/top_emitters.png", p_top_emitters, width = 9, height = 6)
ggsave("visuals/co2_per_capita_distribution.png", p_distribution, width = 8, height = 5)
ggsave("visuals/top_per_capita_emitters.png", p_per_capita, width = 9, height = 6)
ggsave("visuals/high_emission_risk_by_decade.png", p_risk_decade, width = 8, height = 5)
ggsave("visuals/selected_country_trends.png", p_country_trends, width = 9, height = 5)
ggsave("visuals/energy_vs_co2_model_relationship.png", p_model_relationship, width = 8, height = 5)

############################################################
# End of Project Script
############################################################
