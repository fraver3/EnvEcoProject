# Group 10 - Monthly Maximum NO Analysis
# This script is intentionally simple, tidy, and fully commented.
# It focuses on:
# 1) Missing-value patterns
# 2) Response-variable (NO) exploration
# 3) Covariate effects (exploratory plots + simple Gamma GAMs)

# ------------------------------
# 1) Load required packages
# ------------------------------
library(dplyr)
library(ggplot2)
library(mgcv)

# ------------------------------
# 2) Load data
# ------------------------------
# Use local file if available; otherwise use the original URL fallback.

url <- "https://raw.githubusercontent.com/fraver3/EnvEcoProject/main/group_10_data.csv"
NO <- read.csv(url)


# ------------------------------
# 3) Basic structure checks
# ------------------------------
str(NO)
table(NO$city)
colSums(is.na(NO))

# ------------------------------
# 4) Clean names and create time fields
# ------------------------------
NO <- NO %>%
  rename(
    NO_max = Nitric.oxide..NO.,
    wind = Wind.Speed...Resultant,
    temp = Outdoor.Temperature,
    pressure = Barometric.pressure
  ) %>%
  mutate(
    year = as.integer(year),
    month = as.integer(month),
    date = as.Date(sprintf("%d-%02d-01", year, month)),
    month = factor(month, levels = 1:12, labels = month.abb, ordered = TRUE)
  )

# ------------------------------
# 5) Missing-value summaries
# ------------------------------
missing_by_variable <- data.frame(
  variable = names(NO),
  missing_n = sapply(NO, function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)
missing_by_variable$missing_pct <- 100 * missing_by_variable$missing_n / nrow(NO)

missing_by_city <- NO %>%
  group_by(city) %>%
  summarise(
    n_total = n(),
    n_missing_NO = sum(is.na(NO_max)),
    missing_pct_NO = 100 * n_missing_NO / n_total,
    .groups = "drop"
  ) %>%
  arrange(desc(missing_pct_NO))

missing_by_site <- NO %>%
  group_by(city, site) %>%
  summarise(
    n_total = n(),
    n_missing_NO = sum(is.na(NO_max)),
    missing_pct_NO = 100 * n_missing_NO / n_total,
    .groups = "drop"
  ) %>%
  arrange(desc(missing_pct_NO))

missing_by_date <- NO %>%
  group_by(date) %>%
  summarise(
    n_total = n(),
    n_missing_NO = sum(is.na(NO_max)),
    missing_pct_NO = 100 * n_missing_NO / n_total,
    .groups = "drop"
  )

#MISSING DATA SUMMARY
print(missing_by_variable)
print(missing_by_city)
print(head(missing_by_site, 20))

# Clean shared theme for all ggplots
plot_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

# Plot A: Missing values by variable
# This plot shows which columns have most missing values overall.
plot_missing_variable <- ggplot(missing_by_variable, aes(x = reorder(variable, missing_pct), y = missing_pct)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Missing Data by Variable",
    x = "Variable",
    y = "Percent missing"
  ) +
  plot_theme
print(plot_missing_variable)

# Plot B: Missing NO by city
# This plot checks whether missing NO is evenly spread across cities.
plot_missing_city <- ggplot(missing_by_city, aes(x = reorder(city, missing_pct_NO), y = missing_pct_NO)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Missing Monthly Maximum NO by City",
    x = "City",
    y = "Percent missing NO"
  ) +
  plot_theme
print(plot_missing_city)

# Plot C: Missing NO by site
# This plot checks whether specific monitoring stations have more missing NO values.
plot_missing_site <- ggplot(missing_by_site, aes(x = reorder(site, missing_pct_NO), y = missing_pct_NO, fill = city)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Missing Monthly Maximum NO by Site",
    x = "Site",
    y = "Percent missing NO",
    fill = "City"
  ) +
  plot_theme
print(plot_missing_site)

# Plot D: Missing NO over time
# This plot shows whether missing NO is concentrated in specific months/years.
plot_missing_time <- ggplot(missing_by_date, aes(x = date, y = missing_pct_NO)) +
  geom_line(color = "purple", linewidth = 0.8) +
  labs(
    title = "Missing Monthly Maximum NO Over Time",
    x = "Date",
    y = "Percent missing NO"
  ) +
  plot_theme
print(plot_missing_time)


# ------------------------------
# 6) Response-variable exploration (NO)
# ------------------------------
NO_clean <- NO %>% filter(!is.na(NO_max))

# Plot E: Overall distribution of monthly maximum NO
# This histogram shows the overall shape of the response variable.
plot_no_hist <- ggplot(NO_clean, aes(x = NO_max)) +
  geom_histogram(bins = 35, color = "white", fill = "skyblue4") +
  labs(
    title = "Distribution of Monthly Maximum NO",
    x = "Monthly maximum NO",
    y = "Count"
  ) +
  plot_theme
print(plot_no_hist)

# Plot F: NO distribution by city
# This boxplot compares monthly maximum NO levels across cities.
plot_no_city_box <- ggplot(NO_clean, aes(x = reorder(city, NO_max, median), y = NO_max)) +
  geom_boxplot(fill = "lightblue", outlier.alpha = 0.4) +
  coord_flip() +
  labs(
    title = "Monthly Maximum NO by City",
    x = "City",
    y = "Monthly maximum NO"
  ) +
  plot_theme
print(plot_no_city_box)

# Plot G: NO distribution by site
# This boxplot compares monthly maximum NO levels across monitoring stations.
plot_no_site_box <- ggplot(NO_clean, aes(x = reorder(site, NO_max, median), y = NO_max, fill = city)) +
  geom_boxplot(outlier.alpha = 0.35) +
  coord_flip() +
  labs(
    title = "Monthly Maximum NO by Site",
    x = "Site",
    y = "Monthly maximum NO",
    fill = "City"
  ) +
  plot_theme
print(plot_no_site_box)

# Plot H: Seasonal pattern by month
# This boxplot shows how monthly maximum NO changes across calendar months.
plot_no_month_box <- ggplot(NO_clean, aes(x = month, y = NO_max)) +
  geom_boxplot(fill = "khaki") +
  labs(
    title = "Seasonal Pattern of Monthly Maximum NO",
    x = "Month",
    y = "Monthly maximum NO"
  ) +
  plot_theme
print(plot_no_month_box)

# Plot I: Yearly trend by city
# This line plot shows long-term evolution of average monthly maxima in each city.
yearly_no_city <- NO_clean %>%
  group_by(city, year) %>%
  summarise(mean_NO_max = mean(NO_max), .groups = "drop")

plot_no_year_city <- ggplot(yearly_no_city, aes(x = year, y = mean_NO_max, color = city)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  labs(
    title = "Yearly Mean of Monthly Maximum NO by City",
    x = "Year",
    y = "Yearly mean monthly maximum NO",
    color = "City"
  ) +
  plot_theme
print(plot_no_year_city)

# ------------------------------
# 7) Covariate effect plots (exploratory)
# ------------------------------
covariate_long <- bind_rows(
  NO_clean %>%
    select(NO_max, wind) %>%
    filter(!is.na(wind)) %>%
    mutate(covariate = "Wind Speed", value = wind),
  NO_clean %>%
    select(NO_max, temp) %>%
    filter(!is.na(temp)) %>%
    mutate(covariate = "Outdoor Temperature", value = temp),
  NO_clean %>%
    select(NO_max, pressure) %>%
    filter(!is.na(pressure)) %>%
    mutate(covariate = "Barometric Pressure", value = pressure)
)

# Plot J: NO vs each covariate
# This faceted scatter plot gives a direct visual relationship between NO and each meteorological covariate.
plot_covariate_scatter <- ggplot(covariate_long, aes(x = value, y = NO_max)) +
  geom_point(alpha = 0.25, color = "grey30") +
  geom_smooth( se = TRUE, color = "firebrick") +
  facet_wrap(~ covariate, scales = "free_x") +
  labs(
    title = "Monthly Maximum NO vs Meteorological Covariates",
    x = "Covariate value",
    y = "Monthly maximum NO"
  ) +
  plot_theme
print(plot_covariate_scatter)

# ------------------------------
# 8) Simple Gamma GAM models
# ------------------------------

# Wind model data
wind_data <- NO %>%
  filter(!is.na(NO_max), NO_max > 0, !is.na(site), !is.na(year), !is.na(month), !is.na(wind)) %>%
  mutate(site = factor(site))

# Temperature model data
temp_data <- NO %>%
  filter(!is.na(NO_max), NO_max > 0, !is.na(site), !is.na(year), !is.na(month), !is.na(temp)) %>%
  mutate(site = factor(site))

# Pressure model data
pressure_data <- NO %>%
  filter(!is.na(NO_max), NO_max > 0, !is.na(site), !is.na(year), !is.na(month), !is.na(pressure)) %>%
  mutate(site = factor(site))

