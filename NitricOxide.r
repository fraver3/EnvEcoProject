# Group 10 - Monthly Maximum NO Analysis

# ------------------------------
# 1) Load required packages
# ------------------------------
library(dplyr)
library(ggplot2)
library(gridExtra)
library(leaflet)
library(mgcv)
library(magrittr)
library(extRemes)

# ------------------------------
# 2) Load data
# ------------------------------
# Use local file if available; otherwise use the original URL fallback.

url <- "https://raw.githubusercontent.com/fraver3/EnvEcoProject/main/group_10_data.csv"
NO <- read.csv(url)

#### EXPLORATORY ANALYSIS ####

str(NO)
table(NO$city)
colSums(is.na(NO))

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

# Let's check if there are some patterns in the missing values:
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

# MISSING DATA SUMMARY
print(missing_by_variable)
print(missing_by_city)
print(head(missing_by_site, 20))

# We can set a common theme for all the plots:
plot_theme <- theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

# Plot A: Missing values by variable
# This plot shows which columns have most missing values overall.
plot_missing_variable <- ggplot(missing_by_variable,
                                aes(x = reorder(variable, missing_pct), 
                                    y = missing_pct)) +
  geom_col(fill = "lightgreen") +
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
plot_missing_city <- ggplot(missing_by_city,
                            aes(x = reorder(city, missing_pct_NO),
                                y = missing_pct_NO)) +
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
plot_missing_site <- ggplot(missing_by_site,
                            aes(x = reorder(site, missing_pct_NO),
                                y = missing_pct_NO, fill = city)) +
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

# We notice that in 2016 most of the observations are NAs ...
NO %>% 
  filter(year == 2016) %>% 
  select(NO_max) %>% 
  is.na() %>% 
  mean() # ... namely ~93% of them!

# Willows-Colusa Street will not be included in the analysis because it features
# 100% of NA values in NO_max
NO_clean <- NO %>%  
  filter(!year == 2016) %>% 
  filter(!is.na(NO_max))

# Remove thee pressure variable (too many NAs enacoid)
NO_clean <- NO_clean %>% 
  select(!pressure)

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

sites <- NO %>%
  select(city, site, long, lat) %>%
  distinct() %>%
  filter(!is.na(long), !is.na(lat))

library(leaflet)

leaflet(data = sites) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # nice clean basemap
  addCircleMarkers(
    lng = ~long,
    lat = ~lat,
    radius = 6,
    stroke = TRUE,
    weight = 1,
    color = "#2b8cbe",
    fillColor = ~factor(city) %>% as.numeric() %>% colorFactor("Set1", domain = 1:length(unique(sites$city)))(.),
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Site:</b> ", site, "<br>",
      "<b>City:</b> ", city, "<br>",
      "<b>Longitude:</b> ", round(long, 3), "<br>",
      "<b>Latitude:</b> ", round(lat, 3)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorFactor("Set1", domain = sites$city),
    values = ~city,
    title = "City",
    opacity = 1
  )

leaflet(data = sites) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~long, lat = ~lat,
    radius = 6,
    stroke = TRUE,
    weight = 1,
    color = "#2b8cbe",
    fillColor = "#2b8cbe",
    fillOpacity = 0.8,
    popup = ~paste0("<b>Site:</b> ", site, "<br><b>City:</b> ", city),
    clusterOptions = markerClusterOptions()
  )


################################################################################
################## THRESHOLD SELECTION #########################################
################################################################################

x <- NO_clean$NO_max
range(x)
threshrange.plot(x = x,
                 r = c(100, 300),
                 nint = 100)
mrlplot(x = x)
thresh_seq_mrl <- seq(min(x), max(x), length.out = 200)

mrl_df <- do.call(rbind, 
                  lapply(thresh_seq_mrl, function(u) {
                    data.frame(threshold = u,
                               mean_excess = mean(x[x > u] - u),
                               se = sd(x[x > u] - u) / sqrt(length(x[x > u]))
                    )}))
# The regulamentory threshold for NO2, that is highly correlated to NO, is equal
# to 180 ppb. 
# In order to assess if this may be a valid threshold value, we could check the mean
# residual life plot and the parameter stability plots to see if this is consistent 
# with the data.
mrl_plot <- ggplot(mrl_df, 
                   aes(x = threshold,
                       y = mean_excess)) +
  geom_line() + 
  geom_point() +
  geom_ribbon(aes(ymin = mean_excess - qnorm(0.975)*se,
                  ymax = mean_excess + qnorm(0.975)*se),
              alpha = 0.25) +
  labs(title = "Mean residual life plot",
       x = "Threshold value",
       y = "Mean excess") +
  geom_vline(xintercept = 180, 
             colour = "red",
             linetype = 2,
             linewidth = 1.5) +
  plot_theme 


thresh_seq_par <- seq(100, 300, length.out = 100)
modelfits <- lapply(thresh_seq_par, function(u) {
  try(fevd(x, threshold = u, type = "GP"), silent = TRUE)
})

shape_df <- do.call(
  rbind,
  lapply(seq_along(modelfits), function(i) {
    
    fit <- modelfits[[i]]
    
    if(inherits(fit, "try-error")) return(NULL)
    
    ci_par <- try(ci(fit, type = "parameter"), silent = TRUE)
    
    if(inherits(ci_par, "try-error")) return(NULL)
    
    data.frame(
      threshold = thresh_seq_par[i],
      estimate = fit$results$par["shape"],
      lower = ci_par["shape",1],
      upper = ci_par["shape",3]
    )
  })
)
# Now, we can plot them:
shape_plot <- ggplot(data = shape_df,
                     aes(x = threshold,
                         y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.25) +
  labs(title = "Shape parameter across different threshold values",
       x = "Threshold",
       y = "Shape parameter estimate") +
  geom_vline(xintercept = 180,
             color = "red",
             linetype = 2,
             linewidth = 1.2) +
  plot_theme
shape_plot

# Now, we are ready to build the scale data frame, which we will use to plot the 
# reparametrized scale parameter estimate across different threshold value to assess
# its stability:
scale_df <- do.call(
  rbind,
  lapply(seq_along(modelfits), function(i) {
    fit <- modelfits[[i]]
    if (inherits(fit, "try-error")) return(NULL)
    
    ci_par <- try(ci(fit, type = "parameter"), silent = TRUE)
    if (inherits(ci_par, "try-error")) return(NULL)
    
    data.frame(
      threshold = thresh_seq_par[i],
      estimate  = fit$results$par["scale"],
      lower     = ci_par["scale", 1],
      upper     = ci_par["scale", 3]
    )
  })
)

scale_plot <- ggplot(scale_df,
                     aes(x = threshold, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = 0.25) +
  labs(title = "Scale parameter across thresholds",
       x = "Threshold",
       y = "Reparametrized scale") +
  geom_vline(xintercept = 180,
             colour = "red",
             linetype = 2,
             linewidth = 1.2) +
  plot_theme

pars_plot <- grid.arrange(scale_plot, shape_plot, ncol = 1)
grid.arrange(pars_plot, mrl_plot, ncol = 2)
# From what we see in these plots, 180 seems a reasonable choice for the threshold;
# the mean residual life plot seems to suggest a slightly higher threshold, but since
# the difference would be really small, we may keep using 180, the regulamentary
# threshold, as our reference.
threshold <- 180
1 - mean(x > threshold)
sum(x > threshold)
# 180 corresponds approximately to the 93rd percentile of our data, meaning that
# we will be fitting our model using about 7% of the observations in the original
# dataset. In total, we will use 171 observations.

# ============================================================================ #
# MODEL FITTING ####
# ============================================================================ #
library(evgam)
library(evd)


NO_clean$excesses <- NO_clean$NO_max - threshold
is.na(NO_clean$excesses[NO_clean$excesses < 0]) <- TRUE
formula_0 <- list(scale = excesses ~ 1, # scale parameter, fixed by now
                  ~ 1) # shape parameter
model_0 <- evgam(formula = formula_0,
                 data = NO_clean,
                 family = "gpd")
summary(model_0)

# Extract fitted parameters from model_0
sigma <- exp(coef(model_0)[1])   # back-transform from log scale
xi    <- coef(model_0)[2]        # shape (may use a logistic link)

# Empirical excesses (non-NA)
exc <- na.omit(NO_clean$excesses)
n   <- length(exc)

# Plotting positions (Hazen)
probs <- (seq_len(n) - 0.5) / n
theo_q <- qgpd(probs, scale = sigma, shape = xi)  # from evd or texmex package

# Plot
plot(sort(exc) ~ theo_q,
     xlab = "Theoretical GPD Quantiles",
     ylab = "Empirical Quantiles",
     main = "GPD QQ Plot – NO Excesses (model_0)")
abline(0, 1, col = "red", lty = 2)


model_0 <- fevd(x = NO_max,
                data = na.omit(NO_clean), 
                threshold = threshold,    
                use.phi = T,              # if the scale is modelled, add this 
                type = "GP")
summary(model_0)
