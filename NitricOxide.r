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
    mutate(covariate = "Outdoor Temperature", value = temp)
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


# ============================================================================ #
# Q.1 -> THRESHOLD SELECTION #####
# ============================================================================ #

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
# Q.2 -> TEMPORAL EVOLUTION OF EXTREME LEVELS ####
# ============================================================================ #
# To track the temporal evolution of extreme levels, we can compare the 93rd percentiles
# of data and the overall number of threshold exceedances for each year from 2007
# to 2015.
# To calculate the 93rd percentile of NO_max for each year, we can first group the
# data by year and then we calculate the quantile.
NO_q93_by_year <- NO_clean %>%
  group_by(year) %>%
  summarise(NO_max_p93 = quantile(NO_max, 0.93)) %>%
  mutate(level = ifelse(NO_max_p93 > threshold, "Above threshold", "Below threshold"))
# (this last variable could allow us to change the colour of points if they are above
# or below the threshold value).

print(NO_q93_by_year)
# We immediately notice a strongly decreasing trend for the 93rd percentile, meaning that
# air pollution, in terms of NO concentration, has clearly improved across the years.
# In fact, the 93rd quantile is higher than the threshold value 180 until 2008, then
# it is always below.
# We can plot these results as a time series of the 93rd percentile:
ggplot(NO_q93_by_year, aes(x = year, y = NO_max_p93)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Trend of the 93rd Percentile of NO_max (2002–2015)",
       x = "Year",
       y = "93rd Percentile of NO_max") +
  plot_theme

# We can assess this also in an alternative way: check how many threshold exceedances
# we have for each year.
# We can do this in a similar way as before. First, we group by year, then we simply
# count the threshold exceedances
NO_exceedances_by_year <- NO_clean %>%
  group_by(year) %>%
  summarise(exceedances = sum(NO_max > threshold))
# Let's display the result
print(NO_exceedances_by_year)

ggplot(NO_exceedances_by_year, aes(x = as.factor(year),
                                   y = exceedances)) +
  geom_bar(stat = "identity", fill = "orange", colour = "red") +
  labs(title = "Number of NO_max Threshold Exceedances per Year",
       x = "Year",
       y = "Number of Exceedances (NO_max > 180)") +
  plot_theme
# Even in this case, the decreasing trend is quite clear.
# The vast majority of threshold exceedances is registered until 2008, then only
# few occur.


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



# ============================================================================ #
# --------------- GEV MODELLING WITH evgam + deliri del cugino ----------------- 
# ============================================================================ #

# In this section we explore the possibilities of the Block Maxima approach

# ── Step 1: One overall maximum per month (across all sites) ─────────────────
monthly_max <- NO_clean %>%
  filter(!is.na(NO_max), !is.na(wind), !is.na(temp)) %>%
  group_by(site, city, year, month) %>%          # <-- keep site and city
  summarise(
    NO_monthly_max = max(NO_max, na.rm = TRUE),
    wind           = mean(wind, na.rm = TRUE),
    temp           = mean(temp, na.rm = TRUE),
    long           = first(long),
    lat            = first(lat),
    .groups = "drop"
  ) %>%
  mutate(
    year      = as.numeric(as.character(year)),
    month_int = as.integer(month),
    date      = as.Date(sprintf("%d-%02d-01", year, month_int))
  ) %>%
  mutate(site = factor(site), city = factor(city)) %>%
  filter(complete.cases(.))

nrow(monthly_max)


nrow(monthly_max)   # should be ~108 (9 years × 12 months)


# ── Step 2: evgam GEV models ──────────────────────────────────────────────────

# Model 0 — Stationary
gev_m0 <- evgam(
  list(NO_monthly_max ~ 1, ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# Model 1 — Linear year trend in location
gev_m1 <- evgam(
  list(NO_monthly_max ~ year, ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# Model 2 — Smooth year trend in location
gev_m2 <- evgam(
  list(NO_monthly_max ~ s(year, k = 4), ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# Model 3 — Year + seasonality
gev_m3 <- evgam(
  list(NO_monthly_max ~ year + s(month_int, bs = "cc", k = 6), ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# Model 4 — Year + seasonality + wind
gev_m4 <- evgam(
  list(NO_monthly_max ~ year + s(month_int, bs = "cc", k = 6) + s(wind, k = 4), ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# Model 5 — Year + seasonality + temp
gev_m5 <- evgam(
  list(NO_monthly_max ~ year + s(month_int, bs = "cc", k = 6) + temp, ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# Model 6 — Year + seasonality + wind + temp
gev_m6 <- evgam(
  list(NO_monthly_max ~ year + s(month_int, bs = "cc", k = 6) + s(wind, k = 4) + temp, ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

gev_m7 <- evgam(
  list(NO_monthly_max ~ year + s(site, bs = "re"), ~ 1, ~ 1),
  data = monthly_max, family = "gev"
)

# ── Step 3: AIC comparison ────────────────────────────────────────────────────
aic_table <- data.frame(
  model = paste0("gev_m", 0:7),
  AIC   = c(AIC(gev_m0), AIC(gev_m1), AIC(gev_m2),
            AIC(gev_m3), AIC(gev_m4), AIC(gev_m5), AIC(gev_m6),
            AIC(gev_m7))
) %>% arrange(AIC)
print(aic_table)


# ── Step 4: fevd cross-check (stationary + non-stationary) ───────────────────
gev_fevd_0 <- fevd(
  x    = monthly_max$NO_monthly_max,
  data = monthly_max,
  type = "GEV"
)
summary(gev_fevd_0)

gev_fevd_1 <- fevd(
  x            = monthly_max$NO_monthly_max,
  data         = monthly_max,
  location.fun = ~ year,
  type         = "GEV",
  use.phi      = TRUE
)
summary(gev_fevd_1)

lr.test(gev_fevd_0, gev_fevd_1)

# ── Step 5: Diagnostic plots (fevd) ──────────────────────────────────────────
plot(gev_fevd_0)               # QQ, PP, return level, density
plot(gev_fevd_0, type = "rl")  # return level plot with CIs

# Return levels
return.level(gev_fevd_0, return.period = c(10, 20, 50))
ci(gev_fevd_0, type = "return.level", return.period = c(10, 50))


# ── Step 6: Manual QQ plot for best evgam model ───────────────────────────────
library(evd)

best_model <- gev_m3   # replace with whichever model has lowest AIC

gev_preds  <- predict(best_model, newdata = monthly_max, type = "response")
# For stationary parameters, all rows identical — use first row only
fitted_mu  <- gev_preds$location[1]
fitted_psi <- gev_preds$scale[1]
fitted_xi  <- gev_preds$shape[1]

emp  <- sort(monthly_max$NO_monthly_max)
n    <- length(emp)
pp   <- (seq_len(n) - 0.5) / n
theo <- qgev(pp, loc = fitted_mu, scale = fitted_psi, shape = fitted_xi)

plot(emp ~ theo,
     xlab = "Theoretical GEV Quantiles",
     ylab = "Empirical Quantiles",
     main = "GEV QQ Plot – Monthly Maximum NO")
abline(0, 1, col = "red", lty = 2)

################################################################################
################## GPD MODELLING WITH evgam ####################################
################################################################################

# ------------------------------
# 1) Prepare exceedance data
# ------------------------------
threshold <- 180

gpd_data <- NO %>%
  filter(!is.na(NO_max), !is.na(wind), !is.na(temp),
         NO_max > threshold) %>%
  mutate(
    excess      = NO_max - threshold,
    year= as.numeric(year),
    month_int   = as.integer(month),
    site        = factor(site),
    city        = factor(city)
  )

nrow(gpd_data)  # check number of exceedances


table(gpd_data$year)

# ------------------------------
# 2) Fit GPD models directly on excess
# ------------------------------

# ------------------------------
# GPD models with xi fixed (constant) via list formula
# ------------------------------

# Model 0 — Stationary
m0 <- evgam(list(excess ~ 1, ~ 1),
            data = gpd_data, family = "gpd")
summary(m0)

# Model 1 — Smooth term for year 
m1 <- evgam(list(excess ~ s(year,k=4), ~ 1),
            data = gpd_data, family = "gpd")
summary(m1)
plot(m1)
#year appears to have a linear effect

# Model 2 — Smooth for year + cyclic cubic spline for month, to account for seasonality
m2 <- evgam(list(excess ~ s(year,k=4) + s(month_int, bs = "cc", k = 6), ~ 1),
            data = gpd_data, family = "gpd")

summary(m2)

#month doesn't appear to have a significant effet



# Model 3 — year + temperature, we start with meteorological covariates
m3 <- evgam(list(excess ~ s(year,k=4) +s(temp), ~ 1),
            data = gpd_data, family = "gpd")
summary(m3)
plot(m3)

#Temperaure appears to be highly insignificant and shouldn't be included in the 
#final model

# Model 4 — year + wind
m4 <- evgam(list(excess ~ s(year,k=4) +s(wind) , ~ 1),
            data = gpd_data, family = "gpd")
summary(m4)
plot(m4)


# Model 5 — we can add a site random effect
m5 <- evgam(list(excess ~ s(year,k=4)+s(wind) +
                   s(site, bs = "re"), ~ 1),
            data = gpd_data, family = "gpd")
summary(m5)

plot(m5)
AIC(m5)


# Model 6 — Wind+temp
m6 <- evgam(list(excess ~ s(year,k=4) +s(wind) +s(temp), ~ 1),
            data = gpd_data, family = "gpd")
summary(m6)
plot(m6)

# Model 7 — in model 5 wind is not significant, can we remove it?
m7 <- evgam(list(excess ~ s(year,k=4) +
                   s(site, bs = "re"), ~ 1),
            data = gpd_data, family = "gpd")
summary(m7)

plot(m7)
BIC(m7) < BIC(m5)


AIC(m0,m1,m2,m3,m4,m5,m6)


table(gpd_data$site)



#THis should be the expected exceedance in function of the covariates don't know if it makes sense
#DOn't know if it makes any sense 
pred_orig <- predict(m5, newdata = gpd_data, type = "response")
class(pred_orig)
colnames(pred_orig)
head(pred_orig)



# -----------------------------------------------------------
# Medians (no temp needed for m5)
# -----------------------------------------------------------
med_year  <- median(gpd_data$year)
med_wind  <- median(gpd_data$wind, na.rm = TRUE)
ref_site  <- levels(gpd_data$site)[1]   # reference site for the random effect

# -----------------------------------------------------------
# Extract xi from m5
# -----------------------------------------------------------
# We can also compute confidence intervals for the shape parameter estimate:
sum_5 <- summary(m5)
str(sum_5)
se_xi_5 <- sum_5[[1]]$shape[1, "Std. Error"]
xi_hat_5 <- sum_5[[1]]$shape[1, "Estimate"]
#We can use a 95% CI. But what quantiles should we use?
#We can use a Wald/Normal confidence interval, hence:
alpha <- 1-0.95
ci_xi_5 <- c("lower" = xi_hat_5 - qnorm(1 - alpha/2)*se_xi_5,
             "estimate" = xi_hat_5,
             "upper" = xi_hat_5 + qnorm(1 - alpha/2)*se_xi_5)
ci_xi_5

#How can we improve the looks of the smooth plots?
#We could use ggplot, but we would need to extract all the necessary components from
#model_avg to build a dataframe:

# -----------------------------------------------------------
# 1) Effect of YEAR
# -----------------------------------------------------------
year_grid <- data.frame(
  year = seq(min(gpd_data$year), max(gpd_data$year), length.out = 200),
  wind = med_wind,
  site = factor(ref_site, levels = levels(gpd_data$site))
)
pred_year          <- predict(m5, newdata = year_grid, type = "response")
year_grid$sigma    <- pred_year[, "scale"]
year_grid$E_excess <- year_grid$sigma / (1 - xi_hat)

# -----------------------------------------------------------
# 2) Effect of WIND
# -----------------------------------------------------------
wind_grid <- data.frame(
  year = med_year,
  wind = seq(min(gpd_data$wind, na.rm = TRUE),
             max(gpd_data$wind, na.rm = TRUE), length.out = 200),
  site = factor(ref_site, levels = levels(gpd_data$site))
)
pred_wind          <- predict(m5, newdata = wind_grid, type = "response")
wind_grid$sigma    <- pred_wind[, "scale"]
wind_grid$E_excess <- wind_grid$sigma / (1 - xi_hat)

# -----------------------------------------------------------
# Plots
# -----------------------------------------------------------
p_year <- ggplot(year_grid, aes(x = year, y = E_excess)) +
  geom_line(linewidth = 1, color = "steelblue") +
  labs(title = "Effect of Year on Expected NO Excess",
       subtitle = "Wind at median, reference site",
       x = "Year", y = "Expected NO excess (µg/m³)") +
  plot_theme

p_wind <- ggplot(wind_grid, aes(x = wind, y = E_excess)) +
  geom_line(linewidth = 1, color = "tomato") +
  labs(title = "Effect of Wind Speed on Expected NO Excess",
       subtitle = "Year at median, reference site",
       x = "Wind speed (m/s)", y = "Expected NO excess (µg/m³)") +
  plot_theme

p_year / p_wind

