# ============================================================================ #
# Project 10 - Modelling Nitric oxide (NO) concentrations in California
# ============================================================================ #

# First, load the required packages 
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(gridExtra)
library(leaflet)
library(mgcv)
library(magrittr)
library(extRemes)
library(evgam)
library(evd)

# Load the data
data_url <- "https://raw.githubusercontent.com/fraver3/EnvEcoProject/main/group_10_data.csv"
NO <- read.csv(data_url)

# Set working directory to save the plots
# setwd('/Users/francescoverni/Documents/Università/THIRD YEAR GLASGOW/EnvEco Stats/Group Project')

# ============================================================================ #
# -- EXPLORATORY DATA ANALYSIS -----------------------------------------------
# ============================================================================ #

# Having a look at the structure of the dataset
str(NO)

# Variable names cleaning
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

# Have a look at how many observations per city are available 
table(NO$city) # Many observations are labelled as "not in a city"
# really invalid variable --> not ideal to use it
# will see whether site is more balanced


### -- Missing value analysis --------------------------------------------------
colSums(is.na(NO)) # Nitric oxide NAs are not usable for the analysis

# Let's check if there are some patterns in the missing values

# Percentage of NAs for each variable
missing_by_variable <- data.frame(
  variable = names(NO),
  missing_n = sapply(NO, function(x) sum(is.na(x))),
  stringsAsFactors = FALSE
)
missing_by_variable$missing_pct <- 100 * 
  missing_by_variable$missing_n / nrow(NO)

# NAs precentage by city
missing_by_city <- NO %>%
  group_by(city) %>%
  summarise(
    n_total = n(),
    n_missing_NO = sum(is.na(NO_max)),
    missing_pct_NO = 100 * n_missing_NO / n_total,
    .groups = "drop"
  ) %>%
  arrange(desc(missing_pct_NO))

# NAs percentage by monitoring site
missing_by_site <- NO %>%
  group_by(city, site) %>%
  summarise(
    n_total = n(),
    n_missing_NO = sum(is.na(NO_max)),
    missing_pct_NO = 100 * n_missing_NO / n_total,
    .groups = "drop"
  ) %>%
  arrange(desc(missing_pct_NO))

# NAs percentage by year
missing_by_year <- NO %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    n_missing_NO = sum(is.na(NO_max)),
    missing_pct_NO = 100 * n_missing_NO / n_total,
    .groups = "drop"
  )

# Results
print(missing_by_variable)
print(missing_by_city)
print(head(missing_by_site, 20))
print(missing_by_year)

# We can also plot these for better visual clarity

# Missing values by variable
plot_missing_variable <- ggplot(missing_by_variable,
                                aes(x = reorder(variable, missing_pct), 
                                    y = missing_pct)) +
  geom_col(fill = "#005398") +
  coord_flip() +
  labs(
    title = "Missing Data by Variable",
    x = "Variable",
    y = "Percent missing"
  ) +
  theme_bw()
print(plot_missing_variable)
# high percentage for pressure

# Missing NO by city
# This plot checks whether missing NO is evenly spread across cities.
plot_missing_city <- ggplot(missing_by_city,
                            aes(x = reorder(city, missing_pct_NO),
                                y = missing_pct_NO)) +
  geom_col(fill = "#405d18") +
  coord_flip() +
  labs(
    title = "Missing Monthly Maximum NO by City",
    x = "City",
    y = "Percent missing NO"
  ) +
  theme_bw()
print(plot_missing_city)

# Missing NO by site
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
  theme_bw()
print(plot_missing_site)
# Willows-Colusa Street will not be included in the analysis because it features
# 100% of NA values in NO_max

  
# Missing NO year year
plot_missing_year <- ggplot(missing_by_year,
                            aes(x = reorder(year, missing_pct_NO),
                                y = missing_pct_NO)) +
  geom_col(fill = "#405d18") +
  coord_flip() +
  labs(
    title = "Missing Monthly Maximum NO by Year",
    x = "Year",
    y = "Percent missing NO"
  ) +
  theme_bw()
print(plot_missing_year)

# Exploratory data analysis for the response variable (NO)

# We notice that in 2016 most of the observations are NAs ...
NO %>% 
  filter(year == 2016) %>% 
  select(NO_max) %>% 
  is.na() %>% 
  mean() # ... namely ~93% of them!

# then analysis between 2002 and 2015, exclude 2016
NO_clean <- NO %>%  
  filter(!year == 2016) %>% 
  filter(!is.na(NO_max))

# Remove the pressure variable (high percentage of NAs)
NO_clean <- NO_clean %>% 
  select(!pressure)

### -- Visualising the distribution of the response variable -------------------

# Visualising our data
NO_blockmax <- NO %>%
  group_by(year, month, site) %>%
  slice_max(NO_max, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot
ggplot(NO_blockmax, aes(x = date, y = NO_max, colour = site)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Monthly Maximum NO Concentration by Site",
    x = "Date",
    y = "NO Concentration (ppb)",
    colour = "Site"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# Filter to max NO per site per year
NO_max_yr <- NO %>%
  group_by(year, site) %>%
  slice_max(NO_max, n = 1, with_ties = FALSE) %>%
  ungroup()

# Plot by year
ggplot(NO_max_yr, aes(x = year, y = NO_max, colour = site)) +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.8) +
  labs(
    title = "Annual Maximum NO Concentration by Site",
    x = "Year",
    y = "NO Concentration (ppb)",
    colour = "Site"
  ) +
  scale_x_continuous(breaks = unique(NO_max_yr$year)) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Inspect the overall distribution of the response variable, monthly maximum NO
plot_no_hist <- ggplot(NO_clean, aes(x = NO_max)) +
  geom_histogram(bins = 35, color = "white", fill = "#011451", alpha = 0.85) +
  labs(
    title = "Distribution of Monthly Maximua NO levels",
    x = "Monthly Maximum NO (ppb)",
    y = "Count"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", color = "#011451"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
# png(filename = "hist.png", width = 3000, height = 1700, res = 300)
print(plot_no_hist)
# dev.off()

# NO distribution by city
plot_no_city_box <- ggplot(NO_clean, aes(x = reorder(city, NO_max, median), y = NO_max)) +
  geom_boxplot(fill = "#005398", alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Monthly Maximum NO by City",
    x = "City",
    y = "Monthly maximum NO"
  ) +
  theme_bw()
print(plot_no_city_box)

# NO distribution by site
plot_no_site_box <- ggplot(NO_clean, aes(x = reorder(site, NO_max, median), y = NO_max, fill = city)) +
  geom_boxplot(fill = "#005398", alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Monthly Maximum NO by Site",
    x = "Site",
    y = "Monthly maximum NO",
    fill = "City"
  ) + 
  theme_bw()
print(plot_no_site_box)

# Inspect whether a seasonal pattern by month is present
plot_no_month_box <- ggplot(NO_clean, aes(x = month, y = NO_max)) +
  geom_boxplot(colour = "#011451", fill = "#005398", alpha = 0.5) +
  labs(
    title = "Seasonal Pattern of Monthly Maxima NO levels",
    x = "Month",
    y = "Monthly Maximum NO (ppb)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title  = element_text(face = "bold", color = "#011451"),
    axis.title  = element_text(color = "black"),
    axis.text   = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )
# png(filename = "monthpattern.png", width = 3000, height = 1700, res = 300)
print(plot_no_month_box)
# dev.off()

# Yearly trend of NO_max levels by city
yearly_no_site <- NO_clean %>%
  group_by(site, year) %>%
  summarise(mean_NO_max = mean(NO_max), .groups = "drop")

# Set a colour palette for the plot for better distinguishability 
palette <- c(
  "#CC6677", "#332288", "#DDCC77", "#117733",
  "#88CCEE", "#882255", "#44AA99", "#999933",
  "#AA4499", "#6699CC", "#661100", "#AA7744",
  "#4477AA", "#228833", "#BBBBBB", "#EE8866"
)

plot_no_year_site <- ggplot(yearly_no_site, aes(x = year, y = mean_NO_max, color = site)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.7) +
  scale_color_manual(values = palette) +
  labs(
    title = "Yearly Mean of Monthly Maxima NO levels by Site",
    x = "Year",
    y = "Mean NO level (ppb)",
    color = "Site"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", color = "#011451"),
    axis.title        = element_text(color = "black"),
    axis.text         = element_text(color = "black"),
    panel.grid.minor  = element_blank(),
    legend.position   = "right",
    legend.key.height = unit(0.5, "cm")
  )

# png(filename = "yearlymeanbysite.png", width = 2400, height = 1800, res = 300)
print(plot_no_year_site)
# dev.off()


### -- Exploratory Plots fo Covariate Effects ----------------------------------

# First of all let's check that the values for possible data collection errors

# Outdoor Temperature
summary(NO_clean$temp)

# A maximum observed temperature of 72 degrees is most likely an error, it was
# probably measured in Fahrenheit, we convert it to Celsius 
NO_clean[which.max(NO_clean$temp), "temp"] <- 22

# Wind Speed
summary(NO_clean$wind) 

# Remove NAs for the plotting
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

# Plot NO_max vs. Outdoor Temperature and Wind Speed
plot_covariate_scatter <- ggplot(covariate_long, aes(x = value, y = NO_max)) +
  geom_point(alpha = 0.25, color = "#585858") +
  geom_smooth(se = TRUE, color = "#7d2239", fill = "#e8a0b0") +
  facet_wrap(~ covariate, scales = "free_x") +
  labs(
    title = "Monthly Maxima NO levels vs Meteorological Covariates",
    x = "Covariate Value",
    y = "Monthly Maximum NO (ppb)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title        = element_text(face = "bold", color = "#011451"),
    axis.title        = element_text(color = "black"),
    axis.text         = element_text(color = "black"),
    panel.grid.minor  = element_blank(),
    strip.background  = element_rect(fill = "#011451"),
    strip.text        = element_text(color = "white", face = "bold")
  )

# png(filename = "covariatescatter.png", width = 3500, height = 1800, res = 300)
print(plot_covariate_scatter)
# dev.off()

### -- Geostatistical map ------------------------------------------------------

# Let's also have a look at the map of monitoring stations
sites <- NO %>%
  select(city, site, long, lat) %>%
  distinct() %>%
  filter(!is.na(long), !is.na(lat))

# Colour cities
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
  ) # from this map we see a considerable number of sites fall in the category
    # "Not in a city" even though they are situated in meaningful localities 



# ============================================================================ #
# -- FORMAL DATA ANALYSIS ----------------------------------------------------
# ============================================================================ #


### -- Threshold Selection -----------------------------------------------------

# The regulatory threshold for NO_2, that is highly correlated to NO, is equal
# to 180 ppb. 
# In order to assess if this may be a valid threshold value, we could check the 
# mean residual life plot and the parameter stability plots to see if this is 
# consistent with the data.

x <- NO_clean$NO_max
range(x)
threshrange.plot(x = x,
                 r = c(100, 300),
                 nint = 100)

# Mean Residual Life Plot
extRemes::mrlplot(x = x)

# Set up a data frame for ggplot
thresh_seq_mrl <- seq(min(x), max(x), length.out = 200)

mrl_df <- do.call(rbind, 
                  lapply(thresh_seq_mrl, function(u) {
                    data.frame(threshold = u,
                               mean_excess = mean(x[x > u] - u),
                               se = sd(x[x > u] - u) / sqrt(length(x[x > u]))
                    )}))


# Set up ggplots for sensitivity analysis on the shape parameter
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

# Now, we are ready to build the scale data frame, which we will use to plot the 
# reparametrised scale parameter estimate across different threshold value 
# to assess its stability.

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

# Save plot theme for convenience
plotting_theme <- theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#011451"),
        panel.grid.minor = element_blank())

# MRL Plot
mrl_plot <- ggplot(mrl_df, aes(x = threshold, y = mean_excess)) +
  geom_ribbon(aes(ymin = mean_excess - qnorm(0.975) * se,
                  ymax = mean_excess + qnorm(0.975) * se),
              fill = "#005398", alpha = 0.2) +
  geom_line(color = "#011451", linewidth = 0.9) +
  geom_vline(xintercept = 180, colour = "#7d2239",
             linetype = 2, linewidth = 1.2) +
  labs(title = "Mean Residual Life Plot",
       x = "Threshold (ppb)",
       y = "Mean Excess") +
  plotting_theme

# Shape Parameter Stability Plot (Sensitivity Analysis)
shape_plot <- ggplot(shape_df, aes(x = threshold, y = estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#005398", alpha = 0.2) +
  geom_line(color = "#011451", linewidth = 0.9) +
  geom_vline(xintercept = 180, colour = "#7d2239",
             linetype = 2, linewidth = 1.2) +
  labs(title = "Shape Parameter Stability",
       x = "Threshold (ppb)",
       y = "Shape Parameter Estimate") +
  plotting_theme


# Scale Parameter Stability Plot (Sensitivity Analysis)
scale_plot <- ggplot(scale_df, aes(x = threshold, y = estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "#005398", alpha = 0.2) +
  geom_line(color = "#011451", linewidth = 0.9) +
  geom_vline(xintercept = 180, colour = "#7d2239",
             linetype = 2, linewidth = 1.2) +
  labs(title = "Scale Parameter Stability",
       x = "Threshold (ppb)",
       y = "Reparametrized Scale") +
  plotting_theme

pars_plot <- (scale_plot / shape_plot) | mrl_plot

# png(filename = "scaleshape.png", width = 3500, height = 1500, res = 300)
print(pars_plot)
# dev.off()

# From what we see in these plots, 180 ppb seems a reasonable choice for the 
# threshold; the mean residual life plot seems to suggest a slightly higher 
# threshold, but since the difference would be marginal, we may keep using 180 
# ppb, the regulatory threshold, as our reference.

threshold <- 180

ggplot(NO_blockmax, aes(x = date, y = NO_max, colour = site)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_hline(yintercept = threshold, colour = "grey40", linetype = "dashed") +
  labs(
    title = "Monthly Maximum NO Concentration by Site",
    x = "Date",
    y = "NO Concentration (ppb)",
    colour = "Site"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

1 - mean(x > threshold)
sum(x > threshold)

# 180 corresponds approximately to the 93rd percentile of our data, meaning that
# we will be fitting our model using about 7% of the observations in the 
# original dataset. Overall, we will use 171 observations.


### -- Temporal Evolution of Extreme Pollution Levels --------------------------

# To track the temporal evolution of extreme levels, we can compare the 93rd 
# percentiles of data and the overall number of threshold exceedances for each 
# year from 2007 to 2015.
# To calculate the 93rd percentile of NO_max for each year, we can first group 
# the data by year and then we calculate the quantile.

NO_q93_by_year <- NO_clean %>%
  group_by(year) %>%
  summarise(NO_max_p93 = quantile(NO_max, 0.93)) %>%
  mutate(level = ifelse(NO_max_p93 > threshold, "Above threshold", "Below threshold"))

print(NO_q93_by_year)

# We immediately notice a strongly decreasing trend for the 93rd percentile of 
# observed values across years, meaning that air pollution, in terms of NO 
# concentration, has clearly improved across the years.
# In fact, the 93rd quantile is higher than the threshold value 180 until 2008, 
# then it is always below.

# We can plot these results as a time series of the 93rd percentile:
ggplot(NO_q93_by_year, aes(x = year, y = NO_max_p93)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(title = "Trend of the 93rd Percentile of NO_max (2002–2015)",
       x = "Year",
       y = "93rd Percentile of NO_max") +
  theme_bw()

# We can assess this also in an alternative way: check how many threshold 
# exceedances we have for each year.
# We can do this in a similar way as before. First, we group by year, then we simply
# count the threshold exceedances

NO_exceedances_by_year <- NO_clean %>%
  group_by(year) %>%
  summarise(exceedances = sum(NO_max > threshold))

# Let's display the result
print(NO_exceedances_by_year)

ggplot(NO_exceedances_by_year, aes(x = as.factor(year),
                                   y = exceedances)) +
  geom_bar(stat = "identity", fill = "#f2d25c") +
  labs(title = "Number of NO_max Threshold Exceedances per Year",
       x = "Year",
       y = "Number of Exceedances (NO_max > 180)") +
  theme_bw()

# Even in this case, the decreasing trend is quite clear.
# The vast majority of threshold exceedances is registered until 2008, then only
# few occur.


### -- Model Selection ---------------------------------------------------------

# GPD Modelling with evgam

# Define exceedances
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



# GPD models with xi fixed (constant) via list formula

# Model 0 — Stationary model
m0 <- evgam(list(excess ~ 1, ~ 1),
            data = gpd_data, family = "gpd")
summary(m0)

# Model 1 — Smooth term for year 
m1 <- evgam(list(excess ~ s(year,k = 4), ~ 1),
            data = gpd_data, family = "gpd")
summary(m1)
plot(m1) # year appears to have a linear effect

# Model 2 — Smooth for year + cyclic cubic spline for month, to account for 
#           seasonality
m2 <- evgam(list(excess ~ s(year,k = 4) + s(month_int, bs = "cc", k = 6), ~ 1),
            data = gpd_data, family = "gpd")

summary(m2)

# Month doesn't appear to have a significant effect.

# Model 3 — year + temperature, we start with meteorological covariates
m3 <- evgam(list(excess ~ s(year,k = 4) + s(temp), ~ 1),
            data = gpd_data, family = "gpd")
summary(m3)
plot(m3)

# Temperature appears to be non-significant and shouldn't be included in the 
# final model

# Model 4 — year + wind
m4 <- evgam(list(excess ~ s(year, k = 4) + s(wind) , ~ 1),
            data = gpd_data, family = "gpd")
summary(m4)
plot(m4)


# Model 5 — Addition of a site random effect
m5 <- evgam(list(excess ~ s(year, k = 4) + s(wind) +
                   s(site, bs = "re"), ~ 1),
            data = gpd_data, family = "gpd")
summary(m5)

plot(m5)

# Replicationg these plots in ggplot for the report

n_ls <- ncol(predict(m5, newdata = gpd_data[1:2, ], type = "lpmatrix")$logscale)
Vp_ls <- m5$Vp[1:n_ls, 1:n_ls]

smooth_df <- function(newdata, x_col) {
  Xp <- predict(m5, newdata = newdata, type = "lpmatrix")$logscale
  fit <- predict(m5, newdata = newdata)$logscale
  se <- sqrt(diag(Xp %*% Vp_ls %*% t(Xp)))
  data.frame(
    x = newdata[[x_col]],
    fit = fit - mean(fit),
    se = se
  )
}

# Year Partial Effect
newdat_year <- data.frame(
  year = seq(min(gpd_data$year), max(gpd_data$year), length.out = 100),
  wind = mean(gpd_data$wind),
  site = factor(gpd_data$site[1], levels = levels(gpd_data$site))
)
df_year <- smooth_df(newdat_year, "year")

p_year <- ggplot(df_year, aes(x = x, y = fit)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
              fill = "#005398", alpha = 0.2) +
  geom_line(color = "#011451", linewidth = 1) +
  labs(x = "Year",
       y = expression(paste("Partial effect on log(", sigma, ")")),
       title = "Temporal Effect") +
  plotting_theme


# Wind Speed Partial Effect
newdat_wind <- data.frame(
  year = 2015,
  wind = seq(min(gpd_data$wind), max(gpd_data$wind), length.out = 100),
  site = factor(gpd_data$site[1], levels = levels(gpd_data$site))
)
df_wind <- smooth_df(newdat_wind, "wind")

p_wind <- ggplot(df_wind, aes(x = x, y = fit)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = fit - 1.96 * se, ymax = fit + 1.96 * se),
              fill = "#005398", alpha = 0.2) +
  geom_line(color = "#011451", linewidth = 1) +
  labs(x = "Wind Speed (m/s)",
       y = expression(paste("Partial effect on log(", sigma, ")")),
       title = "Meteorological (Wind Speed) Effect") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold", color = "#011451"))

p_combined <- p_year + p_wind + plot_layout(ncol = 2)

# png(filename = "partialeffects.png", width = 3500, height = 1800, res = 300)
print(p_combined)
# dev.off()

AIC(m5)


# Model 6 — Wind + temp
m6 <- evgam(list(excess ~ s(year,k=4) +s(wind) +s(temp), ~ 1),
            data = gpd_data, family = "gpd")
summary(m6)
plot(m6)

# Model 7 — In model 5 wind is not significant, can we remove it?
m7 <- evgam(list(excess ~ s(year,k = 4) +
                   s(site, bs = "re"), ~ 1),
            data = gpd_data, family = "gpd")
summary(m7)
plot(m7)

# We can check the adequacy of models by running a Kolmogorov-Smirnov test to 
# compare their probability integral transform residuals with a Uniform 
# distribution in (0, 1).
# Since we will do this for every model, we can build a function to avoid 
# repeating the same computations every time.

gpd_ks.test <- function(model) {
  
  y <- model$data$excess # First, we extract the exceedances
  pred <- predict(model, type = "response") # Then, we can predict the model 
                                            # parameters
  
  sigma_hat <- as.numeric(pred[, "scale"])
  xi_hat <- as.numeric(pred[, "shape"])
  
  resid <- ifelse(
    abs(xi_hat) > 1e-6,
    1 - (1 + xi_hat * y / sigma_hat)^(-1 / xi_hat),
    1 - exp(-y / sigma_hat)
  ) # Finally, we can compute PIT residuals
  
  return(ks.test(resid, "punif")) # We use the ks.test function to test whether the 
  # empirical residual distribution is compatible with the
  # theoretical one.
}

gpd_ks.test(m0)

# Since the p-value is larger than 0.05, we fail to reject H0, i. e. we have 
# sufficient statistical evidence in favour of the compatibility of the two
# distributions. So, a stationary model would be already adequate, but can we 
# include some variables to model the scale parameter and improve the overall 
# fit?

gpd_ks.test(m1)
gpd_ks.test(m2)
gpd_ks.test(m3)
gpd_ks.test(m4)
gpd_ks.test(m5)
gpd_ks.test(m6)
gpd_ks.test(m7)

# Any model is appropriate according to the test.

AICs <- AIC(m0, m1, m2, m3, m4, m5, m6)
BICs <- BIC(m0, m1, m2, m3, m4, m5, m6)

AICs[which.min(AICs$AIC), ]
BICs[which.min(BICs$BIC), ]

# Therefore m5 is selected as our final model

# Exceedances per site
table(gpd_data$site)

# Medians (no temp needed for m5)
med_year  <- median(gpd_data$year)
med_wind  <- median(gpd_data$wind, na.rm = TRUE)
ref_site  <- levels(gpd_data$site)[1]   # reference site for the random effect


# Extract xi from m5
# We can also compute confidence intervals for the shape parameter estimate:
sum_5 <- summary(m5)
str(sum_5)
se_xi_5 <- sum_5[[1]]$shape[1, "Std. Error"]
xi_hat_5 <- sum_5[[1]]$shape[1, "Estimate"]

# We can use a 95% CI. But what quantiles should we use?
# We can use a Wald/Normal confidence interval, hence:
alpha <- 1-0.95
ci_xi_5 <- c("lower" = xi_hat_5 - qnorm(1 - alpha/2)*se_xi_5,
             "estimate" = xi_hat_5,
             "upper" = xi_hat_5 + qnorm(1 - alpha/2)*se_xi_5)
ci_xi_5

### -- Return Levels -----------------------------------------------------------

# Since evgam does not give confidence intervals for return levels, we use a
# parametric bootstrap approach to derive them.

# Helper functions
gpd_rl <- function(u, sigma, xi, T, m, zeta_u) {
  lambda_T <- T * m * zeta_u
  if (abs(xi) > 1e-6) {
    u + (sigma / xi) * (lambda_T^xi - 1)
  } else {
    u + sigma * log(lambda_T)
  }
}

gpd_return_period <- function(z, u, sigma, xi, m, zeta_u) {
  if (abs(xi) > 1e-6) {
    term <- 1 + xi * (z - u) / sigma
    if (term <= 0) return(NA_real_)
    (term^(1 / xi)) / (m * zeta_u)
  } else {
    exp((z - u) / sigma) / (m * zeta_u)
  }
}

rgpd_boot <- function(n, sigma, xi) {
  u <- runif(n)
  z <- numeric(n)
  idx <- abs(xi) > 1e-6
  z[idx] <- (sigma[idx] / xi[idx]) * (u[idx]^(-xi[idx]) - 1)
  z[!idx] <- -sigma[!idx] * log(u[!idx])
  z
}

# Setup
B <- 500
T_seq <- c(seq(1, 10, by = 0.5), seq(11, 200, by = 1))

zeta_u <- mean(NO_clean$NO_max > threshold, na.rm = TRUE)
m_year <- nrow(NO_clean) / length(unique(NO_clean$year))

site_most_exc <- names(which.max(table(gpd_data$site)))

ref_year <- 2015
ref_wind <- mean(gpd_data$wind, na.rm = TRUE)

newdat_gpd <- data.frame(
  year = ref_year,
  wind = ref_wind,
  site = factor(site_most_exc, levels = levels(gpd_data$site))
)

# Point estimates
pars_fixed_gpd <- predict(m5, newdata = newdat_gpd, type = "response")
sigma_hat_gpd <- pars_fixed_gpd[1, "scale"]
xi_hat_gpd <- xi_hat_5

z_hat_gpd <- sapply(T_seq, function(T)
  gpd_rl(u = threshold, sigma = sigma_hat_gpd, xi = xi_hat_gpd,
         T = T, m = m_year, zeta_u = zeta_u))

z_max <- max(NO_clean$NO_max, na.rm = TRUE)
T_hat_gpd <- gpd_return_period(z = z_max, u = threshold,
                               sigma = sigma_hat_gpd, xi = xi_hat_gpd,
                               m = m_year, zeta_u = zeta_u)

sigma_hat_all <- predict(m5, type = "response")[, "scale"]
xi_hat_all <- rep(xi_hat_gpd, nrow(gpd_data))

# Bootstrap loop (this has been commented because it's computationally expensive,
# can just load the produced objects without running)

# formula_m5 <- list(excess ~ s(year, k = 4) + s(wind) + s(site, bs = "re"), ~ 1)
# z_boot_gpd <- matrix(NA_real_, nrow = B, ncol = length(T_seq))
# T_boot_gpd <- rep(NA_real_, B)
#   
# for (b in seq_len(B)) {
#     
#   y_sim <- rgpd_boot(n = nrow(gpd_data),
#                                sigma = sigma_hat_all,
#                                xi    = xi_hat_all)
#   data_b <- gpd_data
#   data_b$excess <- y_sim
#   data_b$NO_max <- threshold + y_sim
#     
#   fit_b <- try(evgam(formula = formula_m5, data = data_b, family = "gpd"),
#                  silent = TRUE)
#   if (inherits(fit_b, "try-error")) next
#     
#   pars_b <- try(predict(fit_b, newdata = newdat_gpd, type = "response"),
#                   silent = TRUE)
#   if (inherits(pars_b, "try-error") || any(is.na(pars_b))) next
#     
#   sigma_b <- pars_b[1, "scale"]
#   xi_b <- summary(fit_b)[[1]]$shape[1, "Estimate"]
#     
#   z_row <- sapply(T_seq, function(T)
#     gpd_rl(u = threshold, sigma = sigma_b, xi = xi_b,
#              T = T, m = m_year, zeta_u = zeta_u))
#     
#   if (all(is.finite(z_row))) z_boot_gpd[b, ] <- z_row
#     
#   T_b <- gpd_return_period(z = z_max, u = threshold,
#                              sigma = sigma_b, xi = xi_b,
#                              m = m_year, zeta_u = zeta_u)
#   if (is.finite(T_b)) T_boot_gpd[b] <- T_b
# }

load(url("https://raw.githubusercontent.com/fraver3/EnvEcoProject/main/bootstrap_results_final.RData"))


# Confidence intervals

z_lower_gpd <- apply(z_boot_gpd, 2, quantile, probs = 0.025, na.rm = TRUE)
z_upper_gpd <- apply(z_boot_gpd, 2, quantile, probs = 0.975, na.rm = TRUE)

rl_ci_gpd <- data.frame(
  Return_Period = T_seq,
  RL_estimate = z_hat_gpd,
  RL_lower = z_lower_gpd,
  RL_upper = z_upper_gpd
)

T_lower_gpd <- quantile(T_boot_gpd, 0.025, na.rm = TRUE)
T_upper_gpd <- quantile(T_boot_gpd, 0.975, na.rm = TRUE)
rp_largest_gpd <- c(Estimate = T_hat_gpd, Lower = T_lower_gpd, Upper = T_upper_gpd)
print(rp_largest_gpd)

# Return level plot
p_rl <- ggplot(rl_ci_gpd, aes(x = Return_Period)) +
  
  # Confidence interval ribbon
  geom_ribbon(aes(ymin = RL_lower, ymax = RL_upper), 
              fill = "#005398", alpha = 0.2) +
  
  # Point estimate line
  geom_line(aes(y = RL_estimate), color = "#011451", linewidth = 1) +
  
  # Logarithmic scale for Return Period (just for visualisation purposes)
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200),
    expand = c(0.02, 0)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.5, 0.5)) # Adds 10% extra space at the bottom, 20% at the top
  ) +
  labs(
    x = "Return Period (years)",
    y = "Return Level (ppb)",
    title = "Return Levels for Extreme NO Concentrations",
    subtitle = "For Long Beach (North) in 2015, with wind speed fixed at 8 m/s"
  ) +
  plotting_theme

# png(filename = "returnlevels.png", width = 1750, height = 1800, res = 300)
print(p_rl)
# dev.off()

