NO <- read.csv(file.choose())

str(NO)

table(NO$city)

colSums(is.na(NO)) # Nitric.oxide is the response variable. Probably remove 
                   # the 531 observations

hist(NO$Nitric.oxide..NO.)

hist(log(NO$Nitric.oxide..NO.))

library(dplyr)
library(ggplot2)
library(magrittr)

# Assuming your data frame is called NO
NO_clean <- NO %>%
  filter(!is.na(Nitric.oxide..NO.))

ggplot(NO_clean, aes(x = Nitric.oxide..NO.)) +
  geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    title = "Distribution of Monthly Nitric Oxide (NO) by City",
    x = "Nitric oxide (NO) concentration",
    y = "Count"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

length(unique(NO$site))

NO_clean %$%
  table(site)
  table(city, site)
  
  ggplot(NO_clean, aes(x = Nitric.oxide..NO.)) +
    geom_histogram(bins = 30, colour = "white", fill = "steelblue") +
    facet_wrap(~ site, scales = "free_y") +
    labs(
      title = "Distribution of Monthly Nitric Oxide (NO) by City",
      x = "Nitric oxide (NO) concentration",
      y = "Count"
    ) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text = element_text(face = "bold")
    )


  
  # Assuming your full data frame is called NO
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
  
  

