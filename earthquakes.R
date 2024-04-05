library(tidyverse)
library(sf)
library(leaflet)
library(leaflegend)

earthquakes <- read_csv("query (5).csv") %>% 
  mutate(strength = 10^mag,
         year = year(time))

earthquakes %>% select(time, mag, strength) %>% 
  arrange(desc(time)) %>% 
  write_csv("dw_chart.csv")

earthquakes_sf <- earthquakes %>% 
  st_as_sf(coords = c("longitude", "latitude"))


pal <- colorNumeric(
  palette = c("blue", "red"),
  domain = earthquakes$year)

map <- leaflet(earthquakes_sf) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addCircleMarkers(opacity = 0.9,
              weight = 0.5,
              color = ~pal(year),
              radius = ~2^mag,
              popup = ~paste0("<b>",format(time, "%B %d, %Y"),"</b><br>",
                              mag, " magnitude<br>",
                              paste0(floor(strength / 1000), "K"), " strength<br>"),
              #highlightOptions = highlightOptions(color = "red", weight = 1, bringToFront = T)
              ) %>% 
  addLegendNumeric(pal = pal, values = earthquakes$year, title = "Year", position = "bottomleft",
                   labels = c(1750, 1800, 1850, 1900, 1950, 2000)) %>% 
  addLegendSize(pal = pal, values = 2^(earthquakes_sf$mag),
                orientation = "horizontal",
                shape = "circle",
                title = "Magnitude",
                fillColor = "red",
                color = "red",
                fillOpacity = 0.9,
                breaks = 3,
                position = "bottomright",
                numberFormat = function(x){ prettyNum(log(x)/log(2), digits = 1)}
  ) %>% 
  addControl(html = "<h1>NYC Area Earthquakes since 1700</h1>",position = "topright")

map

mapview::mapshot(map, "map/nyc-region-earthquakes.html")

