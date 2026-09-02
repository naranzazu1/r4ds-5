#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(purrr)
library(leaflet)
library(rvest)
library(httr2)
library(jsonlite)
#
#
#
raw <- fromJSON("data/wildfires.geojson")
nrow(raw$features)
#
#
#
first_fire <- raw$features[1, ]
names(first_fire)
#
#
#
first_coordinate <- first_fire$geometry$coordinates[[1]][1, 1, ]
first_coordinate
#
#
#
features <- tibble(features = raw$features)
features <- unnest_wider(features, features)
features <- unnest_wider(features, properties)
features <- unnest_wider(features, geometry, names_sep = "_")
#
#
#
#| cache: true
fires <- features |>
  select(incident, gis_acres, fire_year, agency, state, geometry_coordinates) |>
  mutate(
    gis_acres = as.numeric(gis_acres),
    fire_year = as.integer(fire_year)
  )
#
#
#
august_fires <- fires |>
  filter(str_detect(incident, "August"))
nrow(august_fires)

fires |>
  arrange(desc(gis_acres)) |>
  slice_head(n = 10)

ggplot(fires, aes(x = gis_acres)) +
  geom_histogram(bins = 30) +
  scale_x_log10()

fires_per_year <- fires |>
  group_by(fire_year) |>
  summarise(total_acres = sum(gis_acres, na.rm = TRUE))

ggplot(fires_per_year, aes(x = fire_year, y = total_acres)) +
  geom_line() +
  labs(
    title = "Total Acres Burned by Wildfires per Year",
    x = "Fire year",
    y = "Total acres burned",
    caption = "Source: wildfire records in data/wildfires.geojson"
  )

fires_by_state <- fires |>
  group_by(state) |>
  summarise(total_acres = sum(gis_acres, na.rm = TRUE)) |>
  arrange(desc(total_acres)) |>
  slice_head(n = 10)
fires_by_state
#
#
#
big_fires <- fires |>
  filter(gis_acres >= 100000) |>
  mutate(
    lon = map_dbl(geometry_coordinates, ~ mean(.x[[1]][, , 1], na.rm = TRUE)),
    lat = map_dbl(geometry_coordinates, ~ mean(.x[[1]][, , 2], na.rm = TRUE))
  )
big_fires
#
#
#
#
