# Exercise 3

library(terra)
library(sf)
library(tidyverse)

r <- rast("data/raster/dem_3035.tif")
plot(r)

# 2.
res(r)
crs(r)

r

# 3.
ger <- st_read("data/ger/ger_states_3035.shp")
ger3 <- vect("data/ger/ger_states_3035.shp")

plot(r)
plot(ger, add = TRUE)

ger1 <- st_transform(ger, crs(r))
crs(ger) == crs(r)
crs(ger1) == crs(r)
crs(ger3) == crs(r)


ggplot() + geom_sf(data = ger) + geom_sf(data = ger1, col = "red")

nds <- filter(ger, state == "Niedersachsen")

plot(r)
plot(nds, add = TRUE)

plot(mask(r, nds))
plot(crop(r, nds))
plot(crop(mask(r, nds), nds))

dem.nds <- crop(mask(r, nds), nds)

# 4. 
dem.nds[] |> head()

dem.nds[] |> as_tibble() |>  summarise(m = mean(dem_3035, na.rm = TRUE))
global(dem.nds, "mean", na.rm = TRUE)

terra::extract(r, nds) |> as_tibble() |> summarise(m = mean(dem_3035, na.rm = TRUE))
terra::extract(r, nds, "mean", na.rm = TRUE)

# 5.
plot(dem.nds > 100)

terra::extract(r, nds) |> filter(dem_3035 > 100) |> nrow() /
terra::extract(r, nds) |> nrow()

global(dem.nds > 100, mean, na.rm = TRUE)


# Exercise 4

r <- rast("data/raster/dem_3035.tif")
ger <- st_read("data/ger/ger_states_3035.shp")


nd <- terra::extract(r, ger)
nd |> group_by(ID) |> 
  summarise(n = n(), 
            mean = mean(dem_3035, na.rm = TRUE), 
            sd = sd(dem_3035, na.rm = TRUE)) |> 
  mutate(name = ger$state)


nd
