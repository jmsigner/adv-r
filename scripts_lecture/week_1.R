# Week 1


# List vs vector

v1 <- c(1, 3, 5, 10)
v1

v2 <- c(1, "A", 1)

v2

l1 <- list(1, 3, 5, 10)
l2 <- list(1, "A", 1)

l1
l2

l3 <- list(l1, l2, "ABC")
l3


# CRS
library(sf)
library(tidyverse)

sta <- read.csv("data/weather/feb2015.csv")
head(sta)

sta <- st_as_sf(sta, coords = c("lon", "lat"))

ger <- st_read("data/ger/ger_states_3035.shp")

plot(ger)
points(sta)

ggplot() + geom_sf(data = ger) +
  geom_sf(data = sta)

# wrong: dont do
st_crs(sta) <- st_crs(ger)

ggplot() + geom_sf(data = ger) +
  geom_sf(data = sta)

# correct
st_crs(sta) <- 4326
sta <- st_transform(sta, st_crs(ger))

library(ggplot2)

ggplot() + geom_sf(data = ger) +
  geom_sf(data = sta)


# Spatial subset
nds <- filter(ger, state == "Niedersachsen")

ggplot(nds) + geom_sf() +
  geom_sf(data = sta) +
  geom_sf(data = sta[nds, ], col = "red")

st_join(sta, ger)


# Exc 2: 

set.seed(123)
df1 <- data.frame(
  x = runif(100, 0, 100),
  y = runif(100, 0, 100),
  crown_diameter = runif(100, 1, 15),
  sp = sample(letters[1:4], 100, TRUE)
)

# 1. Use df1 and create a geometry column.

head(df1)
df1 <- st_as_sf(df1, coords = c("x", "y"))
df1


# 2. Buffer each tree with its canopy radius.
df1.b <- st_buffer(df1, dist = df1$crown_diameter * 0.5)
df1.c <- df1 |> mutate(buffer = st_buffer(geometry, dist = crown_diameter * 0.5))
df1.c



# 3. Calculate the crown area of each tree and save it in a new column 
# (hint, you my want to use the function st_area()).

df1.c <- df1.c |> mutate(area = st_area(buffer))


# 4. Find the tree with the largest canopy area.
df1.c |> arrange(-area)
df1.c |> slice_max(area, n = 1)

# 5. Find the tree with the largest canopy area for each species.
df1.c |> 
  group_by(sp) |> 
  slice_max(area, n = 1)

df1.c |> 
  group_by(sp) |> 
  filter(area == max(area))

