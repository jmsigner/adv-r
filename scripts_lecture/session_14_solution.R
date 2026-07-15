
## --------------------------------------------------------- ##
# Session 14: Parameterize a geospatial model with glmmTMB ----
## --------------------------------------------------------- ##

#' The package glmmTMB (generalized linear mixed models using template model builder)
#' is an other R package to fit mixed models and offers a lot of flexibility to account
#' for temporal and spatial autocorrelation. 

# Preparation ----

## Libraries ----

library(glmmTMB) # to fit models
library(broom.mixed) # to work with the results
library(ggeffects) # to visualize results
library(geoR) # to simulate spatial autocorrelation
library(terra)
library(tidyverse)
library(forecast)
library(patchwork)

## Definitions ----

species <- "pine"

## Data ----

# Loading our well known forest health data set. Same as `trees.rds` but with x-y coordinates.

dat <- read_rds("data/trees2.rds")
tt <- read_rds("data/trees.rds")

dat <- dat |> filter(sp %in% c("beech", "spruce", "pine"))
dat$sp <- factor(dat$sp)

dat1 <- filter(dat, sp == species) |> 
  mutate(year1 = factor(year))

dat1$pos <- numFactor(dat1$x/1e3, dat1$y/1e3)
dat1$group <- factor(rep(1, nrow(dat1)))
dat1$point_id <- factor(dat1$point_id)


lm(mean_loss ~ bio1 + ele, data = dat1, weights = dat1$n_trees)
dat1 |> group_by(year) |> summarise(mean_loss = mean(mean_loss), bio1 = mean(bio1)) |> lm(mean_loss ~bio1, data = _)

lm(max_loss ~ bio1 + ele, data = dat1, weights = dat1$n_trees)
dat1 |> group_by(year) |> summarise(max_loss = mean(max_loss), bio1 = mean(bio1)) |> lm(max_loss ~bio1, data = _)

## Modelling ----

### Null model
m2a <- glmmTMB(mean_loss ~ 1,  data = dat1)
m2b <- glmmTMB(mean_loss ~ 1 + (1 | point_id) ,  data = dat1)

### Simple models
m2c <- glmmTMB(mean_loss ~ bio1,  data = dat1)
m2d <- glmmTMB(mean_loss ~ bio1 + (bio1 | point_id) ,  data = dat1)

### Only temporal
m2e <- glmmTMB(mean_loss ~ 1 + ar1(year1 + 0 | point_id),
               data = dat1)
### Only Spatial
m2f <- glmmTMB(mean_loss ~ 1 + exp(pos + 0 | group), data = dat1)

### Incl. bio1
m2g <- glmmTMB(mean_loss ~ bio1 + ar1(year1 + 0 | point_id),
               data = dat1)
m2h <- glmmTMB(mean_loss ~ bio1 + exp(pos + 0 | group), data = dat1)

### Mixed models
m2i <- glmmTMB(mean_loss ~ bio1 + (bio1 | point_id) + ar1(year1 + 0 | point_id), data = dat1)
m2j <- glmmTMB(mean_loss ~ bio1 + (bio1 | point_id) + exp(pos + 0 | group), data = dat1)

## Geospatial model
m2k <- glmmTMB(mean_loss ~ bio1 + (bio1 | point_id) + ar1(year1 + 0 | point_id)  + 
                 exp(pos + 0 | group), data = dat1)

## Results ----
AIC(m2a, m2b, m2c, m2d, m2e, m2f, m2g, m2h, m2i, m2j, m2k)

tidy(m2c) |> filter(term == "bio1")
tidy(m2d) |> filter(term == "bio1")
tidy(m2g) |> filter(term == "bio1")
tidy(m2h) |> filter(term == "bio1")
tidy(m2i) |> filter(term == "bio1")
tidy(m2j) |> filter(term == "bio1")
tidy(m2k) |> filter(term == "bio1")

p1 <- predict_response(m2i) |> plot() + ggtitle("temporal")
p2 <- predict_response(m2j) |> plot() + ggtitle("spatial")
p3 <- predict_response(m2k) |> plot() + ggtitle("both")

plot(simulateResiduals(m2l))


(p1 + p2 + p3) & scale_y_continuous(limits = c(17, 37))

## Extrapolation ----

### Crown defoliation today

m2j_forecast <- m2j |> broom::augment()

dat1 |> bind_cols(m2j_forecast) |> filter(year == 2023) |> ggplot(aes(x = x, y = y, col = .fitted)) + 
  geom_point(size = 5)

