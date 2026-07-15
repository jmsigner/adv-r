## --------------------------------------------------------- ##
# Session 14: Parameterize a geospatial model with glmmTMB ----
## --------------------------------------------------------- ##

#' The package glmmTMB (generalized linear mixed models using template model builder)
#' is an other R package to fit mixed models and offers a lot of flexibility to account
#' for temporal and spatial autocorrelation. 

# Introduction --------

# ... glmmTMB (J): Introduction ----

#' The package glmmTMB (generalized linear mixed models using template model builder)
#' is an other R package to fit mixed models and offers a lot of flexibility to account
#' for temporal and spatial autocorrelation. 

library(glmmTMB) # to fit models
library(broom.mixed) # to work with the results
library(ggeffects) # to visualize results
library(geoR) # to simulate spatial autocorrelation
library(terra)
library(nlme) # required in Intro
library(forecast)
library(ggplot2)

set.seed(131) # for reproducibility

#' The function `glmmTMB()` can be used to fit models. 

# ... glmmTMB (J): Spatial Autocorrelation ----

# Example from here: https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html#spatial-correlations

d <- data.frame(z = as.vector(volcano),
                x = as.vector(row(volcano)),
                y = as.vector(col(volcano)))

# Add some noise and take a random sample of 15
set.seed(1)
d$z <- d$z + rnorm(length(volcano), sd=15)
d <- d[sample(nrow(d), 100), ]

# Plot what we got
volcano.data <- array(NA, dim(volcano))
volcano.data[cbind(d$x, d$y)] <- d$z
image(volcano.data, main="Spatial data", useRaster=TRUE)

d$pos <- numFactor(d$x, d$y)
d$group <- factor(rep(1, nrow(d)))

# Fit the model
m0 <- glmmTMB(z ~ 1, data=d)
m1 <- glmmTMB(z ~ 1 + exp(pos + 0 | group), data=d)

# Make a predictino in space
predict_col <- function(i, m) {
  newdata <- data.frame( pos = numFactor(expand.grid(1:87,i)))
  newdata$group <- factor(rep(1,nrow(newdata)))
  predict(m, newdata=newdata, type="response", allow.new.levels=TRUE)
}

  
pred0 <- sapply(1:61, predict_col, m0)
image(pred0, main="Pred m0")

pred1 <- sapply(1:61, predict_col, m1)
image(pred1, main="Pred m1")

AIC(f0, f1)

# ... glmmTMB (K): Temporal Autocorrelation ----

example_ar <- data.frame(
  time = c(1 : 500),
  y_t = arima.sim(list(order = c(1, 0, 0), ar = .5), n = 500), # autocorrelated response
  x = rnorm(n = 500) # random covariate
)

# Typical workflow

# 1. Elaborate weather the response is auto correlated
# (We have done that during the time series lessons)
  # - Visual inspection of the time series
  # - Components (trend, intercept, seasonality, autocorrelation)
  # - Autocorrelation function
  # - Autoregressive regression

# 2. Find co variables of interest
  # - What would you expect (causality)
  # (We have done that at the end of lesson 3)

# We use glmmTMB, which offers opportunity to consider temporal and spatial autoregression
# in one model. However, glmmTMB is restricted to an autoregregressive process of lag order 1.
# While in nlme::gls, the syntax would have been

nlme::gls(y_t ~ x, data = example_ar, 
          correlation = corARMA(p = 1))

# in glmmTMB it is

glmmTMB::glmmTMB(y_t ~ x + ar1(time + 0 | -1), data = example_ar)

# Note that within ar1, 
# + 0 means that we do not consider an intercept, 
# -1 means that we define no mixed effects.

# Tasks ---------

#' Use the data set `trees2.rds` and model the mean leaf defoliation for pine 
#' as a function of `bio18` (Precipitation of Warmest Quarter).
#' 
#' Work in groups of 2-3 students.
#' 
#' Build 5 models in increasing complexity that 
#' 1. Model only the effect of bio18
#' 2. Account for repeated measurements (random slopes)
#' 3. Account for temporal autocorrelation
#' 4. Account for spatial autocorrelation
#' 5. Account for everything (i.e., combine models 1. to 4.)
#' 
#' Which model do you think is best? Why?

# Model interpretation ----------------------

# We will do this together
