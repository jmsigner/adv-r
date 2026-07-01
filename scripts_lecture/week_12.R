library(splines)
library(tidyverse)
library(broom)

gala <- read_rds("data/gala.rds")

plot(log(gala$Area), gala$Species)

m0 <- lm(Species ~ log(Area), data = gala)
m1 <- lm(Species ~ log(Area) + I(log(Area)^2), data = gala)
m2 <- lm(Species ~ ns(log(Area), 3), data = gala)
m3 <- lm(Species ~ ns(log(Area), 5), data = gala)

glance(m0)$r.squared
glance(m1)$r.squared
glance(m2)$r.squared
glance(m3)$r.squared

AIC(m0, m1, m2, m3)


hist(resid(m0))
hist(resid(m1))
hist(resid(m2))

library(DHARMa)

plot(simulateResiduals(m0))
plot(simulateResiduals(m1))
plot(simulateResiduals(m2))
plot(simulateResiduals(m3))

library(ggeffects)
predict_response(m0) |> plot()
predict_response(m1) |> plot()
predict_response(m2) |> plot()
predict_response(m3) |> plot()
