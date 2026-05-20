# Week 6

# Exercise from week 5
library(tidyverse)
library(ggeffects)



dat <- read_rds("data/trees.rds")
dat

dat <- filter(dat, sp %in% c("pine", "oak", "spruce"), year == 2020)
dat

ggplot(dat, aes(bio18, mean_loss, col = sp)) + geom_point()

# First model

dat$bio18_s <- dat$bio18 - mean(dat$bio18)

m1 <- lm(mean_loss ~ bio18_s, data = dat)
summary(m1)
predict_response(m1) |> plot()





m2 <- lm(mean_loss ~ bio18_s + sp, data = dat)
summary(m2)
predict_response(m2, terms = c("bio18_s", "sp")) |> plot()



m3 <- lm(mean_loss ~ bio18_s * sp, data = dat)
predict_response(m3, terms = c("bio18_s", "sp")) |> plot()

predict_response(m3, terms = c("bio18_s", "sp"), interval = "confidence") |> plot()
predict_response(m3, terms = c("bio18_s", "sp"), interval = "prediction") |> plot()
summary(m3)

# Why does prediction interval stops after 150
range(dat$bio18_s)

hist(resid(m1))
hist(resid(m2))
hist(resid(m3))

AIC(m1, m2, m3)


summary(m1)
summary(m2)
summary(m3)

# Visualize
predict_response(m1) |> plot()
predict_response(m2) |> plot()


# Poisson GLM

rpois(10, 59)
x <- rpois(1000, 15)
hist(x)
mean(x)

# Tubercolosis
dat <- read.table("data/Boar.txt", header = TRUE)
head(dat)

m1 <- glm(Tb ~ SEX + LengthCT, data = dat, family = binomial())
summary(m1)

predict_response(m1, term = c("LengthCT", "SEX")) |> plot()
predict_response(m1, term = c("SEX", "LengthCT")) |> plot() # not meaningful

# Predict
nd <- data.frame(
  SEX = 1, 
  LengthCT = 60:150
)

plot(nd$LengthCT, predict(m1, nd), type = "l")
plot(nd$LengthCT, predict(m1, nd, type = "response"), type = "l")

m1$family$linkinv()
plot(nd$LengthCT, m1$family$linkinv(predict(m1, nd)), type = "l")
plot(nd$LengthCT, predict(m1, nd, type = "response"), type = "l")

summary(m1)
m1$family$linkinv(coef(m1)[1])

glm()

m1a <- glm(Tb ~ SEX + LengthCT, data = dat)
m1b <- lm(Tb ~ SEX + LengthCT, data = dat)

coef(m1a)
coef(m1b)


# How good does the model fit
hist(resid(m1))

library(DHARMa)
plot(simulateResiduals(m1))
plot(simulateResiduals(m1a))


# Exercise
library(DHARMa)
dat <- read_csv("data/woodp_Occ.csv")
dat

m1 <- glm(y.1 ~ snags, family = binomial(), data = dat)
summary(m1)

m1$family$linkinv(coef(m1)[1])
m1$family$linkinv(coef(m1)[2]) # does not make sense, because we would remove the intercept. 


exp(coef(m1)[1])

p1 <- m1$family$linkinv(coef(m1)[1] + coef(m1)[2] * 1)
p2 <- m1$family$linkinv(coef(m1)[1] + coef(m1)[2] * 2)

p1 <- (coef(m1)[1] + coef(m1)[2] * 1)
p2 <- (coef(m1)[1] + coef(m1)[2] * 2)

0.18 * 1.45

# We can calculate
p2 / p1



predict_response(m1) |> plot()

# Evaluate model
plot(simulateResiduals(m1))


# More than one predictors
dat$date.1.1 <- dat$date.1 - min(dat$date.1)
m2 <- glm(y.1 ~ date.1.1 + snags, family = binomial(), data = dat)
summary(m2)

m2$family$linkinv(coef(m2)[1])
exp(coef(m2)[2:3])

predict_response(m2, term = c("snags", "date.1.1")) |>
  plot() +
  geom_hline(yintercept = 0.04, col = "red")

# Ensuring the prediction "crosses" the intercept, i.e., also setting date.1.1 to 0
predict_response(m2, term = c("snags", "date.1.1[0]")) |>
  plot() +
  geom_hline(yintercept = 0.04, col = "red")
