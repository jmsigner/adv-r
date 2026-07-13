
library(forecast)
library(ggplot2)
library(tidyverse)

## Time series exercise 2

dat <- read_rds("data/trees.rds")

temp_1 <- dat %>% filter(sp == "spruce") %>% group_by(year) %>% 
  summarise(mean_loss = mean(mean_loss))

dat_ts_spruce <- ts(temp_1 %>% select(mean_loss), start = min(dat$year))

drought_years <- c(2018 : 2020, 2023)
autoplot(dat_ts_spruce) + geom_vline(xintercept = drought_years) + 
  geom_smooth(method = "lm")

# Alternative solution

species <- c("spruce") #, "pine", "beech"

dat_species <- dat %>% filter(sp %in% species) %>% group_by(year) %>% 
  summarise(mean_loss = mean(mean_loss)) %>% # Annual mean over all locations, other option, such as annuel extreme values or sub-region annual means are also thinkable.
  select(-year) %>% ts(start = 1990)

autoplot(dat_species) + theme_minimal() +
  geom_vline(xintercept = c(2018, 2019, 2020, 2023)) + # Add drought events
  annotate(x = 2018, y = +Inf, label = "D.", vjust = 1, geom = "label") +
  annotate(x = 2019, y = +Inf, label = "D.", vjust = 1, geom = "label") +
  annotate(x = 2020, y = +Inf, label = "D.", vjust = 1, geom = "label") +
  annotate(x = 2023, y = +Inf, label = "D.", vjust = 1, geom = "label") + 
  annotate(x = 1995, y = +Inf, label = "D. = Drought", vjust = 1, geom = "label") + 
  ggtitle(species) + geom_smooth(method = "lm")

## Time series exercise 4

acf(x = dat_species)

## Time series exercise 5

tslm(dat_species ~ trend) |> summary()
tslm(dat_species ~ trend) |> resid() |> acf()

## Time series exercise 6

temp_goe <- read_csv2("data/month_mean_temp_goe.csv")

temp_goe <- temp_goe |> select(mean_daymean_temp) |>
  ts(start = c(min(temp_goe$year), 1), # Starting year = min year
     # Starting month = Jan 
     frequency = 12) # monthly data

decompose(temp_goe |> window(start = c(2000, 1), end = c(2020, 12), type = "additive")) |> autoplot() + theme_minimal()
     

## Time series exercise 7

dat_species_detrend <- tslm(dat_species ~ trend) |> resid()

library(vars)
VARselect(dat_species_detrend)
dat_species_detrend_ar2 <- dat_species_detrend |> Arima(order = c(2, 0, 0), include.mean = FALSE)
checkresiduals(dat_species_detrend_ar2)

ts.union(dat_species_detrend, 
         fitted(dat_species_detrend_ar2)) |> autoplot() +
  theme_minimal() + theme(legend.position = "bottom")

summary(dat_species_detrend_ar2)
