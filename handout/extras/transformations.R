# What does linearity mean?
library(tidyverse)
library(patchwork)

set.seed(123)

x <- runif(100, -2, 2)
b0 <- 2
b1 <- 1.5

df <- data.frame(
  x = x, 
  y1 = rnorm(length(x), b0 + b1 * x, 0.15))

df$y2 <- rnorm(length(x), b0 + b1 * exp(x), 0.15)
df$y3 <- rnorm(length(x), b0 + exp(b1 * x), 0.15)

df1 <- df |> pivot_longer(cols = y1:y3, values_to = "y", names_to = "generating_process") |> 
  nest(data = -generating_process) |> 
  mutate(
    m1 = map(data, ~ lm(y ~ x, data = .x)), 
    m2 = map(data, ~ lm(y ~ exp(x), data = .x)), 
    m3 = map(data, ~ nls(y ~ b0 + exp(b1 * x), start = list(b0 = 1, b1 = 1), data = .x))
  )

df2 <- df1 |> pivot_longer(cols = m1:m3) |> 
  mutate(AIC = map_dbl(value, AIC))

# Comparing with AIC
ggplot(df2, aes(name, AIC, col = generating_process, 
                group = generating_process)) + geom_point() + geom_line()
    
.x <- 1
p <- map(1:nrow(df2), ~ marginaleffects::predictions(
  df2$value[[.x]], 
  newdata = data.frame(x = seq(-2, 2, len = 100))) |> 
    ggplot(aes(x, estimate)) + 
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
    geom_line() +
    geom_point(data = df2$data[[.x]], aes(x, y)) +
    labs(
    title = paste0("Data generating process: ", df2$generating_process[.x]), 
    subtitle = paste0(" (fitted model = ", df2$name[.x], ")")) +
    theme_light()
)

wrap_plots(p) +
  plot_layout(ncol = 3)

plots

