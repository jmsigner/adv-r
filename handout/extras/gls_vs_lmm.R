library(glmmTMB)
library(broom)
library(broom.mixed)
library(tidyverse)

# Set up
K <- 50
n.per.K <- 20
n <- K * n.per.K
set_theme(theme_minimal())

# Linear model
set.seed(81)
res <- replicate(100, {
  df <- data.frame(group = rep(paste0("g", 1:K), each = n.per.K), x = rnorm(n))
  df$y <- 1 + 1 * df$x + rnorm(n, sd = 0.5)
  
  # models
  lm1 <- lm(y ~ x, data = df)
  gls1 <- gls( y ~ x, data   = df, method = "ML")
  lmm1 <- glmmTMB(y ~ x, data = df, REML = FALSE)
  
  bind_rows(
    tidy(lm1, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "lm"), 
    tidy(gls1, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "gls"), 
    tidy(lmm1, conf.int = TRUE, effects = "fixed") |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "lmm"))
}, simplify = FALSE)

res |> bind_rows() |> 
  mutate(mod = fct(mod, levels = c("lm", "gls", "lmm"))) |> 
  ggplot(aes(mod, estimate)) + geom_boxplot() +
  geom_hline(yintercept = 1, lty = 2, col = "red") + 
  facet_wrap(~ term, scale = "free") 
  
# Coverage
res |> bind_rows() |> mutate(covered = conf.low < 1 & conf.high > 1) |> 
  mutate(mod = fct(mod, levels = c("lm", "gls", "lmm"))) |> 
  group_by(mod, term) |> 
  summarise(coverage = mean(covered)) |> 
  ggplot(aes(term, coverage, col = mod)) + 
  geom_hline(yintercept = 0.95) +
  geom_point(position = position_dodge2(width = 0.4), size = 3) +
  scale_y_continuous(limits = c(0, 1))


# Random intercept
set.seed(91)
res <- replicate(100, {
  df <- data.frame(group = rep(paste0("g", 1:K), each = n.per.K), x = rnorm(n))
  df$y <- 1 + 1 * df$x + rnorm(K, sd = 3)[as.numeric(factor(df$group))] + rnorm(n, sd = 0.5)
  
  # models
  lm1 <- lm(y ~ x, data = df)
  gls1 <- gls( y ~ x, correlation = corCompSymm(form = ~ 1 | group), data   = df, method = "ML")
  lmm1 <- glmmTMB(y ~ x + (1 | group), data = df, REML = FALSE)
  
  bind_rows(
    tidy(lm1, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "lm"), 
    tidy(gls1, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "gls"), 
    tidy(lmm1, conf.int = TRUE, effects = "fixed") |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "lmm"))
}, simplify = FALSE)

res |> bind_rows() |> 
  mutate(mod = fct(mod, levels = c("lm", "gls", "lmm"))) |> 
  ggplot(aes(mod, estimate)) + geom_boxplot() +
  geom_hline(yintercept = 1, lty = 2, col = "red") + 
  facet_wrap(~ term, scale = "free") 
  
# Coverage
res |> bind_rows() |> mutate(covered = conf.low < 1 & conf.high > 1) |> 
  mutate(mod = fct(mod, levels = c("lm", "gls", "lmm"))) |> 
  group_by(mod, term) |> 
  summarise(coverage = mean(covered)) |> 
  ggplot(aes(term, coverage, col = mod)) + 
  geom_hline(yintercept = 0.95) +
  geom_point(position = position_dodge2(width = 0.4), size = 2) 


# Random slope and random intercept
set.seed(100)
res <- replicate(100, {
  df <- data.frame(group = rep(paste0("g", 1:K), each = n.per.K), x = rnorm(n))
  df$y <- (1 + rnorm(K, sd = 3)[as.numeric(factor(df$group))]) + 
    (1 + rnorm(K, sd = 3)[as.numeric(factor(df$group))]) * df$x + 
    rnorm(n, sd = 0.5)
  
  # models
  lm1 <- lm(y ~ x, data = df)
  gls1 <- gls( y ~ x, correlation = corCompSymm(form = ~ x | group), data   = df, method = "ML")
  lmm1 <- lmer(y ~ x + (x | group), data = df, REML = FALSE)
  
  bind_rows(
    tidy(lm1, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "lm"), 
    tidy(gls1, conf.int = TRUE) |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "gls"), 
    tidy(lmm1, conf.int = TRUE, effects = "fixed") |> select(term, estimate, conf.low, conf.high) |> mutate(mod = "lmm"))
}, simplify = FALSE)

res |> bind_rows() |> 
  mutate(mod = fct(mod, levels = c("lm", "gls", "lmm"))) |> 
  ggplot(aes(mod, estimate)) + geom_boxplot() +
  geom_hline(yintercept = 1, lty = 2, col = "red") + 
  facet_wrap(~ term, scale = "free") 
  
# Coverage
res |> bind_rows() |> mutate(covered = conf.low < 1 & conf.high > 1) |> 
  mutate(mod = fct(mod, levels = c("lm", "gls", "lmm"))) |> 
  group_by(mod, term) |> 
  summarise(coverage = mean(covered)) |> 
  ggplot(aes(term, coverage, col = mod)) + 
  geom_hline(yintercept = 0.95) +
  geom_point(position = position_dodge2(width = 0.4), size = 2) 
