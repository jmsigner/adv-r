library(glmmTMB)
library(broom.mixed)
library(tidyverse)

# Set up
K <- 50
n.per.K <- 20
n <- K * n.per.K



df <- data.frame(group = rep(paste0("g", 1:K), each = n.per.K), x = rnorm(n))


df$y <- (1 + rnorm(K, sd = 3)[as.numeric(factor(df$group))]) + 
  (1 + rnorm(K, sd = 0.1)[as.numeric(factor(df$group))]) * df$x + 
  rnorm(n, sd = 0.5)

lmm1 <- glmmTMB(y ~ x + (1 | group), data = df, REML = FALSE)
fixef(lmm1)
ranef(lmm1)


beta_hat <- fixef(lmm1)$cond      
vc       <- VarCorr(lmm1)
sigma_b2 <- as.numeric(vc$cond$group) # between-group variances
sigma2   <- sigma(lmm1)^2        

X <- model.matrix(~ x, df)
y <- df$y
groups <- split(seq_len(nrow(df)), df$group)

shrink <- sapply(seq_along(groups), function(g) {
  idx <- groups[[g]]
  ni <- length(idx)
  ri  <- y[idx] - X[idx, , drop = FALSE] %*% beta_hat # Calculate for each obs in the group, how far off they are from the the predicted ys. 
  lambda <- ni * sigma_b2 / (sigma2 + ni * sigma_b2)   # weights between 0 and 1
  lambda * mean(ri)
})

names(shrink) <- names(groups)
cbind(hand = shrink, glmmTMB = ranef(lmm1)$cond$group[, 1])
