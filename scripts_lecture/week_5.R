one_iter <- function(a, b, current, y, K) {
  
  ##
  if (FALSE) {
    a <- 1
    b <- 1
    current <- 0.44
    y <- rbinom(10, 1, prob = 0.1)
    K <- 2
  }
  ##
  
  # a and b are parameters of the prior distribution
  # we use a beta prior here
  # Step 1: propose the next value
  proposal <- runif(1, 0, 1)
  # Step 2:
  prop_plaus <- dbeta(proposal, a, b) *
    prod(dbinom(y, K, prob = proposal)) # The likelihood
  current_plaus <- dbeta(current, a, b) *
    prod(dbinom(y, K, prob = current)) # The likelihood
  alpha <- min(1, prop_plaus / current_plaus )
  next_stop <- sample(c(proposal, current), size = 1, prob = c(alpha, 1 - alpha))
  data.frame(proposal, alpha, next_stop)
}

x <- seq(0, 1, len = 100)
plot(x, dbeta(x, 0.1, 0.1), type = "l")

x <- seq(-50, 50, len = 100)
plot(x, dnorm(x, 20, 50), type = "l")



betabin <- function(N, a, b, y, K) {
  current <- 0.5
  pi <- rep(0, N)
  for (i in 1:N) {
    sim <- one_iter(a, b, current, y = y, K = K)
    pi[i] <- sim$next_stop
    current <- sim$next_stop
  }
  r <- data.frame(iter = 1:N, pi)
}

set.seed(123)
# observed data
y <- rbinom(50, 10, 0.2)
# flat prior
curve(dbeta(x, 1, 1), from = 0, to = 1)

r <- betabin(10000, 1, 1, y, 10)
hist(r$pi)
hist(tail(r$pi, -500))


# Different priors
set.seed(123)
y <- rbinom(500, 1, 0.2)
# prior
curve(dbeta(x, 100, 3), from = 0, to = 1)

r <- betabin(10000, 100, 3, y, 2)
hist(tail(r$pi, -500))
