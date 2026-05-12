# MC - steps
a = b = 1

y <- rbinom(100, 1, 0.5)
K <- 1
sum(y)

current <- 0.7
proposal <- 0.71
# Step 2: 
prop_plaus <- dbeta(proposal, a, b) * 
  prod(dbinom(y, K, prob = proposal)) # The likelihood

current_plaus <- dbeta(current, a, b) * 
  prod(dbinom(y, K, prob = current)) # The likelihood

alpha <- min(1, prop_plaus / current_plaus) 

sample(c(proposal, current), size = 1, prob = c(alpha, 1 - alpha))
