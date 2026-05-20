# Exc 3 (week 3)

# 20 birds, binomial distribution with p = 0.86
# What is the probability that 10 birds arrive

binomial_pmf <- function(n, k, p) {
  choose(n, k) * p^k * (1 - p)^(n - k)
}

binomial_pmf(n = 20, k = 10, p = 0.86)

binomial_pmf(n = 20, k = 1:30, p = 0.86) 
binomial_pmf(n = 20, k = 1:30, p = 0.86) >= 0
sum(binomial_pmf(n = 20, k = 1:30, p = 0.86))

barplot(dbinom(0:20, size = 20, prob = 0.86))

# At least 10 birds
sum(dbinom(10:20, size = 20, prob = 0.86))
pbinom(10, size = 20, prob = 0.86)
pbinom(20, size = 20, prob = 0.86)
?pbinom

sum(dbinom(10:20, size = 20, prob = 0.86))
pbinom(9, size = 20, prob = 0.86, lower.tail = FALSE)

1 - pbinom(9, size = 20, prob = 0.86)

qbinom(0.52, size = 20, prob = 0.86)


# More than 5 and less than 16 birds
sum(dbinom(6:15, size = 20, prob = 0.86))


# Working with distributions ins R
rbinom(20, 1, prob = 0.86 )
?rbinom
rbinom(20, 5, prob = 0.86 )
mean(rbinom(2e6, 1, prob = 0.86 ))


l <- list(a = 1:10, b = 3, c = 1:5)
l


a = 1:10
a[1]
`[`(a, 1)

2 + 3
`+`(2, 3)
length(a)

# Exercise 1 (Week 4; 5. Inference)

# Observed data
y <- c(1, 10, 2, 5, 0)

likelihood <- function(N, k, p) {
  sum(log(choose(N, k)) + k * log(p) + (N - k) * log(1 - p))
}

likelihood(14, k = y, p = 0.4)

sum(dbinom(y, size = 14, prob = 0.4, log = TRUE))
prod(dbinom(y, size = 14, prob = 0.4))

ps <- seq(0, 1, len = 1000)

l1 <- c()
l2 <- c()

for (i in ps) {
  l1 <- c(l1, likelihood(14, y, i)) 
  l2 <- c(l2, prod(dbinom(y, size = 14, prob = i)))
}

l1 <- numeric(length = length(ps))
for (i in 1:length(l1)) {
  l1[i] <- likelihood(14, y, ps[i])
}

l1a <- sapply(ps, function(p) likelihood(14, y, p))
l1b <- sapply(ps, likelihood, N = 14, k = y)
plot(l1a, l1b)


plot(ps, l1, type = "l")
ps[which.max(l1)]

plot(ps, l2, type = "l")
ps[which.max(l2)]


# Sampling distribution
sum(y) / (5 * 14)

x <- rbinom(5, 14, prob = 0.2)
p <- sum(x) / 70
p
# SE
sqrt((p * (1 - p)) / sum(x))

x <- sapply(1:1000, function(i) {
 sum(rbinom(5, 14, 0.2) / 70) 
})

hist(x)
sd(x)
