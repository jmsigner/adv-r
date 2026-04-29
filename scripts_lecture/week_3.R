
# Exercise 1

expand.grid(toss1 = c("H", "T"), toss2 = c("H", "T"), toss3 = c("H", "T"))

expand.grid(list(1:6, 1:6))

res <- expand.grid(replicate(10, 1:6, simplify = FALSE))
nrow(res)


S <- expand.grid(d1 = 1:6, d2 = 1:6, d3 = 1:6, d3 = 1:6)

S <- expand.grid(d1 = 1:6, d2 = 1:6, d3 = 1:6, d3 = 1:6)

1 / nrow(S)

outcome <- rowSums(S)

sum(nrow(S) *  1 / nrow(S))
sum(length(which(outcome > 10)) *  1 / nrow(S))

mean(rowSums(S) > 10)
mean(rowSums(S) <= 5)

# Exc 2
f1 <- function(x, k) {
  2 * x / (k * (k + 1))
}

f1(1, k = 5)

res <- sapply(0:5, function(x) f1(x, k = 5))

all(res >= 0)
sum(res)

res <- sapply(0:25, function(x) f1(x, k = 25))

all(res >= 0)
sum(res)
