###### Random Numbers and Monte Carlo Simulation ######
# 1. dice
N <- 10^5
dice <- function()sample(c(1:6), 6, replace = T)
payoff_1 <- c()
for (i in 1:N) {
  sample <- dice()
  if ((max(sample) - min(sample)) < 3) {
    payoff_1 <- c(payoff_1, 1)
  } else {payoff_1 <- c(payoff_1, 0)}
}
price_1 <- mean(payoff_1)

# 2. coin
k <- c()
coin <- function(){
  sample <- sample(c(0, 1), 1)
  n <- 1
  while (1 == 1) {
    sample <- c(sample, sample(c(0, 1), 1))
    n <- n + 1
    if (sample[n] == sample[n-1] & sample[n] == 1) {
      break
    }
  }
  return(n)
}
for (i in 1:N) {
  k <- c(k, 1/coin())
}
price_2 <- mean(k)

# 3. 
x <- runif(N, -1, 1)
y <- runif(N, -1, 1)
n_red <- sum(x^2 + y^2 <= 1)
area <- n_red/N * 4
(pi <- area)

library(plotrix)
png("pi.png")
plot(x, y, asp = 1)
draw.circle(0, 0, 1, col = "red", border = "red")
dev.off()

# 4.a E & V
Y <- c()
for (i in N) {
  X <- runif(1)
  k <- sample(-N:N, 1)
  Y <- c(Y, x^k)
}
mean(Y)
var(Y)

