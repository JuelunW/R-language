# HW 5
# Juelun Wang, 10470039
# 1.1 
mu <- c(0.0427, 0.0015, 0.0285)
cov <- matrix(c(0.01, 0.002, 0.001, 0.002, 0.011 ,0.003, 0.001, 0.003, 0.02),
              ncol = 3)

f1 <- function(x) 0.5*x%*%cov%*%x
ui <- rbind(mu, rep(-1, 3))
ci <- c(0.05, -1)

theta <- c(2, -2, 0)
constrOptim(theta, f1, grad = NULL, ui,ci)$par

# 1.2
library("quadprog")
D <- cov
d <- rep(0, 3)
A <- cbind(mu, rep(-1, 3))
b <- ci

solve.QP(D, d, A, b)$solution
solve.QP(D, d, A, b)$value



# 2
t <- c(0.25, 0.5, 1, 2, 3, 5, 7, 10)
r <- c(0.09, 0.11, 0.16, 0.20, 0.24, 0.36, 0.53, 0.64)
r <- r/100

tout <- c(0.75, 1.5, 4, 6, 8)
approx(t, r, xout = tout)$y
spline(t, r, xout = tout, method = "natural")$y


# 3
S0 <- 100
K <- 100
T <- 1
r <- 0.05
sig <- 0.2


f <- function(x){
  (S0*exp((r - 0.5*sig^2)*T + sig*sqrt(T)*x) - K)*dnorm(x)
}
d2 <- (log(S0/K) + (r - 0.5*sig^2)*T)/(sig*sqrt(T))

integrate(f, -d2, 1000)$value*exp(-r*T)



bs.call <- function(S0, K, T1, sigma, r){
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  return(S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2))
}

bs.call(S0, K, T, sig, r)








