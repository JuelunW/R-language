library(quantmod)
library(PerformanceAnalytics)

bond <- read.csv("bonds-2.csv")
forward <- read.csv("forward.csv")

############ 1. Bond Portfolio Management ############
#### 1.1 ####
ticker <- list("DGS2", "DGS5", "DGS10", "DGS30")
names(ticker) <- c("DGS2", "DGS5", "DGS10", "DGS30")
dgs <- lapply(ticker, function(x)get(getSymbols(x[1], src = "FRED"
                                                , from = "2018-01-02", to = "2021-11-04")))
dgs <- lapply(dgs, function(x)subset(x, index(x) >= as.Date("2018-01-02") & index(x) <= as.Date("2021-11-04")))

dgs.clean <- lapply(dgs, function(x)na.omit(x))
r.dgs <- lapply(dgs, function(x)na.omit(x/lag(x) - 1))

pp <- Reduce(merge, dgs.clean)
write.csv(pp, "pp.csv")
write.csv(index(pp), "pp1.csv")
plot.zoo(pp, plot.type = "single", col = 1:4, ylab = "DGS")
legend("topright", c("DGS2", "DGS5", "DGS10", "DGS30"), fill = 1:4)


#### 1.2 bond valuation ####
Market_Price <- bond$Price

bond_price_f <- function(y, cop, T, FV) {
  DF <- 1/(1 + y)
  CF_seq <- rep(cop*FV, T)
  CF_seq[length(CF_seq)] <- CF_seq[length(CF_seq)] + FV
  DF_seq <- DF^(1:T)
  DCF <- CF_seq*DF_seq
  Price <- sum(DCF)
  return(Price)
}

freq <- 1
y <- bond$Yield/freq/100
cop <- bond$Coupon/freq/100
T <- bond$Maturity*freq
FV <- 100

our_price <- c()
for (i in 1:nrow(bond)) {
  our_price <- c(our_price, bond_price_f(y[i], cop[i], T[i], FV))
}


plot(our_price ~ Market_Price, type = "p")
abline(0, 1)


#### 1.3 the yield that matches the market price ####
y_seq <- seq(0,0.05,length = 100)
price_seq <- list()
for (i in 1:nrow(bond)) {
  price_seq[[i]] <- sapply(y_seq, function(x) bond_price_f(x, cop[i], T[i], FV))
}


#plot(price_seq[[1]] ~ y_seq, type = "l", ylab = c("bond", bond$Number[1]))
#abline(h = Market_Price[1], col = 2)

# find the root of the bond price such that our price equalts the market
our_yield <- c()
for (i in 1:nrow(bond)) {
  find_yield <- function(x) bond_price_f(x, cop[i], T[i], FV) - Market_Price[i]
  our_yield <- c(our_yield, round(100*freq*uniroot(find_yield, c(0, 1), tol = 10^-16)$root, 2))
}

plot(y*100, our_yield, type = "p", xlab = "market_yield")
abline(0, 1)



#### 1.4 Duration ####
duration <- function(y, cop, T, FV) {
  DF <- 1/(1 + y)
  DF_seq <- DF^(1:T)
  CF_seq <- rep(cop*FV, T)
  CF_seq[length(CF_seq)] <- CF_seq[length(CF_seq)] + FV
  DCF <- CF_seq*DF_seq
  Price <- sum(DCF)
  sum((1:T)*CF_seq*DF_seq)/Price
}


bond$Duration <- rep(NA, nrow(bond))
for (i in 1:nrow(bond)) {
  bond$Duration[i] <- duration(y[i], cop[i], T[i], FV)
}


sb <- split(bond, bond$Maturity)
table.1.4 <- data.frame(min = sapply(sb, function(x)min(x$Duration)), 
                        max = sapply(sb, function(x)max(x$Duration)), 
                        row.names = names(sb))
write.csv(table.1.4, "1.4 duration.csv")


#### 1.5 Taylor Expansion ####
price.changed <- c()
for (i in (nrow(bond) - 3):nrow(bond)) {
  price.changed <- c(price.changed, bond_price_f(y[i]+0.0025, cop[i], T[i], FV))
}
change <- cbind(price.origan = bond$Price[17:20], price.changed = price.changed)

matplot(c(2, 5, 10, 30), change, type = "l", lty = 1, col = 1:2, ylab = "price", xlab = "maturity")
legend("topright", legend = colnames(change), fill = 1:2)





install.packages("pracma")
install.packages("calculus")
library(pracma)
taylor(function(y)bond_price_f(y, cop[1], T[1], FV), 3, 0)



#### 1.6 Portfolio ####
w <- seq(-1, 1, length = 1000)
d.port <- w*bond$Duration[17] + (1 - w)*bond$Duration[18]

plot(w, d.port, type = "l")
abline(h = 3)

get.w <- function(x) {
  uniroot(function(w)w*bond$Duration[17] + (1 - w)*bond$Duration[18] - x, c(-1, 1), tol = 10^-15)$root
}

w3.17<- round(get.w(3)*100000/bond$Price[17])
w3.18 <- round((1 - get.w(3))*100000/bond$Price[18])
w6.17 <- round(get.w(6)*100000/bond$Price[17])
w6.18 <- round((1 - get.w(6))*100000/bond$Price[18])



#### 1.7 ####


############ 2. Forward Contracts and No-Arbitrage Pricing ############
r <- 1.75/100
sigma <- 0.2
d <- 0
S0 <- 100

#### 2.1 fair value ####
k <- seq(1, 5, length = 5)
price <- S0*exp((r - d)*k)

plot(price ~ k, type = "l")



#### 2.2 ####
n <- 252
m <- 10^4
MC <- function(k){
  S.vec <- rep(S0, m)
  for (i in 1:(n*k)){
    S.vec <- S.vec*exp(rnorm(m, (r - d - 0.5*sigma^2)*1/n, sigma*sqrt(1/n)))
  }
  return(pmax(S.vec, 0))
}

mc <- lapply(as.list(k), function(k)exp(-r*k)*pmax(MC(k), 0))
boxplot(mc, xlab = "k", ylab = "price")
price.f <- sapply(mc, mean)



#### 2.3 VaR ?####
S <- MC(1)
port <- S - S0 + (price[1] - S) #MC - fair value ?#S - price[1]
VaR <- mean(port) - quantile(port,0.05)
VaR
boxplot(port)



#### 2.4 ?####
# Borrow in n stock indices of S0 with interest rate of r''.
# Sell them, then invest money in fixed interest market with interest rate of r.
# Meanwhile, enter into a forward contract of S0*exp(r'*T) which r' < r.

# When contract is expiry,
# Withdraw money from fixed interest market of S0*exp(r*T).
# Exercise the contract.
# Payback n stock indices and its interest of S0*(exp(r''*T) - 1).
# Profit: S0*exp(r*T) - S0*exp(r'*T) + n - (n + S0*(exp(r''*T)-1))
#       = S0*(exp(r*T) - exp(r'*T) - exp(r''*T) + 1)
#       = S0*(1 + exp(r*T) - exp(r'*T) - exp(r''*T))

market.price <- price[1] - 0.25

#long forwards short underlying
PnL <- price[1] - market.pricep




############ 3. Managing Linear Risk ############
St <- 1.1594
#### 3.1 theta and volatility ####
forward$price <- 0.5*(forward$High + forward$Low)
theta <- mean(log(forward$price/St)/seq(1/12, 1, length = 12))

ED <- getSymbols("EURUSD=X", from = "2018-01-01", to = "2021-11-05", auto.assign = FALSE)
R <- na.omit(diff(log(ED[, 6])))
sig <- sd(R)*sqrt(252)
#mu <- mean(R)*252 + 0.5*sig^2

#### 3.2 VaR for the Unhedged ####


MC1 <- function(k){
  S.vec <- rep(St, m)
  for (i in 1:round(n*k)){
    S.vec <- S.vec*exp(rnorm(m, (theta - 0.5*sig^2)*1/n, sig*sqrt(1/n)))
  }
  return(pmax(S.vec, 0))
}

VT <- MC1(5/12) - St
boxplot(VT)
VaR <- (mean(VT) - quantile(VT, 0.01))*1250000

#### 3.3 Unitary Hedge ####





FT <- St*exp(theta*(td - t))

VaR <- (mean(FT) - quantile(FT, 0.01))*1250000


############ 4. Option Pricing using Monte Carlo ############
S0 <- 50
K <- 52
tau <- 0.5
sigma <- 0.2
r <- 0.02

#### 4.1 Euro Option ####
bs <- function(S0, K, T1, sigma, r, type){
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  if(type == "call"){
    return(S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2))
  }else if(type == "put"){
    return(exp(-r*T1)*K*pnorm(-d2) - S0*pnorm(-d1))
  }else{warning("type = call, or type = put")}
}

bs(S0, K, T1, sigma, r, "call")

MC.EuroOpt <- function(tau, type = "call"){
  S.vec <- rep(S0, 1000)
  for (i in 1:round(n*tau)){
    S.vec <- S.vec*exp(rnorm(1000, (r - 0.5*sigma^2)*1/n, sigma*sqrt(1/n)))
  }
  if(type == "call"){
    return(pmax(S.vec - K, 0))
  }else if(type == "put"){
    return(pmax(K - S.vec, 0))
  }
}

mean(MC.EuroOpt(tau))

#### 4.2 Asian Option ####
#Unlike a European call option, an Asian call option is path dependent in which the payoff is
#determined by the average price until maturity. For an Asian option to be exercised, you need to
#evaluate whether the average daily simulated price of the underlying asset exceeds the strike price.
#Specifically, you need to simulate the asset price over D days and determine whether the average
#is larger than K. If that¡¯s the case, then the payoff is given by the average price minus the strike
#price, while otherwise is zero.

MC.AsianOpt <- function(tau, type = "call"){
  S.mat <- matrix(S0, nrow = round(n*tau) + 1, ncol = 1000)
  for (i in 2:round(n*tau)){
    S.mat[i, ] <- S.mat[i - 1, ]*exp(rnorm(1000, (r - 0.5*sigma^2)*1/n, sigma*sqrt(1/n)))
  }
  if(type == "call"){
    return(pmax(sapply(S.mat, mean) - K, 0))
  }else if(type == "put"){
    return(pmax(K - sapply(S.mat, mean), 0))
  }
}

mean(MC.AsianOpt(tau))

#### 4.3 Knock-Out Barrier Option ####
#A knock-out barrier option is similar to a European option, whereas the main difference is that it
#also expires (gets knocked-out) if the price drops below a certain level denoted by L. For option
#3, if the price drops at any time below L, then the option is not valuable anymore and its payoff
#is zero. You may be able to find a closed form solution to price this option (see e.g., Hull (2003)).
#However, your main objective is to solve this using MC simulation. You may refer to the closed
#form solution to confirm the simulated price.

L <- 48

MC.KnockOut <- function(tau, L, type = "call"){
  S.mat <- matrix(S0, nrow = round(n*tau) + 1, ncol = 1000)
  for (i in 2:round(n*tau)){
    S.mat[i, ] <- S.mat[i - 1, ]*exp(rnorm(1000, (r - 0.5*sigma^2)*1/n, sigma*sqrt(1/n)))
    S.mat[i, ][S.mat[i, ] < L] <- 0
  }
  if(type == "call"){
    return(pmax(sapply(S.mat, mean) - K, 0))
  }else if(type == "put"){
    return(pmax(K - sapply(S.mat, mean), 0))
  }
}

mean(MC.KnockOut(tau, L))




