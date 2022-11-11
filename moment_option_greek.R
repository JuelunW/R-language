# 1.1 ####
library(quantmod)

getSymbols("SPY",from = "2019-01-01", auto.assign = TRUE)

# 1.2 ####
return <- na.omit(diff(log(SPY[, 6])))

# 1.3 ####
plot(return, col = "red")


# 2.1 ####
skewness <- function(x, adjusted){
  n <- length(x)
  if(adjusted == "TRUE"){
    m3 <- sum((x - mean(x))^3) / (n - 1)
    m2 <- sum((x - mean(x))^2) / (n - 1)
  } else if(adjusted == "FALSE"){
    m3 <- sum((x - mean(x))^3) / n
    m2 <- sum((x - mean(x))^2) / n
  }
  m3_tilde <- m3 / m2^(3/2)
  m3_hat <- sqrt(n * (n - 1)) / (n - 2) * m3_tilde
  return(m3_hat)
}

kurtosis <- function(x, adjusted){
  n <- length(x)
  if(adjusted == "TRUE"){
    m4 <- sum((x - mean(x))^4) / (n - 1)
    m2 <- sum((x - mean(x))^2) / (n - 1)
  } else if(adjusted == "FALSE"){
    m4 <- sum((x - mean(x))^4) / n
    m2 <- sum((x - mean(x))^2) / n
  }
  m4_tilde <- m4 / m2^(4/2)
  m4_hat <- (n - 1)/((n - 2)*(n - 3))*((n + 1)*m4_tilde - 3*(n - 1)) + 3
  return(m4_hat)
}

a <- skewness(coredata(return), "FALSE")
b <- skewness(coredata(return), "TRUE")

c <- kurtosis(coredata(return), "FALSE")
d <- kurtosis(coredata(return), "TRUE")

# 2.2 ####
table_2.2 <- data.frame(SPY.skewness = c(a, b), SPY.kurtosis = c(c, d))
rownames(table_2.2) <- c("Unadjusted", "Adjusted")


# 3.1 ####
SPY.options <- getOptionChain("SPY", NULL)

# 3.2 ####
for (i in 1:length(names(SPY.options))) {
  SPY.options[[i]]$calls$Price <- 0.5 * (SPY.options[[i]]$calls$Bid + SPY.options[[i]]$calls$Ask)
  SPY.options[[i]]$puts$Price <- 0.5 * (SPY.options[[i]]$puts$Bid + SPY.options[[i]]$puts$Ask)
}

# 3.3 ####
bs <- function(S0, K, T1, sigma, r, type){
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  d2 <- d1 - sigma*sqrt(T1)
  if(type == "call"){
    return(S0*pnorm(d1) - exp(-r*T1)*K*pnorm(d2))
  }else if(type == "put"){
    return(exp(-r*T1)*K*pnorm(-d2) - S0*pnorm(-d1))
  }else{warning("type = call, or type = put")}
}

Vega <- function(S0, K, T1, sigma, r){
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  sqrt(T1)*S0*dnorm(d1)
}

NR <- function(f, df, x0, tol = 0.001, N.max = 1000){
  for (n in 1:N.max){
    x1 <- x0 - f(x0)/df(x0)
    if(abs(x1 - x0) < tol){
      break
    }
    x0 <- x1
  }
  return(x1)
}


implied.vol <- function(S0 = getQuote("SPY")$Last, K, T1, r = 0.01, P, type){
  price.diff <- function(sigma) bs(S0, K, T1, sigma, r, type) - P
  dprice.diff <- function(sigma) Vega(S0, K, T1, sigma, r)
  return(NR(price.diff, dprice.diff, 1))
}


implied.vol <- function(S0 = getQuote("SPY")$Last, K, T1, r = 0.01, P, type){
  price.diff <- function(sigma) bs(S0, K, T1, sigma, r, type) - P
  return(uniroot(price.diff,c(-10^7, 10^7)))
}

T1 <- as.numeric(as.Date(names(SPY.options), "%b.%d.%Y") - Sys.Date())/365
sigma <- sd(return)*252

for (i in 1:length(names(SPY.options))) {
  mm <- nrow(SPY.options[[i]]$calls)
  SPY.options[[i]]$calls$ImpliedVol <- rep(NA, mm)
  nn <- nrow(SPY.options[[i]]$puts)
  SPY.options[[i]]$puts$ImpliedVol <- rep(NA, nn)
  for (j in 1:mm) {
    SPY.options[[i]]$calls$ImpliedVol[j] <- implied.vol(K = SPY.options[[i]]$calls$Strike[j], T1 = T1[i], P = SPY.options[[i]]$calls$Price[j], type = "call")$root
  }
  for (j in 1:nn) {
    SPY.options[[i]]$puts$ImpliedVol[j] <- implied.vol(K = SPY.options[[i]]$puts$Strike[j], T1 = T1[i], P = SPY.options[[i]]$puts$Price[j], type = "put")$root
  }
}

# 3.4 ####
plot(NA, xlim = c(250,500), ylim = c(0,2), xlab = "Strike",
     ylab = "ImpliedVol") # xlim: range of strike, ylim: range of vol

lines(SPY.options[[2]]$puts$Strike,
      SPY.options[[2]]$puts$ImpliedVol,col = "red")
lines(SPY.options[[3]]$puts$Strike,
      SPY.options[[3]]$puts$ImpliedVol,col = "green")
lines(SPY.options[[5]]$puts$Strike,
      SPY.options[[5]]$puts$ImpliedVol,col = "blue")

legend("topright", legend = names(SPY.options)[c(2, 3, 5)], fill = c("red","green","blue"))


# 3.5 ####
today <- format(Sys.Date(), "%Y_%m_%d")
Exp <- names(SPY.options)

for (i in 1:length(names(SPY.options))) {
  x <- data.frame(SPY.options[[i]]$calls[c("Strike", "Bid", "Ask", "Price")])
  rownames(x) <- rownames(SPY.options[[i]]$calls)
  colnames(x) <- c("Strike", "Bid", "Ask", "Price")
  #write.csv(x, file = paste("SPYdata", today, "Exp", Exp[i], "calls.csv"))
  
  #y <- data.frame(SPY.options[[i]]$puts[c("Strike", "Bid", "Ask", "Price", "ImpliedVol")])
  #rownames(y) <- rownames(SPY.options[[i]]$puts)
  #colnames(y) <- c("Strike", "Bid", "Ask", "Price", "ImpliedVol")
  #write.csv(y, file = paste("SPYdata", today, "Exp", Exp[i], "puts.csv"))
}

export <- function(n, Exp){
  x <- data.frame(n$calls[c("Strike", "Bid", "Ask", "Price")])
  rownames(x) <- rownames(n$calls)
  write.csv(x, file = paste("SPYdata", today, "Exp", Exp, "calls.csv"))
  
  y <- data.frame(n$puts[c("Strike", "Bid", "Ask", "Price", "ImpliedVol")])
  rownames(y) <- rownames(n$puts)
  write.csv(y, file = paste("SPYdata", today, "Exp", Exp, "puts.csv"))
}

mapply(export, VIX.options, expriation)


