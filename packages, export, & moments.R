
# 1.1
install.packages("quantmod")
library(quantmod)
VIX.options <- getOptionChain("^VIX", NULL)

# 1.2
latest <- getQuote("^VIX")

# 1.3
for (i in 1:length(names(VIX.options))) {
  VIX.options[[i]]$calls$Price <- 0.5 * (VIX.options[[i]]$calls$Bid + VIX.options[[i]]$calls$Ask)
  VIX.options[[i]]$puts$Price <- 0.5 * (VIX.options[[i]]$puts$Bid + VIX.options[[i]]$puts$Ask)
}

# 1.4
for (i in 1:length(names(VIX.options))) {
  VIX.options[[i]]$calls$In_The_Money <- VIX.options[[i]]$calls$Strike < latest$Last
  VIX.options[[i]]$puts$In_The_Money <- VIX.options[[i]]$puts$Strike > latest$Last
}

# 1.5 
setwd("O:/FE 515 Intro to R/HW")
(today <- format(Sys.Date(), "%Y_%m_%d"))
expriation <- format(as.Date(names(VIX.options), "%b.%d.%Y"), "%Y_%m_%d")
for (i in 1:length(names(VIX.options))) {
  x <- data.frame(VIX.options[[i]]$calls[c("Strike", "Bid", "Ask", "Price", "In_The_Money")])
  rownames(x) <- rownames(VIX.options[[i]]$calls)
  colnames(x) <- c("Strike", "Bid", "Ask", "Price", "In_The_Money")
  write.csv(x, file = paste("VIXdata", today, "Exp", expriation[i], "calls.csv"))
  
  y <- data.frame(VIX.options[[i]]$puts[c("Strike", "Bid", "Ask", "Price", "In_The_Money")])
  rownames(y) <- rownames(VIX.options[[i]]$puts)
  write.csv(y, file = paste("VIXdata", today, "Exp", expriation[i], "puts.csv"))
}

ff <- function(options, expriation) {
  x <- data.frame(options$calls[c("Strike", "Bid", "Ask", "Price", "In_The_Money")])
  rownames(x) <- rownames(options$calls)
  write.csv(x, file = paste("VIXdata", today, "Exp", expriation, "calls.csv"))
  
  y <- data.frame(options$puts[c("Strike", "Bid", "Ask", "Price", "In_The_Money")])
  rownames(y) <- rownames(options$puts)
  write.csv(y, file = paste("VIXdata", today, "Exp", expriation, "puts.csv"))
}

mapply(ff, VIX.options, expriation)

# 2.1
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

# 2.2
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

# 2.3
getSymbols("SPY", from = "2012-01-01", to = "2013-12-31")
R <- periodReturn(SPY$SPY.Adjusted, "daily", type = "log")

# 2.4
skewness(coredata(R), "TRUE")
skewness(coredata(R), "FALSE")

# 2.5
kurtosis(coredata(R), "TRUE")
kurtosis(coredata(R), "FALSE")
 2.5
kurtosis(coredata(R), "TRUE")
kurtosis(coredata(R), "FALSE")"log")

# 2.4
skewness(coredata(R), "TRUE")
skewness(coredata(R), "FALSE")

# 2.5
kurtosis(coredata(R), "TRUE")
kurtosis(coredata(R), "FALSE")(coredata(R), "FALSE")

# 2.5
kurtosis(coredata(R), "TRUE")
kurtosis(coredata(R), "FALSE")ALSE")
