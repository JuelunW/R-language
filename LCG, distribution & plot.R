
# 1.1
seed <- 1 
lcg <- function(n){
  m <- 244944
  a <- 1597
  b <- 51749
  x <- rep(NA, n)
  x[1] <- (a * seed + b) %% m
  for(i in 1:(n-1)){
    x[i + 1] <- (a * x[i] + b) %% m
  }
  seed <<- x[n]
  return(x/m)
}

# 1.2
n <- 10000
U <- lcg(n)
chi <- qchisq(U, 10)

# 1.3
hist(chi, 40)

# 2
X <- lcg(n)
Y <- lcg(n)
Z <- lcg(n)
n_red <- sum(X^2 + Y^2 + Z^2 <= 1)
vol <- n_red/n
(sphere <- 8 * vol)

# 3.1
jpm <- read.csv("JPM.csv")
jpm$Date <- as.Date(jpm$Date)
par(mfcol = c(2, 2))

# 3.2
Sys.setlocale("LC_TIME", "English")
plot(jpm$Adj.Close, jpm$Date, type = "l", col = "red", main = "JPM",
     xlab = "Date", ylab = "Adjusted Close Price")

# 3.3
plot(jpm$Close, jpm$Open, type = "p", xlab = "Open Price", ylab = "Close Price")

# 3.4
barplot(table(cut(jpm$Adj.Close, breaks = 4)))

# 3.5
boxplot(jpm$Volume ~ cut(jpm$Adj.Close, breaks = 4))
