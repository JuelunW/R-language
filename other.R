library(quantmod)
library(PerformanceAnalytics)
library(lubridate)

################## Data ##################
# Download
sym_seq <- c("AAPL", "CSCO", "HON", "KO", "NKE", "WBA", 
         "AMGN", "CVX", "IBM", "MCD", "PG", "WMT", 
         "AXP", "DIS", "INTC", "MMM", "TRV", 
         "BA", "GS", "JNJ", "MRK", "UNH", 
         "CAT", "HD", "JPM", "MSFT", "VZ", 
         "SPY")

P_list <- list()
for (sym in sym_seq) {
  P <- get(getSymbols(sym, from = "1999-05-01", to = "2020-12-31"))
  P_list <- c(P_list,list(P))
}

# log daily return
lreturn <- function(x) {
  x <- x[,6]
  log(x/lag(x))
}

R <- Reduce(merge, lapply(P_list,lreturn))
R <- na.omit(R)

################## Performance Summary ##################
# 1.1 mean return, volatility, and SR
Mu <- apply(R,2,mean)*252
Sig <- apply(R,2,sd)*sqrt(252)
SR <- Mu/Sig

Mu_perf <- c(min(Mu[1:27]), mean(Mu[1:27]), max(Mu[1:27]))
Sig_perf <- c(min(Sig[1:27]), mean(Sig[1:27]), max(Sig[1:27]))
SR_perf <- c(min(SR[1:27]), mean(SR[1:27]), max(SR[1:27]))

perf <- cbind(Mu_perf, Sig_perf, SR_perf)
row.names(perf) <- c("Minimum", "Mean", "Maximum")
write.csv(perf, "1.1 perf.csv")

# 1.2 Plot mean returns against volatilities
png("1.2 perf_each.png")
plot(Mu, Sig)
dev.off()

# 1.3 CAPM
CAPM <- table.CAPM(R,R$SPY.Adjusted)

alpha <- c(min(CAPM[1, 1:27]), sum(CAPM[1, 1:27])/27, max(CAPM[1, 1:27]))
beta <- c(min(CAPM[2, 1:27]), sum(CAPM[2, 1:27])/27, max(CAPM[2, 1:27]))
TR <- c(min(CAPM[12, 1:27]), sum(CAPM[12, 1:27])/27, max(CAPM[12, 1:27]))
Error <- c(min(CAPM[9, 1:27]), sum(CAPM[9, 1:27])/27, max(CAPM[9, 1:27]))
IR <- c(min(CAPM[11, 1:27]), sum(CAPM[11, 1:27])/27, max(CAPM[11, 1:27]))

CAPM_perf <- cbind(alpha, beta, TR, Error, IR)
row.names(CAPM_perf) <- c("Minimum", "Mean", "Maximum")
write.csv(CAPM_perf, "1.3 CAPM_perf.csv")

# 1.4 Plot CAPM
for (i in 1:27) {
  png(paste("1.4 CAPM_", names(R)[i], ".png"))
  plot(coredata(R$SPY.Adjusted), coredata(R[, i]),
       xlab = "SPY.Adjusted", ylab = names(R)[i],
       ylim = c(-0.25, 0.25))
  abline(CAPM[1, i], CAPM[2, i])
  dev.off()
}

################## Back-Testing ##################
# 2.1 IN sample & OUT sample
R_IN <- R["2017/2018"]
head(index(R_IN), 1)
tail(index(R_IN), 1)

R_OUT <- R["2019/2020"]
head(index(R_OUT), 1)
tail(index(R_OUT), 1)

# 2.2 the weights for each portfolio
Mu_IN <- apply(R_IN,2,mean)*252
Sig_IN <- apply(R_IN,2,sd)*sqrt(252)
SR_IN <- Mu_IN /Sig_IN

w_1 <- c()
for (i in 1:27) {
  w_1 <-  c(w_1, (1 / Sig_IN[i]^2) / sum(1 / Sig_IN[1:27]^2))
}

w_2 <- c()
for (i in 1:27) {
  w_2 <-  c(w_2, SR_IN[i] / sum(SR_IN[1:27]))
}

w_3 <- rep(1/27, 27)

wtable <- data.frame(row.names = sym_seq[1:27], w_1, w_2, w_3)
write.csv(wtable, file = "2.2 wtable.csv")

# 2.3.1 the cumulative return of each portfolio with respect to the SPY
R_p1 <- R_OUT[, 1:27] %*% w_1
R_p2 <- R_OUT[, 1:27] %*% w_2
R_p3 <- R_OUT[, 1:27] %*% w_3

png("2.3.1 CumReturn_Portfolio_1.png")

rownames(R_p1) <- as.character(index(R_OUT))
R_p1 <- as.xts(R_p1)
R_1 <- cbind(R_p1, coredata(R_OUT$SPY.Adjusted))
chart.CumReturns(R_1, legend.loc = "topleft")
charts.PerformanceSummary(R_1)

dev.off()

png("2.3.1 CumReturn_Portfolio_2.png")

rownames(R_p2) <- as.character(index(R_OUT))
R_p2 <- as.xts(R_p2)
R_2 <- cbind(R_p2, coredata(R_OUT$SPY.Adjusted))
chart.CumReturns(R_2, legend.loc = "topleft")
charts.PerformanceSummary(R_2)

dev.off()

png("2.3.1 CumReturn_Portfolio_3.png")

rownames(R_p3) <- as.character(index(R_OUT))
R_p3 <- as.xts(R_p3)
R_3 <- cbind(R_p3, coredata(R_OUT$SPY.Adjusted))
chart.CumReturns(R_3, legend.loc = "topleft")
charts.PerformanceSummary(R_3)

dev.off()

# 2.3.2 SR, beta, & alpha for each portfolio
Mu_p1 <- mean(R_p1) * 252
Mu_p2 <- mean(R_p2) * 252
Mu_p3 <- mean(R_p3) * 252
Sig_p1 <- sd(R_p1)*sqrt(252)
Sig_p2 <- sd(R_p2)*sqrt(252)
Sig_p3 <- sd(R_p3)*sqrt(252)
SR_p1 <- Mu_p1/Sig_p1
SR_p2 <- Mu_p2/Sig_p2
SR_p3 <- Mu_p3/Sig_p3

CAPM_p1 <- table.CAPM(R_p1, R_1$SPY.Adjusted)
CAPM_p2 <- table.CAPM(R_p2, R_2$SPY.Adjusted)
CAPM_p3 <- table.CAPM(R_p3, R_3$SPY.Adjusted)

SR_p <- c(SR_p1, SR_p2, SR_p3)
beta <- c(CAPM_p1[2, 1], CAPM_p2[2, 1], CAPM_p3[2, 1])
alpha <- c(CAPM_p1[1, 1], CAPM_p2[1, 1], CAPM_p3[1, 1])
CAPM_p <- data.frame(SR_p, beta, alpha)
rownames(CAPM_p) <- c("Portfolio_1", "Portfolio_2", "Portfolio_3")
write.csv(CAPM_p, "2.3.2 CAPM_portfolios.csv")

# 2.3.3 plot mean return against volatilities & CAPM
Mu_p <- c(Mu_p1, Mu_p2, Mu_p3)
Sig_p <- c(Sig_p1, Sig_p2, Sig_p3)
png("2.3.3 perf_portfs.png")
plot(Sig_p, Mu_p)
dev.off()

png("2.3.3 CAPM_Portfolios.png")
par(mfrow = c(3, 1))
plot(coredata(R_OUT$SPY.Adjusted), coredata(R_p1),
       xlab = "SPY.Adjusted", ylab = names(R_p1),
       ylim = c(-0.15, 0.15))
abline(CAPM_p[1, 3], CAPM_p[1, 2])

plot(coredata(R_OUT$SPY.Adjusted), coredata(R_p2),
     xlab = "SPY.Adjusted", ylab = names(R_p2),
     ylim = c(-0.15, 0.15))
abline(CAPM_p[2, 3], CAPM_p[2, 2])

plot(coredata(R_OUT$SPY.Adjusted), coredata(R_p3),
     xlab = "SPY.Adjusted", ylab = names(R_p3),
     ylim = c(-0.15, 0.15))
abline(CAPM_p[3, 3], CAPM_p[3, 2])
dev.off()

################## Value at Risk and Stress Testing ##################
# 4.1.1 calibrating
dt <- 1/252
sig_hat_p1 <- sd(R_p1)/sqrt(dt)
mu_hat_p1 <- mean(R_p1)/dt + (sig_hat_p1^2)/2
p1 <- c(mu_hat_p1, sig_hat_p1)

sig_hat_p2 <- sd(R_p2)/sqrt(dt)
mu_hat_p2 <- mean(R_p2)/dt + (sig_hat_p2^2)/2
p2 <- c(mu_hat_p2, sig_hat_p2)

sig_hat_p3 <- sd(R_p3)/sqrt(dt)
mu_hat_p3 <- mean(R_p3)/dt + (sig_hat_p3^2)/2
p3 <- c(mu_hat_p3, sig_hat_p3)

calibration <- cbind(p1, p2, p3)
rownames(calibration) <- c("Mu_hat", "Sig_hat")
write.csv(calibration, "4.1.1 calibration.csv")

# 4.1.2  simulation
F0 <- 100
sim_gbm <- function(p) {  
  if(p == "p1"){
    m <- mu_hat_p1
    s <- sig_hat_p1
  } else if(p == "p2"){
    m <- mu_hat_p2
    s <- sig_hat_p2
  } else if(p == "p3"){
    m <- mu_hat_p3
    s <- sig_hat_p3
  }else{print("please enter p1, p2, or p3")}
  F_sim <- c()
  for (n in 1:1/dt) {
    dR_seq <- rnorm(n, (m - s^2/2) * dt,  s * sqrt(dt))
    F_sim <- c(F_sim, F0*exp(cumsum(dR_seq)))
  }
  return(F_sim)
}

N <- 1000
sim_p1 <- c()
for (i in 1:N) {
  sim_p1 <- cbind(sim_p1, sim_gbm("p1"))
}

sim_p2 <- c()
for (i in 1:N) {
  sim_p2 <- cbind(sim_p2, sim_gbm("p2"))
}

sim_p3 <- c()
for (i in 1:N) {
  sim_p3 <- cbind(sim_p3, sim_gbm("p3"))
}
png("4.1.2 distributions.png")
par(mfrow = c(3, 1))
hist(sim_p1[nrow(sim_p1), ])
hist(sim_p2[nrow(sim_p2), ])
hist(sim_p3[nrow(sim_p3), ])
dev.off()

# 4.1.3 F1
s_1 <- sim_p1[nrow(sim_p1), ]
mean(s_1)
var(s_1)

s_2 <- sim_p2[nrow(sim_p2), ]
mean(s_2)
var(s_2)

s_3 <- sim_p3[nrow(sim_p3), ]
mean(s_3)
var(s_3)

# 4.1.4 VaR
VaR <- function(s, c = 0.05){
  v <- mean(s) - quantile(s,c)
  return(v)
}

VaR_p <- sapply(list(s_1, s_2, s_3), FUN = VaR)
write.csv(VaR_p, "4.1.4 VaR_p.csv")

plot(density(s_1))
plot(density(s_2))
plot(density(s_3))

# 4.2
a <- 0.1
port1_beta <- cov(R_p1, R_OUT$SPY.Adjusted) / var(R_OUT$SPY.Adjusted)
port2_beta <- cov(R_p2, R_OUT$SPY.Adjusted) / var(R_OUT$SPY.Adjusted)
port3_beta <- cov(R_p3, R_OUT$SPY.Adjusted) / var(R_OUT$SPY.Adjusted)

market_Volatility <- sd(R_OUT$SPY.Adjusted)

port1_volatility <- port1_beta * market_Volatility * (1 + a)
port2_volatility <- port2_beta * market_Volatility * (1 + a)
port3_volatility <- port3_beta * market_Volatility * (1 + a)

VaR_port1_MR <- port1_volatility * sqrt(252) * qnorm(.95)
VaR_port2_MR <- port2_volatility * sqrt(252) * qnorm(.95)
VaR_port3_MR <- port3_volatility * sqrt(252) * qnorm(.95)
