# Midterm
# Juelun Wang, 10470039
# 1.1
library(quantmod)

tickers <- c("JPM", "WFC")

get_data <- function(x) getSymbols(x,from = "2007-01-01", auto.assign = FALSE)
P_list <- lapply(tickers, get_data)

get_return <- function(n) diff(log(n[, 6]))
dailylogreturn <- lapply(P_list, get_return)

# 1.2 
lm1 <- lm(dailylogreturn[[1]] ~ dailylogreturn[[2]])
summary(lm1)

# 1.3
png("1.3 scatter JPM against WFC.png")
plot(coredata(dailylogreturn[[2]]), coredata(dailylogreturn[[1]]), type = "p",
     xlab = tickers[2], ylab = tickers[1])
abline(lm1, col = "red")
dev.off()

# 1.4
setwd("O:/FE 515 Intro to R")
cheese <- read.csv("cheese.csv", )
lm2 <- lm(taste ~ acetic + h2s + lactic, data = cheese)
summary(lm2)
lm3 <- lm(taste ~ 1, data = cheese)
summary(lm3)

# 1.5 
formula <- taste ~ acetic + h2s + lactic

step(lm3, formula, direction = "forward")

step(lm2, formula, direction = "backward")

step(lm3, formula, direction = "both")

ep(lm3, formula, direction = "both")
")

step(lm2, formula, direction = "backward")

step(lm3, formula, direction = "both")
direction = "both")
h")
")
direction = "both")
h")
