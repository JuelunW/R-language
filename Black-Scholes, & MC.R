
# 1.1 
S0 <- 100
K <- 100
T1 <- 1
sigma <- 0.2
r <- 0.05

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
bs(S0, K, T1, sigma, r, "put")

# 1.2
Vega <- function(S0, K, T1, sigma, r){
  d1 <- (log(S0/K) + (r+0.5*sigma^2)*T1)/(sigma*sqrt(T1))
  sqrt(T1)*S0*dnorm(d1)
}

NR <- function(f, df, x0, tol = 0.001, N.max = 100){
  for (n in 1:N.max){
    x1 <- x0 - f(x0)/df(x0)
    if(abs(x1 - x0) < tol){
      break
    }
    x0 <- x1
  }
  return(x1)
}


implied.vol <- function(S0, K, T1, r, P, type){
  price.diff <- function(sigma) bs(S0, K, T1, sigma, r, type) - P
  dprice.diff <- function(sigma) Vega(S0, K, T1, sigma, r)
  return(NR(price.diff, dprice.diff, 0.25))
}

c <- implied.vol(S0, K, T1, r, 10, "call")
p <- implied.vol(S0, K, T1, r, 5, "put")
bs(S0, K, T1, c, r, "call")
bs(S0, K, T1, p, r, "put")

# 2
n <- 252 					
h <- T1/n

MC <- function(){
  m <- 10000
  S.mat <- NULL
  for (j in 1:m){
    S <- NULL
    S[1] <- S0
    for (i in 1:n){
      S[i+1] <- S[i] + r*S[i]*h + sigma*S[i]*rnorm(1)*sqrt(h)
    }
    S.mat <- cbind(S.mat,S)
  }
  exp(-r*T1)*mean(pmax(K - S.mat[n+1,], 0))
}

MC()
