
# 1.1
x <- seq(5, 35, by = 2)
length(x)

# 1.2
A <- matrix(x, nrow = 4, byrow = T)

# 1.3
eigen(A)$values

# 1.4
A[c(1, 2), c(1, 2)] <- 7

# 1.5
det(A)

# 1.6
solve(A)

# 1.7
b <- A[1, ]

# 1.8
y <- solve(A, b)

# 1.9
pmin(y, pi/2)

# 1.10
diag(1:10)


# 2
s <- c()
s[c(1, 2)] <- 1
for (n in 3:50) {
  s[n] = s[n-1] + s[n-2]
}
s[3]
s[50]


# 3
n <- 1:100
for (i in 1:100) {
  if (n[i] %% 3 == 0 && n[i] %% 5 == 0) {
    print(n[i])
  }
}


# 4
function_1 <- function(n){
  x <- 1:n
  y <- c()
  for (i in x) {
    if (x[i] %% 3 == 0 && x[i] %% 5 == 0) {
      y <- c(y, x[i])
    }
  } 
  return(y)
}
function_1(100)
function_1(200)

#5
function_2 <- function(a, b){
  i <- 0
  while(TRUE){
    i <- i + 1
    if (i %% a == 0 && i %% b == 0) {
      print(i)
      break
    }
  }
}
function_2(3, 5)
function_2(6, 10)

#6
jpm <- read.csv("JPM.csv")
sub_table <- data.frame(jpm$Open, jpm$High, jpm$Low, jpm$Close)
sapply(sub_table, mean)
