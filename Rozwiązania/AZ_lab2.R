# Zadanie 2.1 ------------------------------------------------------------------
x <- seq(0,10,0.1)
eps <- rnorm(length(x), mean = 0, sd = 3)
y <- x + eps

plot(x,y)

# a)
sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x,y)

# b)
b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
b0 <- mean(y) - mean(x)*b1

model <- lm(y ~ x)

# c)
plot(x,y)
abline(b0, b1, col = "red")

plot(x,y)
abline(model$coefficients, col = "red")

plot(x,y)
abline(model, col = "red")

# d)
x <- seq(0,10,0.1)
eps <- rnorm(length(x), mean = 0, sd = 0.5)
y <- x + eps

plot(x,y)

sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x,y)

b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
b0 <- mean(y) - mean(x)*b1

model <- lm(y ~ x)

plot(x,y)
abline(b0, b1, col = "red")

plot(x,y)
abline(model$coefficients, col = "red")

plot(x,y)
abline(model, col = "red")



x <- seq(0,10,0.1)
eps <- rnorm(length(x), mean = 0, sd = 5)
y <- x + eps

plot(x,y)

sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x,y)

b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))**2)
b0 <- mean(y) - mean(x)*b1

model <- lm(y ~ x)

plot(x,y)
abline(b0, b1, col = "red")

plot(x,y)
abline(model$coefficients, col = "red")

plot(x,y)
abline(model, col = "red")

# Zadanie 2.4 ------------------------------------------------------------------
# a)
n <- 100
beta <- c(0.5, 1, 0.5, 0.75)
X <- cbind(1,matrix(rnorm(3*n), ncol = 3))
eps <- rnorm(n)
y <- X %*% beta + eps
# b)
X
# c)
QR <- qr(X)
R <- qr.R(QR)
Q <- qr.Q(QR)
Q %*% R
t(Q) %*% Q

# d)

#\hat{\beta}=(X'X)^{-1}X'y
#\hat{\beta}=(R'Q'QR)^{-1}R'Q'y
#\hat{\beta}=(R'R)^{-1}R'Q'y
#\hat{\beta}=R^{-1}R'^{-1}R'Q'y
#\hat{\beta}=R^{-1}Q'y
#R\hat{\beta}=Q'y

# e)
solve(R, t(Q)%*%y)

method1 <- numeric(1000)
method2 <- numeric(1000)
method3 <- numeric(1000)
for (i in 1:1000){
  start_time <- Sys.time()
  solve(t(X) %*% X) %*% t(X) %*% y
  end_time <- Sys.time()
  method1[i] <- end_time - start_time
  
  start_time <- Sys.time()
  solve(t(X) %*% X, t(X) %*% y)
  end_time <- Sys.time()
  method2[i] <- end_time - start_time
  
  start_time <- Sys.time()
  solve(R, t(Q)%*%y)
  end_time <- Sys.time()
  method3[i] <- end_time - start_time
}
mean(method1)
mean(method2)
mean(method3)

# Zadanie 2.2 ------------------------------------------------------------------
library(MASS)
hills
par(mfrow=c(1,2))
plot(hills$dist, hills$time)
plot(hills$time ~ hills$climb)
par(mfrow=c(1,1))

cor(hills$dist, hills$time)
cor(hills$climb, hills$time)


# b)
model_dist <- lm(time ~ dist, data = hills)
model_climb <- lm(time ~ climb, data = hills)

par(mfrow=c(1,2))
plot(hills$dist, hills$time)
abline(model_dist, col = "red")
plot(hills$time ~ hills$climb)
abline(model_climb, col = "red")
par(mfrow=c(1,1))

summary(model_dist)
