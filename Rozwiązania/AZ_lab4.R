# Zadanie 3.2 ------------------------------------------------------------------
# b)
n <- 100
X <- cbind(1, matrix(rnorm(n*3), ncol = 3))
beta <- c(2, 0.5, 1, 0.7)
eps <- rnorm(n, mean = 0, sd = 10)
y <- X%*%beta + eps

model <- lm(y ~ X - 1)

X <- matrix(rnorm(n*3), ncol = 3)
beta <- c(2, 0.5, 1, 0.7)
eps <- rnorm(n, mean = 0, sd = 10)
y <- 2 + X%*%beta[-1] + eps

model <- lm(y ~ X)

sum(residuals(model))

# c)
k <- 1000
vars <- numeric(k)
for (i in 1:k){
  X <- matrix(rnorm(n*3), ncol = 3)
  beta <- c(2, 0.5, 1, 0.7)
  eps <- rnorm(n, mean = 0, sd = 10)
  y <- 2 + X%*%beta[-1] + eps
  
  model <- lm(y ~ X)
  
  vars[i] <- sum(residuals(model) * residuals(model))/(n-4)
}

vars
mean(vars)
#wartosci estymatorow wariancji moga sie roznic dla poszczegolnych prob, ale srednio daja to teoretyczne 100, ktore bylo podane
#w tresci zadania, co swiadczy o nieobciazonosci estymatora

# Zadanie 4.1 ------------------------------------------------------------------
air <-  read.table("./dane/airpollution.txt", header = T)
air$NOxPot <- NULL

# a)
model <- lm(Mortality ~ ., data = air)

# b)
X <- model.matrix(model)
# t(X) %*% X
# solve(t(X) %*% X)
p <- ncol(X)
n <- nrow(X)
QR <- qr(X)
R <- qr.R(QR)
XX_inv <- solve(R) %*% t(solve(R))
XX_inv_ii <- diag(XX_inv)[length(diag(XX_inv))]
s2 <- residuals(model) %*% residuals(model) / (n-p)
SE <- sqrt(s2 * XX_inv_ii)

t <- model$coefficients[length(model$coefficients)]/SE
2 * min(pt(t, df = n-p), 1-pt(t, df = n-p))
# zatem B_Nox = 0 bo p-value > 0.05, co oznacza, że zmienna Nox nie jest istotna

summary(model)

# c)
SSR <- sum((model$fitted.values-mean(air$Mortality))**2)
SSE <- as.vector(residuals(model) %*% residuals(model))

F_stat <- SSR*(n-p)/(SSE*(p-1))
1 - pf(F_stat, p-1, n-p)
# p-val < 0.05 tzn odrzucamy hipotezę zerową, czyli ktoras ze zmiennych w modelu jest istotna

summary(model)


# Zadanie 4.2 ------------------------------------------------------------------
n <-  1000
x1 <- runif(n, 0, 1)
eps <-rnorm(n)
y <- 0.5 + 1 * x1^2 + eps
plot(x1, y)
m <- lm(y ~ x1)
summary(m)$coef[2,3]
summary(m)$coef[2,4]
# p-val < 0.05 zatem odrzucamy hipotezę zerową, zmienna x1 jest istotna, ale
# model liniowy jest niewystarczający, gdyż nie uwzględnia kwadratowej zależności
summary(m)$fstatistic
summary(m)$coef[2,3]^2

# Zadanie 4.3 ------------------------------------------------------------------
n <- 100
B <- 100
beta <-  c(0.5, 1,  0.5, 0.05)
moc1 <- numeric(B)
moc2 <- numeric(B)
moc3 <- numeric(B)
for (i in 1:B){
  X <- cbind(1,matrix(rnorm(3*n), ncol = 3))
  eps <-  rnorm(n, 0, 1)
  y <- X %*% beta + eps
  M <- lm(y ~ X-1)
  pvals <- summary(M)$coef[,4]
  moc1[i] <- ifelse(pvals[2]<0.05, 1, 0)
  moc2[i] <- ifelse(pvals[3]<0.05, 1, 0)
  moc3[i] <- ifelse(pvals[4]<0.05, 1, 0)
}
mean(moc1)
mean(moc2)
mean(moc3)
