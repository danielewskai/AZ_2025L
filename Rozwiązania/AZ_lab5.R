# Zadanie 4.2 ------------------------------------------------------------------
n <- 100
beta <- c(0.5, 1)
x <- runif(n, 0, 1)
eps <- rnorm(n)
y <- beta[1] + beta[2]* x^2 + eps
m <- lm(y ~ x)
summary(m)
summary(m)$fstat[1]
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

# b)
n <-  c(20,50,100,200,300,400,500)
B <-  100

MOC1 <- numeric(length(n))
MOC2 <- numeric(length(n))
MOC3 <- numeric(length(n))

for (j in 1:length(n)) {
  moc1 <- numeric(B)
  moc2 <- numeric(B)
  moc3 <- numeric(B)
  for (i in 1:B){
    X <- cbind(1,matrix(rnorm(3*n[j]), ncol = 3))
    eps <-  rnorm(n[j], 0, 1)
    y <- X %*% beta + eps
    M <- lm(y ~ X-1)
    pvals <- summary(M)$coef[,4]
    moc1[i] <- ifelse(pvals[2]<0.05, 1, 0)
    moc2[i] <- ifelse(pvals[3]<0.05, 1, 0)
    moc3[i] <- ifelse(pvals[4]<0.05, 1, 0)
  }
  MOC1[j] <- mean(moc1)
  MOC2[j] <- mean(moc2)
  MOC3[j] <- mean(moc3)
}

plot(n, MOC1, type = "b", ylim = c(0,1))
lines(n, MOC2, type = "b", ylim = c(0,1), col = "red")
lines(n, MOC3, type = "b", ylim = c(0,1), col = "blue")

# c)
n <-  c(20,50,100,200,300,400,500)
B <-  100
beta <-  c(0.05, 0.05,  0.05, 0.05)

MOC <- numeric(length(n))

for (j in 1:length(n)){
  moc <- numeric(B)
  for (i in 1:B){
    X <- matrix(rnorm(3 * n[j]), ncol = 3)
    X1 <- cbind(1, X)
    eps <- rnorm(n[j])
    y <- X1 %*% beta + eps
    model <- lm(y ~ X)
    F_test <- summary(model)$fstat
    p_value <- 1 - pf(F_test[1], F_test[2], F_test[3])
    moc[i] <- ifelse(p_value < 0.05, 1, 0)
  }
  MOC[j] <- mean(moc)
}

plot(n, MOC, type = "b", ylim = c(0,1))

# Zadanie 5.1 ------------------------------------------------------------------

air <- read.table("./dane/airpollution.txt", header = T)

#a)
model <-  lm(Mortality ~ NOx, data = air)

plot(air$NOx, air$Mortality)
abline(model, col = "red")

plot(model,6) # różne cyfry przedstawiają różne wykresy

#x <- rnorm(1000)
#eps <- rnorm(1000)
#y <- 2*x + 3 + eps
#model_liniowy <- lm(y ~ x)

#plot(model_liniowy, 6)

# b)
model_log <-  lm(Mortality ~ log(NOx), data = air)

plot(log(air$NOx), air$Mortality)
abline(model_log, col = "red")

plot(model_log, 6)

# c)

# rezydua studentyzowane
rstud <- rstandard(model_log)
which(abs(rstud) > 2)

model_log_rstud <- lm(Mortality ~ log(NOx), data = air, subset = -c(29,37,47,49) )
plot(log(air$NOx), air$Mortality)
abline(model_log_rstud, col = "red")
abline(model_log, col = "blue")

plot(model_log_rstud)

summary(model_log)
summary(model_log_rstud)

rstud <- rstandard(model)
which(abs(rstud) > 2)
model_rstud <- lm(Mortality ~ NOx, data = air, subset = -c(5,37,49) )
summary(model)
summary(model_rstud)

# Zadanie 5.2 ------------------------------------------------------------------
phila <- read.table("./dane/phila.txt", header = T)
any(is.na(phila$HousePrice))
phila2 <- phila[-which(is.na(phila$CrimeRate)),]

model <- lm(HousePrice~CrimeRate, data = phila2)
plot(phila2$HousePrice~phila2$CrimeRate)
abline(model)

# rezydua studentyzowane modyfikowane
which(abs(rstudent(model))>2)

h <- hatvalues(model)
p <- 2
n <- nrow(phila2)
which(h>2*p/n)

# do dokończenia na następnych zajęciach