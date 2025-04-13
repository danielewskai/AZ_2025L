# Zadanie 5.4 ------------------------------------------------------------------
saving <- read.table("./dane/savings.txt", header = T)

# a)
model <- lm(Savings~. - Country, data = saving)

which(abs(rstudent(model))>2)
X <- model.matrix(model)
which(hatvalues(model)>2*ncol(X)/nrow(X))
plot(model, 4)
cd <- cooks.distance(model)
barplot(cd)
which.max(cd)

model2 <- lm(Savings~. - Country, data = saving, subset = -c(49))

summary(model)
summary(model2)

# b)
model_dpi <- lm(dpi ~ . - Savings - Country, data = saving, subset = -c(49))
model_bez_dpi <- lm(Savings ~ . - dpi - Country, data = saving, subset = -c(49))
plot(residuals(model_dpi), residuals(model_bez_dpi))
abline(lm(residuals(model_bez_dpi)~residuals(model_dpi)))

model_ddpi <- lm(ddpi ~ . - Savings - Country, data = saving, subset = -c(49))
model_bez_ddpi <- lm(Savings ~ . - ddpi - Country, data = saving, subset = -c(49))
plot(residuals(model_ddpi), residuals(model_bez_ddpi))
abline(lm(residuals(model_bez_ddpi)~residuals(model_ddpi)))

# c)
cor(saving$Pop15[-49],saving$Pop75[-49])
summary(model2, cor = T)

# d)
coef(model2)
plot(saving$Pop15[-49],saving$Pop15[-49]*coef(model2)[2]+residuals(model2))
abline(lm(saving$Pop15[-49]*coef(model2)[2]+residuals(model2)~saving$Pop15[-49]))

m3 <- lm(Savings~.-Country, data = saving[-49,], subset = Pop15<35)
m4 <- lm(Savings~.-Country, data = saving[-49,], subset = Pop15>35)

saving <- saving[-49,]
plot(saving$Pop15[saving$Pop15<35],saving$Pop15[saving$Pop15<35]*coef(m3)[2]+residuals(m3))
abline(lm(saving$Pop15[saving$Pop15<35]*coef(m3)[2]+residuals(m3)~saving$Pop15[saving$Pop15<35]))

library(faraway)
prplot(model2, 1)

# Zadanie 5.5 ------------------------------------------------------------------

# a)
n <-  30
x1 <- runif(n,0,10)
x2 <- runif(n,10,20)
x3 <- runif(n,20,30)

eps1 <- rnorm(n,0,1)
eps2 <- rnorm(n,0,3)
eps3 <- rnorm(n,0,5)

x <- c(x1,x2,x3)
eps <- c(eps1,eps2,eps3)
y <- x+eps

plot(x,y)
model <- lm(y~x)
abline(model)
# abline(model2, col = "red")

# b)
plot(1:(3*n), rstudent(model), xlab = "Index")
plot(model,3)

# c)
res <- residuals(model)
sigma2 <- lm(abs(res)~model$fitted.values)$fitted.values^2
weights <- 1/sigma2
model2 <- lm(y~x, weights = weights) 
plot(1:(3*n), rstudent(model2), xlab = "Index")

# d)
x <- c(x,5)
y <- c(y,10)

plot(x,y)
model <- lm(y~x)
abline(model)
#abline(model2, col = "red")

plot(1:(3*n+1), rstudent(model), xlab = "Index")
plot(model,1)

res <- residuals(model)
sigma2 <- lm(abs(res)~model$fitted.values)$fitted.values^2
weights <- 1/sigma2
model2 <- lm(y~x, weights = weights) 
plot(1:(3*n+1), rstudent(model2), xlab = "Index")


abs(rstudent(model))>2
abs(rstudent(model2))>2

# Zadanie 6.1 ------------------------------------------------------------------
library(MASS)
longley

# a)
model <- lm(Employed~., data = longley)

# b)
cor(longley[,-ncol(longley)])
pairs(X)
X <- as.matrix(longley[,-ncol(longley)]) 
R2 <- numeric(ncol(X))
for (i in 1:(ncol(X))) {
  m <- lm(X[,i]~X[,-i], data = as.data.frame(X))
  R2[i] <- summary(m)$r.squared
}  
R2
VIF <- 1/(1-R2)  
VIF

