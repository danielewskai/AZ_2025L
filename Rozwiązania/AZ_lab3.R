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

SST <- sum((hills$time-mean(hills$time))^2)
SSR <- sum((model_dist$fitted.values-mean(hills$time))^2)
SSE <- sum((hills$time-model_dist$fitted.values)^2)

SST
SSR+ SSE

R2 <- SSR/SST


SST <- sum((hills$time-mean(hills$time))^2)
SSR <- sum((model_climb$fitted.values-mean(hills$time))^2)
SSE <- sum((hills$time-model_climb$fitted.values)^2)

SST
SSR+ SSE

R2 <- SSR/SST

summary(model_dist)$r.sq
summary(model_climb)$r.sq

#c)

model_dist$coefficients[1] + model_dist$coefficients[2] * 15
sum(model_dist$coef * c(1,15))
predict(model_dist, newdata = data.frame(dist = c(1,15,30)))

# Zadanie 2.3 ------------------------------------------------------------------
ans <- read.table("./dane/anscombe_quartet.txt", header = T)

plot(ans$X1, ans$Y1)
plot(ans$X2, ans$Y2)
plot(ans$X3, ans$Y3)
plot(ans$X4, ans$Y4)

# a)
m1 <- lm(Y1 ~ X1, data = ans)
m2 <- lm(Y2 ~ X2, data = ans)
m3 <- lm(Y3 ~ X3, data = ans)
m4 <- lm(Y4 ~ X4, data = ans)

# b)
coef(m1)
coef(m2)
coef(m3)
coef(m4)

summary(m1)$r.sq
summary(m2)$r.sq
summary(m3)$r.sq
summary(m4)$r.sq

cor(ans$X1, ans$Y1)^2
cor(ans$X2, ans$Y2)
cor(ans$X3, ans$Y3)
cor(ans$X4, ans$Y4)

# d
par(mfrow=c(2,2))
plot(ans$X1, ans$Y1)
abline(m1)
plot(ans$X2, ans$Y2)
abline(m2)
plot(ans$X3, ans$Y3)
abline(m3)
plot(ans$X4, ans$Y4)
abline(m4)
par(mfrow=c(1,1))
# na wykresach widzimy, ze model powinien byc dopasowany do pierwszej pary, 
# druga nie jest liniowa, trzecia ma obserwacje odstajaca a czwarta tylko dwie rozne wartosci na x-ie


# Zadanie 3.1 ------------------------------------------------------------------
rel <- read.table("./dane/realest.txt", header = T)

model <- lm(Price~., data = rel)

# a)
X <- as.matrix(cbind(1, rel[,-1]))
model.matrix(model)

# b)
y <- rel$Price
solve(t(X)%*%X)%*%t(X)%*%y
coef(model)

SST <- sum((y-mean(y))**2)
SSR <- sum((model$fitted.values-mean(y))^2)
SSE <- sum((y-model$fitted.values)^2)
sum(model$residuals^2)
residuals(model)

R2 <- SSR/SST
summary(model)$r.sq

# c)
coef(model)
# wspolczynniki modelu gdzie uzalezniamy Price od calej reszty, liczba sypialni obniza cene domu, gdzie
# inne zmienne pozostaja stale (dom z wieksza liczba sypialni jest tanszy niz z mniejsza liczba sypialni
# gdy w obydwu przypadkach powierzchnia domu jest taka sama)
# mamy taką samą powierzchnię przy większej liczbie pokoi 


lm(Price~Bedroom, data = rel)
# tu model tylko uzaleznjajacy Price od Bedroom, wowczas wspolczynnik przy Bedroom dodatni,
# bo za zwiekszaniem liczby sypialni idzie prawdopodobnie zwiekszanie powierzchni domu 
# i innych parametrow

# d)
predict(model, newdata = data.frame(Condition = 0, Bedroom = 3, Space = 1500, Room = 8, Lot = 40, Bathroom = 2, Garage = 1, Tax = 1000))
sum(model$coef * c(1, 3, 1500, 8, 40, 1000, 2, 1, 0))

# e)
n <- nrow(X)
p <- ncol(X)
SSE/(n-p)

summary(model)$sigma^2

# Zadanie 3.2 ------------------------------------------------------------------
# a)
# liczymy X'e = X'(Y-\hat{Y}) = X'(Y-X(X'X)^{-1}X'Y) = X'Y-X'X(X'X)^{-1}X'Y = X'Y-X'y=0
# pierwszy wiersz macierzy X', to wiersz jedynek ozn. (1') zatem 1'e = \sum_{i=1}^n e_i = 0 z wczesniejszej linijki

