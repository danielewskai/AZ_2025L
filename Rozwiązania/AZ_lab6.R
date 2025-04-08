# Zadanie 5.2 ------------------------------------------------------------------
phila <- read.table("./dane/phila.txt", header = T)
any(is.na(phila$HousePrice))
phila2 <- phila[-which(is.na(phila$CrimeRate)),]
rownames(phila2) <- NULL

model <- lm(HousePrice~CrimeRate, data = phila2)
plot(phila2$HousePrice~phila2$CrimeRate)
abline(model)

# rezydua studentyzowane modyfikowane
which(abs(rstudent(model))>2)

h <- hatvalues(model)
p <- 2
n <- nrow(phila2)
which(h>2*p/n)

model2 <- lm(HousePrice~CrimeRate, data = phila2, subset = -c(63))
abline(model2, col = "red")

summary(model)
summary(model2)

plot(model,4)

cooks_dist <- rstandard(model)^2 * hatvalues(model)/(p*(1-hatvalues(model)))
which(cooks_dist > 4/(n-p-1))
cooks.distance(model)

# Zadanie 5.3 ------------------------------------------------------------------
cel <- read.table("./dane/cellular.txt", header = T)

plot(cel$Period, cel$Subscribers)
model <- lm(Subscribers~ Period, data = cel)
abline(model)
summary(model)
plot(model,1)

# b)
model1 <- lm(log(Subscribers)~ Period, data = cel)
model2 <- lm((Subscribers)^(1/2)~ Period, data = cel)
model3 <- lm((Subscribers)^(1/4)~ Period, data = cel)

plot(cel$Period, log(cel$Subscribers))
abline(model1)

plot(cel$Period, (cel$Subscribers)^(1/2))
abline(model2)

plot(cel$Period, (cel$Subscribers)^(1/4))
abline(model3)

plot(model3,1)

summary(model1)
summary(model2)
summary(model3)

# c)
library(MASS)
bc <- boxcox(Subscribers~Period, data = cel, lambda = seq(0, 1, 0.01))
lambda <- bc$x[which.max(bc$y)]
new_Sub <- (cel$Subscribers^lambda - 1)/lambda
plot(cel$Period,new_Sub)
model_bc <- lm(new_Sub ~ cel$Period)
abline(model_bc)

summary(model_bc)

# Zadanie 5.4 ------------------------------------------------------------------
saving <- read.table("./dane/savings.txt", header = T)
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
