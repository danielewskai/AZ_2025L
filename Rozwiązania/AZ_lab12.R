# Zadanie 8.5 ------------------------------------------------------------------ 
# Eksperyment symulacyjny ilustrujący bias-variance
library(glmnet)
set.seed(123)
p <- 10   # liczba predyktorów
n <- 100  # liczba obserwacji
iL <- 1000 # liczba powtórzeń
lambda_sim <- 100 # przyjmijmy ze jest to już lambda z glmnet

# Macierze na estymatory w każdej iteracji
beta <- rep(1, p)
beta_lm    <- matrix(0, nrow = p, ncol = iL)
beta_ridge <- matrix(0, nrow = p, ncol = iL)

for (i in seq_len(iL)) {
  # Losowanie danych
  X_sim <- matrix(rnorm(n * p), nrow = n)
  eps   <- rnorm(n)
  Y_sim <- X_sim %*% beta + eps
  # MNK bez interceptu
  beta_lm[, i]     <- lm(Y_sim ~ X_sim - 1)$coef
  # Ridge bez interceptu
  beta_ridge[, i] <- as.vector(coef(glmnet(X_sim, Y_sim, alpha = 0, intercept = FALSE, lambda = lambda_sim)))[-1]
}

# Obliczenie biasu: średnia różnica między estymatorem a prawdą
apply(apply(beta_lm,2,function(x){x-beta}),1,mean) # tutaj widzimy dość małe wartości - estymator nieobciążony
apply(apply(beta_ridge,2,function(x){x-beta}),1,mean) # natomiast tutaj są one zdecydowanie większe - estymator obciążony

# Sprawdzenie wariancji macierzy estymatorów
cov_lm    <- cov(t(beta_lm))
cov_ridge <- cov(t(beta_ridge))
# Czy różnica (cov_lm - cov_ridge) jest nieujemnie określona?
library(matrixcalc) # aby użyć funkcji do sprawdzania czy macierz jest nieujemnie określona
PSD_check <- is.positive.semi.definite(cov_lm - cov_ridge)
cat("Macierz cov_lm - cov_ridge jest pół-dodatnio określona? ", PSD_check, "\n")


# Zad 9.1 ----------------------------------------------------------------------

miasta <- read.table("./dane/Miasta.txt", header = T)

# a)
Miasta_std <- scale(miasta)
Miasta_std <- as.data.frame(Miasta_std)

# b)
PC_m <- princomp(~., data = Miasta_std[,1:2], cor = FALSE)

PC_m$loadings

plot(Miasta_std$Work, Miasta_std$Price, asp = 1)
abline(0, -1, col = "red")
abline(0, 1, col = "blue")

PC_m$scores

# c)
PC_m2 <- princomp(~., data = Miasta_std, cor = FALSE)
PC_m2$loadings

# d)
summary(PC_m2)

(PC_m2$sdev)^2/sum((PC_m2$sdev)^2)
cumsum((PC_m2$sdev)^2/sum((PC_m2$sdev)^2))

plot(PC_m2)

# e)
PC_m2$loadings
PC_m2$scores

# f)
wh <- which.max(PC_m2$scores[,1])
PC_m2$scores[wh,]

biplot(PC_m2, choices = 1:2)

# Zad 9.2 ----------------------------------------------------------------------
library(faraway)
meatspec

# a)
meat_train <- meatspec[1:172, ]
meat_test <- meatspec[173:nrow(meatspec), ]

m1 <- lm(fat~., data = meat_train)

rmse <- function(y_hat, y) {sqrt(mean((y_hat-y)^2))}
rmse(predict(m1, meat_test[,-101]), meat_test$fat)

# b)
m2 <- step(m1, direction = "backward", k = log(nrow(meat_train)))
rmse(predict(m2, meat_test[,-101]), meat_test$fat)

# c)
meat_train_std <- scale(meat_train[,-101])
means_tr <- attr(meat_train_std, "scaled:center")
stds_tr <- attr(meat_train_std, "scaled:scale")
meat_test_std <- scale(meat_test[,-101], center = means_tr, scale = stds_tr)

meat_train_std <- as.data.frame(meat_train_std)
PC_M <- princomp(~., data = meat_train_std, cor = FALSE)
PC_M$loadings
PC_M$scores

# d)
PC_M$loadings[,1]

# e)
summary(PC_M)
plot(1:20, PC_M$sdev[1:20])

D5_train <-  as.data.frame(cbind(PC_M$scores[,1:5], meat_train$fat))
colnames(D5_train)[6] <- "fat"

comp_test <-  meat_test_std%*%PC_M$loadings[,1:5]
D5_test <- as.data.frame(cbind(comp_test, meat_test$fat))
colnames(D5_test)[6] <- "fat"


m3 <- lm(fat~., data = D5_train)
rmse(predict(m3, D5_test[,-6]), D5_test$fat)


beta_pcr <- coef(m3)[-1]%*%t(PC_M$loadings[,1:5])/stds_tr
beta0_pcr <- coef(m3)[1] - sum(means_tr*beta_pcr)


