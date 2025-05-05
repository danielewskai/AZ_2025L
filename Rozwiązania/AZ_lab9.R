# Zadanie 6.3 ------------------------------------------------------------------
L <- 50
N <- c(25,50,75,100,125,150,175,200)
p <-  9
prob_AIC <-  numeric(L)
prob_BIC <-  numeric(L)
probsy_AIC <- numeric(length(N))
probsy_BIC <- numeric(length(N))
for (j in 1:length(N)){
  for (i in 1:L) {
    X <- matrix(rnorm(N[j]*p), ncol = p)
    eps <- rnorm(N[j])
    beta <- c(1,0.5,0.2,0,0,0,0,0,0)
    y <-  X%*%beta + eps
    d <- data.frame(X,y)
    m <- lm(y~.-1,data = d)
    mAIC <- step(m, direction = "backward", trace = FALSE, k = 2)
    mBIC <- step(m, direction = "backward", trace = FALSE, k = log(N[j]))
    prob_AIC[i] <- setequal(names(mAIC$coef), c("X1","X2","X3"))
    prob_BIC[i] <- setequal(names(mBIC$coef), c("X1","X2","X3"))
  }
  probsy_AIC[j] <- mean(prob_AIC)
  probsy_BIC[j] <- mean(prob_BIC)
}

plot(N, probsy_AIC, type = "b", col = "red", ylim = c(0,1))
lines(N, probsy_BIC, type = "b", col = "blue")

# Zadanie 6.2 ------------------------------------------------------------------
uscrime <- read.table("./dane/uscrime.txt", header = T)

# a)
model <- lm(R~., data = uscrime)
summary(model)
X <- model.matrix(model)[,-1]
pairs(X)

round(cor(X),3)
max(abs(round(cor(X),3)-diag(rep(1, times = ncol(X)))))
abs(round(cor(X),3)) == 0.994

model2 <- lm(R~.-Ex0, data = uscrime)
summary(model2)

# b)
AIC(model2)
-2*logLik(model2)+2*(ncol(X)+1)
n <- nrow(X)
SSE <- sum(residuals(model2)^2)
p <- ncol(X) +1
n*log(SSE/n)+2*p 


BIC(model2)
AIC(model2, k = log(n))
-2*logLik(model2)+log(n)*(ncol(X)+1)
n*log(SSE/n)+log(n)*p


expand.grid(rep(list(c(TRUE, FALSE)), p-2))
col_names <- colnames(X)[-4]
wybierz_nazwe <- function(x){return(col_names[x])}

lista_nazw <- apply(expand.grid(rep(list(c(TRUE, FALSE)), p-2)), 1, wybierz_nazwe)

formulas <- lapply(lista_nazw,function(x){return(paste0("R~",paste0(x,collapse = "+")))})
formulas[[1]] <- "R~1"

formulas[[length(formulas)]] <- NULL

n <- nrow(X)
sigma2 <- summary(model2)$sigma^2
wyniki <- data.frame(formula_lm = numeric(length(formulas)),
                     AIC = numeric(length(formulas)),
                     BIC = numeric(length(formulas)),
                     adjR2 = numeric(length(formulas)),
                     Mallows = numeric(length(formulas)))

for (i in 1:(length(formulas))){
  f <- as.formula(formulas[[i]])
  m <- lm(f, data = uscrime)
  X <- model.matrix(m)
  pf <- ncol(X)
  SSE <- sum(residuals(m)^2)
  wyniki$formula_lm[i] <-  formulas[[i]]
  wyniki$AIC[i] <- n*log(SSE/n)+2*pf
  wyniki$BIC[i] <- n*log(SSE/n)+log(n)*pf
  wyniki$Mallows[i] <- SSE+2*pf*sigma2
  wyniki$adjR2[i] <- 1-(1-summary(m)$r.squared)*(n-1)/(n-pf)
}
wyniki$formula_lm[which.min(wyniki$AIC)]
wyniki$formula_lm[which.min(wyniki$BIC)]
wyniki$formula_lm[which.min(wyniki$Mallows)]
wyniki$formula_lm[which.max(wyniki$adjR2)]

# c)
m <- step(model2, direction = "backward", k = 2)
summary(m)

m_null <- lm(R~1, data = uscrime)
step(m_null, direction = "forward", k = 2, scope = list(lower = m_null, upper = model2))

step(model2, direction = "both", k = 2, scope = list(lower = m_null, upper = model2))
step(m_null, direction = "both", k = 2, scope = list(lower = m_null, upper = model2))


m <- step(model2, direction = "backward", k = log(n))
summary(m)

m_null <- lm(R~1, data = uscrime)
step(m_null, direction = "forward", k = log(n), scope = list(lower = m_null, upper = model2))

step(model2, direction = "both", k = log(n), scope = list(lower = m_null, upper = model2))

# d)
rank(abs(summary(model2)$coef[,3]))
L <- list()
L[[1]]<-lm(R~1,data=uscrime)
L[[2]]<-lm(R~Ex1,data=uscrime)
L[[3]]<-lm(R~Ex1+X,data=uscrime)
L[[4]]<-lm(R~Ex1+X+Age,data=uscrime)
L[[5]]<-lm(R~Ex1+X+Age+Ed,data=uscrime)
L[[6]]<-lm(R~Ex1+X+Age+Ed+U2,data=uscrime)
L[[7]]<-lm(R~Ex1+X+Age+Ed+U2+U1,data=uscrime)
L[[8]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W,data=uscrime)
L[[9]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M,data=uscrime)
L[[10]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S,data=uscrime)
L[[11]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N,data=uscrime)
L[[12]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N+LF,data=uscrime)
L[[13]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N+LF+NW,data=uscrime)

which.min(lapply(L, AIC))
which.min(lapply(L, BIC))
sigma2 <- summary(L[[13]])$sigma^2
which.min(lapply(L, function(x){sum(residuals(x)^2)+2*length(x$coef)*sigma2}))
