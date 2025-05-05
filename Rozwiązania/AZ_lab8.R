# Zadanie 6.2 ------------------------------------------------------------------
us <- read.table("./dane/uscrime.txt", header = T)

# a)
model <- lm(R~., data = us)
summary(model)

X <- model.matrix(model)[,-1]
max(abs(round(cor(X), 3)-diag(rep(1, times = ncol(X)))))
abs(round(cor(X), 3))==0.994

model2 <- lm(R~.-Ex0, data = us)
summary(model2)

pairs(X)

# b)
p <- ncol(X)-1
M_logiczna <- expand.grid(rep(list(c(TRUE, FALSE)), p-1))
col_names <- colnames(X)[-4]


lista_nazw <- apply(M_logiczna, 1, function(x){col_names[x]})
formulas <- lapply(lista_nazw, function(x){paste0("R~", paste0(x,collapse = "+"))})
formulas[[1]] <- "R~1"
formulas[[length(formulas)]] <- NULL

wyniki <- data.frame(formula_lm = numeric(length(formulas)),
                     AIC = numeric(length(formulas)),
                     BIC = numeric(length(formulas)),
                     Mallows = numeric(length(formulas)),
                     adjR2 = numeric(length(formulas)))


AIC(model2)
-2*logLik(model2)+2*(length(coef(model2))+1)
n <- nrow(X)
n*log(sum(residuals(model2)^2)/n)+2*length(coef(model2)+1)

BIC(model2)
-2*logLik(model2)+log(n)*(length(coef(model2))+1)
n*log(sum(residuals(model2)^2)/n)+log(n)*length(coef(model2)+1)
AIC(model2, k = log(n))

sigma2 <- summary(model2)$sigma^2

for (i in 1:length(formulas)){
  f <- as.formula(formulas[[i]])
  m <- lm(f, data = us)
  X <- model.matrix(m)
  pf <- ncol(X)
  SSE <- sum(residuals(m)^2)
  wyniki$formula_lm[i] <- formulas[[i]]
  wyniki$AIC[i] <-  AIC(m)
  wyniki$BIC[i] <- BIC(m)
  wyniki$Mallows[i] <- SSE + 2* pf * sigma2
  wyniki$adjR2[i] <-  1 - (1 - summary(m)$r.squared)*(n-1)/(n-pf)
}

wyniki$formula_lm[which.min(wyniki$AIC)]
wyniki$formula_lm[which.min(wyniki$BIC)]
wyniki$formula_lm[which.min(wyniki$Mallows)]
wyniki$formula_lm[which.max(wyniki$adjR2)]

# c)
m <- step(model2, direction = "backward", k = 2)
summary(m)

m_null <- lm(R~1, data = us)
step(m_null, direction = "forward", k = 2, scope = list(lower = m_null, upper = model2))

step(model2, direction = "both", k = 2, scope = list(lower = m_null, upper = model2))
step(m_null, direction = "both", k = 2, scope = list(lower = m_null, upper = model2))


m <- step(model2, direction = "backward", k = log(n))
summary(m)

m_null <- lm(R~1, data = us)
step(m_null, direction = "forward", k = log(n), scope = list(lower = m_null, upper = model2))

step(model2, direction = "both", k = log(n), scope = list(lower = m_null, upper = model2))
step(m_null, direction = "both", k = log(n), scope = list(lower = m_null, upper = model2))
