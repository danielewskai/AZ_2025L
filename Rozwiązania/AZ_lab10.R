# Zad 7.1 ----------------------------------------------------------------------
trees
m1 <- lm(Volume~Girth, data = trees)
m2 <- lm(Volume~Girth+Height, data = trees)
m3 <- lm(Volume~Girth+Height+I(Height*Height), data = trees)

SSE1 <- sum(residuals(m1)^2)
SSE2 <- sum(residuals(m2)^2)
SSE3 <- sum(residuals(m3)^2)
p1 <- ncol(model.matrix(m1))
p2 <- length(m2$coef)
p3 <- length(m3$coefficients)
n <- nrow(trees)

# m1 z m2
F_stat <- (SSE1 - SSE2)/(p2-p1)/(SSE2/(n-p2))
1-pf(F_stat, p2 - p1, n - p2)

# m2 z m3
F_stat <- (SSE2 - SSE3)/(p3-p2)/(SSE3/(n-p3))
1-pf(F_stat, p3 - p2, n - p3)

# m1 z m3
F_stat <- (SSE1 - SSE3)/(p3-p1)/(SSE3/(n-p3))
1-pf(F_stat, p3 - p1, n - p3)
anova(m1, m3, test = "F")

# Zadanie 7.2 ------------------------------------------------------------------
library(car)
?linearHypothesis

mod.davis <- lm(weight ~ repwt, data=Davis)

## the following are equivalent:
linearHypothesis(mod.davis, diag(2), c(0,1))
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0,1))
linearHypothesis(mod.davis, c("(Intercept)", "repwt = 1"))

## use asymptotic Chi-squared statistic
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")


mod.duncan <- lm(prestige ~ income + education, data=Duncan)

## the following are all equivalent:
linearHypothesis(mod.duncan, "1*income - 1*education = 0")
linearHypothesis(mod.duncan, "income = education")
linearHypothesis(mod.duncan, "income - education")
linearHypothesis(mod.duncan, "1income - 1education = 0")
linearHypothesis(mod.duncan, "0 = 1*income - 1*education")
linearHypothesis(mod.duncan, "income-education=0")
linearHypothesis(mod.duncan, "1*income - 1*education + 1 = 1")
linearHypothesis(mod.duncan, "2income = 2*education")

C <- matrix(0, nrow = 1, ncol = 3)
C[1,1] = 0
C[1,2] = 1
C[1,3] = -1
d <- c(0)
linearHypothesis(mod.duncan, C, d)

data <- read.table("./dane/ExerciseCholesterol.txt")
int1 <- ifelse(data$Group==1, 1, 0)
int2 <- ifelse(data$Group==2, 1, 0)
int3 <- ifelse(data$Group==3, 1, 0)
Weight1 <- numeric(nrow(data))
Weight1[data$Group == 1] <- data$Weight[data$Group == 1] 
Weight2 <- numeric(nrow(data))
Weight2[data$Group == 2] <- data$Weight[data$Group == 2] 
Weight3 <- numeric(nrow(data))
Weight3[data$Group == 3] <- data$Weight[data$Group == 3] 
Data_new <- data.frame(int1,int2,int3,Weight1,Weight2,Weight3, HDL = data$HDL)
m <- lm(HDL~.-1, data = Data_new)                                        

C <- matrix(0, ncol = 6, nrow = 2)
C[1,4] <- 1
C[1,5] <- -1
C[2,4] <- 1
C[2,6] <- -1
d <- c(0,0)
linearHypothesis(m, C, d)

X <- as.matrix(Data_new[,-ncol(Data_new)])
XX_inv <- solve(t(X)%*%X)
CXXC <- C%*%XX_inv%*%t(C)
solve(CXXC)
CBd <- C%*%m$coefficients-d
r <- 2
s2 <- summary(m)$sigma^2

t(CBd)%*%solve(CXXC)%*%CBd/(r*s2)
