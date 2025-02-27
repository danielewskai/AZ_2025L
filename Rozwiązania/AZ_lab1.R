# Zadanie 1.1 ------------------------------------------------------------------
n <- 10 # 50, 100, 500

# a)
a <- rnorm(n) #generujemy probke (przedrostek r) z rozkladu normalnego (norm) standardowego licznosci 10
qqnorm(a)
qqline(a)

# b)
# plot(seq(0, 4, 0.01), dgamma(seq(0, 4, 0.01), 2, 2), type = "l", xlab = "x", ylab = "Ga(2,2)")
b <- rgamma(n,2,2)
qqnorm(b)
qqline(b)

# c)
c <- rcauchy(n)
qqnorm(c)
qqline(c)


# Zadanie 1.2 ------------------------------------------------------------------
A <- matrix(c(-1, 4, -2, 5), ncol = 2, byrow = TRUE)

# a)
eigA <- eigen(A)
P <- eigA$vectors 
D <- diag(eigA$values)
solve(P)


# b)
all(P %*% D %*% solve(P) - A < 1e-6)

# c)
plot(NULL,xlim=c(-2,2),ylim=c(-2,2))
arrows(0, 0, P[1,1], P[2,1])
text(P[1,1], P[2,1], "x", pos=1)
arrows(0, 0, P[1,2], P[2,2])
text(P[1,2], P[2,2], "y", pos=1)

sqrt((P[1,2])^2+(P[2,2])^2)

# d)
B <- matrix(c(2, 1, 1, 2), ncol = 2, byrow = T)
eigB <- eigen(B)
PB <- eigB$vectors
DB <- diag(eigB$values)

PB
solve(PB)

# e)
svdA <- svd(A)
t(A) %*% A
svdA$v %*% diag(svdA$d)^2 %*% t(svdA$v)

eigAA <- eigen(t(A) %*% A)
PAA <- eigAA$vectors
DAA <- eigAA$values
DAA
svd(A)$d^2
PAA
svd(A)$v

# Zadanie 1.3 ------------------------------------------------------------------
M <- as.matrix(read.csv("./dane/zebra.csv"))
image(M, asp = TRUE, col = c("white", "black"), xaxt = "n", yaxt = "n")

# a)
svd.M <- svd(M)
u <- svd.M$u
v <- svd.M$v
d <- svd.M$d
n <- nrow(M) 

# b)
M50 <- u %*% diag(c(d[c(1:(n/2))], rep(0, n - n/2))) %*% t(v)
M10 <- u %*% diag(c(d[c(1:(n/10))], rep(0, n - n/10))) %*% t(v)
M4 <- u %*% diag(c(d[c(1:(n/25))], rep(0, n - n/25))) %*% t(v)
M2 <- u %*% diag(c(d[c(1:(n/50))], rep(0, n - n/50))) %*% t(v)

# c)
par(mfrow = c(2, 2))
image(M50, asp = TRUE, col = c("white", "black"))
image(M10, asp = TRUE, col = c("white", "black"))
image(M4, asp = TRUE, col = c("white", "black"))
image(M2, asp = TRUE, col = c("white", "black"))
par(mfrow = c(1, 1))

# d)
plot(d)
abline(v = c(n/2, n/10, n/25, n/50), col = "red")
