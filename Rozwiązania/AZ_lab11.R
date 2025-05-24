# Zadanie 8.1 ------------------------------------------------------------------
us <- read.table("./dane/uscrime.txt", header = T)

L0 <- lm(R~.-Ex0, data = us)

L1 <- lm(R~Age+Ed+Ex1+U2+W+X, data = us)

L2 <- lm(R~Ed+Ex1, data =us)

errL0 <- numeric(nrow(us))
errL1 <- numeric(nrow(us))
errL2 <- numeric(nrow(us))
for (i in 1:nrow(us)){
  L0 <- lm(R~.-Ex0, data = us, subset = -i)
  L1 <- lm(R~Age+Ed+Ex1+U2+W+X, data = us, subset = -i)
  L2 <- lm(R~Ed+Ex1, data =us, subset = -i)
  errL0[i] <- (us$R[i] - predict(L0, us[i,]))^2 
  errL1[i] <- (us$R[i] - predict(L1, us[i,]))^2 
  errL2[i] <- (us$R[i] - predict(L2, us[i,]))^2 
}

mean(errL0)
mean(errL1)
mean(errL2)

median(errL0)
median(errL1)
median(errL2)

boxplot(errL0,errL1,errL2)

boxplot(errL0-errL1, errL2-errL1)



# Zadanie 8.2 ------------------------------------------------------------------
# Regresja grzbietowa na zbiorze danych 'longley'

# Wczytanie danych i przygotowanie zmiennych
longley

# a)
m <- lm(Employed ~ ., data = longley)

# b)
# Liczba obserwacji
n <- nrow(longley)

# Algorytm obliczania współczynników ridge z definicji
# 1) centrowanie Y i standaryzacja X
#    Yc = Y - mean(Y), Xc^(i) = (X^(i)-mean(X^(i))) / ||X^(i)-mean(X^(i))||
#    (dla i-tej kolumny bez interceptu; intercept w tym algorytmie rozpatrujemy osobno,
#     dlatego Xc to macierz TYLKO predyktorów bez interceptu)
#    W mianowniku mamy normę z licznika i to jest pierwiastek z wariancji, ALE
#    estymatora obciążonego (dzielenie przez n, a nie przez n-1).
# 2) Beta_r <- ((Xc)'Xc + lambda * I)^(-1) (Xc)' Yc – współczynniki bez interceptu
# 3) po standaryzacji X z pkt 1) chcemy wrócić do pierwotnych skal poprzez Beta_r <- Beta_r / ||X^(i)-mean(X^(i))||
#    ze względu na własności skalowania i przesuwania o wektor w metodzie ridge (patrz wykład)
# 4) obliczenie interceptu: Beta_0r <- mean(Y) - colMeans(X) %*% Beta_r

# Końcowo wektor estymatorów współczynników ridge: c(Beta_0r, Beta_r)

# Wektor odpowiedzi Y (zmienna Employed to kolumna 7)
Y <- longley$Employed
# Centrowanie Y, potrzebne do obliczeń ridge bez interceptu
Yc <- Y - mean(Y)

# Macierz predyktorów X (wszystkie kolumny poza Employed)
X <- as.matrix(longley[, -7])
# Średnie z każdej kolumny X
mean_X <- colMeans(X)
# Obciążone odchylenie standardowe (dzielenie przez n)
std_X <- sqrt(apply(X, 2, var) * (n - 1) / n)

# Standaryzacja X
X_scaled <- matrix(0, nrow = n, ncol = ncol(X))
for (i in 1:ncol(X)) {
  X_scaled[, i] <- (X[, i] - mean_X[i]) / std_X[i]
}

# Parametr regularyzacji lambda dla ridge (tutaj ma on dowolną wartość)
lambda <- 10

# Estymacja współczynników ridge: (Xc'Xc + λI)^(-1) Xc' Yc
B_scaled <- solve(t(X_scaled) %*% X_scaled + lambda * diag(ncol(X_scaled))) %*% t(X_scaled) %*% Yc
# Przywrócenie skali oryginalnej
B_unscaled <- B_scaled / std_X
# Intercept
intercept <- mean(Y) - mean_X %*% B_unscaled
# Finalny wektor współczynników
beta_ridge_manual <- c(intercept, B_unscaled)

# c) Sprawdzenie zgodności z pakietem glmnet
library(glmnet)
model_r <- glmnet(X, Y, alpha = 0, intercept = TRUE, standardize = TRUE, thresh = 1e-20)
# tutaj warto spojrzeć sobie na help(glmnet), mamy tam dokładny opis funkcji
# X - *macierz* predyktorów bez interceptu
# Y - wektor odpowiedzi
# alpha - parametr określający karę; jeżeli 0 to kara L2, jeżeli 1 to kara L1, coś pomiędzy to kara mieszana - metoda elastic net
# intercept - określa czy ma liczyć wyraz wolny czy też nie
# standarize - określa czy ma standaryzować dane tak jak my w algorytmie
# thresh - wartość błędu przy liczeniu współczynników metodą coordinate descent
# ostatnie można pominąć, ale jeżeli ktoś jest ciekawy można poczytać o co chodzi

# Wyświetlenie współczynników dla różnych wartości lambda
coef(model_r)
# Wynik ma postać macierzy, gdzie w wierszach mamy współczynniki, a w kolumnach lambdy
# Domyślnie nlambda, czyli dla ilu lambd ma liczyć współczynniki, jest ustawione na 100,
# dlatego macierz ta ma 100 kolumn

# Obliczenie współczynników dla konkretnej wartości lambda (przeliczonej na skalę glmnet)
# Kolejna ważna sprawa - jeżeli chcemy liczyć współczynniki dla konkretnej lambdy,
# to musimy pamiętać, że lambda_glmnet nie jest tym samym co lambda_ridge 
# (ponieważ funkcja glmnet minimalizuje inną funkcję niż my na wykładzie)
# Aby policzyć lambda_glmnet trzeba przeskalować odpowiednio lambda_ridge
# lambda_glmnet = sd(Y)*lambda_ridge/n      (gdzie sd(Y) to obciążony estymator - dzielimy przez n - a n liczba obserwacji)
lambda_glmnet <- sqrt(var(Y) * (n - 1) / n) * lambda / n
# żeby policzyć współczynniki dla tej lambdy, używamy parametru s jako tej lambdy
# parametry exact = T, x = X, y = Y są potrzebne, aby policzyć współczynniki konkretnie dla podanej lambdy
# jeżeli pominiemy je, to zwróci nam współczynniki dla lambdy, z siatki 100 lambd, która jest najbliżej podanej w s
as.vector(coef(model_r, s = lambda_glmnet, exact = TRUE, x = X, y = Y))

# Dobór optymalnej λ poprzez 3‑krotną walidację krzyżową
# Do kroswalidacji mamy specjalną funkcję cv.glmnet, która działa podobnie co glmnet
# różnica jest taka, że tu możemy podać liczbę pętli kroswalidacyjnych, które chcemy policzyć - parametr nfolds
c1 <- cv.glmnet(X, Y, alpha = 0, nfolds = 3)
# funkcja ta zwraca nam dwie wartości lambd
opt_cv <- c1$lambda.min    # λ minimalizująca błąd z pętli kroswalidacyjnych
opt_1se <- c1$lambda.1se    # największa wartość lambda, która jest 1 błąd standardowy od minimum
# wszystkie lambdy między tymi dwoma wartościami możemy uznać za optymalne
# i tutaj mamy dowolność wyboru, w tym przypadku wybierzmy wartość lambda.min

# Jeżeli policzyliśmy lambda za pomocą pętli kroswalidacyjnej, to nie ma potrzeby już
# zamieniać ją na lambda_glmnet, ponieważ to jest już ta lambda, którą należy wpisać

# Współczynniki dla lambda.min
grid_coef <- coef(model_r, s = opt_cv)
# lambda.min jest w siatce, dlatego tutaj, jeżeli nie użyjemy exact, to uzyskamy ten sam wynik co jakbyśmy go użyli,
# ponieważ wartości współczynników już zostały policzone dla tej lambdy

# Możemy liczyć wartości współczynników używając również funkcji glmnet i wpisując konkretną wartość lambda
glmnet_min <- glmnet(X, Y, alpha = 0, lambda = opt_cv, intercept = TRUE, standardize = TRUE, thresh = 1e-20)
coef(glmnet_min)

# Teraz przeanalizujemy zachowanie współczynników w zależności od wartości lambdy
tot_lambdas <- length(model_r$lambda)
# Rysunek ścieżek estymatorów w zależności od indeksu lambda
matplot(tot_lambdas:1, t(coef(model_r))[,-1], type = "l", xlab = "Index of lambda", ylab = "Beta coefficients")
# w macierzy coef(model_r) wartości współczynników są w kolumnach dla wartości od 
# lambdy największej do lambdy najmniejszej
model_r$lambda # pokazuje nam wszystkie wartości lambda z siatki w odpowiedniej kolejności
# my chcemy rysować współczynniki dla lambdy od najmniejszej do największej, dlatego zmieniamy kolejność
# za pomocą tot_lambdas:1
# transpozycja macierzy współczynników jest potrzebna, aby w wierszach mieć wartości współczynników
# matplot będzie pobierał wartości po wierszach
# usuwamy pierwszą kolumnę, bo to intercept

# na samym wykresie widzimy, że współczynniki maleją do zera 
# mamy tutaj efekt ściągania tzn. współczynniki nie są zerowane tylko zminiejszane do małych wartości
# współczynniki, które pozostają znaczące (wartości daleko od 0) dla szerokiego zakresu lambd możemy uznać za stabilne
# tzn. że rzeczywiście zmienne te mają kluczowy wpływ na wartość Y
# jeżeli współczynniki szybko zbliżają się do zera, to możemy przypuszczać, że występuje np. współliniowość w danych
# a w ten sposób ridge minimalizuje wpływ tej współliniowości na wartości predykcji
# dzięki temu wartości współczynników dla różnych danych są bardziej stabilne niż dla MNK
# (patrz zadanie 8.4) kosztem obciążoności

# możemy też rysować pojedyńcze ścieżki (rev odwraca kolejność wektora)
plot(rev(coef(model_r)[2,]), type ="l")

# Estymatory dla małej λ = 0.03
lambda_fixed <- 0.03
glmnet_fixed <- coef(model_r,
                     s = sqrt(var(Y) * (n - 1) / n) * lambda_fixed / n,
                     exact = TRUE, x = X, y = Y)

# d) Porównanie współczynnika przy GNP: MNK vs ridge (λ = 0.03)
coef_mnk <- coef(m)
coef_ridge_0.03 <- as.numeric(glmnet_fixed)[which(rownames(glmnet_fixed) == "GNP")]
cat("MNK GNP coefficient:", coef_mnk["GNP"], "\n")
cat("Ridge (λ = 0.03) GNP coefficient:", coef_ridge_0.03, "\n")
# MNK GNP coefficient: -0.03581918 
# Ridge (lambda=0.03) GNP coefficient: 0.00799427
# widzimy, że wspołczynniki mają różny znak i zdecydowanie dla Ridge GNP ma mniejszą wartość
# jak wrócimy do któregoś z wcześniejszych zadań, to zobaczymy, że mamy w tych danych współliniowość
# i to właśnie w taki sposób ta współliniowość się może objawiać - różne znaki dla różnych metod
# ridge stara się zmniejszyć efekt współliniowości i wpływu tej zmiennej na predykcje




# Zadanie 8.3 ------------------------------------------------------------------
# Analiza danych 'prostate.data'

# Wczytanie danych z pliku i usunięcie kolumny train
pros <- read.table("./dane/prostate.data", header = TRUE)
pros$train <- NULL
# Numer kolumny z odpowiedzią lpsa to 9 (ostatnia)

# a) Model MNK ze wszystkimi predyktorami
a_model <- lm(lpsa ~ ., data = pros)
# Selekcja wsteczna według kryterium AIC
a_step  <- step(a_model, direction = "backward", k = 2)
# a_step zawiera ostateczny model po eliminacji zmiennych

# b) Ścieżka Lasso (alpha=1)
Xp <- as.matrix(pros[, -9])  # macierz predyktorów
Yp <- pros$lpsa             # wektor odpowiedzi
# do obliczania estymatorów LASSO używamy również funkcji glmnet, ale z alpha = 1 (kara L1)
glasso <- glmnet(Xp, Yp, alpha = 1)
# Wyświetlenie współczynników dla wszystkich lambda
# analogicznie jak wcześniej współczynniki liczone są dla różnych wartości lambda
print(coef(glasso))

# Rysunek ścieżki Lasso
tot_las <- length(glasso$lambda)
matplot(tot_las:1, t(coef(glasso))[,-1], type = "l", xlab = "Index of lambda", ylab = "Beta coefficients")
# w tym przypadku mamy efekt zerowania niektórych współczynników, czyli mamy tu selekcję zmiennych 
# współczynniki, które przez wiele wartości lambd utrzymują wartości różne od zera 
# są stabilne i może to oznaczać, że mają spory wpływ na ostateczną predykcję 
# a to znaczy, że ich nie chcemy usuwać
# jeżeli współczynniki szybko spadają do zera, to możemy to interpretować jako:
# informacja zawarta w tej zmiennej już jest tłumaczona przez inną zmienną (nie w całości oczywiście, ale spora jej część)


# c) CV dla Lasso (3-krotna)
# pętla kroswalidacyjna działa tu analogicznie jak w przypadku ridge
c2 <- cv.glmnet(Xp, Yp, alpha = 1, nfolds = 3)
lambda_las_min <- c2$lambda.min
lambda_las_1se <- c2$lambda.1se
# i tutaj też wszystkie wartości pomiędzy lambda.min a lambda.1se są optymalne

# Współczynniki przy wybranym lambda_1se
beta_lasso_sel <- coef(glasso, s = lambda_las_1se)
print(beta_lasso_sel)
# kropki to zera dla odpowiednich współczynników

# d) Porównanie z modelem stepwise
coef(a_step)
beta_lasso_sel
# widzimy, że model lasso wybrał mniej zmiennych niż model selekcji wstecz z karą AIC
# jeżeli porównamy z BIC to zachowują się one w tym przypadku identycznie


# Zadanie 8.4 ------------------------------------------------------------------
# rozwiązanie tego zadania jest na wykładzie - slajdy


# Zadanie 8.5 ------------------------------------------------------------------ 
# Eksperyment symulacyjny ilustrujący bias-variance

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

