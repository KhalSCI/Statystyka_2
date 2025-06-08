dane <- read.csv2("Kolos2\\lab5\\dane_hip.csv")
wiatr <- na.omit(dane[,1])
pompa <- na.omit(dane[,2])
morze <- na.omit(dane[,3])
blaszki <- na.omit(dane[,4])
mleko <- na.omit(dane[,5])
kukulki <- na.omit(dane[,6])

#zad01

# H0: mu <= 4 m/s
# H1: mu > 4 m/s

mu <- 4
alpha <- 0.05
t <- (mean(wiatr)-mu)/sd(wiatr)*length(wiatr)^.5; t
R <- c(qt(1-alpha, length(wiatr)-1), Inf); R
# Odrzucamy H0
# Na pozomie istotności alpha = 0.05 dane potwierdzają
# hipotezę, że średnia prędkość wiatru przekracza 4 m/s
# Wniosek: okolice Darłowa nadają się do budowy elektrowni wiatrowej

t.test(wiatr, mu = 4, alternative = "greater")
# alpha = 0.05 > p-value = 0.0171


#zad02

# H0: mu >= 3.5
# H1: mu < 3.5

t.test(pompa, mu = 3.5, alternative = "less")
# alpha = 0.01 < p-value = 0.153
# Brak podstaw do odrzucenia H0


#zad03

# H0: mu = 870
# H1: mu != 870

z.test(morze, sigma.x = 5, mu = 870)
# alpha = 0.05 < p-value = 0.655
# Nie możemy odrzucić H0


#zad04

# H0: mu <= 0.04 mm
# H1: mu > 0.04 mm

zsum.test(mean(blaszki), sd(blaszki), length(blaszki), mu = 0.04, alternative = "greater", conf.level = 0.98)
# alpha = 0.02 < p-value = 0.051
# Nie możemy odrzucić H0
# Na poziomie istotności aplha = 0.02 dane nie potwierdzają hipotezy,
# że produkowane przez automat blaszki są grubsze niż nominalna grubość


#zad05

# a)

# H0: mu = 1.7 %
# H1: mu != 1.7 %

t.test(mleko, mu = 1.7)
# alpha = 0.05 < p-value = 0.112
# Nie możemy odrzucić H0

# b)

# H0: sigma >= 0.02 %
# H1: sigma < 0.02 %

sigma.test(mleko, sigmasq = 0.02, alternative = "less")
# alpha = 0.05 < p-value = 0.184
# Nie możemy odrzucić H0


#zad06

# a) (i)

# H0: mu = 17 mm
# H1: mu != 17 mm

t.test(kukulki, mu = 17)
# alpha = 0.05 > p-value = 0.011
# Odrzucamy H0

# a) (ii)

# H0: sigma = 2.5 mm
# H1: sigma != 2.5 mm

sigma.test(kukulki, sigmasq = 2.5^2)
# alpha = 0.05 < p-value = 0.499
# Brak podstaw do odrzucenia H0

# b)
t.test(kukulki) $ conf.int


#zad07

mu0 <- 55
sigma <- 18
n <- 100
mu <- 60
S <- 20
alpha <- 0.01

# a)

# H0: mu <= 55 
# H1: mu > 55

z <- (mu-mu0)/S*n^.5; z
R <- c(qnorm(1-alpha), Inf); R
zsum.test(mu, S, n, mu = mu0, alternative = "greater")

# z należy do R
# alpha = 0.01 > p-value = 0.0063
# Odrzucamy H0

# b)

# H0: mu <= 55 
# H1: mu > 55

chisq <- (n-1)*S^2 / sigma^2; chisq
R <- c(qchisq(1-alpha, n-1), Inf); R
# chisq nie należy do R
# Brak podstaw do odrzucenia H0


#zad08

T <- 1600
n <- 2500
p <- T/n
p0 <- 0.6
alpha <- 0.05

# H0: p = 60 % 
# H1: p != 60 %

z <- (p-p0) / (p0*(1-p0) / n)^.5; z
R <- c(-Inf, -qnorm(1-alpha/2), qnorm(1-alpha/2), Inf); R

binom.test(T, n, p = p0)
prop.test(T, n, p = p0)

#Odrzucamy H0
#Na poziomie istotności alpha = 0.05 próba potwierdza hipoteze, 
# że 60% nie zamierza wziąć udziału w wyborach


#zad09

T <- 16
n <- 1200
p <- T/n
p0 <- 0.02
alpha <- 0.05

# H0: p >= 2 % 
# H1: p < 2 %

z <- (p-p0) / (p0*(1-p0) / n)^.5; z
R <- c(-Inf, -qnorm(1-alpha/2)); R

binom.test(T, n, p = p0, alternative = "less")

# alpha = 0.05 < p-value = 0.055
# Brak podstaw do odrzucenia H0


#zad10

T <- 1000
n <- 1100
p <- T/n
p0 <- 0.9
alpha <- 0.05

# H0: p <= 90 % 
# H1: p > 90 %

z <- (p-p0) / (p0*(1-p0) / n)^.5; z
R <- c(qnorm(1-alpha/2), Inf); R

binom.test(T, n, p = p0, alternative = "greater")

# alpha = 0.05 < p-value = 0.171
# Brak podstaw do odrzucenia H0





