#zad01
dane <- read.csv2("Anova_cisnienie.csv"); dane
obiekty <- rep(names(dane), each=length(dane$Niskie))
wyniki <- c(dane[,1],dane[,2],dane[,3],dane[,4])

cisnienieTest <- data.frame(wyniki,obiekty); cisnienieTest
srednie <- sapply(split(cisnienieTest$wyniki, cisnienieTest$obiekty),mean); srednie

# H0: sigsq1 = sigsq2 = sigsq3 = sigsq4
# H1: ~H0

bartlett.test(wyniki~obiekty)
# alpha = 0.05 < p-value = 0.5009
# brak podstaw do odrzucenia H0
# na poziomie istotności 0.05 nie mamy podstaw
# do odruzcenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE


# H0: mu1 = mu2 = mu3 = mu4
# H1: ~H0

anova(lm(wyniki~obiekty))
# n = 40, k = 4
qf(0.95,4-1,40-4)
# F = 2.2665 < qf = 2.8663
# brak podstaw do odrzucenia H0

# alpha = 0.05 < p-value = 0.09735
# brak podstaw do odrzucenia H0

# Zatem ciśnienie nie ma wpływu na wielkość produkcji


#zad03
dane <- read.csv2("Anova_mikrometr.csv"); dane
obiekty <- rep(names(dane), c(4,6,5))
wyniki <- na.omit(c(dane[,1],dane[,2],dane[,3]))

# H0: sigsq1 = sigsq2 = sigsq3
# H1: ~H0

bartlett.test(wyniki~obiekty)
# alpha = 0.05 < p-value = 0.148
# brak podstaw do odrzucenia H0
# na poziomie istotności 0.05 nie mamy podstaw
# do odruzcenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE


# H0: mu1 = mu2 = mu3
# H1: ~H0

anova(lm(wyniki~obiekty))
qf(0.95,3-1,15-3)
# F = 3.3779 < qf = 3.8853
# brak podstaw do odrzucenia H0

# alpha = 0.05 < p-value = 0.06859
# brak podstaw do odrzucenia H0

# Zatem wybór mikrometrów nie ma wpływu na uzyskane wyniki


#zad04
dane <- read.csv2("Anova_sportowcy.csv"); dane
obiekty <- rep(names(dane), each=6)
wyniki <- na.omit(c(dane[,1],dane[,2],dane[,3],dane[,4]))

# H0: sigsq1 = sigsq2 = sigsq3 = sigsq4
# H1: ~H0

bartlett.test(wyniki~obiekty)
# alpha = 0.05 < p-value = 0.8517
# brak podstaw do odrzucenia H0
# na poziomie istotności 0.05 nie mamy podstaw
# do odruzcenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE


# H0: mu1 = mu2 = mu3 = mu4
# H1: ~H0

anova(lm(wyniki~obiekty))
qf(0.99,4-1,24-4)
# F = 6.1203 > qf = 4.9382
# odrzucamy H0

# alpha = 0.01 > p-value = 0.00398
# odrzucamy H0

# Zatem palenie papierosów może wpływać na rytm zatokowy serca


# Porównanie par średnich
# H0: mu_i = mu_j
# H1: mu_i != mu_j, j,i=1,2,3,4, i!=j

Tukey <- TukeyHSD(aov(wyniki~obiekty), conf.level=0.99, ordered=TRUE); Tukey
plot(Tukey)

# Jeżeli p adj < alpha
# to para (i,j) jest znacząco różna

# Podobni: Ś-N, D-N, D-Ś, L-D
# I grupa jednorodna: Ś-N-D
# II grupa jednorodna: L-D

dat <- data.frame(wyniki,obiekty)
srednie <- sapply(split(dat$wyniki, dat$obiekty),mean); srednie


#zad05
dane <- read.csv2("Anova_chomiki.csv"); dane
obiekty <- rep(names(dane), c(7,6,5,5))
wyniki <- na.omit(c(dane[,1],dane[,2],dane[,3],dane[,4]))

# H0: sigsq1 = sigsq2 = sigsq3 = sigsq4
# H1: ~H0

bartlett.test(wyniki~obiekty)
# alpha = 0.05 < p-value = 0.2139
# brak podstaw do odrzucenia H0
# na poziomie istotności 0.05 nie mamy podstaw
# do odruzcenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE


# H0: mu1 = mu2 = mu3 = mu4
# H1: ~H0

anova(lm(wyniki~obiekty))
qf(0.95,4-1,23-4)
# F = 3.9515 > qf = 3.1274
# odrzucamy H0

# alpha = 0.05 > p-value = 0.02398
# odrzucamy H0

# Zatem słuszne jest przypuszczenie, że masa gruczołu tarczycowego zależy od poziomu inbredu


# Porównanie par średnich
# H0: mu_i = mu_j
# H1: mu_i != mu_j, j,i=1,2,3,4, i!=j

Tukey <- TukeyHSD(aov(wyniki~obiekty), conf.level=0.95, ordered=TRUE); Tukey
plot(Tukey)

# Jeżeli p adj < alpha
# to para (i,j) jest znacząco różna

# Podobni: II-I, III-I, III-II, IV-II, IV-III
# I grupa jednorodna: I-II-III
# II grupa jednorodna: IV-III-II

dat <- data.frame(wyniki,obiekty)
srednie <- sapply(split(dat$wyniki, dat$obiekty),mean); srednie


#zad06
dane <- read.csv2("Anova_pulapki.csv"); dane
obiekty <- rep(names(dane), each=5)
wyniki <- na.omit(c(dane[,1],dane[,2],dane[,3],dane[,4],dane[,5]))

# H0: sigsq1 = sigsq2 = sigsq3 = sigsq4 = sigsq5
# H1: ~H0

bartlett.test(wyniki~obiekty)
# alpha = 0.05 < p-value = 0.06804
# brak podstaw do odrzucenia H0
# na poziomie istotności 0.05 nie mamy podstaw
# do odruzcenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE


# H0: mu1 = mu2 = mu3 = mu4 = mu5
# H1: ~H0

anova(lm(wyniki~obiekty))
qf(0.95,5-1,25-5)
# F = 39.382 > qf = 2.866
# odrzucamy H0

# alpha = 0.05 > p-value = 0
# odrzucamy H0

# Zatem strategia lokalizacji może mieć wpływ na liczbę uwięzionych ciem cygańskich


# Porównanie par średnich
# H0: mu_i = mu_j
# H1: mu_i != mu_j, j,i=1,2,3,4, i!=j

Tukey <- TukeyHSD(aov(wyniki~obiekty), conf.level=0.95, ordered=TRUE); Tukey
plot(Tukey)

# Jeżeli p adj < alpha
# to para (i,j) jest znacząco różna

# Podobni: roz-gru, skon-ros, pow-ros, pow-skon
# I grupa jednorodna: skon-ros-pow
# II grupa jednorodna: roz-gru

dat <- data.frame(wyniki,obiekty)
srednie <- sapply(split(dat$wyniki, dat$obiekty),mean); srednie


#zad02
dane <- read.csv2("Anova_kopalnie.csv"); dane
obiekty <- rep(names(dane), each=4)
wyniki <- na.omit(c(dane[,1],dane[,2],dane[,3],dane[,4],dane[,5]))

# H0: sigsq1 = sigsq2 = sigsq3 = sigsq4 = sigsq5
# H1: ~H0

bartlett.test(wyniki~obiekty)
# alpha = 0.01 < p-value = 0.03188
# brak podstaw do odrzucenia H0
# na poziomie istotności 0.01 nie mamy podstaw
# do odruzcenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE


# H0: mu1 = mu2 = mu3 = mu4 = mu5
# H1: ~H0

anova(lm(wyniki~obiekty))
qf(0.99,5-1,20-5)
# F = 0.9563 < qf = 4.893
# brak podstaw do odrzucenia H0

# alpha = 0.01 < p-value = 0.4594
# brak podstaw do odrzucenia H0

# Zatem średnie zawartości popiołu dla ekogroszku produkowanego 
# w pięciu kopalniach można uznać za jednakowe
