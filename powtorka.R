#HIPOTEZY

#zad01 MAŁA PRÓBA

# H0: mu <= 4 m/s
# H1: mu > 4 m/s

t.test(wiatr, mu = 4, alternative = "greater")
# alpha = 0.05 > p-value = 0.0171
# Odrzucamy H0
# Na pozomie istotności alpha = 0.05 dane potwierdzają
# hipotezę, że średnia prędkość wiatru przekracza 4 m/s
# Wniosek: okolice Darłowa nadają się do budowy elektrowni wiatrowej

#zad04 DUŻA PRÓBA

# H0: mu <= 0.04 mm
# H1: mu > 0.04 mm

zsum.test(mean(blaszki), sd(blaszki), length(blaszki), 
          mu = 0.04, alternative = "greater", conf.level = 0.98)
# alpha = 0.02 < p-value = 0.051
# Brak podstaw do odrzucenia H0
# Na poziomie istotności aplha = 0.02 dane nie potwierdzają hipotezy,
# że produkowane przez automat blaszki są grubsze niż nominalna grubość

#zad09 PROPORCJA

# H0: p >= 2 % 
# H1: p < 2 %

binom.test(T, n, p = p0, alternative = "less")

# alpha = 0.05 < p-value = 0.055
# Brak podstaw do odrzucenia H0



#ANOVA

#zad01 ANOVA
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
# do odrzucenia hipotezy o jednorodności wariancji
# Możemy przeprowadzić ANOVE
#

# H0: mu1 = mu2 = mu3 = mu4
# H1: ~H0

anova(lm(wyniki~obiekty))
n <- 40 # wszystkie próby
k <- 4  # liczba obiektów
qf(0.95,k-1,n-k)
# F = 2.2665 < qf = 2.8663
# brak podstaw do odrzucenia H0

# alpha = 0.05 < p-value = 0.09735
# brak podstaw do odrzucenia H0

# Zatem ciśnienie nie ma wpływu na wielkość produkcji


# zad04 PORÓWNANIE PAR ŚREDNICH
# H0: mu_i = mu_j
# H1: mu_i != mu_j, j,i=1,2,3,4, i!=j

Tukey <- TukeyHSD(aov(wyniki~obiekty), conf.level=0.99, ordered=TRUE); Tukey
plot(Tukey)

# Jeżeli p adj < alpha
# to para (i,j) jest znacząco różna (nie podobni)

# Podobni: Ś-N, D-N, D-Ś, L-D
# I grupa jednorodna: Ś-N-D
# II grupa jednorodna: L-D



# REGRESJA

setwd("D:/R/Kolos2/lab7")
#zad01
dane <- read.csv2("Reg_chemikalia.csv"); dane
X <- dane$surowiec
Y <- dane$produkt

plot(X,Y, pch=20, main="chemikalia")

cov(X,Y) #dodatnia, więc wraz ze wzrostem surowca rośnie ilość produktów

cor(X,Y) # bardzo silny związek liniowy między ilością surowców a produktów

prosta <- lm(Y ~ X); prosta # y = 22.405 + 3.619x

b0 <- prosta$coefficients[1]
b1 <- prosta$coefficients[2]

abline(prosta, col="blue")

# wielkość produkcji wzrośnie o 3.62kg, jeśli ilość surowca wzrośnie o 1 litr

predict(prosta, data.frame(X=c(20,15)))

cor(X,Y)^2 #dopasowanie prostej dosyć dobre czy coś
summary(prosta) $ r.squared

# y = b0 + b1 x
# H0: b1 = 0
# H1: b1 != 0
anova(prosta)
# alpha = 0.05 > p-value = 0.00046
# odrzucamy H0, regresja liniowa jest istotna

qf(1-0.05, 1, length(X)-2)
# F-value = 32.332 > qf = 5.318
# odrzucamy H0



# TESTY DOPASOWANIA I NIEZALEŻNOŚCI CHI2

#zad01 PORÓWNANIE

# H0: zatrudnienie emerytów pokrywa się z danymi podanymi przez fundacje
# H1: ~H0

expected <- c(.38, .32, .23, .07)
observed <- c(122, 85, 76, 17) 

wynik <- chisq.test(observed, p=expected); wynik
wynik <- wynik $ p.value

if(wynik > 0.1){
  print("Brak podstaw do odrzucenia H0")
}else{
  print("odrzucamy H0")
}

# brak podstaw do odrzucenia H0
# rozkład zatrudnienia emerytów pokrywa się z danymi podanymi przez fundacje


#zad04 NORMALNOŚĆ

# H0: stężenie wodoru ma rozkład normalny
# H1: ~H0

pearson.test(dane, adjust=F)
pearson.test(dane, adjust=T)


# zad07 ZALEŻNOŚĆ

# H0: miejsce zamieszkania danej osoby nie zależy od liczby lat studiów
# H1: ~H0

o1 <- c(15,12,8)
o2 <- c(8,15,9)
o3 <- c(6,8,7)
chisq.test(data.frame(o1, o2, o3))

# brak podstaw do odrzucenia H0
# miejsce zamieszkania danej osoby nie zależy od liczby lat studiów



# TESTY NIEPARAMETRYCZNE

#zad01 NIEZALEŻNE
dane <- read.csv2("task_1.csv"); dane
twin <- dane$twin
single <- dane$single

# mu1 - średnia liczba goleń maszynką z jednym ostrzem
# mu2 - średnia liczba goleń maszynką z dwoma ostrzami
# H0: mu2 <= mu1
# H1: mu2 > mu1

wilcox.test(twin, single, exact = F, alternative = "greater")

# alpha = 0.05 < p-value = 0.0777
# brak podstaw do odrzucenia H0
# na poziomie istotności 5%  średnia liczba goleń maszynką z dwoma ostrzami jest
# nie większa niż średnia liczba goleń maszynką z jednym ostrzem


#zad04 ZALEŻNE
dane <- read.csv2("task_4.csv"); dane
before <- dane$before
after <- dane$after

# H0: before <= after
# H1: before > after

wilcox.test(before-after, exact = F, alternative = "greater")

# alpha = 0.05 > p-value = 0.009
# odrzucamy H0
# na poziomie istotności 5% dane potwierdzają, że
# kampania odniosła sukces w zmniejszeniu liczby wypadków


#zad05 NIEZALEŻNE, WIĘCEJ NIŻ 2 OBIEKTY
dane <- read.csv2("task_5.csv"); dane
line1 <- dane$line1
line2 <- dane$line2
line3 <- dane$line3

# H0: line1 = line2 = line3
# H1: ~H0

wyniki <- c(line1,line2,line3)
obiekty <- rep(c("l1", "l2", "l3"),each=7)
data.frame(wyniki,obiekty)
kruskal.test(wyniki ~ obiekty)

# alpha = 0.05 > p-value = 0.02796
# odrzucamy H0
# na poziomie istotności 5% dane dostarczają wystarczających dowodów, aby
# wskazać jakiekolwiek różnice w lokalizacji dla trzech zestawów


#zad08 BLOKI, WIELE PARAMETRÓW
dane <- read.csv2("task_8.csv"); dane
Br <- dane$Bristol_Myers
El <- dane$Eli_Lilly
Pf <- dane$Pfizer

# H0: skutki inflacji były odczuwalne w równym stopniu przez poszczególne firmy
# H1: przynajmniej jedna para się różni

wyniki <- c(Br, El, Pf)
obiekty <- rep(c("f1", "f2", "f3"),each=4)
bloki <- rep(c("b1", "b2", "b3", "b4"),3)
data.frame(wyniki,obiekty,bloki)

friedman.test(wyniki, obiekty, bloki)
# alpha = 0.05 > p-value = 0.0388
# odrzucamy H0
# na poziomie istotności 5% dane potwierdzają, że skutki inflacji 
# nie były odczuwalne w równym stopniu przez poszczególne firmy


#zad09 KORELACJA
dane <- read.csv2("task_9.csv"); dane
Ref <- dane$References
Job <- dane$JobPerformance

cor.test(Ref, Job, method = "spearman", alternative = "")