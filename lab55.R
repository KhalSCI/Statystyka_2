#LAB5

dane = read.csv("C:/Users/student/Desktop/tmp/lab5/dane_hip.csv", sep=";", dec=",")

#####################################################################################
#ZAD1

#(idealne zadanie na kolosa za 5 punktów)
wiatr = na.omit(dane$wiatr)
#odchylenie nie jest znane

#Procedura testowa

#1. H0: mu<=4m/s H1: mu>4m/s

#2. Statystyka testowa
srednia = mean(wiatr)
mu0 = 4
S = sd(wiatr)
n = length(wiatr)
t = (srednia-mu0)/(S/sqrt(n))
#t = 2.418622

#3. Obszar krytyczny R
alfa = 0.05
qt(1-alfa, n-1)
#R = (1.795885;inf)

#4. t należy do R -> odrzucamy H0 (obszar krytyczny to obszar odrzucenia)

#5. Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że średnia prędkość wiatru przekracza 4m/s, zatem sensowna jest budowa elektrowni wiatrowej.

#---------------------------
#ALTERNATYWNA METODA PONIŻEJ
#---------------------------

#Procedura testowa w R

#1. H0: mu<=4m/s H1: mu>4m/s

#2.
t.test(wiatr, mu=4, alternative = "greater")

#3. p-value = 0.01705

#4. alfa = 0.05 > p-value = 0.01705 -> odrzucamy H0

#5. Na poziomie istotności alfa = 0.05 dane potwierdzają hipotezę, że średnia prędkość wiatru przekracza 4m/s, zatem sensowna jest budowa elektrowni wiatrowej.

####################################################################################
#ZAD2
#ZADANIE DOMOWE

####################################################################################
#ZAD3

morze = na.omit(dane$morze)
sigma = 5 #odchylenie jest znane

#1. H0: mu = 870m H1: mu != 870m

#2.
#library(BSDA)
BSDA::z.test(morze, sigma.x = 5, mu = 870, alternative = "two.sided")

#3. p-value = 0.6547

#4. alfa = 0.05 < p-value = 0.6547 -> brak podstaw do odrzucenia H0

#5. Na poziomie istotności alfa = 0.05 dane nie potwierdzaja hipotezy, że średnia głębokość morza w tym rejonie jest różna od 870m.

####################################################################################
#ZAD4

blaszki=na.omit(dane$blaszki)
#rozkład nie jest znany, próba jest duża

#1. H0: mu <= 0.04mm H1: mu > 0.04mm

#2.
#library(BSDA)
BSDA::zsum.test(mean(blaszki), sd(blaszki), length(blaszki), mu = 0.04, alternative = "greater")

#3. p-value = 0.05041

#4. alfa = 0.02 < p-value = 0.05041 -> brak podstaw do odrzucenia H0

#5. Na poziomie istotności alfa = 0.02 dane nie potwierdzaja hipotezy, że średnia produkowane są... .

####################################################################################
#ZAD5

mleko = na.omit(dane$mleko)

#a)
#ZADANIE DOMOWE
#zastosować t.test

#b) testowanie wariancji
n = length(mleko)
#1. H0: sig^2 >= 0.02 %^2 H1: sig^2 < 0.02 %^2

#2. Statystyka testowa
Chi2 = (n-1)*var(mleko)/0.02
#Chi2 = 5.2

#3. Obszar krytyczny
alfa = 0.05
qchisq(alfa, n-1)
#R = (0;3.325113)

#4. Chi2 nie należy do R -> brak podstaw do odrzucenia H0

#5. Ma poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy, że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0.02 (%^2).

#---------------------------
#ALTERNATYWNA METODA PONIŻEJ
#---------------------------

#1. H0: sig^2 >= 0.02 %^2 H1: sig^2 < 0.02 %^2

#2.
library(TeachingDemos)
sigma.test(mleko, sigmasq = 0.02, alternative = "less")

#3. p-value = 0.1835

#4. alfa = 0.05 < p-value = 0.1835 -> brak podstaw do odrzucenia H0

#5. Ma poziomie istotności alfa = 0.05 dane nie potwierdzają hipotezy, że wariancja zawartości tłuszczu w mleku jest mniejsza niż 0.02 (%^2).


####################################################################################
#ZAD6
#ZADANIE DOMOWE

####################################################################################
#ZAD7
#ZADANIE DOMOWE

####################################################################################
#ZAD8

#proporcja populacyjna
T = 1600
n = 2500
phat = T/n
alfa = 0.05

p0 = 0.6
#1. H0: p = 0.6 H1: p != 0.6

#2. Statystyka testowa
Z=(phat-p0)/sqrt(p0*(1-p0)/n)
#Z = 4.082483

#3. Obszar krytyczny
qnorm(1-alfa/2)
#R = (-inf;-1.959964) suma (1.959964;inf)

#4. Z należy do R -> odrzucamy H0

#5. Na poziomie istotności alfa = 0.05 próba przeczy twierdzeniu, że 60% ogółu osób zamierza wziąć udział w wyborach.

#---------------------------
#ALTERNATYWNA METODA PONIŻEJ
#---------------------------

#część rozwiązania:
binom.test(T, n, p=p0, alternative = "two.sided")
#alfa = 0.05 > p-value = 4.413e-05 -> odrzucamy H0

#---------------------------
#ALTERNATYWNA METODA PONIŻEJ
#---------------------------

#część rozwiązania:
prop.test(T, n, p=p0, alternative = "two.sided")
#alfa = 0.05 > p-value = 4.864e-05 -> odrzucamy H0

####################################################################################
#ZAD9
#ZADANIE DOMOWE

####################################################################################
#ZAD10
#ZADANIE DOMOWE

####################################################################################