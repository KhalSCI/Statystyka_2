setwd("C:/Users/Student/Desktop/aj3")

#zad01
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


#zad02
dane <- read.csv2("task_2.csv"); dane
plasticA <- dane$plasticA
plasticB <- dane$plasticB

# mu1 - średnia granica wytrzymałości plastiku A
# mu2 - średnia granica wytrzymałości plastiku B
# H0: mu2 = mu1
# H1: mu2 != mu1

wilcox.test(plasticA, plasticB)

# alpha = 0.05 < p-value = 0.065
# brak podstaw do odrzucenia H0
# na poziomie istotności 5% średnia granica wytrzymałości plastiku A jest
# zbliżona do średniej granicy wytrzymałości plastiku B


#zad03
dane <- read.csv2("task_3.csv"); dane
January1984 <- dane$January1984
January1983 <- dane$January1983

# H0: j83 = j84
# H1: j83 != j84

wilcox.test(January1984 - January1983)

# alpha = 0.05 < p-value = 0.438
# brak podstaw do odrzucenia H0
# na poziomie istotności 5% średnie wartości tych wskaźników
# dla stycznia 1983 i stycznia 1984 są zbliżone


#zad04
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


#zad05
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


#zad08
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


#zad09
dane <- read.csv2("Kolos2\\lab9\\task_9.csv"); dane
Ref <- dane$References
Job <- dane$JobPerformance

cor.test(Ref, Job, method = "spearman", alternative = "two.sided")

# H0: 
