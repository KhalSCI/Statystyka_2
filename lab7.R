setwd("D:/R/Kolos2/lab7")
#zad01
dane <- read.csv2("Reg_chemikalia.csv"); dane
X <- dane$surowiec
Y <- dane$produkt

plot(X,Y, pch=20, main="chemikalia")

cov(X,Y) #dodatnia, więc wraz ze wzrostem surowca rośnie ilość produktów

cor(X,Y) # bardzo silny związek liniowy między ilością surowców a produktów

prosta <- lm(Y ~ X); prosta # y = 22.405 + 3.619x

abline(prosta, col="blue")

# wielkość produkcji wzrośnie o 3.62kg, jeśli ilość surowca wzrośnie o 1 litr

predict(prosta, data.frame(X=c(20,15)))

cor(X,Y)^2
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


#zad02
dane <- read.csv2("Reg_urzadzenie.csv"); dane
X <- dane$efektywnosc
Y <- dane$zywotnosc

plot(X,Y, pch=20, main="urzadzenia")

cov(X,Y) #ujemna, więc wraz ze wzrostem efektywnosci maleje żywotność urządzenia

cor(X,Y) # bardzo silny ujemny związek liniowy między ilością surowców a produktów

prosta <- lm(Y ~ X); prosta # y = 18.8823 - 0.8629x

b0 <- prosta$coefficients[1]
b1 <- prosta$coefficients[2]

abline(prosta, col="blue")

# żywotność urządzenia zmaleje o 0.86 miesiąca, jeśli efektywność wzrośnie o 1 element

predict(prosta, data.frame(X=c(11,19)))

cor(X,Y)^2
summary(prosta) $ r.squared

# y = b0 + b1 x
# H0: b1 = 0
# H1: b1 != 0
anova(prosta)
# alpha = 0.01 > p-value = 0.00067
# odrzucamy H0, regresja liniowa jest istotna

qf(1-0.01, 1, length(X)-2)
# F-value = 33.471 > qf = 12.246
# odrzucamy H0


#zad03
dane <- read.csv2("Reg_arszenik.csv"); dane
X <- dane$pH
Y <- dane$arszenik

plot(X,Y, pch=20, main="arszenik", xlab="pH", ylab="% arszeniku")

cov(X,Y) #ujemna, więc wraz ze wzrostem pH maleje ilość arszeniku w wodach gruntowych

cor(X,Y) # bardzo silny ujemny związek liniowy między pH a ilością arszeniku

prosta <- lm(Y ~ X); prosta # y = 190.27 - 18.03 x

b0 <- prosta$coefficients[1]
b1 <- prosta$coefficients[2]

abline(prosta, col="blue")

# ilość arszeniku zmaleje o 18.03%, jeśli jeśli pH gleby wzrośnie o 1

predict(prosta, data.frame(X=c(7.5,9)))

cor(X,Y)^2
summary(prosta) $ r.squared

# y = b0 + b1 x
# H0: b1 = 0
# H1: b1 != 0
anova(prosta)
# alpha = 0.01 > p-value = 0
# odrzucamy H0, regresja liniowa jest istotna

qf(1-0.01, 1, length(X)-2)
# F-value = 149.7 > qf = 8.53
# odrzucamy H0
