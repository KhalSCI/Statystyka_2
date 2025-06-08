#LAB7
#Korelacje i analiza regresji

#####################################################################################
#ZAD1

dane = read.csv("C:/Users/student/Desktop/tmp/lab7/Reg_chemikalia.csv", sep = ";", dec = ",")

#a)
dim(dane)
plot(dane)
x=dane$surowiec
y=dane$produkt
plot(dane$surowiec,dane$produkt,main='zaleznosc ilosci produktu od surowca',xlab="surowiec",ylab="produkt") #wykres punktowy

#b)
cov(dane$surowiec,dane$produkt)
#138.4889 - kowariancja (niezerowa) dodatnia, wraz ze wzrostem surowca rosnie ilosc produktu (liniowa zależmość między ilością surowca a ilością produkcji)

#c)
cor(dane$surowiec,dane$produkt)
#0.89534 - bardzo silny związek liniowy (istnieje silna zaleznosc miedzy iloscia zuzytego surowca a iloscia produktu)

#d)
prosta=lm(y~x)
#y = 22.4 + 3.619x
plot(dane$surowiec,dane$produkt,main='zaleznosc ilosci produktu od surowca',xlab="surowiec",ylab="produkt",pch=20)

#e)
abline(prosta,col='red')

#f)
#wraz ze wzrostem ilosci materiału - surowca - o 1l wzrośnie wielkosc produkcji o 3.619kg

#g) h)
predict(prosta,data.frame(x=c(15,20)))
22.4+3.619*20 #= 94.78
22.4+3.619*15 #= 76.685

#i)
summary(prosta)
summary(prosta)$r.squared
#r^2 = 80.16%  => dobre dopasowanie danych, zatem prosta regresji jest dobrze dopasowana
#ilosc zuzytego surowca wyjasnia w ok. 80% koncowa wielkosc produkcji

#j)
#H0: b1 = 0
#H1: b1 != 0

alpha = 0.05 #poziom istotnosci
n = length(x) #x -> surowiec
anova(prosta)
#Pr(>F) = 0.0004617 < alpha = 0.05 => odrzucamy H0
#F = 32.332
kw = qf(1-alpha,1,n-2) #= 5.32
#F > kw => odrzucamy H0
#na poziomie istotnosci 0.05 dane potwierdzaja hipoteze, że regresja liniowa jest istotna

####################################################################################
#ZAD2
#ZADANIE DOMOWE

dane = read.csv("C:/Users/student/Desktop/tmp/lab7/Reg_urzadzenie.csv", sep = ";", dec = ",")
 
#a)
x=dane$efektywnosc
y=dane$zywotnosc
plot(x,y)

#b)
cov(x,y)
#-8.652778 ze wzrostem efektywnosci maleje zywotnosc

#c)
cor(x,y)
#-0.91 bardzo silny zwiazek liniowy

#d)
prosta=lm(y~x) #y = 18.8823 - 0.8629*x

#e)
#wraz ze wzrostem efektywnosci o 1 element zmaleje żywotność o 0.86 miesiąca

#f) g)
predict(prosta,data.frame(x=c(11,19)))
#przy efektywnosci 11 elementow -> zywotnosc 9.39 miesiaca
#przy efektywnosci 19 elementow -> zywotnosc 2.49 miesiaca

#h)
summary(prosta)
#r^2 = 0.827

#i)
#H0: b1 = 0
#H1: b1 != 0
alfa = 0.01
anova(prosta)
#p-val = 0.0006735 < alpha = 0.01 =>  odrzucamy H0
#na poziomie istotnosci 0.01 dane potwierdzaja hipoteze, że regresja liniowa jest istotna

####################################################################################
#ZAD3
#ZADANIE DOMOWE

dane = read.csv("C:/Users/student/Desktop/tmp/lab7/Reg_arszenik.csv", sep = ";", dec = ",")

#a)
x=dane$pH
y=dane$arszenik
plot(x,y)

#b)
cov(x,y)
#-18.32216 ze wzrostem efektywnosci maleje zywotnosc
cor(x,y)
#-0.9504953 bardzo silny zwiazek liniowy

#c)
lm(y~x) #y = 190.27 - 18.03*x
prosta=lm(y~x)

#d)
#zmaleje o b1 -> 18.03 [%]

#e) f)
predict(prosta,data.frame(x=c(7.5,9)))
#przy pH gleby 7.5 -> usuniete zostanie 55.01 [%] arszeniku
#przy pH gleby 9 -> usuniete zostanie 27.96 [%] arszeniku

#g)
summary(prosta)
#r^2=0.9034

#h)
#H0: b1 = 0
#H1: b1 != 0
alpha = 0.01
anova(prosta)
#pval = 1.552e-09 < alpha = 0.01 => odrzucamy H0
n = length(x)
kw = qf(1-alpha,1,n-2) #= 3.04 < F = 149.7 => odrzucamy H0
#na poziomie istotnosci 0.01 dane potwierdzaja hipoteze, że regresja liniowa jest istotna

####################################################################################
