#LAB8
#Test dobroci dopasowania i niezależności chi-kwadrat

#####################################################################################
#ZAD1

dane = read.csv("C:/Users/student/Desktop/tmp/lab8/normalnosc_ozon.csv", sep = ";", dec = ",")

#w nawiasie z moich zajec poza nawiasem z notatek pozostawionych przez kogos
#H0: (rozklad emerytow ktorzy wrocili do pracy w hrabstwie Alleghenny odpowiada ogolnemu rozkladowi podanemu przez stowarzyszenie Russela Reynolda) częstotliwości formy zatrudnienia w hrabstwie są takie same jak podało SRR
#H1: (~H0) częstotliwości formy zatrudnienia w hrabstwie są inne niż podało SRR

wektorObserwacji = c(122,85,76,17) #observedf -> u nas tak nazwala
wektorOczekiwanychPrawdopodobienstw = c(0.38,0.32,0.23,0.07) #expectedp -> u nas tak nazwala

sum((wektorObserwacji - wektorOczekiwanychPrawdopodobienstw*300)^2/(wektorOczekiwanychPrawdopodobienstw*300)) #X-squared policzone recznie, wszystko i tak wychodzi z chisq.test

chisq.test(wektorObserwacji,p=wektorOczekiwanychPrawdopodobienstw)
#p-value = 0.3485 > alfa = 0.1 => brak podstaw do odrzucenia H0
#na poziomie istotnosci 10% nie mozna potwierdzic ze dane z hrabstwa Alleghenny roznia sie od ogolnego rozkladu

####################################################################################
#ZAD2
#ZADANIE DOMOWE

####################################################################################
#ZAD3

#H0: częstotliowść smaku cukierków jest taka sama jak twierdzi producent
#H1: ~H0

Ex = c(0.2,0.2,0.2,0.2,0.2)
Ob = c(7+20+4+12,20+5+16+9,10+5+13+16,7+13+21+3,14+17+4+17) #sumujemy bo calkowite wartosci
chisq.test(Ob,p=Ex)
#p-value = 0.8369 > alfa = 0.05 => brak podstaw do odrzucenia H0
#rozklad czestotliwosci cukierkow w torebce odpowiada rozkladowi deklarowanemu przez producenta na poziomie istotnosci 0.05

####################################################################################
#ZAD4

#H0: stężenie ozonu ma rozkład normalny
#H1: ~H0

ozon = dane$ozon
library(nortest)

#1.test Pearsona
pearson.test(ozon)
#p-value 0.146 > alfa = 0.05
#brak podstaw do odrzucenia H0

#2.Lillie test
lillie.test(ozon)
#p-value=0.2774 > alfa
#brak podstaw do odrzucenia H0

#3.Shapiro test
shapiro.test(ozon)
#p-value=0.1098 > alfa
#brak podstaw do odrzucenia H0

#Na poziomie istotnosci 0.05, nie mamy podstaw do odrzucenia hipotezy zerowej


#u nas robila tylko sposobem ponizej, te wyzej z pozostawionych notatek przez kogos (u tej osoby tym naszym sposobem tez robila)
ozon = dane$ozon

#H0: stężenie ozonu ma rozkład normalny
#H1: ~H0

Ob1 = c(7,19,31,17,3,1)

mi = mean(ozon) #srednia z proby
sigma = sd(ozon) #odchylenie z proby

#jak nie ma gotowej tabelki to robimy tak:
przedzialy = c(0,2,4,6,8,10,12) #u nas nazwała "br"
k = 6 #ilosc przedzialow (lenght(br) - 1 tez moze byc)
series = cut(ozon,przedzialy) #tak robimy jak tej tabelki nie ma i to nam wyswietla ktora wartosc nalezy do ktorego przedzialu
y = table(series) #tutaj wychodzi nam identyczna tabelka jak jest podana w poleceniu

observedf = c(7,19,31,17,3,1) #przepisujemy z y bo odwolania do y ktory jest table jest ciezkie (w sumie to wyzej oni tez tylko nazwali Ob1)
  
Ex1 = c() #u nas nazwala "normprobabilities"
normprobabilities = c()
for (i in 1:k){
  Ex1[i] = pnorm(przedzialy[i+1],mi,sigma) - pnorm(przedzialy[i],mi,sigma) #tak oni robili
  #a tak robilismy my:
  normprobabilities = c(normprobabilities,pnorm(przedzialy[i+1],mi,sigma) - pnorm(przedzialy[i],mi,sigma))
}
#musi sie sumowac do 1 wiec nadpisujemy pierwsza i ostatnia wartosc rozszerzajac o czesci od -inf do 2 i od 10 do +inf

#ich
Ex1[1] = pnorm(przedzialy[2],mi,sigma)
Ex1[6] = 1 - pnorm(przedzialy[6],mi,sigma)
#nasze
normprobabilities[1] = pnorm(przedzialy[2],mi,sigma)
normprobabilities[6] = 1 - pnorm(przedzialy[6],mi,sigma)

#ich
Ex1[5] = Ex1[5] + Ex1[6]
Ex1=Ex1[-6]

Ob1[5] = Ob1[5] + Ob1[6]
Ob1 = Ob1[-6]

#to tylko my robilismy
normalfreq = normprobabilities*78
expectedp = normalfreq/sum(normalfreq) #czyli wychodzi w sumie w przyblizeniu to co wczesniej bylo normalprobabilities, bo w rozkladzie normalnym tak bedzie wychodzilo

#nasze
#scalamy dwa ostatnie, bo "Należy pamiętać, że do ostatnich klas kwalifikuje się mniej niż 5 pomiarów."
observedf[5] = observedf[5] + observedf[6]
observedf=observedf[-6] #usuwamy ostatni
expectedp[5] = expectedp[5] + expectedp[6]
expectedp = expectedp[-6]

chisq.test(Ob1,p=Ex1)
#p-val = 0.9206 > alfa = 0.05
#brak podstaw do odrzucenia H0

chisq.test(observedf,p=expectedp)
#p-val = 0.9206 > alfa = 0.05 => brak podstaw do odrzucenia H0

####################################################################################
#ZAD5
#ZADANIE DOMOWE

####################################################################################
#ZAD6
#ZADANIE DOMOWE

####################################################################################
#ZAD7

#H0: cechy są zależne 
#H1: ~H0

m = c(15,12,8)
pm = c(8,15,9)
w = c(6,8,7)

tablica = data.frame(m,pm,w) #u nas nazwala "tablica_kontyngencji"

chisq.test(tablica)
#p-val = 0.557 > alfa = 0.05
#brak podstaw do odrzucenia H0

####################################################################################
#ZAD8
#ZADANIE DOMOWE

####################################################################################
#ZAD9
#ZADANIE DOMOWE

#notatki innej grupy pozostawione
z = c(96,96,90,36)
p = c(201,189,195,234)
nw = c(3,15,15,30)
tablica1 = data.frame(z,p,nw)

chisq.test(tablica1)

#p-val = 0 < alfa = 0.05
#odrzucamy H0

####################################################################################
