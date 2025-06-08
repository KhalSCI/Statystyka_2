#LAB6
#Analiza wariancji (ANOVA)

#####################################################################################
#ZAD1

#opis zadan moze zawierac te punkty jak przy hipotezach (obszar krytyczny itd.) - tak Adam sobie notuje, a byl na wykladzie
cisnienie = read.csv("C:/Users/student/Desktop/tmp/lab6/Anova_cisnienie.csv", sep=";")

metoda = rep(names(cisnienie), each=length(cisnienie$Niskie))
wyniki = c(cisnienie$Niskie,cisnienie$Srednie,cisnienie$Silne,cisnienie$BardzoSilne)

cisnienieTest=data.frame(wyniki,metoda)

srednie = sapply(split(cisnienieTest$wyniki, cisnienieTest$metoda),mean) #srednie probkowe dla poszczegolnych poziomow cisnien

#H0: sig^2_1 = sig^2_2 = sig^2_3 = sig^2_4 (srednie takie same)
#H1: ~HO (rózne)

bartlett.test(wyniki~metoda,cisnienieTest)

#p-value = 0.5009 > alfa = 0.05 => brak podstaw do odrzucenia H0
#Na poziomie istotnosci 0.05 (5%) nie mamy podstaw do odrzucenia hipotezy o jednorodności wariancji, zatem możemy przeprowadzić ANOVE.

#H0: mu1 = mu2 = mu3 = mu4 H1: ~H0

model = lm(wyniki~metoda)
anova(model)

#-----------------
#I SPOSOB PONIZEJ
#-----------------

#F = 2.2665 [F(value) z anova(model)]
k = 4 #ilosc porownywanych obiektow
n = 40 #ilosc wszystkich obserwacji
alfa = 0.05 #poziom istotnosci
qf(1-alfa,k-1,n-k) #2.866266 (kwantyl rozkladu)

#Jeżeli F > qf(1-alfa,k-1,n-k) => odrzucamy H0
#F = 2.2665 < 2.866266 => brak podstaw do odrzucenia H0
#Zatem cisnienie nie ma wpływu na wielkosc produkcji.

#-----------------
#II SPOSOB PONIZEJ
#-----------------

#p-value = 0.09735 [Pr(>F) z anova(model)]

#alfa = 0.05 < p-value = 0.09735 => brak podstaw do odrzucenia H0

####################################################################################
#ZAD2
#ZADANIE DOMOWE

####################################################################################
#ZAD3

mikrometry = read.csv("C:/Users/student/Desktop/tmp/lab6/Anova_mikrometr.csv", sep = ";", dec=",")
metoda1 = rep(names(mikrometry),c(length(na.omit(mikrometry$mikrometrI)),length(na.omit(mikrometry$mikrometrII)),length(na.omit(mikrometry$mikrometrIII))))
wyniki1 = c(na.omit(mikrometry$mikrometrI), na.omit(mikrometry$mikrometrII), na.omit(mikrometry$mikrometrIII))

mikrometryTest = data.frame(wyniki1,metoda1)

srednie1 = sapply(split(mikrometryTest$wyniki1, mikrometryTest$metoda1),mean)

#H0: ...  H1: ~H0

bartlett.test(wyniki1~metoda1,mikrometryTest)

#p-value = 0.148 > alfa = 0.05 => brak podstaw do odrzucenia H0
#zatem możemy przeprowadzić ANOVE (wariancje sa jednorodne P.S. u nas na zajeciach zawsze tak bedzie, bo nie ma czasu wprowadzac przypadku gdy nie sa)

model1 = lm(wyniki1~metoda1)
anova(model1)

#H0: ... H1: ~H0

#-----------------
#I SPOSOB PONIZEJ
#-----------------

k1 = 3
n1 = 15
alfa1 = 0.05
qf(1-alfa1,k1-1,n1-k1) #3.885294
#F = 3.3779 [F(value) z anova(model)]
#F < qf => brak podstaw do odrzucenia H0

#-----------------
#II SPOSOB PONIZEJ
#-----------------

#p-value = 0.06859 [Pr(>F) z anova(model)]

#p-value > alfa => brak podstaw do odrzucenia H0

####################################################################################
#ZAD4

#a)
sportowcy = read.csv("C:/Users/student/Desktop/tmp/lab6/Anova_sportowcy.csv", sep = ";")
metoda2 = rep(names(sportowcy), each=length(sportowcy$Niepalacy))
wyniki2 = c(sportowcy$Niepalacy, sportowcy$Lekkopalacy, sportowcy$Sredniopalacy, sportowcy$Duzopalacy)

sportowcyTest = data.frame(wyniki2,metoda2)

srednie2 = sapply(split(sportowcyTest$wyniki2, sportowcyTest$metoda2),mean)

#H0: ...  H1: ~H0

bartlett.test(wyniki2~metoda2,sportowcyTest)

#p-value = 0.8517 > alfa = 0.01 => brak podstaw do odrzucenia H0
#zatem możemy przeprowadzić ANOVE

model2 = lm(wyniki2~metoda2)
anova(model2)

#H0: ... H1: ~H0

#alfa = 0.01 > p-value = 0.0039 => odrzucamy H0
#Zatem palenie papierosow wplywa na rytm zatokowy serca.

#b) 
#test uczciwych istotnych różnic (honest significant differences) Tukey’a
#bedziemy porownywac wszystkie pary srednich, H0: mu_i = mu_j, H1: mu_i != mu_j; i,j = 1,2,3,4; i != j

TukeyHSD(aov(wyniki2~metoda2),ordered=TRUE)

#Jezeli p adj < alfa to para(i,j) rozni sie miedzy soba istotnie

#Podobne pary:
#S-N, D-N, D-S, L-D (podobne, bo ich p adj > alfa; laczymy trzy pierwsze, bo powtarzaja sie czlonkowie par)

#(Dwie) grupy jednorodne (ostateczne):
#1. Niepal-Sredpal-Duzpal
#2. Lekkopal-Duzopal

plot(TukeyHSD(aov(wyniki2~metoda2),ordered=TRUE))

####################################################################################
#ZAD5

#a)
chomiki = read.csv("C:/Users/student/Desktop/tmp/lab6/Anova_chomiki.csv", sep = ";")
metoda3 = rep(names(chomiki),c(length(na.omit(chomiki$I)),length(na.omit(chomiki$II)),length(na.omit(chomiki$III)),length(na.omit(chomiki$IV))))
wyniki3 = c(na.omit(chomiki$I), na.omit(chomiki$II), na.omit(chomiki$III), na.omit(chomiki$IV))

chomikiTest = data.frame(wyniki3,metoda3)
srednie3 = sapply(split(chomikiTest$wyniki3, chomikiTest$metoda3),mean)

bartlett.test(wyniki3~metoda3,chomikiTest)

#H0: ... H1: ~H0

#p-value = 0.2139 > alfa = 0.05 => brak podstaw do odrzucenia H0
#zatem możemy przeprowadzić ANOVE

model3 = lm(wyniki3~metoda3)
anova(model3)

#H0: ... H1: ~H0

#alfa = 0.05 > p-value = 0.02398 => odrzucamy H0
#Zatem...

#b)
TukeyHSD(aov(wyniki3~metoda3),ordered=TRUE)

#Podobne pary:
#II-I, III-I, III-II, IV-II, IV-III

#(Dwie) Grupy jednorodne (ostateczne):
#1. I-II-III
#2. II-III-IV

####################################################################################
#ZAD6

#a)
pulapki = read.csv("C:/Users/student/Desktop/tmp/lab6/Anova_pulapki.csv", sep = ";")
metoda4 = rep(names(pulapki), each=length(pulapki$rozsiany))
wyniki4 = c(pulapki$rozsiany, pulapki$skoncentrowany, pulapki$roslina.zywicielka, pulapki$powietrzny, pulapki$gruntowy)

pulapkiTest = data.frame(wyniki4,metoda4)
srednie4 = sapply(split(pulapkiTest$wyniki4, pulapkiTest$metoda4),mean)

bartlett.test(wyniki4~metoda4,pulapkiTest)


#H0: ... H1: ~H0

#p-value = 0.06804 > alfa = 0.05 => brak podstaw do odrzucenia H0
#zatem możemy przeprowadzić ANOVE

model4 = lm(wyniki4~metoda4)
anova(model4)

#H0: ... H1: ~H0

#alfa = 0.05 > p-value = ok. 0 => odrzucamy H0
#Zatem...

#b)
TukeyHSD(aov(wyniki4~metoda4),ordered=TRUE)

#Podobne pary:
#r-g, s-rż, p-rż

#Grupy jednorodne:
#1. s-rż-p
#2. p-rż

####################################################################################
