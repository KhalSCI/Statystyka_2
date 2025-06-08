#zad01

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



#zad02

# v_1 - częstotliwość wypadków
# v_2 - częstotliwość zabójstw
# v_3 - częstotliwość samobójstw
# n_1 - częstość zaobserwowanych wypadków
# n_2 - częstość zaobserwowanych zabójstw
# n_3 - częstość zaobserwowanych samobójstw

# H0: odsetki poszczególnych zgonów pokrywają się z przedstawionymi w artykule;
#     v_1 = n_1 i v_2 = n_2 i v_3 = n_3
# H1: ~H0

expected <- c(.74, .16, .1)
observed <- c(68, 27, 5) 

wynik <- chisq.test(observed, p=expected); wynik
wynik <- wynik $ p.value

if(wynik > 0.1){
  print("Brak podstaw do odrzucenia H0")
}else{
  print("odrzucamy H0")
}

# odrzucamy H0
# odsetki poszczególnych zgonów różnią się od przedstawionych w artykule



#zad03

# H0: rozkład smaków cukierków jest równomierny i wynosi 20% dla każdeog
# H1: ~H0

expected <- c(rep(0.2, 5))
observed <- c(43, 50, 44, 44, 52) 

wynik <- chisq.test(observed, p=expected); wynik
wynik <- wynik $ p.value

if(wynik > .05){
  print("Brak podstaw do odrzucenia H0")
}else{
  print("odrzucamy H0")
}

# brak podstaw do odrzucenia H0
# rozkład smaków cukierków jest równomierny



# zad04

dane <- read.csv2("normalnosc_ozon.csv")[,1]

# H0: stężenie wodoru ma rozkład normalny
# H1: ~H0

pearson.test(dane, adjust=F)
pearson.test(dane, adjust=T)
lillie.test(dane)
shapiro.test(dane)
cvm.test(dane)
ad.test(dane)
sf.test(dane)

# brak podstaw do odrzucenia H0
# stężenie wodoru ma rozkład normalny

ozon <- table(cut(dane, breaks=seq(0,12,2))); ozon
values <- as.vector(ozon); values 
mu <- mean(dane)
sigma <- sd(dane)
norm <- c()
for (i in seq(2,12,2)) {
  norm <- c(norm, pnorm(i,mu,sigma)-pnorm(i-2,mu,sigma))
}
norm
sum(norm)
norm[1] <- pnorm(2,mu,sigma)
norm[6] <- 1-pnorm(10,mu,sigma)

values
values[5] <- values[5] + values[6]
values <- values[-6]
norm
norm[5] <- norm[5] + norm[6]
norm <- norm[-6]

chisq.test(values, p=norm)



# zad05

# H0: objętość guza ma rozkład normalny
# H1: ~H0



# zad07

# H0: miejsce zamieszkania danej osoby nie zależy od liczby lat studiów
# H1: ~H0

o1 <- c(15,12,8)
o2 <- c(8,15,9)
o3 <- c(6,8,7)
chisq.test(data.frame(o1, o2, o3))

# brak podstaw do odrzucenia H0
# miejsce zamieszkania danej osoby nie zależy od liczby lat studiów



# zad08

# H0: odsetek pasażerów, którzy zagubili bagaż w trakcie lotu nie zależy od linii lotniczej
# H1: ~H0

t <- c(10,7,4)
n <- c(90,93,96)
chisq.test(data.frame(t, n))

# brak podstaw do odrzucenia H0
# odsetek pasażerów, którzy zagubili bagaż w trakcie lotu nie zależy od linii lotniczej



# zad09

za <- c(96,96,90,36)
przeciw <- c(201,189,195,234)
nw <- c(3,15,15,30)

# H0: opinia zależy od wieku