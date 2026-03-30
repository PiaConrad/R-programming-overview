# Tutorial 11, 04.2026

# 1 Sie möchten das Geschlechterverhältnis der Puppen der aquatischen Motte Acentria ephermerella und dessen Abhängigkeit vom Probenahmeart und Zeitpunkt analysieren.
# a) Lesen Sie hierfür die Datei pup_size.txt ein, und ermitteln Sie die Anzahl an Beobachtungen für jede Geschlecht - Region - Zeitpunkt (m) Kombination

a <- read.table('pup_size.txt', skip=2, header = T)
as.data.frame(table(region = a$region,  m = a$m, sex = a$sex))
# table erstellt Häufigkeitstabellen

# b) Führen Sie anschließend eine statistische Analyse der Daten durch. (Tip: Die Variablen “sex” und “m” sollten zuvor als Faktor deklariert werden).

# c) Beeinflussen Region und Zeitpunkt signifikant das Geschlechterverhältnis?

# d) Gibt es Hinweise darauf, daß der Einfluß der Region abhängig vom Zeitpunkt der Probenahme ist? 

a$sex = as.factor(a$sex)
a$m = as.factor(a$m)
m1 = glm(sex ~  region * m, data = a, family = binomial)

# Geschlecht hat nur 2 Ausprägungen -> binomial, keine lineare Regression
# -> wird nicht numerisch behandelt
# glm modelliert Wahrscheinlichkeiten, lm Mittelwerte

library(car)
library(carData)

Anova(m1) # bei glm Chisq statt F Werte - = Likelihood - Differenzen,
# wie gut Modell durch Prädikator erklärt wird

library(effects)
plot(allEffects(m1))
levels(a$sex) 
# Referenz = 1. Kategorie (m), also wird P für weiblich  (Nicht - Referenz) deutlicher Regionseffekt in Monat 2, geringerer Weibchenanteil in Region H

m2 = glm(sex ~   m + region, data = a, family = binomial)
Anova(m1) 
plot(allEffects(m2))

library(bbmle)
AICtab(m1, m2)
BICtab(m1, m2)

# Signifkanter Einfluß von sowohl Region als auch Monat. Interaktionsterm ist nicht signifikant, d.h. kein Hinweis darauf, daß Zeitpunkt der Probenahme die regionalen Unterschiede beeinflußt. Allerdings kaum Unterschied in den AIC Werten, 
# d.h. mit höherer Stichprobenzahl hätten vielleicht Unterschiede nachgewiesen werden können (nur 10 Weibchen in Region H Monat 2)

# Berechen Sie aus den estimates des Modells ohne Interaktionsterm den Weibchenanteil für die jeweiligen Monat-Region Kombinationen. 

summary(m2)
# bei den Estimates im glm werden log(odds) angezeigt
# odds ratio = exp(estimates) (prozentuales Verhältnis)
# zB exp(0.58)= 1.79 -> 79% höhere Wsl auf Weibchen in Monat 2

# generiere neuen Datensatz mit Region - Monat Kombinationen
a2 = as.data.frame(expand.grid(region = unique(a$region), m = as.factor(c(1,2))))
a2

# nutze predict() Funktion um für diese Kombinationen die vorhergesagten Mittelwerte auszurechnen 
# (type = 'response' liefert die Werte in der Ausgangsskala)

a2$pc_w = predict(m2, a2, type = 'response')
a2

# oder Wert für Wert direkt berechnen:
# p = e^n / (1 + e^n)

# zB AR in mo 1
exp(-0.66062) / (1 + exp(-0.66062)) 
# weil: 
# wsh für weibl = gewicht wbl / gewicht wbl + gewicht männl

# AR mo 2
exp(-0.66062 + 0.58377) / (1 + exp(-0.66062 + 0.58377))
# Referenz + Abweichung (Estimates)
# wieso 1? -> männl = Referenz, die ist per Def. 0
# e⁰ = 1

# usw

#  Um wieviel % höher ist der Anteil Weibchen in der Region AR im vergleich zur Region H? 

# 1. odds ratio berechnen

exp(-0.79657) # Estimate Wert für H, AR = Ref 
# odds ratio (Chancenverhältnis) von 0.45 ein  Weibchen zu finden in Region H als in AR, umgekehrt: odds für AR 1/0.45 = Faktor 2.22 größer als in H 

# Aber prozentuale Wsh beruht auf Ausgangsniveau, das ist monatsabhängig

# AR Monat 1 
exp(-0.66062 ) / (1 + exp(-0.66062 ))
# H Monat 1
exp(-0.66062 + -0.79657) / (1 + exp(-0.66062 + -0.79657 ))

# (p(AR) - p(H))/ p(H)
( 34.06 - 18.89 )/18.89  #    80 % mehr Weibchen in AR in M1
# wie viele Prozentpunkte mehr i Vrgl zu H

# im Monat 2:
(48.08 - 29.45)/29.45  #    63 % mehr Weibchen in AR in M2

# Beschreiben Sie das erzielte Ergebnis der statistischen Analyse mittels der erforderlichen Kennzahlen in zwei Sätzen. 

Die Geschlechterverhältnisse unterschieden sich signifikant zwischen den Monaten (LR-Chisq = 8.59, Df = 1, p = 0.0034) sowie zwischen den Regionen (LR-Chisq = 12.69, Df = 2, p = 0.0018), während kein signifikanter Interaktionseffekt zwischen 
Region und Zeitpunkt nachgewiesen wurde (p = 0.20). Dabei ist der Weibchenanteil im Monat 2 signifikant höher als im Monat 1 (β = 0.58, SE = 0.20, p = 0.003), und in der Region H signifikant niedriger als in der Referenzregion AR 
(β = −0.80, SE = 0.25, p = 0.001), während sich die Region G nicht signifikant von AR unterscheidet.

# 2 Im Bodensee wurden die Gelegegrößen von 2 Daphnia-Arten für jeweils 20 Tiere ermittelt. 
# Laden Sie die Datei clutch_sizes.csv von Ilias herunter und lesen Sie sie in R-Studio ein.

# a) Stellen Sie die Verteilung der Gelegegrößen in einer Abbildung dar. 

da = read.table('clutch_sizes.csv', header = T, sep = ';')

dga = subset(da, species == 'Daphnia galeata')
dlo = subset(da, species == 'Daphnia longispina')

par(mfrow = c(2,1), mar = c(4,4,1,1))

plot(table(clutch_size = dga$clutch_size), col = "purple", xlim = c(0, 4), lend = 2, lwd = 10, main = 'D. galeata', ylab = 'Häufigkeit')
plot(table(clutch_size = dlo$clutch_size), col = 'lightblue',xlim = c(0,4), lend = 2, lwd = 10, main = 'D. longispina', ylab = 'Häufigkeit')

# b) Berechnen Sie Mittelwert und Varianz für beide Arten.
# c) Untersuchen Sie ob sich die Gelegegrößen signifikant unterscheiden mit einer Poisson - Regression.

aggregate(clutch_size ~ species, data = da, function(x) c(m = mean(x), var = var(x)))

# Poisson Verteilung: Varianz = ca Mittelwert
# Standardabweichung = ⎷Varianz

dd = glm(clutch_size ~ species, data = da, family = poisson)
summary(dd)

# Poisson Regression = ~ glm für Zählwerte (Gelegegröße)

plot(allEffects(dd))

# Wie hoch ist Gelegegröße von Daphnia longispina relativ zu der von Daphnia galaeta in Prozent?

exp(-1.2657)
# # D. longipina Gelegröße 28 % der Gelegröße von D. galeata

# e Beschreiben Sie das Ergebnis der statistischen Analyse in einem Satz (inklusive der relevanten statistischen Kennzahlen). 

Die Gelegröße der beiden Arten unterschied sich signifikant (beta = -1.2657, SE = 0.3414, z =-3.707, p < 0.000209), wobei die Gelegröße von D. longispina 28 % der Gelegröße von D. galeata betrug.
