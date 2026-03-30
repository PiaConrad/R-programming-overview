# Tutorial 9

# Untersuchen Sie die Abhängigkeit der Puppengröße von Acentria 
# vom Geschlecht der Puppen, der Region und dem Monat der 
# Probenahme. Lesen Sie hierfür die Datei pup_size.txt ein,
# und erstellen Sie jeweils ein Histogramm der Puppengröße für 
# die verschiedenen Kombinationen der 3 unabhängigen Variablen. 
# Wieviele Beobachtungen sind jeweils für die verschiedenen 
# Kombinationen vorhanden?

pup = read.table('pup_size.txt', header = T, skip = 2,
                stringsAsFactor = T)
pup$mo = as.factor(pup$m)
pup$g = paste(pup$sex, pup$region, pup$mo)

#install.packages('dplyr')
library(dplyr)

pup = arrange(pup,  region, mo, sex)

colv = rep(c('cyan', 'salmon'), 6)

par(mfrow = c(3,4))
for (i in 1:12) {
  x = subset(pup, g == unique(pup$g)[i])
  ttt = paste(unique(x$sex),unique(x$region), unique(x$mo), ', n = ', length(x$l))
  hist(x$l, breaks = seq(5,12,0.5), main = ttt, col = colv[i],
       las = 1, xlab = 'length [mm]')}

# unique = immer eine Gruppe auswählen (zB m AR 1)
# ttt = Titel

# 2 Untersuchen Sie dann den Einfluß von Geschlecht,
# Region und Monat auf die Puppengröße mittels linearen 
# Modellen und Anova. Vergleichen Sie Modelle mit 
# verschiedener Komplexität (verschiedene 2 fach 
# Interaktionen und 3 fach Interaktion von Geschlecht, 
# Region und Monat) mittels des adjusted R2, und der 
# Informationskriterien AIC und BIC.

# Welches Modell bzw. welche Modelle werden anhand der 
# verschiedenen Auswahlkriterien bevorzugt?

m_pup = lm(l ~ sex * mo * region, data = pup) # alle 2-&3-fach Interaktionen
m_pup2 = lm(l ~ (sex + mo + region)^2, data = pup) # nur 2- fach Interaktionen
m_pup3 = lm(l ~ (sex * mo + region), data = pup) # nur sex*mo Interaktion
m_pup4 = lm(l ~ (sex + mo * region), data = pup) # nur mo*region Interaktion
m_pup5 = lm(l ~ (sex *  region + mo), data = pup) # nur sex*region Interaktion

summary(m_pup)$adj.r.squared # alle R²
summary(m_pup2)$adj.r.squared # -> höchste R², am meisten Varianz wird erklärt
summary(m_pup3)$adj.r.squared
summary(m_pup4)$adj.r.squared
summary(m_pup5)$adj.r.squared

#install.packages("bbmle")
library(bbmle)

AICtab(m_pup, m_pup2, m_pup3, m_pup4, m_pup5, weights = T) #Modell2 am besten
# vergleicht Modelle danach, wie gut sie die Daten erklären, und bestraft unnötige 
# Komplexität, wobei kleinere Werte bessere Modelle anzeigen

BICtab(m_pup, m_pup2, m_pup3, m_pup4, m_pup5, weights = T) #Modell3 am besten
# ähnlich, bevorzugt aber bei gleichen Daten eher einfachere Modelle

# sollen beide möglichst niedrig sein
library(carData)
library(car)
Anova(m_pup2)
Anova(m_pup3)

# bei adjusted R2 kaum Unterschiede zwischen den Modellen 
# vorhanden. AIC wählt m_ac2 aus (weight von 81%, aber immerhin 
# noch 19% Unterstützung für das volle Modell mit 3 fach Interaktion). 
# BIC wählt m_ac3 aus (weight 75%), wobei auch m_ac2 noch 25% 
# Unterstützung hat. Anova zeigt deutliche Effekte bei genau diesen
# Modellen

# 3 Überprüfen Sie mit Hilfe von diagnostischen plots die Annahmen 
# des jeweils besten linearen Modells, erstellen Sie jeweils eine 
# Abbildung der Zusammenhänge mittels der allEffects() Funktion und 
# beschreiben Sie die Ergebnisse der beiden Modelle in kurzer Form 
# wobei die notwendigen statistischen Kennzahlen des gewählten Modells, 
# bzw. der gewählten Varianzanalyse mit aufgelistet werden sollten.

par(mfrow = c(2,2), oma = c(3,3,5,2))
plot(m_pup2) # best AIC, sieht ok aus
plot(m_pup3) # best BIC, sieht ok aus

# Residuals vs Fitted: Prüft Linearität und gleichmäßige Streuung

# Q–Q: Prüft Normalverteilung

# Scale–Location: Prüft konstante Varianz 

# Residuals vs Leverage: Identifiziert einflussreiche Ausreißer

#install.packages("effects")
library(effects)
plot(allEffects(m_pup2))
plot(allEffects(m_pup3))

