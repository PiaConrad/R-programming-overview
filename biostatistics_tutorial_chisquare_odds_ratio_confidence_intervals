# Tutorial 10

# 1 Sie beproben Fische im Bodensee und fangen bei verschiedenen Probenahmen insgesamt 10 verschiedene Arten. Wieviele Möglichkeiten gibt es genau 3 Arten in einem Fang zu haben?

# n = 10 Arten
# k = 10 Arten / Fang

# n über k = n! / (k!(n-k)!) =  120
# -> 120 Möglichkeiten

choose(10,3)

# 2 Sie beproben Wasserflöhe an 5 verschiedenen Stellen im Bodensee und finden jeweils 13,15,14,7, und 10 Tiere. Sie wollen untersuchen ob die Wasserflöhe über die 5 Stellen homogen verteilt sind.

daphnia = c(13,15,14,7,10)
# N = 13 + 15 + 14 + 7 + 10 = 59

# a) Stellen Sie die diesbezügliche Nullhypothese und Alternativhypothese auf.

# H0: Die Wasserflöhe sind homogen verteilt, jede Stelle hat die gleiche Wahrscheinlichkeit p = 1/5.

# H1: Die Verteilung ist nicht homogen, mindestens eine Stelle hat eine andere Wahrscheinlichkeit 

# b) Rechnen Sie die erwarteten Anzahlen unter einer Gleichverteilung aus

# E = N * 1/5 = 59 * 0.2 = 11.8 -> für jede Stelle

erwartet = rep(sum(daphnia)/5, 5)
erwartet

# c) Berechnen Sie Chi für jede Stelle

# Chi pro Stelle misst: wie weit weicht eine Beobachtung von der Nullhypothese ab?

chi = (daphnia - erwartet) / sqrt(erwartet)
chi

# d) Berechnen Sie die Chi2 test-Statistik.

chi2 = sum(chi**2)
chi2

# Das Gesamtmaß dafür, wie stark alle Beobachtungen zusammen von der Nullhypothese abweichen

# e) Verwenden Sie die Funktion pchisq() um die Wahrscheinlichkeit für eine Gleichverteilung zu ermitteln

p <- 1 - pchisq(chi2, df=length(daphnia)-1)
p

# pchisq = Die Wahrscheinlichkeit, einen Chi2-Wert ≤ den 
# beobachteten zu bekommen, wenn H₀ stimmt. (also zufällig)

# -> 1 - pchisq = Wie wahrscheinlich ist eine höhere Abweichung?

# f) Verwenden Sie die Funktion Chisq.test() um auf Gleichverteilung zu testen. 

chisq.test(daphnia, p = rep(0.2, 5))

# H0 wird nicht verworfen, nicht signifikant
# -> Unterschiede durch Zufall erklärbar

# Aufgabe 3 -> höhere Zahlen = bei gleicher Beobachtungszahl höhere Abweichungen (abhängig von df)

# 5 Johnson et al. (2002, zusammengefasst durch Whitlock & Schluter 2020) haben den Zusammenhang zwischen TV-Konsum (inklusive von Gewaltszenen) von Kindern zwischen 1-10 Jahren und späterem (nach 8 Jahren) 
# auffälligem (aggressiven) Verhalten in den USA untersucht.

# 88 Kinder schauten täglich weniger als 1 Stunde TV, 386 schauten 1 – 3 Stunden, und 233 schauten mehr als 3 Stunden. Von denen fielen jeweils 5, 87 und 67 Kinder während der folgenden 8 Jahre durch aggressives Verhalten auf.

# a) Berechnen Sie den prozentualen Anteil an Kinden 
# in jeder TV-Kategorie, die später auffällig wurden

# < 1h  : 5/88 = 0.057 -> 5.7%
# 1 - 3h: 87/386 = 0.225 -> 22.5%
# > 3h  : 67/233 = 0.288 -> 28.8%

#oder

ges = c(88, 386, 233)
agg = c(5, 87, 67)
percent = agg/ges
percent

gesamt <- c(88, 386, 233)
auffaellig <- c(5, 87, 67)

nicht_auffaellig <- gesamt - auffaellig

tabelle <- matrix(
  c(auffaellig, nicht_auffaellig),
  nrow = 3,
  byrow = FALSE
)

rownames(tabelle) <- c("<1h", "1–3h", ">3h")
colnames(tabelle) <- c("Auffällig", "Unauffällig")

tabelle


# b) Gibt es einen Zusammenhang zwischen TV-Konsum und  späterer Auffälligkeit? Führen Sie einen entsprechenden Signifikanztest durch.


chisq.test(tabelle)

#-> Nullhypothese verworfen

# c) Belegt ein signifikantes Testergebnis einen Einfluß von TV-Konsum auf späteres Verhalten? Warum bzw. warum nicht? 

# Nein -> statistischer, aber nicht unbedingt kausaler Zusammenhang (zu viele unkontrollierte Faktoren)


# 4 Beim Unglück der Titanic haben von den 1. Klasse Passagieren 203 überlebt und 122 starben, wogegen von allen anderen Passagieren/Crew Mitgliedern 508 überlebten und 1368 starben.

# a) Geben Sie diese Daten mithilfe der matrix Funktion in R ein.

titanic <- matrix(
  c(203, 122,
    508, 1368),
  nrow = 2,
  byrow = TRUE
)

rownames(titanic) <- c("1.Klasse", "Andere")
colnames(titanic) <- c("Überlebt", "Gestorben")

titanic

# b) berechnen Sie das relative Risiko zu sterben als Funktion der Klassenzugehörigkeit.

risk_1st <- 122 / (203 + 122) #Tod + 1. Klasse
risk_other <- 1368 / (508 + 1368) #Tod Andere

RR <- risk_1st / risk_other
RR

# 1. Klasse hatten nur 51% des Risikos relativ zu 'Andere'

# c) Berechnen Sie die odds ratio als Funktion der Klassenzugehörigkeit.

odds_1st <- 122 / 203
odds_other <- 1368 / 508
# Wahrscheinlichkeit Ereignis / W. Gegenereignis

OR <- odds_1st / odds_other
OR

# 22% -> odds zu sterben war in der 1. Klasse nur 22% so hoch wie bei den anderen (= 78% niedriger)

# d) Berechnen Sie das 95% Konfidenzintervall für das in c) berechnete odds ratio.

a <- 122; b <- 203; c <- 1368; d <- 508

SE_log_OR <- sqrt(1/a + 1/b + 1/c + 1/d) # Standardfehler von log(OR)

lower <- exp(log(OR) - 1.96 * SE_log_OR)
upper <- exp(log(OR) + 1.96 * SE_log_OR)

c(lower, upper)

# 95% KI für OR ≈[0.17,0.28] -> enthält nicht 1 -> Effekt
# Mit 95 % Sicherheit liegt die wahre Odds Ratio zwischen 0.17 und 0.28.

# e) Berechnen Sie den Chi2 Wert mithilfe der beobachteten und erwarteten Werte.

expected <- chisq.test(titanic, correct = FALSE)$expected
expected

chi2 <- sum((titanic - expected)^2 / expected)
chi2

# f) Testen Sie ob eine signifikante Assoziation zwischen Überleben und 1. Klasse Zugehörigkeit bei dem Unglück gab mittels Chi2 Test. 

chisq.test(titanic, correct = FALSE)

# Klassenzugehörigkeit und Überleben hochsignifikante Assoziation
