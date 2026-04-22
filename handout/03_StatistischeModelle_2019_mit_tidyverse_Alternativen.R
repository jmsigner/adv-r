
library(tidyverse)  # Laden des Pakets 'tidyverse'.


#----------------------------------------------------------------------------
# Daten einlesen, Arbeitsordner definieren und gewuenschte Pakete aktivieren 
#----------------------------------------------------------------------------

# setwd ("P:/Statistik/")             # So kann der Arbeitsordner explizit gewaehlt werden; 
                                      # Das empfehlen wir aber nicht, stattdessen
                                      # legen Sie ein Projekt an (RStudio -> New Project);
                                      # Dadurch ist man automatisch im richtigen Arbeitsordner
getwd()                               # So findet man heraus, in welchem Arbeitsordner wir 
                                      # uns derzeit befinden 

# Die Daten liegen im Unterordner "data" im Projektverzeichnis. 
bmi_data <- read.csv("data/bmi_data.csv")    # Einlesen der Daten aus .csv-Datei und speichern im Data Frame bmi_data

# Alternative bei Benutzung des Pakets 'tidyverse':
bmi_data <- read_csv("data/bmi_data.csv")   # Einlesen der Daten aus .csv-Datei und speichern im tibble bmi_data


#------------------------------------------------------------------------------------------------
# t-Test
#------------------------------------------------------------------------------------------------

## Einstichproben-t-Test (One Sample t-test) ## -------------------------------------------------

groesse175 <- rnorm(999, mean=1.75, sd=0.075)  # Erzeuge einen Vektor 'groesse175' mit kuenstlichen Daten,
                                               # die um den Mittelwert 1.75 normalverteilt sind.
                                               # Dies ist die erste kuenstlich erstellte Stichprobe.
                                               # Achtung, da per Zufall Werte aus einer Verteilung
                                               # gezogen werden, sind die Ergebnisse bei Wiederholung nur
                                               # ungefaehr gleich, aber nicht exakt.
mean(groesse175)  # Mittelwert der kuenstlichen Stichprobe ueberpruefen
                  # Dieser Mittelwert ist nur bei unendlich großen Stichproben genau bei 1.75.
                  # Durch die relativ große Stichprobe (N=999) sollte er aber nahe an 1.75 sein.

par(mfrow=c(1, 2))  # Grafikeinstellung so modifizieren, dass Platz fuer zwei Grafiken nebeneinander ist
breaks <- seq(from=1, to=2.5, by=0.05)  # Definiere Klassengrenzen ('breaks') fuer die Histogramme

hist(groesse175, breaks=breaks, col="grey",  # Zeichne Histogramm fuer die erste Stichprobe
     main=paste("p-Wert = ", round(t.test(groesse175, mu=1.75)$p.value, 3)), xlab="Body height in m")
abline(v=1.75, col="darkred", lwd=3)  # Zeichne eine rote Linie beim Erwartungswert 1.75 ein. 
                                      # Der Erwartungswert ist frei gewaehlt, je nachdem, was einen interessiert.
                                      # Oft ist es auch ein Wert aus der Literatur.

t.test(groesse175, mu=1.75)  # Einstichproben-t-Test, um zu testen, ob der Mittelwert der ersten 
                             # Stichprobe signifikant vom Erwartungswert 1.75 verschieden ist.
                             # Sollte hier einen NICHT-signifikanten Unterschied ergeben 
                             # (p-Wert NICHT kleiner als 0.05), weil die Stichprobe ja aus 
                             # einer Normalverteilung mit Mittelwert 1.75 stammt.
                             # Wir koennen also die Nullhypothese nicht ablehnen, 
                             # dass es keinen Unterschied zwischen dem Erwartungswert und 
                             # dem Mittelwert der Stichprobe gibt

groesse185 <- rnorm(999, mean=1.85, sd=0.075)  # Erzeuge einen Vektor 'groesse185' mit kuenstlichen Daten,
                                               # auch als Zufallsziehung aus einer Normalverteilung, 
                                               # aber mit einem anderen Mittelwert, naemlich 1.85.
                                               # Dies ist die zweite Stichprobe.
mean(groesse185)  # Tatsaechlicher Mittelwert der zweiten Stichprobe

hist(groesse185, breaks=breaks, col="grey",    # Zeichne Histogramm fuer die zweite Stichprobe
     main=paste("p-Wert = ", round(t.test(groesse185, mu=1.75)$p.value, 3)), xlab="Body height in m")
abline(v=1.75, col="darkred", lwd=3)  # Zeichne eine rote Linie beim Erwartungswert 1.75 ein. 
                                      # Wir testen wieder gegen denselben Erwartungswert wie 
                                      # bei der ersten Stichprobe, um zu schauen, ob es diesmal
                                      # einen signifikanten Unterschied gibt. 
                                      # Probieren Sie gerne auch aus, gegen 1.85 zu testen - 
                                      # Was erwarten Sie? Einen signifikanten Unterschied
                                      # oder keinen signifikanten Unterschied?

par(mfrow=c(1, 1))  # Grafikeinstellungen zuruecksetzen

t.test(groesse185, mu=1.75)  # Einstichproben-t-Test, um zu testen, ob der Mittelwert der zweiten 
                             # Stichprobe signifikant vom Erwartungswert 1.75 verschieden ist.
                             # Sollte hier einen SIGNIFIKANTEN Unterschied ergeben 
                             # (p-Wert KLEINER als 0.05), weil die Stichprobe ja aus 
                             # einer Normalverteilung mit Mittelwert 1.85 stammt.
                             # Wir koennen also die Nullhypothese ablehnen, 
                             # dass es keinen Unterschied zwischen dem Erwartungswert und 
                             # dem Mittelwert der Stichprobe gibt.

## Zweistichproben-t-Test (Two Sample t-test) ## -----------------------------------------------

# Grafische Darstellung

# Uebliche Darstellung als Boxplot
boxplot(bmi_data$groesse ~ bmi_data$geschlecht, 
        col=c("purple", "orange"), 
        xlab="Geschlecht", 
        ylab="Body height in m",
        las=1,
        ylim = c(1.5, 2))

# Ebenfalls moeglich. Darstellung als zwei Histogramme
bmi_data_m <- bmi_data[bmi_data$geschlecht == "m", ]  # Datensatz nach Geschlecht in zwei 
bmi_data_w <- bmi_data[bmi_data$geschlecht == "w", ]  # Datensaetze aufteilen
breaks <- seq(from=1, to=2.5, by=0.05)  # Definiere Klassengrenzen ('breaks') fuer die Histogramme
hist(bmi_data_m$groesse, breaks = breaks, col = "purple", xlab = "Body height in m", 
     ylab = "Haeufigkeit", main = "")
hist(bmi_data_w$groesse, breaks = breaks, col = "orange", add = TRUE)  # Zweites Histogramm wird über das erste geplottet
legend("topright", legend = c("Maenner", "Frauen"), fill = c("purple", "orange"))

# Zweistichproben-t-Test
t.test(bmi_data_m$groesse, bmi_data_w$groesse)  # Zweistichproben-t-Test
t.test(groesse ~ geschlecht, bmi_data)        # Alternative Schreibweise (ueblicher)

# Alternative bei Nutzung des R-Paketes 'tidyverse': --------------------------------------------
# Boxplot
boxplot(bmi_data$groesse ~ bmi_data$geschlecht, 
        col=c("purple", "orange"), 
        xlab="Geschlecht", 
        ylab="Body height in m",
        las=1,
        ylim = c(1.5, 2))

# Histogramme (Shortcut 'strg + shift + m' -> '%>%' )
bmi_data_m <- bmi_data %>% filter(geschlecht == "m")
bmi_data_w <- bmi_data %>% filter(geschlecht == "w")
breaks <- seq(from=1, to=2.5, by=0.05)  
hist(bmi_data_m$groesse, breaks=breaks, col="purple", xlab="Body height in m", 
     ylab="Haeufigkeit", main="")
hist(bmi_data_w$groesse, breaks=breaks, col="orange", add=TRUE)
legend("topright", legend=c("Maenner", "Frauen"), fill = c("purple", "orange"))

# Zweistichproben-t-Test
t.test(bmi_data_m$groesse, bmi_data_w$groesse)         # Zweistichproben-t-Test
t.test(groesse ~ geschlecht, bmi_data)                 # Alternative Schreibweise (ueblicher)


## Zweistichproben-t-Test fuer gepaarte Stichproben ## ------------------------------------------

# Kuenstlich erstellter Datensatz (signifikantes Beispiel)
# Es wird angenommen, dass es sich in beiden Jahren um dieselben
# acht Kirschbaeume handelt, daher die Gepaartheit:
# Jeder Baum wird einmal in Jahr X abgeernet und einmal in Jahr Y.
year_X <- c(36, 31.5, 34, 32.5, 35, 31.5, 31, 35.5)  # Kischenernte Jahr X
year_Y <- c(34, 35.5, 33.5, 36, 39, 35, 33, 39.5)    # Kischenernte Jahr Y   

# Kuenstlich erstellter Datensatz (NICHT-signifikantes Beispiel)
# Es wird angenommen, dass es sich in beiden Jahren um dieselben
# acht Kirschbaeume handelt, daher die Gepaartheit
# Jeder Baum wird einmal in Jahr X abgeernet und einmal in Jahr Y.
year_X.ns <- c(36, 31.5, 34, 32.5, 35, 31.5, 31, 35.5)  # Kischenernte Jahr X 
year_Y.ns <- c(31, 32.5, 31.5, 32, 39, 35, 33, 39.5)    # Kischenernte Jahr Y

# Grafische Darstellung:
# Analog zu ungepaarten Daten als Boxplot oder Histogramme (siehe oben);
# Allerdings wird dabei nicht deutlich, dass die Daten gepaart sind.
# Das muss man wissen oder sich erschließen, z.B. sind Stichprobennamen
# am selben Ort zu verschiedenen Zeiten (wie hier) immer gepaart.
# Für Interessierte gibt es am Ende dieses Scripts den Versuch 
# einer Visualisierung der Gepaartheit auf Basis von fortgeschrittenen 
# tidyverse-Funktionen zur Info (nicht als Inhalt dieses Moduls)

# Gepaarter Zweistichproben-t-Test
(Ergebnis <- t.test(x=year_X, y=year_Y, paired=TRUE))           # Gepaarter t-Test
(Ergebnis.ns <- t.test(x=year_X.ns, y=year_Y.ns, paired=TRUE))  # Gepaarter t-Test (nicht-signifikante Daten)

#--------------------------------------------------------------------      
#  Test auf Normalverteilung
#--------------------------------------------------------------------      

## Shapiro-Wilk Test ## ------------------------------------------------------------------------

par(mfrow=c(1, 2))  # Platz fuer zwei Grafiken nebeneinander schaffen

norm <- rnorm(100, mean=5, sd=3)  # Generiere kuenstlich normalverteilte Daten
hist(norm, col="tomato2", main="normalverteilt")  # Visualisiere Daten als Histogramm         
shapiro.test(norm)   # Shapiro-Wilk Test auf Normalverteilung
                     # Sollte KEINEN signifikanten Unterschied zeigen (p-Wert NICHT kleiner 0.05)
                     # Man kann von normalverteilten Daten ausgehen.

unif <- runif(100, min=2, max=4)  # Generiere kuenstlich gleichverteilte Daten ('uniform distribution')
hist(unif, col="tomato2", main="nicht normalverteilt")  # Visualisiere Daten als Histogramm
shapiro.test(unif)   # Shapiro Test auf Normalverteilung
                     # Sollte einen signifikanten Unterschied zeigen (p-Wert KLEINER 0.05)
                     # Die Daten sind signifikant anders als normalverteilt.

par(mfrow=c(1, 1))  # Grafikeinstellungen zuruecksetzen

## Visuelle Ueberpruefung der Normalverteilung ## ----------------------------------------------

# Da viele statistische Tests relativ robust gegenueber leichten Abweichungen von der 
# Normalverteilung sind, kann unter Umstaenden eine visuelle Bestaetigung ungefaehr
# normalverteilter Daten genuegen. (In diesem Modul muss das von mindestens einem
# Dozenten/einer Dozentin oder Hilfskraft bestaetigt werden.)

# Normalverteilungskurve einzeichnen
par(mfrow=c(1, 2))  # Platz fuer zwei Grafiken nebeneinander schaffen

hist(norm, probability=TRUE, xlab="norm-Variable",  # Kuenstlich erzeugte normalverteilte Daten aus 
     ylab="Relative Haeufigkeit", main="", las=1)   # vorigem Beispiel nutzen und als Histogramm darstellen
curve(dnorm(x, mean=mean(norm), sd=sd(norm)),  # Wahre Normalverteilung in rot einzeichnen
      col="red", add=TRUE, lwd=2)              # -> passt ganz gut

hist(unif, probability=TRUE, xlab="unif-Variable",  # Kuenstlich erzeugte gleichverteilte Daten aus 
     ylab="Relative Haeufigkeit", main="", las=1)   # vorigem Beispiel nutzen und als Histogramm darstellen
curve(dnorm(x, mean=mean(unif), sd=sd(unif)),  # Wahre Normalverteilung in rot einzeichnen
      col="red", add=TRUE, lwd=2)              # -> zeigt deutliche Abweichungen

par(mfrow=c(1, 1))  # Grafikeinstellungen zuruecksetzen

#--------------------------------------------------------------------
# Wilcoxon-Rangsummentest / Mann-Whitney-U-Test
#--------------------------------------------------------------------

# Immer, wenn man den t-Test prinzipiell anwenden koennte,
# aber die Daten die Normalverteilungsannahme des t-Tests verletzen,
# kann man den Wilcoxon-Rangsummentest (heißt auch Mann-Whitney-U-Test) anwenden.

# Am Rande: Man darf diesen Test auch auf normalverteilte Daten anwenden,
# er ist dann aber nicht so testscharf wie der t-Test. Das heisst,
# sehr kleine Unterschiede werden eventuell nur vom t-Test als signifikant erkannt 
# und nicht vom Wilcoxon-Rangsummentest.

# Generiere kuenstliche Daten
# Analog zu Kirschernte-Daten fuer die t-Test-Beispiele
# Gepaarte Ertrags-Daten fuer dieselben 25 Baeume 
# aus zwei aufeinanderfolgenden Jahren Y1 und Y2.
Y1 <- c(31.05776, 31.69364, 33.77563, 33.52916, 35.37559, 31.79224, 
        30.90630, 32.91255, 31.32630, 31.02445, 31.22386, 33.23698, 
        33.64540, 34.21198, 32.67450, 35.00229, 32.73169, 33.02775, 
        31.44812, 34.74847, 30.00516, 30.57577, 32.07847, 30.34449, 35.00733)
Y2 <- c(31.22298, 31.58749, 31.58417, 32.99136, 34.58925, 32.96002,
        32.70426, 36.82496, 33.81405, 35.75746, 34.21165, 36.76244, 
        34.21703, 33.84982,32.01071, 32.26550, 33.22157, 33.10803, 
        32.02771, 31.60143, 32.21559, 34.53319, 36.83190, 34.39646, 36.24789)

# Normalverteilung visuell pruefen
hist(Y1, probability=TRUE, xlab="Y1-Variable", ylab="Relative Haeufigkeit", main="")
curve(dnorm(x, mean=mean(Y1), sd=sd(Y1)), add=TRUE)
# --> Fuer die Daten aus Jahr Y1 nicht gegeben

hist(Y2, probability=TRUE, xlab="Y2-Variable", ylab="Relative Haeufigkeit", main="")
curve(dnorm(x, mean=mean(Y2), sd=sd(Y2)), add=TRUE)
# --> Fuer die Daten aus Jahr Y2 nicht gegeben

# Testen mit einem Test, der keine Normalverteilung der Daten voraussetzt,
# naemlich dem Wilcoxon-Rangsummen-Test
# Achtung, da Daten gepaart sind, muss das Argument paired = TRUE gesetzt werden.
# Bei ungepaarten Daten das Argument paired auf FALSE setzen (Das ist auch der Default, das
# Argument kann also dann auch einfach weggelassen werden)
wilcox.test(Y1, Y2, paired=TRUE)
# -> Ergebnis: p-Wert ist kleiner als 0.05, also gibt es einen signifikanten Unterschied
#              zwischen den Kirscherneertraegen in Jahr Y1 und Jahr Y2.

#--------------------------------------------------------------------
# Varianzanalyse (ANOVA)
#--------------------------------------------------------------------

# Voraussetzungen: 
# x-Achse: kategorielle Variable ('Faktor') mit mindestens zwei Faktorstufen (in R 'levels')
# y-Achse: kontinuierliche Variable
# Modellstruktur: y ~ x
# Am Rande: Hier zeigen wir die einfaktorielle Varianzanalyse; bei der mehrfaktoriellen Varianzanalyse
#           gibt es mehrere x-Achsen, also mehrere Faktoren mit jeweils mindestens zwei Faktorstufen,
#           die die y-Variable erklaeren sollen; in der aov()-Funktion werden die weiteren Variablen
#           einfach mit + (additiver Effekt) oder * (multiplikativer Effekt) hinten angehaengt; daraus
#           ergibt sich die Modellstruktur y ~ x1 + x2 + x3 ... oder y ~ x1 * x2 * x3 ...

# 1. Beispiel: Pflanzenwachstum
library(datasets)  # R-Paket datasets laden, damit eine Reihe von Datensaetzen direkt verfuegbar werden
str(PlantGrowth)  # Beispieldatensatz PlantGrowth (aus dem R-Paket datasets) zusammengefasst anschauen 

# Daten grafisch darstellen 
levels(PlantGrowth$group) <- c("Kontrolle", "Behandlung.1", "Behandlung.2")  # Fuer eine verstaendlichere
                                                                             # Beschriftung der x-Achse
                                                                             # zunaechst die Faktorstufen des
                                                                             # Faktors 'group' neu benennen
plot(PlantGrowth$weight ~ PlantGrowth$group, xlab="Gruppen im Experiment", ylab="Trockengewicht [g]", 
     las=1, col=c("brown", "green1", "green4"), ylim=c(3.5, 7))

# Varianzanalyse (ANOVA) durchfuehren 
fit.1 <- aov(PlantGrowth$weight ~ PlantGrowth$group) # Einfaktorielle Varianzanalyse (One-Way ANOVA)
summary(fit.1)  # Ergebnisansicht der ANOVA
                # Die letzte Spalte 'Pr(>F)' zeigt den p-Wert (=die Wahrscheinlichkeit oder Probility Pr, unter 
                # der Nullhypothese, dass es keinen Unterschied zwischen den Stufen in 'group' gibt,
                # einen 'F-Value' zu erhalten, der größer als der hier in der vorletzten Spalte berechnete 
                # F-Value ist; anders ausgedrückt ist das die Irrtumswahrscheinlichkeit, 
                # wenn ich die Nullhypothese ablehne und einen Unterschied zwischen den Stufen 
                # von 'group' annehme)
                # Da der p-Wert kleiner als 0.05 ist, kann ich die Nullhypothese ablehnen und stattdessen 
                # annehmen, dass mindestens zwischen zwei Stufen des Faktors 'group' 
                # ein signifikanter Unterschied besteht.
                # Der Test sagt allerdings nichts darueber aus, zwischen welchen Stufen 
                # und ob zwischen mehr als einem Stufen-Paar ein Unterschied besteht. Um das herauszufinden,
                # kann der Tukey HSD Posthoc Test genutzt werden:
TukeyHSD(fit.1)  # Tukey HSD posthoc Test 
                 # Interpretationshilfe fuer den Output:
                 # 1. Spalte diff: Differenz in den Mittelwerten zwischen den Fakorstufenpaaren in den Zeilen,
                 #                 z.B. erste Zeile: zwischen Behandlung.1 und Kontrolle
                 # 2. Spalte lwr: Unterer 95%-Vertrauensintervall fuer die Differenz (lower - lwr)
                 # 3. Spalte upr: Oberer 95%-Vertrauensintervall fuer die Differenz (upper - upr)
                 # 4. Spalte p adj: Angepasster p-Wert fuer den Vergleich der Mittelwerte des
                 #                  jeweiligen Faktorstufenpaares (adjusted - adj); 
                 #                  Da man mit diesem Posthoc-Test immer mehrere Vergleiche auf einmal macht
                 #                  (naemlich so viele wie es Zeilen im Output gibt), muss der p-Wert darauf
                 #                  angepasst werden (je mehr Vergleiche, desto wahrscheinlicher bekommt man
                 #                  auch mal per Zufall ein signifikantes Ergebnis und nicht weil es wirklich
                 #                  einen Unterschied gibt; das wird mit dem 'adj' beruecksichtigt)
                 # Das Ergebnis zeigt, dass es tatsaechlich nur einen signifikanten Unterschied 
                 # mit p < 0.05 zwischen den Mittelwerten von zwei Faktorstufen gibt, 
                 # naemlich zwischen Behandlung.1 und Behandlung.2.

# Visuelle Pruefung der Haupt-Annahme der Varianzanalyse,
# naemlich dass die Residuen (nicht die Daten) normalverteilt sind.
# Die Residuen sind sozusagen das, was nach Berechnung der Varianzanalyse uebrig bleibt,
# also der Anteil der y-Variable (hier 'groesse'), der nicht durch die Aufteilung auf die
# Faktorstufen der x-Variable (hier 'geschlecht') erklaert werden kann.
# In R erhaelt man die Residuen mit dem Befehl resid(name.meines.modells)
hist(resid(fit.1), probability=TRUE, ylim=c(0, 0.7), las=1)
curve(dnorm(x, mean=mean(resid(fit.1)), sd=sd(resid(fit.1))), col="red", add = TRUE)
# --> Keine nennenswerte Abweichung von der Normalverteilung zu erkennen
# --> Die Ergebnisse der ANOVA duerfen genutzt werden


# 2. Beispiel: BMI-Daten
fit.2 <- aov(bmi_data$groesse ~ bmi_data$geschlecht) # Varianzanalyse 
summary(fit.2) # Ergebnisansicht der ANOVA
               # --> Es gibt einen signifikanten Unterschied zwischen den Mittelwerten 
               #     der Body height der zwei Faktorstufen 'm' und 'w' (p < 0.05).
               # Am Rande: Da es hier nur einen Faktor mit nur zwei Faktorstufen gibt,
               #           muss kein Posthoc-Test gemacht werden, denn es gibt ja nur 
               #           einen moeglichen Vergleich (zwischen maennlich und weiblich).
# Pruefung auf normalverteilte Residuen
hist(resid(fit.2), probability=TRUE, las=1)
curve(dnorm(x, mean=mean(resid(fit.2)), sd=sd(resid(fit.2))), col="red", add=TRUE)
# --> Keine nennenswerte Abweichung von der Normalverteilung zu erkennen
# --> Die Ergebnisse der ANOVA duerfen genutzt werden

#--------------------------------------------------------------------
# Korrelationsanalyse
#--------------------------------------------------------------------  

# Zugrundeliegende Frage: Gibt es einen Zusammenhang zwischen zwei kontinuierlichen Variablen?
# Achtung! Korrelation bedeutet nicht Kausalitaet! Es kann also nicht festgestellt werden,
#          wer wen beeinflusst und ob nicht einfach beide Variablen von einer dritten Variablen
#          bestimmt werden

# 1. Beispiel: Kuenstlich erzeugte BHD- und Baumhoehendaten
bhd <- c(53, 48, 47, 46, 42, 42.5, 38, 35, 28)      # Brusthoehendurchmesser fuer 9 Baeume
hoehe <- c(27.5, 27, 26,27, 25.5, 25.5, 24, 23, 22) # Baumhoehe derselben 9 Baeume

# Zusammenhang der Variablen bhd und hoehe grafisch darstellen
plot(bhd, hoehe, xlab="Brusthoehendurchmesser in cm", ylab="Baumhoehe in m")

# Korrelation der Variablen bhd und hoehe berechnen
cor(bhd, hoehe) # Korrelationskoeffizient (Pearson's r)
cor(bhd, hoehe, method="spearman") # Rangkorrelationskoeffizient (Spearman's rank)
# Korrelationskoeffizienten interpretieren:
# -1: Maximale negative Korrelation (große Werte der einen Variablen fallen immer mit kleinen Werten 
#                                    der anderen Variable zusammen)
#  0: Keine Korrelation
#
# +1: Maximale positive Korrelation (große Werte der einen Variablen fallen immer mit großen Werten 
#                                    der anderen Variable zusammen und kleine Werte mit kleinen Werten)


# 2. Beispiel: Datensatz 'Iris' (aus dem R-Paket 'datasets', das also mit library() geladen sein muss)
str(iris)
# Variablen: 
# 1. Spalte: Sepal.Length - Laenge der Kelchblaetter
# 2. Spalte: Sepal.Width - Breite der Kelchblaetter
# 3. Spalte: Petal.Length - Laenge der Bluetenblaetter
# 4. Spalte: Petal.Width - Breite der Bluetenblaetter
# 5. Spalte: Species - Artname mit 3 Faktorstufen: "setosa", "versicolor" und "virginica"

# Grafische Darstellung aller moeglichen Kombinationen von Variablen des Datensatzes
# zur visuellen Abschaetzung von potenziellen Korrelationen
plot(iris[, 1:4], col=(iris[, 5]))  
# Bei dieser Darstellung ist die in einer Zeile genannte Variable immer die y-Achse in allen Plots der Zeile
# und die in einer Spalte genannte Variable die x-Achse der Plots in der jeweiligen Spalte.
# Die Farbe steht fuer die Artzugehoerigkeit.
# Je schmaler und linienfoermiger eine Punktwolke ist, 
# desto staerker ist die Korrelation der entsprechenden Variablen.

# Korrelationskoeffizienten in Form einer Korrelationsmatrix,
# also fuer alle potenziell moeglichen Kombinationen von Variablen
cor(iris[, 1:4])  # Pearson's Korrelationskoeffizient r
cor(iris[, 1:4], method="spearman")  # Spearman's Rangkorrelationskoeffizient
cor(iris$Sepal.Length, iris$Petal.Length)  # Beispiel einer Gesamtkorrelation (Pearson's r) aller 
                                           # Kelchblattlaengen mit allen Bluetenblattlaengen im Datensatz

# Alternative mit dem R-Paket 'tidyverse':
iris %>% 
  select(-Species) %>% plot(col=iris %>% 
                              pull(Species))
iris %>% 
  select(-Species) %>% cor()  # Korrelationsmatrix (Pearson's r) 
iris %>% 
  select(-Species) %>% cor(method="spearman")  # Spearman's Rangkorrelationskoeffizient
iris %>% 
  select(Sepal.Length) %>% cor(iris %>% select(Petal.Length))  # Am Rande: Hier ist die klassische Variante etwas übersichtlicher

#--------------------------------------------------------------------
# Lineare Regressionsanalyse
#--------------------------------------------------------------------  

# Voraussetzungen:
# x-Achse: kontinuierliche Variable
# y-Achse: kontinuierliche Variable
# Annahme, dass x-Variable die y-Variable beeinflusst (und nicht umgekehrt), 
# dass also ein kausaler Zusammenhang vorliegt

# Beispiel-Datensatz 'Iris' (aus dem R-Paket 'datasets', das also mit library() geladen sein muss) kurz anschauen
str(iris)

# Grafische Darstellung
# Achtung! x- und y-Variable nicht vertauschen, es wird ja eine Kausalitaet angenommen! - Befehl: plot(y ~ x)  
plot(iris$Sepal.Length ~ iris$Petal.Length, 
     xlab="Laenge der Bluetenblaetter in cm", 
     ylab="Laenge der Kelchblaetter in cm",
     main="Alle drei Irisarten")

# Lineares Regrssionsmodell aufstellen 
# Fragestellunge: Gibt es einen Einfluss der Laenge der Bluetenblaetter auf die Laenge der Kelchblaetter?
mod1 <- lm(iris$Sepal.Length ~ iris$Petal.Length)  
summary(mod1) # Ergbnisansicht fuer das lineare Regressionsmodell
# Interpretationshilfe fuer den Output:
# Interessant sind die Coefficients, das sind:
# die Schaetzung des y-Achsenabschnitts in der ersten Zeile der Tabelle und 
# die Schaetzung der Steigung in der zweiten Zeile der Tabelle
# Estimate:   Die geschaetzten Werte des y-Achsenabschnitts und der Steigung
# Std. Error: Standardfehler zu diesen Schaetzwerten (in etwa: wie schlecht sagt der Schaetzwert den Wert voraus, 
#             der bei einer erneuten Stichprobe geschaetzt werden wuerde)
# t value:    t-Werte fuer diese Schaetzwerte (anhand von Tabellen kann man (bzw. R) herausfinden, 
#             wie extrem der hier berechnete t-Wert ist - je extremer, 
#             desto sicherer ist der Schaetzwert unterschiedlich von Null)
# Pr(>|t|):   p-Wert fuer diese Schaetzwerte, also die Wahrscheinlichkeit (Probability Pr), unter
#             der Nullhypothese, dass der Schaetzwert sich nicht von Null unterscheidet,
#             einen 't value' zu erhalten, der extremer als der hier in der vorletzten Spalte berechnete 
#             t value ist; extremer heisst groesser als der Betrag des t value; 
#             anders ausgedrückt ist das die Irrtumswahrscheinlichkeit, 
#             wenn ich die Nullhypothese ablehne und einen Unterschied zwischen dem Schaetzwert und Null annehme
# Der Output zeigt, dass sowohl der y-Achsenabschnitt als auch die Steigung signifikant von 0 verschieden sind.
# Das ist vor allem bei der Steigung interessant, denn das bedeutet, dass die Laenge der Bluetenblaetter die 
# Laenge der Kelchblaetter signifikant positiv beeinflusst (p < 0.05).

# Regressionsline in den zuletzt erstellten Plot einfuegen
# a = y-Achsenabschnitt
# b = Steigung
# Option 1: a und b als 'Estimates' aus dem summary() des Modells ablesen
abline(a=4.3006, b=0.40892, col="blue")
# Option 2: sich die 'Estimates' von R berechnen lassen
# Das ist genauer (weil mehr Nachkommstellen beruecksichtigt werden) 
# Der erste Koeffizient des Modells ist der y-Achsenabschnitts,
# der zweite Koeffizient ist die Steigung
abline(a=coef(mod1)[1], b=coef(mod1)[2], col="red") 
# Option 3: einfach nur das Modell als Argument uebergeben
# exakt das gleiche Ergebnis wie Option 2
abline(mod1, col = "darkgrey")
# Am Rande: Man kann eingezeichnete Linien nicht ohne weiteres wieder loeschen; 
#           dafuer einfach den Plot nocheinmal zeichnen lassen und nur die gewuenschten
#           (oder gar keine) Linien hinzufuegen.


# Grafische Darstellung 
# Artzugehoerigkeit farblich hervorgehoben
plot(iris$Sepal.Length ~ iris$Petal.Length, 
     col=iris$Species,  
     xlab="Laenge der Bluetenblaetter in cm", 
     ylab="Laenge der Kelchblaetter in cm",
     main="Drei Iris-Arten separat eingefaerbt")

# Je eine separate Lineare Regression pro Art
mod1.a <- lm(Sepal.Length ~ Petal.Length, data=iris[iris$Species == "setosa", ])
mod1.b <- lm(Sepal.Length ~ Petal.Length, data=iris[iris$Species == "versicolor", ])
mod1.c <- lm(Sepal.Length ~ Petal.Length, data=iris[iris$Species == "virginica", ])

# Regressionslinien entsprechend der separaten linearen Regressionsergebnisse pro Art hinzufuegen
abline(mod1.a, col=1)
abline(mod1.b, col=2)
abline(mod1.c, col=3)
# Zum Vergleich noch die Regressionslinie der globalen Regression ueber alle Arten hinzufuegen:
abline(mod1, col="darkgrey")


# R-Paket 'tidyverse' laden
library(tidyverse) # lädt automatisch auch das R-Paket ggplot2 fuer die Visualisierung 

## Zweistichproben-t-Test fuer gepaarte Stichproben ##

# Kuenstliche Daten erzeugen
cherryharvest <- tibble(harvest = c(36, 31.5, 34, 32.5, 35, 31.5, 31, 35.5,
                                    34, 35.5, 33.5, 36, 39, 35, 33, 40,
                                    36, 31.5, 34, 32.5, 35, 31.5, 31, 35.5,
                                    31, 32.5, 31.5, 32, 39, 35, 33, 38),
                        tree_id = factor(c(1, 2, 3, 4, 5, 6, 7, 8,
                                           1, 2, 3, 4, 5, 6, 7, 8,
                                           11, 12, 13, 14, 15, 16, 17, 18,
                                           11, 12, 13, 14, 15, 16, 17, 18)),
                        plantation_id = c("plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1",
                                          "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1", "plantation 1",
                                          "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2",
                                          "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2", "plantation 2"),
                        year = factor(c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
                                        2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019,
                                        2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
                                        2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019))) 
cherryharvest <- cherryharvest |> group_by(tree_id) |>  mutate(differenz = diff(harvest))

# Grafische Darstellung mit ggplot()
theme_set(theme_gray())  # default

cherryharvest_plot <- ggplot(data = cherryharvest, aes(x = year, y = harvest))  + 
  xlab("Year of harvest") +
  ylab("harvest per tree [kg]") + 
  labs(title = "Comparison of cherry harvest 2018 and 2019 ") +
  geom_line(aes(group = tree_id, colour = differenz)) +
  geom_point(size = 4) + 
  scale_colour_gradient(name = "Development of harvest [kg]", low = "red", high = "green") +
  facet_grid(. ~ plantation_id)

cherryharvest_plot

plantation1 <- t.test(x = cherryharvest |> 
                        filter(plantation_id == "plantation 1") |> 
                        filter(year == 2018) |> 
                        pull(harvest), 
                      y = cherryharvest |> 
                        filter(plantation_id == "plantation 1") |> 
                        filter(year == 2019) |> 
                        pull(harvest),
                      paired = TRUE)           # Gepaarter t-Test

plantation2 <- t.test(x = cherryharvest |> 
                        filter(plantation_id == "plantation 2") |> 
                        filter(year == 2018) |> 
                        pull(harvest), 
                      y = cherryharvest |> 
                        filter(plantation_id == "plantation 2") |> 
                        filter(year == 2019) |> 
                        pull(harvest),
                      paired = TRUE)     # Gepaarter t-Test (nicht-signifikante Daten)

# Test results
plantation1 
plantation2



