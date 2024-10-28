meteodaten <- read.csv('Data/meteodaten_saison.csv',
                       sep = ',',
                       header = TRUE)

# Jahresdaten erstellen
# Berechne Jahresmittelwerte und -summen
jahresmitteltemp_bern <- aggregate(meteodaten$Bern_Mitteltemperatur ~ meteodaten$Jahr,
                                   FUN = mean, 
                                   na.rm = TRUE)
jahresmitteltemp_grstbernhard <- aggregate(meteodaten$GrStBernhard_Mitteltemperatur ~ meteodaten$Jahr, 
                                           FUN = mean, 
                                           na.rm = TRUE)
jahresniederschlag_bern <- aggregate(meteodaten$Bern_Niederschlagssumme ~ meteodaten$Jahr, 
                                     FUN = sum, 
                                     na.rm = TRUE)
jahresniederschlag_grstbernhard <- aggregate(meteodaten$GrStBernhard_Niederschlagssumme ~ meteodaten$Jahr, 
                                             FUN = sum, 
                                             na.rm = TRUE)

# Kombiniere die Ergebnisse in eine neue Tabelle
jahresdaten <- data.frame(
  Jahr = jahresmitteltemp_bern$`meteodaten$Jahr`,
  Bern_Mitteltemperatur = jahresmitteltemp_bern$`meteodaten$Bern_Mitteltemperatur`,
  GrStBernhard_Mitteltemperatur = jahresmitteltemp_grstbernhard$`meteodaten$GrStBernhard_Mitteltemperatur`,
  Bern_Niederschlagssumme = jahresniederschlag_bern$`meteodaten$Bern_Niederschlagssumme`,
  GrStBernhard_Niederschlagssumme = jahresniederschlag_grstbernhard$`meteodaten$GrStBernhard_Niederschlagssumme`
)

jahresdaten


hist(jahresdaten$Bern_Mitteltemperatur,
     main = 'Histogramm der Mitteltemperatur in Bern',
     xlab = 'Mitteltemperatur (°C)',
     ylab = 'Anzahl der Jahre',
     xlim = c(6, 12),
     ylim = c(0, 15),
     breaks = 40)

hist(jahresdaten$Bern_Niederschlagssumme,
     main = 'Histogramm der Niederschlagssumme in Bern',
     xlab = 'Niederschlagssumme (mm)',
     ylab = 'Anzahl der Jahre',
     xlim = c(500, 1500),
     ylim = c(0, 10),
     breaks = 40)

mean_temp_bern <- mean(jahresdaten$Bern_Mitteltemperatur)
print(mean_temp_bern)
median_temp_bern <- median(jahresdaten$Bern_Mitteltemperatur)
print(median_temp_bern)
range_temp_bern <- diff(range(jahresdaten$Bern_Mitteltemperatur))
print(range_temp_bern)
sd_temp_bern <- sd(jahresdaten$Bern_Mitteltemperatur)
print(sd_temp_bern)

summary(jahresdaten$Bern_Mitteltemperatur)

round(mean_temp_bern - median_temp_bern, digits = 2)

# 1. Erstelle Klassen für Niederschlagssummen in Bern in 100-mm-Schritten
# Die Spalte 'Saison' enthält die Jahreszeiten (z.B. 'Fruehling(MAM)', 'Sommer(JJA)')
# Die Spalte 'Bern_Niederschlagssumme' enthält die Niederschlagssummen

# Klassen für Niederschlagssummen in 100-mm-Schritten erstellen
meteodaten$Niederschlag_klassen <- cut(
  meteodaten$Bern_Niederschlagssumme,                           # Niederschlagssumme in Bern
  breaks = seq(0, max(meteodaten$Bern_Niederschlagssumme, na.rm = TRUE), by = 100),  # Klassenbreite von 100 mm
  include.lowest = TRUE,                                         # Das unterste Intervall enthält den kleinsten Wert
  right = FALSE                                                  # Links-offen, d.h. Intervalle wie [0,100)
)

# 2. Erstelle eine Kontingenztabelle
# Zeige die Häufigkeit der Niederschlagssummen in den 100-mm-Klassen nach Jahreszeiten (Saison)

kontingenz_tabelle <- table(
  meteodaten$Saison,                 # Spalte für Jahreszeiten
  meteodaten$Niederschlag_klassen    # Erstellte Klassen für Niederschlagssummen
)

# 3. Zeige die Kontingenztabelle an
# Diese Tabelle zeigt die Anzahl der Jahre, in denen die Niederschlagsmengen in die 100-mm-Klassen fallen, aufgeteilt nach Jahreszeiten

print(kontingenz_tabelle)

# Optional: Falls du die Tabelle besser formatieren möchtest, kannst du sie in ein DataFrame konvertieren und dann anzeigen:
kontingenz_df <- as.data.frame.matrix(kontingenz_tabelle)
print(kontingenz_df)



# Bibliothek caTools laden
library(caTools)

# Berechnung des 31-jährigen gleitenden Mittels (Running Mean)
# Die Spalte für die Mitteltemperatur in Bern heisst 'Bern_Mitteltemperatur'
gleitendes_mittel <- runmean(jahresdaten$Bern_Mitteltemperatur, 31, align = "center", endrule = "mean")

# Liniendiagramm erstellen
plot(jahresdaten$Jahr, jahresdaten$Bern_Mitteltemperatur, type = "l", col = "blue",
     xlab = "Jahr", ylab = "Mitteltemperatur in Bern (°C)",
     main = "Erwärmungstrend der Station Bern mit 31-jährigem Gleitendem Mittel")

# Hinzufügen der Gleitenden Mittel-Linie (31-jähriges Running Mean)
lines(jahresdaten$Jahr, gleitendes_mittel, col = "red", lwd = 2)

# Legende hinzufügen
legend("topright", legend = c("Jahresmitteltemperatur", "31-jähriges Gleitendes Mittel"),
       col = c("blue", "red"), lty = 1, lwd = 2)


# Bibliotheken
library(ggplot2)

jahresdaten$Abweichung <- jahresdaten$Bern_Mitteltemperatur - mean(jahresdaten$Bern_Mitteltemperatur[jahresdaten$Jahr >= 1961 & jahresdaten$Jahr <= 1990])

# Erstelle einen Barplot, der die Abweichungen darstellt
ggplot(jahresdaten, aes(x = Jahr, y = Abweichung, fill = Abweichung > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("blue", "red")) +  # Farben: Blau für kälter, Rot für wärmer
  theme_minimal() +
  labs(title = "Jahres-Temperatur Abweichungen – Bern",
       x = "Jahr",
       y = "Abweichung in °C") +
  theme(plot.title = element_text(hjust = 0.5))  # Zentriere den Titel


# Bibliothek
library(ggplot2)

# Beispiel-Daten (Jahr und Temperaturabweichung vom Durchschnitt 1961–1990)
# Wieder verwenden wir die Abweichung aus den vorherigen Daten

# Erstelle die "Warming Stripes"
ggplot(jahresdaten, aes(x = Jahr, y = 1, fill = Abweichung)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("blue", "lightblue", "white", "orange", "red", "darkred")) +
  theme_void() +  # Entfernt Achsen, Titel etc.
  theme(legend.position = "none") +
  labs(title = "Schweizer Temperatur seit 1864")



