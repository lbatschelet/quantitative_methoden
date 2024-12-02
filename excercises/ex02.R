# Übung 2
    
# 1.1

x <- c(5, 2, 1, 4)
xx <- c(1, 10, 15, 18)
z <- c(TRUE, FALSE, TRUE, TRUE)

plot(x,xx)
plot(x[z], xx[z])

plot(x[x != 2], xx[xx != 10])

saison <- read.table("Data/meteodaten_saison.csv", header = TRUE, sep = ",")

str(saison)
head(saison)
summary(saison)

# 2.1 Grafik erstellen

Jahreszeitentabelle <- read.table("Data/meteodaten_saison.csv", 
                     header = TRUE, 
                     sep = ",")

saison_fruehling <- Jahreszeitentabelle[Jahreszeitentabelle[, 2] == "Fruehling(MAM)", ]
saison_sommer <- Jahreszeitentabelle[Jahreszeitentabelle$Saison == "Sommer(JJA)", ]
saison_herbst <- Jahreszeitentabelle[Jahreszeitentabelle$Saison == "Herbst(SON)", ]

plot(saison_fruehling$Jahr, 
     saison_fruehling$Bern_Mitteltemperatur,
     type = "line",
     col = "green",
     xlab = "Jahr",
     ylab = "Mitteltemperatur",
     main = "Mitteltemperatur in Bern nach Jahreszeiten",
     ylim = c(-5, 25)
     )

lines(saison_sommer$Jahr, 
      saison_sommer$Bern_Mitteltemperatur,
      col = "red"
      )

lines(saison_herbst$Jahr,
      saison_herbst$Bern_Mitteltemperatur,
      col = "orange"
      )

lines(saison_winter$Jahr,
      saison_winter$Bern_Mitteltemperatur,
      col = "blue"
      )

legend("topright",
       legend = c("Fruehling", "Sommer", "Herbst", "Winter"),
       col = c("green", "red", "orange", "blue"),
       lty = 1,
       cex = 0.8
       )

abline(mean(saison_fruehling$Bern_Mitteltemperatur),
       0,
       col = "green")




