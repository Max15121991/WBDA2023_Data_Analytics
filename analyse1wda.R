
#Datensätzeladen:

data = read.csv2("/Users/maxfilling/Desktop/WBDA2023/wellbeing_data_v3new2.csv") 
df_input = data



#Fertig



df_input$date = as.Date(df_input$date)

summary(df_input) 
str(df_input)

for(i in 7:23){ #7 is the first column with numbers
  
  df_input[,i] = as.numeric(df_input[,i]) 
  
}

for(i in 28:123){ #123 is the last column with numbers
  
  df_input[,i] = as.numeric(df_input[,i]) 
  
}

df_input$BS_week = as.factor(df_input$BS_week)
df_input$WS_week = as.factor(df_input$WS_week)
df_input$MO_type_of_Activity_planned_Morning = as.factor(df_input$MO_type_of_Activity_planned_Morning)
df_input$MO_Activity_planned_todo_Morning = as.factor(df_input$MO_Activity_planned_todo_Morning)


df_input$MO_Time_to_get_asleep[is.na(df_input$MO_Time_to_get_asleep) | df_input$MO_Time_to_get_asleep >= 300] = NA #Replace all entries higher than 5 hours with NA

summary(df_input$MO_Time_to_get_asleep)


# Erstelle den Dataframe df_weekly mit der Aggregationsfunktion
df_weekly = aggregate(x = df_input[,c("WS_Performance", "WS_Cognitive_Well_Being")], na.action = na.omit, nan.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, WS_week = df_input$WS_week)) #Weekly Survey data for each week 
data_clean <- na.omit(df_weekly) # hier wurden die NA rausgenommen

#Test
#Balkendiagramm / Test

ggplot(data=data_clean, aes(WS_week))+
ggplot(data=data_clean, aes(WS_Performance))+
  geom_bar(fill="steelblue")+
  labs(y="Verlauf")
scale_x_continuous(breaks = seq(1,7,1))+
  ggtitle("Titel")
theme(plot.titel = element_text(hjust = 4))

# Lade die erforderlichen Pakete
library(ggplot2)

# Erstelle das ggplot-Diagramm
ggplot(data = data_clean, aes(x = WS_week, y = WS_Performance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Kalenderwoche", y = "Verlauf") +
  ggtitle("Verlauf der Performance nach Kalenderwochen") +
  scale_x_continuous(breaks = seq(1, 7, 1)) +
  theme(plot.title = element_text(hjust = 0.5))

# Lade die erforderlichen Pakete
library(ggplot2)

# Konvertiere WS_week in einen Faktor
data_clean$WS_week <- factor(data_clean$WS_week)

# Erstelle das ggplot-Diagramm
ggplot(data = data_clean, aes(x = WS_Performance, y = WS_week)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Kalenderwoche", y = "Verlauf") +
  ggtitle("Verlauf der Performance nach Kalenderwochen") +
  scale_x_discrete(labels = seq(1, 7, 1)) +
  theme(plot.title = element_text(hjust = 0.5))

#Grafiken:

#Gruppen-Performence:!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Lade die erforderlichen Pakete
library(ggplot2)

# Konvertiere WS_week in einen Faktor
data_clean$WS_week <- factor(data_clean$WS_week)

# Berechne den Mittelwert der Performance pro Woche
mean_performance <- aggregate(WS_Performance ~ WS_week, data = data_clean, mean)

# Füge eine lineare Trendlinie hinzu
trend <- lm(WS_Performance ~ as.numeric(WS_week), data = mean_performance)

# Erstelle das ggplot-Diagramm mit dem Mittelwert der Performance pro Woche und der Trendlinie
ggplot(data = mean_performance, aes(x = WS_week, y = WS_Performance, fill = WS_Performance)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = round(WS_Performance, 2)), vjust = -0.5, size = 3) +  # Füge die Durchschnittswerte über jedem Balken hinzu
  geom_smooth(method = "lm", formula = y ~ as.numeric(x), se = FALSE, color = "red", linetype = "dashed") +
  geom_line(aes(group = 1), color = "blue", size = 1.5) +  # Füge die Linie hinzu, indem du aes(group = 1) verwendest, um alle Werte zu einer Linie zu verbinden
  labs(x = "Kalenderwoche (X)", y = "Durchschnittliche Performance (Y)") +
  ggtitle("Gruppen-Performance über 7 Wochen") +
  scale_fill_gradient(low = "grey", high = "red", name = "Trend") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Hier haben wir die Schriftgröße auf 14 und die horizontale Justierung auf 0.5 (Mitte) gesetzt
        axis.text.y = element_text(size = 12, color = "black"),  # Größe und Farbe der Achsentexte anpassen
        axis.text.x = element_text(size = 12, color = "black"),  # Größe und Farbe der Achsentexte anpassen
        axis.title = element_text(size = 14, face = "bold"),  # Größe und Schriftart der Achsentitel anpassen
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),  # Größe und Schriftart der Legendentitel anpassen
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1.5, color = "black"),
        panel.background = element_rect(fill = "white"),  # Hintergrundfarbe des Diagramms auf weiß setzen
        plot.background = element_rect(fill = "white"))  # Hintergrundfarbe des gesamten Diagramms auf weiß setzen


#Einzel-Performence zur Gruppe

# Lade die erforderlichen Pakete
library(ggplot2)

# Filtere die Daten für die Gruppe "Group.id ojdoychl"
group_data <- subset(data_clean, Group.id == "ojdoychl")

# Konvertiere WS_week in einen Faktor
group_data$WS_week <- factor(group_data$WS_week)

# Berechne den Mittelwert der Performance pro Woche
mean_performance <- aggregate(WS_Performance ~ WS_week, data = group_data, mean)

# Füge eine lineare Trendlinie hinzu
trend <- lm(WS_Performance ~ as.numeric(WS_week), data = mean_performance)

# Erstelle das ggplot-Diagramm mit dem Mittelwert der Performance pro Woche und der Trendlinie
ggplot(data = mean_performance, aes(x = WS_week, y = WS_Performance, fill = WS_Performance)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_text(aes(label = round(WS_Performance, 2)), vjust = -0.5, size = 3) +  # Füge die Durchschnittswerte über jedem Balken hinzu
  geom_smooth(method = "lm", formula = y ~ as.numeric(x), se = FALSE, color = "red", linetype = "dashed") +
  geom_line(aes(group = 1), color = "blue", size = 1.5) +  # Füge die Linie hinzu, indem du aes(group = 1) verwendest, um alle Werte zu einer Linie zu verbinden
  labs(x = "Kalenderwoche (X)", y = "Durchschnittliche Performance (Y)") +
  ggtitle("Einzel-Performance über 7 Wochen") +
  scale_fill_gradient(low = "grey", high = "red", name = "Trend") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Hier haben wir die Schriftgröße auf 14 und die horizontale Justierung auf 0.5 (Mitte) gesetzt
        axis.text.y = element_text(size = 12, color = "black"),  # Größe und Farbe der Achsentexte anpassen
        axis.text.x = element_text(size = 12, color = "black"),  # Größe und Farbe der Achsentexte anpassen
        axis.title = element_text(size = 14, face = "bold"),  # Größe und Schriftart der Achsentitel anpassen
        legend.position = "right",
        legend.title = element_text(size = 12, face = "bold"),  # Größe und Schriftart der Legendentitel anpassen
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1.5, color = "black"),
        panel.background = element_rect(fill = "white"),  # Hintergrundfarbe des Diagramms auf weiß setzen
        plot.background = element_rect(fill = "white"))  # Hintergrundfarbe des gesamten Diagramms auf weiß setzen


# Multiple lineare Regression

# Datensatz erstellen
df_base = aggregate(x = df_input[,c("Social.Media","BS_Cognitive_Well_Being","Entertainment")],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, BS_week = df_input$BS_week))
data_base_clean <- na.omit(df_base) # hier wurden die NA rausgenommen


#plot(data_base_clean$BS_Cognitive_Well_Being, data_base_clean$Entertainment)

# 1Normalverteilung Residuen
#plot(modell, 2)

# 2a Homoskedastizität
#plot(modell, 1)


#Test
plot(data_base_clean$BS_Cognitive_Well_Being,data_base_clean$Entertainment)

model <- lm(Entertainment~BS_Cognitive_Well_Being, data = data_base_clean)

abline(model, col="red")

summary(model)


#Beispiele:

#Gruppen
#Fall1 / Entertainment
# Zuerst ggplot2 installieren, falls noch nicht vorhanden
# install.packages("ggplot2")
library(ggplot2)

# Streudiagramm mit Regressionsgerade erstellen
ggplot(data_base_clean, aes(x = BS_Cognitive_Well_Being, y = Entertainment)) +
  geom_point(size = 2) +  # Punkte kleiner machen
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(x = "BS_Cognitive Well-Being pro Woche (X)", y = "Screentime_Entertainment in Sek (Y)") +
  ggtitle("Gesamte Gruppe (Beziehung)") +
  theme_light() +  # Helleres Theme verwenden
  scale_x_continuous(breaks = c(2, 4, 6), labels = c(1, 4, 7)) +  # Skalierung der x-Achse ändern
  theme(plot.title = element_text(hjust = 0.5))  # Titel in die Mitte setzen

# Annahme: Du hast bereits ein lineares Regressionsmodell erstellt
model <- lm(Entertainment ~ BS_Cognitive_Well_Being, data = data_base_clean)
summary(model)


#Fall2 Social.Media

# Zuerst ggplot2 installieren, falls noch nicht vorhanden
# install.packages("ggplot2")
library(ggplot2)

# Streudiagramm mit Regressionsgerade erstellen
ggplot(data_base_clean, aes(x = BS_Cognitive_Well_Being, y = Social.Media)) +
  geom_point(size = 2) +  # Punkte kleiner machen
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(x = "BS_Cognitive Well-Being pro Woche (X)", y = "Screentime_Social.Media in Sek (Y)") +
  ggtitle("Gesamte Gruppe (Beziehung)") +
  theme_light() +  # Helleres Theme verwenden
  scale_x_continuous(breaks = c(2, 4, 6), labels = c(1, 4, 7)) +  # Skalierung der x-Achse ändern
  theme(plot.title = element_text(hjust = 0.5))  # Titel in die Mitte setzen

# Annahme: Du hast bereits ein lineares Regressionsmodell erstellt
model <- lm(Social.Media ~ BS_Cognitive_Well_Being, data = data_base_clean)
summary(model)

#Einzel

#Datensatz

data_single_clean <- na.omit(df_single) # hier wurden die NA rausgenommen


# Fall 1 / Entertainment
# Zuerst ggplot2 installieren, falls noch nicht vorhanden
# install.packages("ggplot2")
library(ggplot2)

# Filter data for the specific Group.id: ojdoychl
data_specific_person <- subset(data_base_clean, Group.id == "u4x8dwkv") 

# Streudiagramm mit Regressionsgerade erstellen for the specific person
ggplot(data_specific_person, aes(x = BS_Cognitive_Well_Being, y = Entertainment)) +
  geom_point(size = 2) +  # Punkte kleiner machen
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(x = "BS_Cognitive Well-Being pro Woche (X)", y = "Screentime_Entertainment in Sek (Y)") +
  ggtitle("Einzelne Person (Beziehung)") +
  theme_light() +  # Helleres Theme verwenden
  scale_x_continuous(breaks = c(1, 4, 7), labels = c(1, 4, 7)) +  # Skalierung der x-Achse ändern
  theme(plot.title = element_text(hjust = 0.5)) +  # Titel in die Mitte setzen
  geom_vline(xintercept = c(1, 4, 7), linetype = "dashed", color = "blue")  # Vertikale Linien einzeichnen

# Annahme: Du hast bereits ein lineares Regressionsmodell erstellt
model <- lm(Entertainment ~ BS_Cognitive_Well_Being, data = data_base_clean)
summary(model)


# Fall 2 / Social.Media
# Zuerst ggplot2 installieren, falls noch nicht vorhanden
# install.packages("ggplot2")
library(ggplot2)

# Filter data for the specific Group.id: ojdoychl
data_specific_person <- subset(data_base_clean, Group.id == "u4x8dwkv") 

# Streudiagramm mit Regressionsgerade erstellen for the specific person
ggplot(data_specific_person, aes(x = BS_Cognitive_Well_Being, y = Social.Media)) +
  geom_point(size = 2) +  # Punkte kleiner machen
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(x = "BS_Cognitive Well-Being pro Woche (X)", y = "Screentime_Social.Media in Sek (Y)") +
  ggtitle("Einzelne Person (Beziehung)") +
  theme_light() +  # Helleres Theme verwenden
  scale_x_continuous(breaks = c(1, 4, 7), labels = c(1, 4, 7)) +  # Skalierung der x-Achse ändern
  theme(plot.title = element_text(hjust = 0.5)) +  # Titel in die Mitte setzen
  geom_vline(xintercept = c(1, 4, 7), linetype = "dashed", color = "blue")  # Vertikale Linien einzeichnen

# Annahme: Du hast bereits ein lineares Regressionsmodell erstellt
model <- lm(Social.Media ~ BS_Cognitive_Well_Being, data = data_base_clean)
summary(model)

#Laufzeiten für Marathon ploten


# Lade das ggplot2-Paket, falls noch nicht installiert
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Die Laufzeiten in Minuten pro Woche



# Lade das ggplot2-Paket, falls noch nicht installiert
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# Die Laufzeiten in Minuten pro Woche
wochen <- c(1, 2, 3, 4)
laufzeiten <- c(90, 80, 80, 122)

# Erstelle ein Dataframe
laufzeit_df <- data.frame(Woche = wochen, Laufzeit = laufzeiten)

# Die Zielezeit für den Halbmarathon am 25.06.2023
zielzeit_halbmarathon <- 122

# Erstelle die Grafik
library(ggplot2)

ggplot(data = laufzeit_df, aes(x = Woche, y = Laufzeit, group = 1)) +
  geom_line(size = 1.5, color = "black") +
  geom_point(size = 3, color = "black") +
  geom_segment(aes(x = 3, xend = 4, y = 80, yend = 122), color = "black", linetype = "dashed") +
  geom_point(aes(x = 4, y = 122), color = "darkred", size = 3) +
  labs(x = "Woche", y = "Laufzeit (Minuten)", title = "Verlauf der Laufzeiten pro Woche",
       subtitle = "Vorbereitung für den Halbmarathon am 25.06.2023") +
  annotate("text", x = 3.5, y = 100, label = "15 km", color = "black") +
  annotate("text", x = 3.5, y = 122, label = "Halbmarathon (21.1 km)", color = "darkred") +
  geom_hline(yintercept = zielzeit_halbmarathon, color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = wochen) +
  scale_y_continuous(limits = c(0, 140), breaks = seq(0, 140, by = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 10),
        axis.title = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        panel.border = element_rect(color = "black", size = 1, fill = NA))





#Herzfrequenz messsung über 6 Wochen der Wellnes 

# Installieren und Laden von erforderlichen Paketen
#install.packages("ggplot2")
library(ggplot2)

# Erstellen eines Datensatzes für die Herzfrequenz
wochen <- c(1:6)
herzfrequenz <- c(57, 55, 56, 54, 51, 50)

# Berechnen des Durchschnitts
durchschnitt <- mean(herzfrequenz)

# Datensatz zusammenstellen
herzfrequenz_datensatz <- data.frame(Wochen = wochen, Herzfrequenz = herzfrequenz)

# Erstellen der Grafik
grafik <- ggplot(herzfrequenz_datensatz, aes(x = Wochen, y = Herzfrequenz)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  geom_hline(yintercept = durchschnitt, linetype = "dashed", color = "red") +
  labs(title = "Ruheherzfrequenz während 6. Wöchigen Wellness Activity",
       x = "Wochen",
       y = "Herzfrequenz (Schläge pro Minute)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),  # Titelgröße auf 14 setzen
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        plot.background = element_rect(color = "black", fill = NA, size = 1))

# Grafik mit Rahmen anzeigen
print(grafik)


# Installieren und Laden von erforderlichen Paketen
#install.packages("ggplot2")
library(ggplot2)

# Erstellen eines Datensatzes für die Herzfrequenz
wochen <- c(1:6)
herzfrequenz <- c(57, 55, 56, 54, 51, 50)

# Berechnen des Durchschnitts
durchschnitt <- mean(herzfrequenz)

# Datensatz zusammenstellen
herzfrequenz_datensatz <- data.frame(Wochen = wochen, Herzfrequenz = herzfrequenz)

# Erstellen der Grafik
grafik <- ggplot(herzfrequenz_datensatz, aes(x = Wochen, y = Herzfrequenz)) +
  geom_line(color = "blue") +
  geom_point(color = "blue", size = 3) +
  geom_hline(yintercept = durchschnitt, linetype = "dashed", color = "red") +
  labs(title = "Ruheherzfrequenz während der 6. Wöchigen Wellness Activity",
       x = "Wochen",
       y = "Herzfrequenz (Schläge pro Minute)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10, hjust = 0.5),  # Titelgröße auf 14 setzen
        plot.subtitle = element_text(hjust = 0.5),
        panel.border = element_blank(),  # Äußeren Rahmen entfernen
        plot.background = element_rect(color = "black", fill = NA, size = 1))  # Inneren Rahmen beibehalten

# Grafik ohne äußeren Rahmen anzeigen
print(grafik)



#Grafik fasting, usw

# Installieren des readxl-Pakets, wenn es noch nicht installiert ist
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

# Laden der benötigten Bibliotheken
library(readxl)
library(ggplot2)

# Annahme: Die Daten sind bereits in einer Excel-Datei namens 'wellbeing_data_v3new4.xlsx' vorhanden

# Daten laden und filtern für ID='deine ID'
data <- read_excel("/Users/maxfilling/Desktop/WBDA2023/wellbeing_data_v3new4.xlsx")
filtered_data_zuejamns <- subset(data, ID == 'ojdoychl')

# Gruppieren nach WS_week und MO_type_of_Activity_planned_Morning und Durchschnitt berechnen
grouped_data_all <- aggregate(WS_Cognitive_Well_Being ~ WS_week + MO_type_of_Activity_planned_Morning, data, mean)

# Gruppieren nach WS_week und Durchschnitt berechnen für ID='deine ID'
grouped_data_ID <- aggregate(WS_Cognitive_Well_Being ~ WS_week, filtered_data_zuejamns, mean)

# Mapping für die Aktivitätstypen
activity_mapping <- c('None' = 1, 'Fasting' = 2, 'Indulging' = 3, 'Reflection' = 4)

# Erstellen des Diagramms
ggplot() +
  # Schleife über die verschiedenen Aktivitätstypen
  geom_line(data = grouped_data_all, aes(x = WS_week, y = WS_Cognitive_Well_Being, group = MO_type_of_Activity_planned_Morning, color = factor(MO_type_of_Activity_planned_Morning)), linetype = 'solid') +
  # Linie für ID='deine ID'
  geom_line(data = grouped_data_ID, aes(x = WS_week, y = WS_Cognitive_Well_Being), linetype = 'dashed', color = 'black') +
  scale_color_manual(values = c('blue', 'red', 'green', 'purple'), labels = c('Keine', 'Fasten', 'Genießen', 'Reflexion')) +
  xlab('WS_week') +
  ylab('WS_Cognitive_Well_Being') +
  ggtitle('Durchschnittliches kognitives Wohlbefinden im Zeitraum') +
  theme_minimal()


# Laden der benötigten Bibliotheken
library(ggplot2)

# Annahme: Die Daten sind bereits in einer CSV-Datei namens 'wellbeing_data.csv' vorhanden

# Daten laden und filtern für ID='deine ID'
data <- read.csv("/Users/maxfilling/Desktop/WBDA2023/wellbeing_data_v3new3.csv")
filtered_data_zuejamns <- subset(data, Group.id == 'ojdoychl')

# Gruppieren nach WS_week und MO_type_of_Activity_planned_Morning und Durchschnitt berechnen
grouped_data_all <- aggregate(WS_Cognitive_Well_Being ~ WS_week + MO_type_of_Activity_planned_Morning, data, mean)

# Gruppieren nach WS_week und Durchschnitt berechnen für ID='deine ID'
grouped_data_ID <- aggregate(WS_Cognitive_Well_Being ~ WS_week, filtered_data_zuejamns, mean)

# Mapping für die Aktivitätstypen
activity_mapping <- c('None' = 1, 'Fasting' = 2, 'Indulging' = 3, 'Reflection' = 4)

# Erstellen des Diagramms
ggplot() +
  # Schleife über die verschiedenen Aktivitätstypen
  geom_line(data = grouped_data_all, aes(x = WS_week, y = WS_Cognitive_Well_Being, group = MO_type_of_Activity_planned_Morning, color = factor(MO_type_of_Activity_planned_Morning)), linetype = 'solid') +
  # Linie für ID='deine ID'
  geom_line(data = grouped_data_ID, aes(x = WS_week, y = WS_Cognitive_Well_Being), linetype = 'dashed', color = 'black') +
  scale_color_manual(values = c('blue', 'red', 'green', 'purple'), labels = c('Keine', 'Fasten', 'Genießen', 'Reflexion')) +
  xlab('WS_week') +
  ylab('WS_Cognitive_Well_Being') +
  ggtitle('Durchschnittliches kognitives Wohlbefinden im Zeitraum') +
  theme_minimal()














