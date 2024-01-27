data = read.csv2("/Users/maxfilling/Desktop/WBDA2023/wellbeing_data_v3new2.csv") 
df_input = data

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





df_input = df_input #The main Dataset including all data

df_weekly = aggregate(x = df_input[,91:123],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, WS_week = df_input$WS_week)) #Weekly Survey data for each week 

df_base = aggregate(x = df_input[,63:90],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, BS_week = df_input$BS_week)) #Base survey data for basiseline, break and endline

df_single = df_input[df_input$ID == "ojdoychl", ] #Here you should use your own ID. This ID was randomly picked for demonstration purposes

df_overall = aggregate(x = df_input[,7:120],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.date = df_input$date)) #representation of the complete course overall without differentation between IDs      



df_base = aggregate(x = df_input[,c("Entertainment", "BS_Cognitive_Well_Being")],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, BS_week = df_input$BS_week))

df_weekly = aggregate(x = df_input[,c("WS_Performance", "WS_Cognitive_Well_Being")],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, WS_week = df_input$WS_week))


install.packages("ggplot2") 

install.packages("dplyr") 

#Fall 1
# Laden der benötigten Bibliotheken
library(ggplot2)

# Ihr Datensatz
df_weekly <- aggregate(x = df_input[, c("WS_Performance", "WS_Cognitive_Well_Being")],
                       na.action = na.omit, na.rm = TRUE, FUN = mean,
                       by = list(Group.id = df_input$ID, WS_week = df_input$WS_week))

# Erstellen der Grafik
ggplot(df_weekly, aes(x = WS_week)) +
  geom_line(aes(y = WS_Performance, color = "Performance"), size = 1) +
  geom_line(aes(y = WS_Cognitive_Well_Being, color = "Cognitive Well-Being"), linetype = "dashed", size = 1) +
  geom_point(aes(y = WS_Performance, color = "Performance"), size = 3, shape = 21, fill = "white") +
  geom_point(aes(y = WS_Cognitive_Well_Being, color = "Cognitive Well-Being"), size = 3, shape = 23, fill = "white") +
  labs(x = "Woche", y = "Durchschnittliche Bewertung") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_color_manual(name = "Bewertung",
                     values = c("Performance" = "blue", "Cognitive Well-Being" = "red"),
                     labels = c("Performance", "Cognitive Well-Being")) +
  scale_linetype_manual(name = "Wohlbefinden", values = c("dashed"), guide = guide_legend(override.aes = list(color = c("red")))) +
  theme_minimal()

#Fall2
# Laden der benötigten Bibliotheken
library(ggplot2)

# Ihr Datensatz
df_weekly <- aggregate(x = df_input[, c("WS_Performance", "WS_Cognitive_Well_Being")],
                       na.action = na.omit, na.rm = TRUE, FUN = mean,
                       by = list(Group.id = df_input$ID, WS_week = df_input$WS_week))

# Erstellen der Grafik
ggplot(df_weekly, aes(x = WS_week, y = WS_Performance, size = WS_Cognitive_Well_Being)) +
  geom_point(color = "blue", alpha = 0.8) +
  labs(x = "Woche", y = "Performance", size = "Cognitive Well-Being") +
  scale_y_continuous(limits = c(0, 10)) +
  scale_size_continuous(range = c(3, 10)) +
  theme_minimal()














