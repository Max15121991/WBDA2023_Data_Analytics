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

plot(df_base$BS_Cognitive_Well_Being,df_base$Entertainment)

model <- lm(Entertainment)






























# verschieden Datensätze die ich für die Analyse brauche

# Lade die ggplot2-Bibliothek
library(ggplot2)

# Erstelle den Dataframe df_weekly mit der Aggregationsfunktion
df_weekly = aggregate(x = df_input[,c("WS_Performance", "WS_Cognitive_Well_Being")], na.action = na.omit, nan.rm = TRUE, FUN = mean, by = list(Group.id = df_input$ID, WS_week = df_input$WS_week)) #Weekly Survey data for each week 

ggplot(data=df_weekly, aes(x = WS_week))+
  geom_line(aes(y=WS_Performance, color="1"), linetype=1, size=2)+
  geom_line(aes(y=WS_Performance, color="1"), size=4)+




















#Balkendiagramm / Test
ggplot(data=df_weekly, aes(WS_Performance))+
  geom_bar(fill="steelblue")+
  labs(y="WS_week")
  scale_x_continuous(breaks = seq(1,7,1))+
  ggtitle("Titel")
  theme(plot.titel = element_text(hjust = 4))


  # Wenn Sie ein Balkendiagramm bevorzugen, können Sie den folgenden Code verwenden:
  ggplot(data = df_weekly, aes(x = factor(WS_week), y = WS_Performance, fill = factor(WS_Cognitive_Well_Being))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Week", y = "Performance", title = "Performance über 7 Wochen in Abhängigkeit vom Wohlbefinden") +
  scale_x_discrete(labels = seq(1, 7, 1)) +
  theme(plot.title = element_text(hjust = 0.5)) # Anpassen der Titelposition
  







install.packages("ggplot2") 

install.packages("dplyr") 

library(ggplot2)

library(dplyr)

p = ggplot(df_overall, aes(x=Group.date, y=EV_Cognitive_Well_Being)) +
  geom_line() + 
  xlab("")
p # show plot

ids = df_input[df_input$date == "2023-05-30" & df_input$MO_type_of_Activity_planned_Morning == 2, "ID" ]
df = df_input[df_input$ID %in% ids, ]

df = aggregate(x = df[,7:123],na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Group.date = df$date)) #Format it into an timeseries

p = ggplot(df, aes(x=Group.date, y= EV_Cognitive_Well_Being)) +
  geom_line() + 
  xlab("")
p #show plot

p = ggplot(df_single, aes(x=date, y=EV_Cognitive_Well_Being)) +
  geom_line() + 
  xlab("")
p #show plot



mean(df_single$EV_Cognitive_Well_Being, na.rm = TRUE) #na.rm = TRUE is necessary to ignore NA values

mean(df_overall$EV_Cognitive_Well_Being, na.rm = TRUE)


mean(df_overall$MO_Time_to_get_asleep, na.rm = TRUE)

mean(df_single$MO_Time_to_get_asleep, na.rm = TRUE)



aggregate(x = df_base$BS_Cognitive_Well_Being,na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(BS_Week = df_base$BS_week))


aggregate(x = df_weekly$WS_Performance,na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(WS_Week = df_weekly$WS_week))



ids = df_input[df_input$date == "2023-05-30" & df_input$MO_type_of_Activity_planned_Morning == 2, "ID" ]

df = df_base[df_base$Group.id %in% ids, ] #for Base survey(Group.activity = IDs)
aggregate(x = df$BS_Cognitive_Well_Being,na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(BS_Week = df$BS_week))


df = df_weekly[df_weekly$Group.id %in% ids, ] #for weekly survey(Group.activity = IDs)
aggregate(x = df$WS_Cognitive_Well_Being,na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(WS_Week = df$WS_week))

# inferential statistics
# OLS Regression

wlb_ols = lm(df_single$EV_Cognitive_Well_Being ~  df_single$Entertainment + df_single$EV_Activity_Check)

summary(wlb_ols)

wlb_ols = lm(df_overall$EV_Cognitive_Well_Being ~  df_overall$Social.Media)

summary(wlb_ols)

# Analysis of Variance (ANOVA)

aggregate(x = df_input$Entertainment, na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Activity = df_input$MO_type_of_Activity_planned_Morning))

model = aov(aov(df_input$EV_Cognitive_Well_Being ~ df_input$MO_type_of_Activity_planned_Morning))
summary(model)


aggregate(x = df_input$EV_Cognitive_Well_Being, na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Activity = df_input$MO_type_of_Activity_planned_Morning))

TukeyHSD(model)


model = aov(aov(df_input$MO_sleep_time ~ df_input$MO_type_of_Activity_planned_Morning))
summary(model)
TukeyHSD(model)
aggregate(x = df_input$MO_sleep_time, na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Activity = df_input$MO_type_of_Activity_planned_Morning))


model = aov(aov(df_base$BS_Cognitive_Well_Being ~ df_base$BS_week))
summary(model)
TukeyHSD(model)
aggregate(x = df_base$BS_Cognitive_Well_Being, na.action=na.omit, na.rm = TRUE, FUN = mean, by = list(Activity = df_base$BS_week))

# Panel Regression

install.packages("plm")

library("plm")

E = pdata.frame(df_input, index=c("ID","date"), drop.index=TRUE, row.names=TRUE)

head(E)

head(attr(E, "index"))


panel_reg = plm(df_input$MO_Cognitive_Well_Being ~ df_input$MO_sleep_time, data = E, model = "random")

panel_reg = plm(df_input$MO_Cognitive_Well_Being ~ df_input$Social.Media, data = E, model = "random")


summary(panel_reg)
















