# Bruderhahn #

# Distribution ####

# data structure ###
library(readxl)
#Pfad von G nehmen, damit Yamenah das auch einlesen kann!
Distribution <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Distribution_R.xlsx")

#Pen und Treatments als Faktor setzen, weil wir brauchen Pen als random term 
str(Distribution)
Distribution$pen <- as.factor(Distribution$pen)
Distribution$treat <- as.factor(Distribution$treat)
str(Distribution)

summary(Distribution)
summary(Distribution$pen)
## Visualisierung ####

#jitter -> bricht Punkte auf, die dieselben sind 
library(ggplot2)
ggplot(Distribution, aes(timecut, `second floor inkl. Top ramp`,colour = treat)) +
  geom_point() +   geom_jitter()

ggplot(Distribution, aes(timecut, `second floor inkl. Top ramp`,fill = treat)) +
  geom_boxplot()#+
  #facet_grid(~woa)


#Access ####

# data structure ###
library(readxl)
#Pfad von G nehmen, damit Yamenah das auch einlesen kann!
access <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/access_R.xlsx")

#Pen und Treatments als Faktor setzen, weil wir brauchen Pen als random term 
str(access)
access$pen <- as.factor(access$pen)
access$treat <- as.factor(access$treat)
access$time <- as.factor(access$time)
str(access)

summary(access)
summary(access$pen)
## Visualisierung ####

#jitter -> bricht Punkte auf, die dieselben sind 
library(ggplot2)
ggplot(access, aes(time, `1 to 2 floor ramp`,colour = treat)) +
  geom_point() +   geom_jitter()

ggplot(access, aes(time, `1 to 2 floor ramp`,fill = treat)) +
  geom_boxplot()+
facet_grid(~woa)


#Feed ####

library(readxl)
Test <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Futter/Test.xlsx")
View(Test)

summary(Test)
FV_AW4 <- Test[, c(2,7)]
head(FV_AW4)
summary(FV_AW4)

library(readr)
anova <- aov(`Futterverbrauch AW 3-4 [kg]` ~ Verfahren, data = FV_AW4)
summary(anova)

#Ergebnis ist signifikant, Tukeys HDS um zu schauen, welche Gruppen sich signifikant unterscheiden

tukey <- TukeyHSD(anova)
print(tukey)

#Health ####
