#Final code for analysis 

#benutzte Packages mti #
library(readxl)#
library(ggplot2)#
library(tidyr)#
library(lme4)#
library(boot)
library(parameters)
library(emmeans)
library(performance)
library(car)
library(lmerTest)
library(DHARMa)#
library(pbkrtest)

#Ramp use ####-------------------------------------------------------------------------------------------------------------------------------------------------------------------
#male_move<- read_excel("D:/Projekt_Bruderhaehne/Auswertung/Bruderhahn_Analysis/Moving_R.yg.xlsx")
male_move<- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/ramp_use_males.nz.xlsx")
#male_move <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/ramp_use_males.nz.xlsx")

str(male_move)
male_move$pen <- as.factor(male_move$pen)
male_move$treat <- as.factor(male_move$treat)
male_move$time <- as.factor(male_move$time)
male_move$direction <- as.factor(male_move$direction)
male_move$ramp_use <- male_move$ramp / male_move$total
male_move$ramp_use <- as.numeric(male_move$ramp_use)
male_move$woa <- as.integer(male_move$woa)
View(male_move)
maleR <- subset(male_move, treat =="M+") # subset dateset for M+ only
View(maleR)


##Visualisiserung ####


maleR_filtered <- maleR %>%
  #filter(!is.na(direct), !is.na(ramp)) %>% # Entfernt NAs aus direct und rampe
  mutate(`time` = factor(`time`, levels = c("dawn", "mid", "dusk")))%>%   # Reihenfolge defin
  pivot_longer(cols = c(direct, ramp), names_to = "group", values_to = "value") # Daten umformen


# Plot mit beiden Gruppen (direct und rampe)
ggplot(maleR_filtered, aes(x = time, y = value, fill = interaction(group, direction))) +
  geom_boxplot(aes(color = interaction(group, direction)), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  facet_grid(~woa) + 
  scale_fill_manual(
    values = c("lightblue", "darkblue"),
    labels = c("Male", "Male add. ramp", "Female")) +
  scale_color_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female"))+
  scale_x_discrete(labels = c(
  theme_minimal() +
  labs(
    title = "Percentage of males using ramps (direct and rampe)",
    x = "time",
    y = "count of movement",
    fill = "group",
    color = "group"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5), # Titel zentrieren
    strip.text = element_text(face = "bold") # Facettenüberschrift hervorheben
  )



# ggplot of percentage of birds using or not using ramps by movement direction and woa

library(dplyr)

maleR_filtered <- maleR %>%
  #filter(!is.na(ramp_use)) %>%
  mutate(`time` = factor(`time`, levels = c("dawn", "mid", "dusk")))   

ggplot(maleR_filtered, aes(x = time, y = `ramp_use`, fill = direction)) +
  geom_boxplot(aes(color = direction), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  facet_grid(~woa) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Percentage of males using ramps",
    x = "time",
    y = "using of ramp [%]",
    fill = "direction",
  ) +
  theme(plot.title = element_text(hjust = 0.5))



#Feed ####-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Visualisiserung ####

feed_all <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")
feed_all <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")

#Normaler Futterverbrauch gemäss Aviforum
norm <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Norm_futter_Gewicht.xlsx")
norm_feed <- subset(norm,select = c(woa,FV_2_wochen))
summary(feed_all)

feed_tot <- feed_all[,c(-1)]
names(feed_tot)
table(feed_tot)
feed_tot= subset(feed_tot, woa !="13")
feed_tot$feed_animal <- feed_tot$feed *1000 / 20 
str(feed_tot)
summary(feed_tot)
View(feed_tot)
str(feed_tot)
feed_tot$woa <- as.numeric(feed_tot$woa) #evtl. kontinuierlich
feed_tot$pen <- as.factor(feed_tot$pen)
feed_tot$treat <- as.factor(feed_tot$treat)
feed_tot$feed <- as.numeric(feed_tot$feed)


### ganzes Pen ####

ggplot(feed_tot, aes(x = factor(woa),feed, fill=treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  scale_fill_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female")) +
  scale_color_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female"))+
  scale_x_discrete(labels = c(
    "2" = "1-2",
    "4" = "3-4",
    "6" = "5-6",
    "8" = "7-8",
    "10" = "9-10",
    "12" = "11-12" ))+
  theme_minimal() +
  labs(
    title = "Feed use per pen of 20 birds",
    y = "Feed use [kg]",
    x = "Week of age",
    fill = "Experimental groups",
    color = "Experimental groups"
  ) +
  theme(
    plot.title = element_text(hjust = 0.75),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.title.x = element_text(margin = margin(t = 10)))

### Einzeltier in g ####

ggplot(feed_tot, aes(x = factor(woa),feed_animal, fill=treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  scale_fill_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female")) +
  scale_color_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female"))+
  scale_x_discrete(labels = c(
    "2" = "1-2",
    "4" = "3-4",
    "6" = "5-6",
    "8" = "7-8",
    "10" = "9-10",
    "12" = "11-12" ))+
  theme_minimal() +
  labs(
    title = "Feed use per bird",
    y = "Feed use [g]",
    x = "Week of age",
    fill = "Experimental groups",
    color = "Experimental groups"
  ) +
  theme(
    plot.title = element_text(hjust = 0.75),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.title.x = element_text(margin = margin(t = 10)))

# nur woa 1-5 statistik ausrechnen, bei Plot die hinderen drei woa grau hinterlegen, wenn i.o. auch letzter Zeitpunkt nehmen
# Plot Linie mit Normalverbrauch für Hennen einfügen -> Durchschnitt und range, falls vorhanden



## Statistik ####

feed_tot_less_woa <-subset(feed_tot,woa<7)
str(feed_tot_less_woa)

model_Futter <- lmer(feed~(treat+as.factor(woa))^2 + (1|pen), data =feed_tot_less_woa) # 3fach Interaktion ()^3 nicht sign. kann ersetzt werden durch ^2 because variance explained by pen = 0 ==> removal of random term:
anova(model_Futter)
#Global average effekt: Wie viel erklärt mein volles Modell zu keinem Modell
model_Futter0 <- lmer(feed~ 1+ (1|pen), data =feed_tot_less_woa) # Intercept only model, nur random effekt wird betrachtet
summary(model_Futter0) #Pen ist 0 -> erklärt das Modell nicht
modell.null.p <- PBmodcomp (model_Futter, model_Futter0) 
summary (modell.null.p) #unser Modell ist signifikant unterschieden zu einem leren Modell -> Ist gut, unser Model erklärt

### model assumptions ####
par (mfrow= c (3, 3)) # alle Teilabbildungen gemeinsam anschauen
qqnorm (resid (model_Futter))
qqline(resid(model_Futter))
qqnorm (unlist (ranef (model_Futter, level= 1))) # für jeden geschachtelten zufäll. Effekt
scatter.smooth (fitted (model_Futter), resid (model_Futter))
boxplot (split (resid (model_Futter), feed_tot [, 'pen'])) # für jede erklärende Variable

### model ####
summary(model_Futter)

#Random effects:
#Groups   Name        Variance  Std.Dev.
#pen      (Intercept) 1.721e-05 0.004149
#Residual             3.540e-04 0.018814 -> Pen erklärt 4.6 % der Varianz, deshalb lassen wir es drin als random term (ab rund 5%)

plot(feed_tot$pen, feed_tot$feed) # varianz durch pen sehr klein, fast 0



#p-Wert(e) Einzeleffekte berechnen

anova (model_Futter, type= 'marginal')

anova (model_Futter)
#für jeden Haupteffekt und jede Interaktion (je einmal weglassen): (habe ich noch nicht gemacht: xxx jeweils durch die weggelassene Variable ersetzen)
modell.tw.lmer <- lmer (feed ~ treat + # einmal weglassen: Haupteffekt VarA1
                           
                           woa + # einmal weglassen: Haupteffekt VarB1
                           
                           #treat:woa + # einmal weglassen: 1. Zweifachinteraktion
                           
                           (1 | pen),
                         
                         feed_tot_less_woa, REML= FALSE)

summary (modell.tw.lmer)

# Bootstrap test (müsste gleich sein wie der anova test)
modell.tw.p <- PBmodcomp (model_Futter, modell.tw.lmer)
summary (modell.tw.p)
#LRT      55.687  2.000        8.085e-13 *** #-> dasselbe wie anova, für den Bericht relevant
#PBtest   55.687                0.000999 *** #-> zeigt noch die Robustheit an 
anova(modell.tw.lmer, model_Futter)

modell.w.lmer <- lmer (feed ~ treat + # einmal weglassen: Haupteffekt VarA1
                           
                           #woa + # einmal weglassen: Haupteffekt VarB1
                           
                           treat:woa + # einmal weglassen: 1. Zweifachinteraktion
                           
                           (1 | pen),
                         
                         feed_tot_less_woa, REML= FALSE)

summary (modell.w.lmer)
modell.tw.p <- PBmodcomp (model_Futter, modell.w.lmer)
summary (modell.w.p)
#Diese Interaktion ist auch signifikant

#Untersuchen der Hauptfaktoren, weil die Interaktion nur sign. sein soll, weil 
modell.treat.lmer <- lmer (feed ~ #treat + # einmal weglassen: Haupteffekt VarA1
                          
                          woa + # einmal weglassen: Haupteffekt VarB1
                          
                          treat:woa + # einmal weglassen: 1. Zweifachinteraktion
                          
                         (1 | pen),
                        
                        feed_tot_less_woa, REML= FALSE)
vif(modell.treat.lmer)
summary (modell.treat.lmer)
modell.treat.p <- PBmodcomp (model_Futter, modell.treat.lmer)
summary (modell.treat.p)
anova(model_Futter)

model_parameters(model_Futter)

### post hoc contrasts ####
emmeans::emmip(model_Futter,  ~ treat | woa)
emm_interaction <- emmeans(model_Futter, ~ treat * woa, type="response") #This calculates the estimated marginal means for treat at different values of woa (by default, at the mean of woa).
# Let's define a few values of 'woa' (mean, 25th percentile, 75th percentile)
woa_values <- quantile(feed_tot_less_woa$woa, probs = c(0,0.5, 1))

# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
emm_interaction_custom <- emmeans(model_Futter, ~ treat | woa, at = list(woa = woa_values))
# Perform pairwise comparisons between levels of 'treat' at those values of 'woa'
interaction_comparisons_custom <- pairs(emm_interaction_custom)
print(interaction_comparisons_custom)

# View the results of mean and CI per factor
summary(emm_interaction)
# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
contrast(emm_interaction, method = "pairwise") #adjust = "tukey")

# confidenzintervall (alles mit null ist nicht signifikant)
confint(model_Futter, method="boot",nsim=1000,boot.type="perc")


### modell schätzungen ####

#ACHTUNG hier zuerst lmerTest deinstallieren (oder einfach R neustarten, contrast funktioniert nicht mit lmerTest)
library (contrast)
modell.pred <- contrast (model_Futter, list (treat= levels (feed_tot_less_woa [, 'treat']),
                                             
                                             woa_s= levels (feed_tot_less_woa [, 'woa'])))

modell.pred [['Contrast']]
modell.pred [['Lower']]
modell.pred [['Upper']]



#Gewicht ####------------------------------------------------------------------------------------------------------------------------------------ 
##Visualisiserung ####
#wenn ein Faktor quadratisch verwendet wird, dann numerisch weil ein Faktro nicht quadratisch gerechnet werden kann 

#weight <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Gewicht_R.xlsx")
weight_all <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Gewicht_R.xlsx")
str(weight_all)
hist(weight_all$weight)
weight.df <- weight_all[,c(-1)]

str(weight.df)
summary(weight.df)
weight.df$woa <- factor(weight.df$woa, ordered = TRUE) 
weight.df$pen <- as.factor(weight.df$pen)
weight.df$treat <- as.factor(weight.df$treat)
weight.df$weight <- as.numeric(weight.df$weight)
str(weight.df)


ggplot(weight_all, aes(x = factor(woa), y = weight, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  scale_fill_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female")  
  ) +
  scale_color_manual(
    values = c("#808080", "#5B9BD5", "brown"),
    labels = c("Male", "Male add. ramp", "Female")
  ) +
  theme_minimal() +
  labs(
    title = "Weight of birds",
    x = "Week of age",
    y = "Weight [g]",
    fill = "experimental groups",
    color = "experimental groups"
  ) +
  theme(
    plot.title = element_text(hjust = 0.75),
    axis.title.y = element_text(margin = margin(r = 20)),
    axis.title.x = element_text(margin = margin(t = 10)))


##Statistik ####
#Log genommen, damit Model assumptions besser werden
model_weight <- lmer(log(weight)~(treat+as.factor(woa))^2 + (1|pen), data =weight.df) 
anova(model_weight)

#Global average effekt: Wie viel erklärt mein volles Modell zu keinem Modell
model_weight0 <- lmer(weight~ 1+ (1|pen), data =weight.df) # Intercept only model, nur random effekt wird betrachtet
summary(model_weight0) #Pen ist 0 -> erklärt das Modell nicht
modell.null.p <- PBmodcomp (model_weight, model_weight0) 
summary (modell.null.p) #unser Modell ist signifikant unterschieden zu einem leren Modell -> Ist gut, unser Model erklärt

### model assumptions ####
par (mfrow= c (3, 3)) # alle Teilabbildungen gemeinsam anschauen
qqnorm (resid (model_weight))
qqline(resid(model_weight))
qqnorm (unlist (ranef (model_weight, level= 1))) # für jeden geschachtelten zufäll. Effekt
scatter.smooth (fitted (model_weight), resid (model_weight))
boxplot (split (resid (model_weight), feed_tot [, 'pen'])) # für jede erklärende Variable

### model ####
summary(model_weight)


#Random effects:
#  Groups   Name        Variance Std.Dev.
#pen      (Intercept) 0.000029 0.005385
#Residual             0.005902 0.076825-> Pen erklärt 0.5 % der Varianz, deshalb brauchen wir den random term nicht? 
# -> funktioniert ohne Pen nicht, weil error no random effects terms specified in formula
#Futter hatte einen Pen-Effekt aber Gewicht nicht: Erklärung: gewisse Pens haben mehr Futter rausgescharrt / verschwendet als andere

plot(weight.df$pen, weight.df$weight) # varianz durch pen sehr klein, fast 0


#p-Wert(e) Einzeleffekte berechnen

anova (model_weight, type= 'marginal')

anova (model_weight)
#für jeden Haupteffekt und jede Interaktion (je einmal weglassen)
modell.weight.tw.lmer <- lmer (weight ~ treat + # einmal weglassen: Haupteffekt VarA1
                          
                          woa + # einmal weglassen: Haupteffekt VarB1
                          
                          #treat:woa + # einmal weglassen: 1. Zweifachinteraktion
                          
                          (1 | pen),
                        
                        weight.df, REML= FALSE)

summary (modell.weight.tw.lmer)

# Bootstrap test 
modell.weight.tw.p <- PBmodcomp (model_weight, modell.weight.tw.lmer)
summary (modell.tw.p)
#LRT      55.687  2.000        8.085e-13 *** #-> dasselbe wie anova, für den Bericht relevant
#PBtest   55.687                0.000999 *** #-> zeigt noch die Robustheit an 
anova(modell.weight.tw.lmer, model_weight) #signifikant Unterschienden -> Interaktion darf nicht wegelassen werden

### post hoc contrasts ####
#bei emmeans type response einfügen, damit ich keine Log skala habe

emmeans::emmip(model_weight,  ~ treat | woa)
emm_interaction.weight <- emmeans(model_weight, ~ treat * woa, type="response") #This calculates the estimated marginal means for treat at different values of woa (by default, at the mean of woa).
# mean, 25th percentile, 75th percentile)
woa_values.weight <- quantile(weight.df$woa, probs = c(0,0.5, 1), typ =1) #Wirklich so?

# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
emm_interaction_custom.weight <- emmeans(model_weight, ~ treat | woa, at = list(woa = woa_values.weight))
# Perform pairwise comparisons between levels of 'treat' at those values of 'woa'
interaction_comparisons_custom.weight <- pairs(emm_interaction_custom.weight)
print(interaction_comparisons_custom.weight)

# View the results of mean and CI per factor
summary(emm_interaction.weight)
# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
contrast(emm_interaction.weight, method = "pairwise") #adjust = "tukey")

# confidenzintervall (alles mit null ist nicht signifikant)
confint(model_weight, method="boot",nsim=1000,boot.type="perc")



#Kamm ####----------------------------------------------------------------------------------------------------------------------------------------

Gesundheitsdaten <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Gesundheitsdaten_R.xlsx")
Kamm <- Gesundheitsdaten[, c(2,4, 6, 10)]
str(Kamm)
Kamm$treat <- as.factor(Kamm$treat)
Kamm$pen <- as.factor(Kamm$pen)


### Visualisierung ####

ggplot(Kamm, aes(treat,comb,fill =treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  scale_fill_manual(values = c("#808080", "#5B9BD5", "brown"))+
  scale_color_manual(values = c("#808080", "#5B9BD5", "brown")) + 
  scale_y_continuous(breaks = seq(0, max(Kamm$comb, na.rm = TRUE), by = 1)) +  # Y-Achse ganzzahlig
  scale_x_discrete(labels = c("M" = "Male", "M+" = "Male add. ramp", "W" = "Female")) + 
  theme_minimal() +
  labs(
    title = "comb and thorat of birds",
    y = "comb injuries",
    x = NULL
  ) +
  theme(
  plot.title = element_text(hjust = 0.5),
  axis.title.y = element_text(margin = margin(r = 20)),
  axis.title.x = element_text(margin = margin(t = 10)),
  legend.position = "none", 
  panel.grid.minor.y = element_blank())

###Statistik ####

#Kann log auf Grund der vielen Null-Werten nicht verwenden
model_kamm <- lmer(comb ~ treat+ (1|pen), data =Kamm) 
anova(model_kamm)

#Global average effekt: Wie viel erklärt mein volles Modell zu keinem Modell
model_kamm0 <- lmer(comb~ 1+ (1|pen), data =Kamm) # Intercept only model, nur random effekt wird betrachtet
summary(model_kamm0) 
modell.null.p <- PBmodcomp (model_kamm, model_kamm0) 
summary (modell.null.p) #unser Modell ist signifikant unterschieden zu einem leren Modell -> Ist gut, unser Model erklärt

#### model assumptions ####
par (mfrow= c (3, 3)) # alle Teilabbildungen gemeinsam anschauen
qqnorm (resid (model_kamm))
qqline(resid(model_kamm))
qqnorm (unlist (ranef (model_kamm, level= 1))) # für jeden geschachtelten zufäll. Effekt
scatter.smooth (fitted (model_kamm), resid (model_kamm))
boxplot (split (resid (model_kamm), feed_tot [, 'pen'])) # für jede erklärende Variable

#### model ####
summary(model_kamm)

#Random effects:
#Groups   Name        Variance  Std.Dev.
#pen      (Intercept) 0.098     0.3131
#Residual             3.622     1.9032 -> Pen erklärt 2.7 % der Varianz, wir lassen es trotdem drin als random term (ab rund 5%)

plot(Kamm$pen, Kamm$comb) # varianz durch pen sehr klein



