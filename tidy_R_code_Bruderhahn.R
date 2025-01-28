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
#Feed ####-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

##Visualisiserung ####

feed_all <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")
feed_all <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")
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
custom_labeller <- as_labeller(c(
  "2" = "1-2 woa",
  "4" = "3-4 woa",
  "6" = "5-6 woa",
  "8" = "7-8 woa",
  "10" = "9-10 woa",
  "12" = "11-12 woa"
))

ggplot(feed_tot, aes(treat,feed)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa, labeller = (woa=custom_labeller)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "feed use per pen (20 animals)",
    y = "feed use [kg]",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())

### Einzeltier in g ####

ggplot(feed_tot_less_woa, aes(treat,feed_animal)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa, labeller = (woa=custom_labeller)) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "feed use from two weeks per animal",
    y = "feed use per animal [g]",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())

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

summary(model_Futter)

#Random effects:
#Groups   Name        Variance  Std.Dev.
#pen      (Intercept) 1.721e-05 0.004149
#Residual             3.540e-04 0.018814 -> Pen erklärt 4.6 % der Varianz, deshalb lassen wir es drin als random term

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

### post hoc contrasts
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


# modell schätzungen

#ACHTUNG hier zuerst lmerTest deinstallieren (oder einfach R neustarten, contrast funktioniert nicht mit lmerTest)
library (contrast)
modell.pred <- contrast (model_Futter, list (treat= levels (feed_tot_less_woa [, 'treat']),
                                             
                                             woa_s= levels (feed_tot_less_woa [, 'woa'])))

modell.pred [['Contrast']]
modell.pred [['Lower']]
modell.pred [['Upper']]


#Health ####-------------------------------------------------------------------------------------------------------------------------------------
##Gewicht #### 
###Visualisiserung ####
library(readxl)
library(ggplot2)
library(tidyr)

#weight <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Gewicht_R.xlsx")
weight_all <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Gewicht_R.xlsx")

weight <- weight_all[,c(-1)]
names(weight)
table(weight)

str()
summary(weight)
View(weight)
str(weight)
weight$woa <- as.integer(weight$woa) #weil ein quadratischer Term und diese dann kein Faktor sein kann
weight$pen <- as.factor(weight$pen)
weight$treat <- as.factor(weight$treat)
weight$weight <- as.numeric(weight$weight)

ggplot(weight, aes(treat,weight,)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "weight of birds",
    y = "weight [g]",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())
##Kamm #### 
##Körperbreite ####
