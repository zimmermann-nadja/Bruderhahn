# Bruderhahn #

# Distribution ####
#setwd("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-")
#getwd()
## data structure ####
#"C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Distribution_R.xlsx"
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

Distribution <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Distribution_R.xlsx")
#Distribution <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Distribution_R.xlsx")

#Pen und Treatments als Faktor setzen, weil wir brauchen Pen als random term 
str(Distribution)
Distribution$pen <- as.factor(Distribution$pen)
Distribution$treat <- as.factor(Distribution$treat)
Distribution$timepoint <- as.factor(Distribution$timepoint)
str(Distribution)
summary(Distribution)
summary(Distribution$pen)
View(Distribution)

## Kor zw Variablen ####
#Wenn ich zu viele Korrelierende Variablen habe, ist das nicht gut für das Modell, dann müsste ich mit Multitesting korrigieren 

names(Distribution)
Distribution$woa <- as.integer(Distribution$woa)
# Code für die Darstellung der Korrelation von Variablen, wichtig dabei sind die Zahlen und nicht die Sterne
# Korrelieren Variablen zusammen, kann man sie für die Statistik zusammennehmen
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}
pairs(Distribution[,c(-1,-2,-3,-4,-5,-6,-14)], lower.panel=panel.smooth, upper.panel=panel.cor) #wichtiger Teil, Zeigt Korrelation zwischen top pen, middle pen, litter,...

# Korrelation von bestimmten Variablen bestimmen
# Perform multiple linear regression
less_timepoints <- Distribution  %>% filter((timepoint %in% c("1","13"))) #idealerweise mit 7, wenn mehr Daten (jetzt kann er nicht schätzen)
less_woa <- less_timepoints  %>% filter((woa %in% c("5", "7","9","11","13"))) # woa 1 und 3 nicht drin, weil keine Sitzstangennutzung
regression_model_less_woa <- cor.test(less_woa$topP, less_woa$midP, m='p')
summary(regression_model_less_woa)
less_woa$topP <-as.numeric(less_woa$topP)
less_woa$midP <-as.numeric(less_woa$midP)

less_woa$topP <-as.integer(less_woa$topP)
less_woa$midP <-as.integer(less_woa$midP)
cor.test(less_woa$topP, less_woa$midP, m='kendall')

regression_model <- lm(topP ~ midP+treat, data = less_woa)



# View the summary of the regression results
summary(regression_model)

cor_perches <- Distribution  %>% filter((woa %in% c("5", "7","9","11","13"))) # woa 1 und 3 nicht drin, weil keine Sitzstangennutzung
#regression_model_cor_perches<- glm(topP ~ midP+treat, family="poisson",data = cor_perches)
# View the summary of the regression results
#Wir nehmen den, weil unsere Daten nicht numerisch sind, sondern integer, m=s, m = kendall -> Werte werden nach Grösse sortiert (von 0 bis 20) und gibt diesen einen Rang, DAtenpunkte mit dem selben Rang werden Bindungen genannt, Kendall genommen um die Bindungen zu berücksichtigen
#tie corrected rank-based statistic -> kendall-tau Methode
cor.test(cor_perches$topP, cor_perches$midP, m='s')
summary(regression_model_cor_perches)
#>0.45 ist starke korrelation, ich habe 0.7
plot(cor_perches$midP, cor_perches$topP)
str(cor_perches)

ggplot(less_woa, aes(x=midP, y=topP)) +
  
  geom_point()+
  
  geom_smooth(method=loess, color="black")+
  
  # labs(title="Correlation between Diagnostic testscore and Percentage",
  
  # x= "Total testscore", y = "Percentage" )+
  
  theme(plot.title = element_text(size=15, face="bold", hjust = 0.5))
## Visualisierung ####
### Positon ####
#jitter -> bricht Punkte auf, die dieselben sind 
library(ggplot2)
#Anzahl Tiere in der Einstreu pro woa 
ggplot(Distribution, aes(timepoint, `litter`,colour = treat)) +
  geom_point() +   geom_jitter()

ggplot(Distribution, aes(x = timepoint, y = `litter`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  facet_grid(~woa) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals in the litter per treatment and per woa",
    x = "Zeitpunkt",
    y = "Verteilung",
    fill = "Verfahren",
    color = "Verfahren"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Anzahl Tiere auf der ersten Etage pro woa 
ggplot(Distribution, aes(timepoint, `first_floor`,colour = treat)) +
  geom_point() +   geom_jitter()

ggplot(Distribution, aes(x = timepoint, y = `first_floor`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  facet_grid(~woa) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals in first_floor",
    x = "Zeitpunkt",
    y = "Verteilung",
    fill = "Verfahren",
    color = "Verfahren"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#Anzahl Tiere auf der zweiten Etage pro woa 
ggplot(Distribution, aes(timepoint, `second_floor`,colour = treat)) +
  geom_point() +   geom_jitter()

ggplot(Distribution, aes(x = timepoint, y = `second_floor`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  facet_grid(~woa) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals on the second floor per treatment and per woa",
    x = "Zeitpunkt",
    y = "Verteilung",
    fill = "Verfahren",
    color = "Verfahren"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Anzahl Tiere auf der mittleren Sitzstange pro woa 
Distribution %>% filter(!(timepoint %in% c("2", "6", "11", "12")))
ggplot(Distribution %>% filter((timepoint %in% c("1", "13"))), aes(timepoint, `midP`,fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa)+
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals on the middle perch per treatment and per woa",
    x = "timepoint",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#Anzahl Tiere auf der obersten Sitzstange pro woa 
ggplot(Distribution  %>% filter((timepoint %in% c("1", "13"))), aes(timepoint, `topP`,fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa)+
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals on the top perch per treatment and per woa",
    x = "timepoint",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  scale_x_discrete(labels = c("1" = "dawn", "13" = "dusk"))

  theme(plot.title = element_text(hjust = 0.5))

#Anzahl Tiere auf den obeeren beiden Sitzstangen pro woa 
 Distribution$perches34 <- Distribution$topP+Distribution$midP
 str(Distribution)
   ggplot(Distribution %>% filter((timepoint %in% c("13"))), aes(as.factor(woa),`perches34`,fill=treat)) +
    geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
    facet_wrap(~woa)+
    scale_fill_brewer(palette = "Set1") + 
    scale_color_brewer(palette = "Set1") + 
    theme_minimal() +
    labs(
      title = "Number of animals on the middle perch per treatment and per woa",
      x = "timepoint",
      y = "number of animals",
      fill = "treatment",
      color = "treatment"
    ) +
    theme(plot.title = element_text(hjust = 0.5))
  
### stacked barplot ####
# Alle Verteilungen unterhalb in eine Kolone bringen
library(ggplot2)
library(dplyr)
library(tidyr)

Distribution[,7:13]
View(test)
stacked.df <- Distribution  %>%
  gather(Position, count, 7:13) 
str(stacked.df)
stacked.df$Position <- as.factor(stacked.df$Position)


# Stacked + percent
ggplot(stacked.df, aes(fill=Position, y=count, x=timepoint)) +
  geom_bar(position="fill", stat="identity")+
  facet_grid(~woa)


#Stacked nach timepoint und treatment
ggplot(stacked.df, aes(fill=Position, y=count, x=timepoint)) +
  geom_bar(position="fill", stat="identity")+
#  facet_grid(~treat)+
  facet_wrap(~treat+woa)


#Distribution of birds per treatment and woa
stacked_sub.df <- stacked.df %>% 
  filter(!(timepoint %in% c("2", "6", "11", "12"))) %>%
  mutate(Position = factor(Position, levels = c(
    "top perch", 
    "middle perch", 
    "second floor perch", 
    "first floor perch", 
    "second floor incl. top ramp",
    "first floor incl. middle ramp", 
    "litter incl. lowest ramp"
  )))

ggplot(stacked_sub.df, aes(fill = Position, y = count, x = treat)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~woa)+
  ggtitle("Distribution of birds during light phase per treatment and woa")+
  labs(
  title = "Distribution of birds per treatment and woa",  # Diagrammtitel
  x = "treatment",
   y = "percentage",    # Bezeichnung der y-Achse
  fill = "position"               # Bezeichnung der Legende
)

## statistic ####
### top perch ####
library(lme4)
library(optimx)
str(Distribution)
Distribution$woa <- as.factor(Distribution$woa)
#Distribution$timepoint <- as.integer(Distribution$timepoint)
Distribution$timepoint <- factor(Distribution$timepoint, ordered=T)

less_timepoints <- Distribution  %>% filter((timepoint %in% c("13"))) #idealerweise mit 7, wenn mehr Daten (jetzt kann er nicht schätzen)
less_woa <- less_timepoints  %>% filter((woa %in% c("5", "7","9","11","13"))) # woa 1 und 3 nicht drin, weil keine Sitzstangennutzung
str(less_timepoints)
topP <- glmer(`topP` ~ (treat + woa + timepoint)^3 + (1|pen), family=poisson, data = less_woa)

topP <- glmer(`topP` ~ treat + woa + timepoint + 
                treat:woa +
                treat:timepoint +
                woa:timepoint + 
                treat:woa:timepoint+              
                (1|pen), family=poisson, data = less_woa)

topP1 <- glmer(`topP` ~ treat + woa + timepoint + 
                treat:woa +
                treat:timepoint +
                woa:timepoint + 
                #treat:woa:timepoint+              
                (1|pen), family=poisson, data = less_woa)
#Likelyhood_ration Test, um zu schauen, ob ^2 und ^3 gleich sind. ^2 ist zu bevorzugen. Wenn  nicht signifikant, kann man ^2 nehmen
anova(topP,topP1)

topP1.wtp <- glmer(`topP` ~ treat + woa + timepoint + 
                 treat:woa +
                 treat:timepoint +
                 #woa:timepoint + 
                 #treat:woa:timepoint+              
                 (1|pen), family=poisson, data = less_woa)

anova(topP1,topP1.wtp)
#Hier ist es signifikant, deshalb darf ich nicht eine Stufe weiter (das wäre linear)

topP1.trttp <- glmer(`topP` ~ treat + woa + timepoint + 
                     treat:woa +
                     #treat:timepoint +
                     woa:timepoint + 
                     #treat:woa:timepoint+              
                     (1|pen), family=poisson, data = less_woa)

anova(topP1,topP1.trttp)
#HIer ist es nicht signifikant 

topP1.trtwoa <- glmer(`topP` ~ treat + woa + timepoint + 
                       #treat:woa +
                       treat:timepoint +
                       woa:timepoint + 
                       #treat:woa:timepoint+              
                       (1|pen), family=poisson, data = less_woa)

anova(topP1,topP1.trtwoa)

library(emmeans)
emm1 = emmeans(topP1.wtp, specs = pairwise ~ woa:timepoint,type = "response") #type = "response": gives back-transformed scale
emm1

emm1$contrasts

### logistic regression with ceiling effect ####
# Mein Hauptmodell, zweigt an, welche die topP nutzen, keine Dreifachinteraktion, da 0.969
Model_perch34 <- glmer(cbind(topP, 20-topP) ~ treat + woa + 
                         treat:woa +
                         + (1|pen), family=binomial, data=less_woa)
Model_perch34.no3w <- glmer(cbind(topP, 20-topP) ~ treat + woa + 
                              #treat:woa +
                              + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34,Model_perch34.no3w)

Model_perch34.woa <- glmer(cbind(topP, 20-topP) ~ treat + #woa + 
                             #treat:woa +
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.woa,Model_perch34.no3w)

Model_perch34.trt <- glmer(cbind(topP, 20-topP) ~ #treat + 
                             woa + 
                             #treat:woa +
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.trt,Model_perch34.no3w)
# LRT: X2 =10.615, df = 2, P = 0.005 -> treatment
#Plot für treatment und woa separat machen (die Links von Yamenah)
library(parameters)
parameters(Model_perch34.no3w,exponentiate = T)
#Titt auf, wenn man zu viele Nullen im Modell hat, Varianz ist grösser als der Mittelwert
library(performance)
check_overdispersion(Model_perch34)

### logistic regression with ceiling effect both top perches ####
Model_perch34 <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa + I(woa^2)+
                         treat:woa +
                         + (1|pen), family=binomial, data=less_woa)
Model_perch34.no3w <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa + 
                              #treat:woa +
                              + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34,Model_perch34.no3w)

Model_perch34.woa <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + #woa + 
                             #treat:woa +
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.woa,Model_perch34.no3w)

Model_perch34.trt <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ #treat + 
                             woa + 
                             #treat:woa +
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.trt,Model_perch34.no3w)

#Anzahl Tiere auf der obersten Sitzstange pro woa 
ggplot(Distribution  %>% filter((timepoint %in% c("1", "13"))), aes(timepoint, `topP`)) +
  geom_boxplot(aes(), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa)+
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals on the top perch per treatment and per woa",
    x = "timepoint",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

#check assumptions of poisson?
# Pearson's goodness-of-fit
library(DHARMa)
testDispersion(topP1.wtp)
simulationOutput <- simulateResiduals(fittedModel = topP1.wtp, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)



#Access ####-------------------------------------------------------------------------------------------------------------------------------------
library(readxl)
access <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/access_R.xlsx")

## data structure ####
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
ggplot(access, aes(time, `2_floor_to_mP_ramp`,colour = treat)) +
  geom_point() +   geom_jitter()

ggplot(access, aes(time,`2_floor_to_mP_ramp`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  scale_x_discrete(
    limits = c("dusk", "mid", "dawn") # Hier die gewünschte Reihenfolge festlegen
  ) +
  labs(
    title = " 2_floor_to_mP_ramp",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())

#Movement####------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

#Übersicht falling

ggplot(access, aes(time,`falling`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  scale_x_discrete(
    limits = c("dust", "mid", "dawn") # Hier die gewünschte Reihenfolge festlegen
  ) +
  labs(
    title = "number of fallings per woa",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())

#Übersicht tried and failed 
ggplot(access, aes(time,`tried_and_failed`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  scale_x_discrete(
    limits = c("dust", "mid", "dawn") # Hier die gewünschte Reihenfolge festlegen
  ) +
  labs(
    title = "number of tried and failed per woa",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())

#Übersicht balancing 
ggplot(access, aes(time,`balancing`, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  scale_x_discrete(
    limits = c("dusk", "mid", "dawn") # Hier die gewünschte Reihenfolge festlegen
  ) +
  labs(
    title = "number of balancing per woa",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())
http://127.0.0.1:12935/graphics/plot_zoom_png?width=833&height=822
#Feed ####---------------------------------------------------------------------------------------------------------------------------------------
#Visualisiserung
library(readxl)
library(ggplot2)
library(tidyr)
#feed_all <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")
feed_all <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")
  
feed_tot <- feed_all[,c(-1)]
names(feed_tot)
table(feed_tot)
feed_tot= subset(feed_tot, woa !="13")
str()
summary(feed_tot)
View(feed_tot)
str(feed_tot)
feed_tot$woa <- as.numeric(feed_tot$woa) #evtl. kontinuierlich
feed_tot$pen <- as.factor(feed_tot$pen)
feed_tot$treat <- as.factor(feed_tot$treat)
feed_tot$feed <- as.numeric(feed_tot$feed)

ggplot(feed_tot, aes(treat,feed,)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) +
   scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "feed use per pen of 20 animals",
    y = "feed use [kg]",
    fill = "treatment",
    color = "treatment"
  ) +
  theme(plot.title = element_text(hjust = 0.5),axis.title.x = element_blank())

#Statistik###
library(lme4)
library(lmerTest)
#PEn scheint kein relevanter Faktor gewesen zu sein 
model_Futter <- lmer(feed^2~(treat+woa+I(woa^2))^2 + (1|pen), data =feed_tot) # 3fach Interaktion ()^3 nicht sign. kann ersetzt werden durch ^2 because variance explained by pen = 0 ==> removal of random term:
model_Futter.1 <- lm(feed~(treat+woa+I(woa^2))^2, data =feed_tot)
anova(model_Futter, model_Futter.1) # yes, we can remove pen: does not explain anything 
summary(model_Futter)
summary(model_Futter.1) # in both cases estimates are very similar: e.g. compare intercept estimates...
plot(feed_tot$pen, feed_tot$feed) # varianz durch pen sehr klein, fast 0

model_Futter <- lm(feed^2~(treat+woa+ I(woa^2))^3 , data =feed_tot)

summary(model_Futter)
anova(model_Futter)
## vom Output her: du hast eine Interaktion zwischen treat und woa und treat mit woa^2 ==> also Interaktion sowohl linear als auch quadratisch (erkenntnlich an der abfallenden Kurve im Plot)
# model assumptions
par (mfrow= c (3, 3)) # alle Teilabbildungen gemeinsam anschauen
qqnorm (resid (model_Futter))
qqnorm (unlist (ranef (model_Futter, level= 1))) # für jeden geschachtelten zufäll. Effekt
scatter.smooth (fitted (model_Futter), resid (model_Futter))
boxplot (split (resid (model_Futter), feed_tot [, 'pen'])) # für jede erklärende Variable

# mit Einbezug vom Quadratischen Term für Zeit (woa), werden Model assumptions besser, kritisch ist noch die Heteroscedastizität (2. Plot), dort gibt es eine ABweichung


# therefore double check with following: Globalen p-Wert berechnen

#lmer:
  
modell.null.lmer <- lmer(feed ~ 1 + (1 | pen),feed_tot)
anova (model_Futter, modell.null.lmer)


library (pbkrtest)
modell.null.p <- PBmodcomp (model_Futter, modell.null.lmer)
summary (modell.null.p)

#p-Wert(e) Einzeleffekte berechnen

anova (model_Futter, type= 'marginal')
#für jeden Haupteffekt und jede Interaktion (je einmal weglassen): (habe ich noch nicht gemacht: xxx jeweils durch die weggelassene Variable ersetzen)
  modell.XXX.lmer <- lmer (feed ~ treat + # einmal weglassen: Haupteffekt VarA1
                               
                               woa + # einmal weglassen: Haupteffekt VarB1
                               
                               I(woa^2) + # einmal weglassen: Haupteffekt VarC1
                               
                               treat:woa + # einmal weglassen: 1. Zweifachinteraktion
                               
                               treat:I(woa^2) + # einmal weglassen: 2. Zweifachinteraktion
                               
                               treat:I(woa^2) + # einmal weglassen: 3. Zweifachinteraktion
                               
                               treat:woa:I(woa^2) + # einmal weglassen: Dreifachinteraktion
                               
                               (1 | pen),
                             
                             feed_tot, REML= FALSE)

modell.XXX.p <- PBmodcomp (model_Futter, modell.XXX.blmer)
summary (modell.XXX.p)


# Modellschätzungen berechnen

#ACHTUNG hier zuerst lmerTest deinstallieren (oder einfach R neustarten, contrast funktioniert nicht mit lmerTest)
library (contrast)
modell.pred <- contrast (model_Futter, list (treat= levels (feed_tot [, 'treat']),
                                           
                                           woa= levels (feed_tot [, 'woa'])))

modell.pred [['Contrast']]
modell.pred [['Lower']]
modell.pred [['Upper']]

# diese Schätzungen werden dann im Plot eingezeichnet...zeige ich dir noch oder ergänze ich dir noch, einfach dieses Skript ins github pushen 

##Statistik ####
library(lme4)
library(lmerTest)
#PEn scheint kein relevanter Faktor gewesen zu sein 
model_Futter <- lmer(feed^2~(treat+woa+I(woa^2))^2 + (1|pen), data =feed_tot) # 3fach Interaktion ()^3 nicht sign. kann ersetzt werden durch ^2 because variance explained by pen = 0 ==> removal of random term:
model_Futter.1 <- lm(feed~(treat+woa+I(woa^2))^2, data =feed_tot)
anova(model_Futter, model_Futter.1) # yes, we can remove pen: does not explain anything 
summary(model_Futter)
summary(model_Futter.1) # in both cases estimates are very similar: e.g. compare intercept estimates...
plot(feed_tot$pen, feed_tot$feed) # varianz durch pen sehr klein, fast 0

model_Futter <- lm(feed^2~(treat+woa+ I(woa^2))^3 , data =feed_tot)

summary(model_Futter)
#Health ####-------------------------------------------------------------------------------------------------------------------------------------
##Gewicht #### 
#Visualisiserung
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
