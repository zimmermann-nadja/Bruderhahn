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
Distribution <- read_excel("D:/Projekt_Bruderhaehne/Auswertung/Bruderhahn_Analysis/Distribution_R.xlsx")
Distribution <- read_excel("E:/Projekt_Bruderhaehne/Auswertung/Bruderhahn_Analysis/Distribution_R.xlsx")

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
#Wenn ich zu viele korrelierende Variablen habe, ist das nicht gut für das Modell, dann müsste ich mit Multitesting korrigieren 

names(Distribution)
Distribution$woa <- as.integer(Distribution$woa)
# Code für die Darstellung der Korrelation von Variablen, wichtig dabei sind die Zahlen und nicht die Sterne
# Korrelieren Variablen zusammen, kann man sie für die Statistik zusammennehmen

pairs(Distribution[,c(-1,-2,-3,-4,-5,-6,-14)], 
      lower.panel = panel.smooth, 
      upper.panel = function(x, y, digits=2, prefix="", cex.cor) {
        usr <- par("usr"); on.exit(par(usr)) 
        par(usr = c(0, 1, 0, 1)) 
        r <- abs(cor(x, y)) 
        txt <- format(c(r, 0.123456789), digits=digits)[1] 
        txt <- paste(prefix, txt, sep="") 
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
        
        test <- cor.test(x, y) 
        Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                         symbols = c("***", "**", "*", ".", " ")) 
        
        text(0.5, 0.5, txt, cex = cex * r) 
        text(.8, .8, Signif, cex = cex, col = 2) 
      })
#wichtiger Teil, Zeigt Korrelation zwischen top pen, middle pen, litter,...


# Korrelation von topP und midP
# Perform multiple linear regression
less_timepoints <- Distribution  %>% filter((timepoint %in% c("1","13"))) 
less_woa <- less_timepoints  %>% filter((woa %in% c("5", "7","9","11","13"))) # woa 1 und 3 nicht drin, weil keine Sitzstangennutzung
regression_model_less_woa <- cor.test(less_woa$topP, less_woa$midP, m='p') # perason: untersucht linerare Beziehung, idealerweise normalverteilte Daten 
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
#summary(regression_model_cor_perches)

#tie corrected rank-based statistic -> kendall-tau Methode
#Wir nehmen den (und nicht perason), weil unsere Daten nicht numerisch sind, m=s, m = kendall -> Werte werden nach Grösse sortiert (von 0 bis 20) und gibt diesen einen Rang, Datenpunkte mit dem selben Rang werden Bindungen genannt, Kendall genommen um die Bindungen zu berücksichtigen
#cor.test(cor_perches$topP, cor_perches$midP, m='s') #speraman, nicht lineare Korrelation 
cor.test(cor_perches$topP, cor_perches$midP, m='k') #kendall, für Daten mit vielen gleichen Werten, berücksichtigt Bindungen  
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
library(ggplot2)

#Anzahl Tiere in der Einstreu pro woa 
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

#Anzahl Tiere auf den oberen beiden Sitzstangen pro woa 
Distribution$perches34 <- Distribution$topP+Distribution$midP
str(Distribution)
ggplot(Distribution %>% filter((timepoint %in% c("13"))), aes(timepoint,`perches34`,fill=treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa)+ #oder wrap
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "Number of animals on middle and top perche per treatment and per woa",
    x = "",
    y = "number of animals",
    fill = "treatment",
    color = "treatment"
  ) +
  scale_x_discrete(labels = "dusk")+
  
  theme(plot.title = element_text(hjust = 0.5))

#Anzahl Tiere auf den oberen beiden Sitzstangen pro woa 
#### 26.01.2025 NEW EINSCHUB ####
#Anzahl Tiere auf den oberen beiden Sitzstangen pro woa 
Distribution$perches34 <- Distribution$topP+Distribution$midP
str(Distribution)
less_woa_1$perches34 <- less_woa_1$topP+less_woa_1$midP
ggplot(less_woa_1, aes(x = woa, y = perches34, colour =treat, group = treat)) + stat_summary(fun = sum, geom = "line")+facet_grid(~timepoint_cat)

ggplot(less_woa_1, aes(x = woa, y = (perches34/20),color=treat, group=treat)) +
  geom_smooth(stat = 'summary', fun.y = mean, se =TRUE, method="lm") +
  stat_summary(fun.y = mean, geom="line")+
  stat_summary(fun.y = mean, geom="point")+
  facet_grid(~timepoint_cat)

#### 26.01.2025 ENDE EINSCHUB ####

#### NEW YG EINSCHUB ####
library(boot)
extract.ci <- function(x) {
  out <- data.frame (numeric(0),
                     numeric (0),
                     numeric (0))
  for (i in 1:length (x [["t0"]])) {
    out <- rbind (out, c (x [["t0"]] [i],
                          boot.ci (x, index=1,
                                   type="perc") [["percent"]] [,4:5]))
  }
  print (dim (out))
  names(out) <- c ("estim","lo.ci","up.ci")
  out
}

### extract model predictions ####
less_woa$woa_s_squared <- (less_woa$woa_s)^2
less_woa$treat <- as.factor(less_woa$treat)
length(less_woa[,"treat"])
Model_perch34.tiw <- glmer((cbind((topP + midP), (20 - (topP + midP)))) ~ treat + woa_s +  woa_s_squared + 
                             treat:woa_s + (1 | pen), family=binomial,data=less_woa)

Model_perch34.tiw.estim.df <- expand.grid (treat = levels (less_woa [,"treat"]),
                                           timepoint_cat = levels (less_woa [,"timepoint_cat"])) # for categories

Model_perch34.tiw.estim.df <- expand.grid (treat = levels (unlist(less_woa [,"treat"])),
                                           woa_s = seq(from = min(unlist(less_woa[,"woa_s"])),    # Use a sequence for continuous 'vars'
                                                       to = max(unlist(less_woa[,"woa_s"]))),
                                           woa_s_squared = seq(from = min(unlist(less_woa[,"woa_s"]^2)),    # Use a sequence for continuous 'vars'
                                                               to = max(unlist(less_woa[,"woa_s"]^2)) )
)
str(Model_perch34.tiw.estim.df)
summary(Model_perch34.tiw)
Model_perch34.tiw.estim.fn <- function (x) predict (x, Model_perch34.tiw.estim.df,
                                                    re.form=NA)

Model_perch34.tiw.estim.raw <- bootMer (Model_perch34.tiw, Model_perch34.tiw.estim.fn,
                                        nsim=1000)
Model_perch34.tiw.estim.val <- extract.ci (Model_perch34.tiw.estim.raw)

cbind(Model_perch34.tiw.estim.df, Model_perch34.tiw.estim.val)


### plot with added model estimates #### ich mache es am Do fertig
ggplot(Distribution %>% filter((timepoint %in% c("13"))), aes(as.factor(woa),(`perches34`/20),fill=treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5)+
  geom_line()+
  geom_line(data = Model_perch34.tiw.estim.df, aes(x = woa_s, y = estim, color = treat), size = 1) +
  geom_ribbon(data = Model_perch34.tiw.estim.df, aes(x = woa_s, ymin = lo.ci, ymax = up.ci, fill = treat), alpha = 0.2) 
#### ENDE EINSCHUB ####   


newdat <- less_woa %>% select(treat,woa,pen)
newdat$"I(woa^2)"<-(newdat$woa)^2
newdat$pred <- predict(Model_perch34,newdata=newdat, type="response")

df <- newdat %>% 
  group_by(treat, woa,"I(woa^2)") %>% 
  summarise(mean = mean(pred),
            std = sd(pred))
df


### stacked barplot ####

library(ggplot2)
library(dplyr)
library(tidyr)

Distribution[,7:13]
View(test)
stacked.df <- Distribution  %>%
  gather(Position, count, 7:13) 
str(stacked.df)
stacked.df$Position <- as.factor(stacked.df$Position)

#Distribution of birds per treatment and woa
stacked_sub.df <- stacked.df %>% 
  filter(!(timepoint %in% c("2", "6", "11", "12"))) %>%
  mutate(Position = factor(Position, levels = c(
    "topP", 
    "midP", 
    "second_floor_perch", 
    "first_floor_perch", 
    "second_floor",
    "first_floor", 
    "litter"
  )))

ggplot(stacked_sub.df, aes(fill = Position, y = count, x = treat)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~woa)+
  ggtitle("Distribution of birds per treatment and woa")+
  ggtitle("Distribution of birds during light phase per treatment and woa")+
  labs(
    title = "Distribution of birds per treatment and woa",  # Diagrammtitel
    x = "treatment",
    y = "percentage",    # Bezeichnung der y-Achse
    fill = "position"               # Bezeichnung der Legende
  )

#Distribution of birds per treatment, woa and timepoint
ggplot(stacked_sub.df, aes(fill = Position, y = count, x = timepoint)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~ treat+woa)+
  ggtitle("Distribution of birds during light phase per treatment, woa and time of day")+
  labs(
    title = "Distribution of birds per treatment and woa",  # Diagrammtitel
    x = "timepoint",
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

# because only few events counted during light phase: only dawn and dusk will be considered for perch use:
less_timepoints <- Distribution  %>% filter((timepoint %in% c("1","13"))) 
less_woa <- less_timepoints  %>% filter((woa %in% c("5", "7","9","11","13"))) # woa 1 und 3 nicht drin, weil keine Sitzstangennutzung
View(less_woa)
str(less_woa)
less_woa$woa_s <- scale(less_woa$woa)
less_woa$timepoint <- droplevels(less_woa$timepoint)


topP <- glmer(`topP` ~ (treat + woa_s + timepoint)^3 + (1|pen), family=poisson, data = less_woa)

topP <- glmer(`topP` ~ treat + woa_s + timepoint + 
                treat:woa_s +
                treat:timepoint +
                woa_s:timepoint + 
                treat:woa_s:timepoint+              
                (1|pen), family=poisson, data = less_woa)

topP1 <- glmer(`topP` ~ treat + woa_s + timepoint + 
                 treat:woa_s +
                 treat:timepoint +
                 woa_s:timepoint + 
                 #treat:woa_s:timepoint+              
                 (1|pen), family=poisson, data = less_woa)
#Likelyhood_ration Test, um zu schauen, ob ^2 und ^3 gleich sind. ^2 ist zu bevorzugen. Wenn  nicht signifikant, kann man ^2 nehmen
anova(topP,topP1)


topP1.wtp <- glmer(`topP` ~ treat + woa_s + timepoint + 
                     treat:woa_s +
                     treat:timepoint +
                     #woa_s:timepoint + 
                     #treat:woa_s:timepoint+              
                     (1|pen), family=poisson, data = less_woa)

anova(topP1,topP1.wtp)
#Hier ist es signifikant, deshalb darf ich nicht eine Stufe weiter (das wäre linear)

topP1.trttp <- glmer(`topP` ~ treat + woa_s + timepoint + 
                       treat:woa_s +
                       #treat:timepoint +
                       woa_s:timepoint + 
                       #treat:woa_s:timepoint+              
                       (1|pen), family=poisson, data = less_woa)

anova(topP1,topP1.trttp)
#HIer ist es nicht signifikant 

topP1.trtwoa <- glmer(`topP` ~ treat + woa_s + timepoint + 
                        #treat:woa_s +
                        treat:timepoint +
                        woa_s:timepoint + 
                        #treat:woa_s:timepoint+              
                        (1|pen), family=poisson, data = less_woa)

anova(topP1,topP1.trtwoa)


library(emmeans)
emm1 = emmeans(topP1.wtp, specs = pairwise ~ woa_s:timepoint,type = "response") #type = "response": gives back-transformed scale
emm1

emm1$contrasts

#### logistic regression with ceiling effect ####
# Mein Hauptmodell, zweigt an, welche die topP nutzen, keine Dreifachinteraktion, da 0.969
Model_perch34 <- glmer(cbind(topP, 20-topP) ~ treat + woa_s + I(woa_s^2)+
                         treat:woa_s +
                         treat:I(woa_s^2)+
                         woa_s:I(woa_s^2)+
                         (1|pen), family=binomial, data=less_woa)
Model_perch34.no3w <- glmer(cbind(topP, 20-topP) ~ treat + woa_s + 
                              #treat:woa_s +
                              + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34,Model_perch34.no3w)

Model_perch34.woa <- glmer(cbind(topP, 20-topP) ~ treat + #woa_s + 
                             #treat:woa_s +
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.woa,Model_perch34.no3w)

Model_perch34.trt <- glmer(cbind(topP, 20-topP) ~ #treat + 
                             woa_s + 
                             #treat:woa_s +
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.trt,Model_perch34.no3w)

# LRT: X2 =10.615, df = 2, P = 0.005 -> treatment
#Plot für treatment und woa separat machen (die Links von Yamenah)


library(parameters)
parameters(Model_perch34.no3w,exponentiate = T)
#Titt auf, wenn man zu viele Nullen im Modell hat, Varianz ist grösser als der Mittelwert
library(performance)
check_overdispersion(Model_perch34)

### both perches ####
#### logistic regression with ceiling effect both perches ####
#### 27.1.2025 EINSCHUB ####
# best fitting approach:
# perch use yes/no (incl timepoint_cat dusk versus dawn)
# perch use odds ratio versus no perch use (no timepoint_cat)

#### logistic regression with ceiling effect both perches ####
library(lme4)
str(less_woa)
less_woa$woa_s <- scale(less_woa$woa)
less_woa$timepoint <- droplevels(less_woa$timepoint)
less_woa_sub <- less_woa %>%
  mutate(
    timepoint_cat = case_when(
      timepoint %in% c("1") ~ "dawn",           # forgot whether dusk or dawn ;-)
      timepoint %in% c("3", "4","5","7","8","9","10") ~ "light", # time of day between dusk and dawn
      timepoint %in% c("13") ~ "dusk", 
      TRUE ~ "Other"                                   # Keep other values as 'Other'
    )
  )
less_woa_sub$woa
# with timepoint dusk/dawn
Model_perch34 <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa + I(woa^2)+ timepoint_cat+
                         treat:woa +
                         treat:I(woa^2) +
                         treat:timepoint_cat +
                         (1|pen), family=binomial, data=less_woa_sub)

summary(Model_perch34)
library(car)
vif(Model_perch34)
testDispersion(Model_perch34)
simulationOutput <- simulateResiduals(fittedModel = Model_perch34, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)
# slightly overdispersed, does it matter?, test with binary outcome:
less_woa_sub$perches34>0 <- less_woa_sub$topP+less_woa_sub$midP
Model_perch34 <- glmer(perches34>0 ~ treat + woa +  I(woa^2)+timepoint_cat+
                         treat:woa +
                         treat:I(woa^2) +
                         treat:timepoint_cat +
                         (1|pen), family=binomial, data=less_woa_sub)
library(performance)
check_model(Model_perch34)
summary(Model_perch34)
# we have issue with multicollinearity and convergence issues, therefore exclude quadratic term

#wahrscheinlichkeit, dass sie auf SS gehen: oben ja/nein
Model_perch34 <- glmer(perches34>0 ~ treat + woa +  timepoint_cat+
                         treat:woa +
                         treat:timepoint_cat +
                         (1|pen), family=binomial, data=less_woa_sub)

testDispersion(Model_perch34)
simulationOutput <- simulateResiduals(fittedModel = Model_perch34, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

anova(Model_perch34)

Model_perch34.tw <- glmer(perches34>0 ~ treat + woa +  timepoint_cat+
                            #treat:woa +
                            treat:timepoint_cat +
                            (1|pen), family=binomial, data=less_woa_sub)

anova(Model_perch34, Model_perch34.tw)

Model_perch34.main <- glmer(perches34>0 ~ treat + woa +  timepoint_cat+
                              #treat:woa +
                              #treat:timepoint_cat +
                              (1|pen), family=binomial, data=less_woa_sub)

anova(Model_perch34, Model_perch34.main)

Model_perch34.main.t <- glmer(perches34>0 ~ treat + woa +  #timepoint_cat+
                                #treat:woa +
                                #treat:timepoint_cat +
                                (1|pen), family=binomial, data=less_woa_sub)

anova(Model_perch34.main.t, Model_perch34.main)

Model_perch34.main.woa <- glmer(perches34>0 ~ treat + #woa +  
                                  timepoint_cat+
                                  #treat:woa +
                                  #treat:timepoint_cat +
                                  (1|pen), family=binomial, data=less_woa_sub)

anova(Model_perch34.main.woa, Model_perch34.main)

Model_perch34.main.treat <- glmer(perches34>0 ~ #treat + 
                                    woa +  
                                    timepoint_cat+
                                    #treat:woa +
                                    #treat:timepoint_cat +
                                    (1|pen), family=binomial, data=less_woa_sub)

anova(Model_perch34.main.treat, Model_perch34.main)

# second step of zero inflated approach
less_woa_sub_nonzero <- subset(less_woa_sub, perches34>0)
Model_perch34 <- glmer(perches34 ~ treat + woa +  timepoint_cat+
                         treat:woa +
                         treat:timepoint_cat +
                         (1|pen), family=poisson, data=less_woa_sub_nonzero)

testDispersion(Model_perch34)
simulationOutput <- simulateResiduals(fittedModel = Model_perch34, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

anova(Model_perch34)
Model_perch34.tt <- glmer(perches34 ~ treat + woa +  timepoint_cat+
                            treat:woa +
                            #treat:timepoint_cat +
                            (1|pen), family=poisson, data=less_woa_sub_nonzero)

anova(Model_perch34.tt,Model_perch34)

Model_perch34.tw <- glmer(perches34 ~ treat + woa +  timepoint_cat+
                            #treat:woa +
                            treat:timepoint_cat +
                            (1|pen), family=poisson, data=less_woa_sub_nonzero)
anova(Model_perch34.tw,Model_perch34)

Model_perch34.main <- glmer(perches34 ~ treat + woa +  timepoint_cat+
                              #treat:woa +
                              #treat:timepoint_cat +
                              (1|pen), family=poisson, data=less_woa_sub_nonzero)
anova(Model_perch34.main, Model_perch34)

Model_perch34.tc <- glmer(perches34 ~ treat + woa +  #timepoint_cat+
                            #treat:woa +
                            #treat:timepoint_cat +
                            (1|pen), family=poisson, data=less_woa_sub_nonzero)
anova(Model_perch34.main, Model_perch34.tc)

Model_perch34.w <- glmer(perches34 ~ treat + #woa +  
                           timepoint_cat+
                           #treat:woa +
                           #treat:timepoint_cat +
                           (1|pen), family=poisson, data=less_woa_sub_nonzero)
anova(Model_perch34.main, Model_perch34.w)

Model_perch34.treat <- glmer(perches34 ~ #treat + 
                               woa +  
                               timepoint_cat+
                               #treat:woa +
                               #treat:timepoint_cat +
                               (1|pen), family=poisson, data=less_woa_sub_nonzero)
anova(Model_perch34.main, Model_perch34.treat)


# odds ratio von wie viel wahrscheinlicher sind Tiere auf SSt versus wahrscheinlichkeit nicht oben zu sein
### ENDE EINSCHUB ####


library(lme4)
str(less_woa)
less_woa$woa_s <- scale(less_woa$woa)
less_woa$timepoint <- droplevels(less_woa$timepoint)

# with timepoint dusk/dawn
Model_perch34 <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+ timepoint+
                         treat:woa_s +
                         treat:I(woa_s^2) +
                         treat:timepoint +
                         (1|pen), family=binomial, data=less_woa)

summary(Model_perch34)
library(car)
vif(Model_perch34)

Model_perch34.tt <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+ timepoint+
                            treat:woa_s +
                            treat:I(woa_s^2) +
                            #treat:timepoint +
                            (1|pen), family=binomial, data=less_woa)

anova(Model_perch34, Model_perch34.tt)

Model_perch34.tiw <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+ timepoint+
                             treat:woa_s +
                             #treat:I(woa_s^2) +
                             treat:timepoint +
                             (1|pen), family=binomial, data=less_woa)
anova(Model_perch34, Model_perch34.tiw)

Model_perch34.tw <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+ timepoint+
                            #treat:woa_s +
                            treat:I(woa_s^2) +
                            treat:timepoint +
                            (1|pen), family=binomial, data=less_woa)
anova(Model_perch34, Model_perch34.tw)
library(parameters)
#Ergibt eine schöne Übersicht über die Werte, welche ich brauche
model_parameters(Model_perch34.tiw, exponentiate=T)

library(emmeans)

emmeans::emmip(Model_perch34.tiw,  ~ treat | woa_s)
emm_interaction <- emmeans(Model_perch34.tiw, ~ treat * woa_s, type="response") #This calculates the estimated marginal means for treat at different values of woa (by default, at the mean of woa).
# Let's define a few values of 'woa' (mean, 25th percentile, 75th percentile)
woa_values <- quantile(less_woa$woa_s, probs = c(0,0.25, 0.5, 0.75,1))

# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
emm_interaction_custom <- emmeans(Model_perch34.tiw, ~ treat | woa_s, at = list(woa_s = woa_values))
# Perform pairwise comparisons between levels of 'treat' at those values of 'woa'
interaction_comparisons_custom <- pairs(emm_interaction_custom)
print(interaction_comparisons_custom)

emmeans::emmip(Model_perch34.tiw,  ~ treat | timepoint)
emm_treat_at_timepoint <- emmeans(Model_perch34.tiw, ~ treat | timepoint)#, type="response")

# View the results
summary(emm_treat_at_timepoint)
# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
contrast(emm_treat_at_timepoint, method = "pairwise") #adjust = "tukey")

# without timepoint
Model_perch34 <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+ 
                         treat:woa_s +
                         treat:I(woa_s^2) +
                         (1|pen), family=binomial, data=less_woa)

Model_perch34.tiw <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+
                             treat:woa_s +
                             #treat:I(woa_s^2)+
                             + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34,Model_perch34.tiw)

Model_perch34.tiw.iw <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + #I(woa_s^2)+
                                treat:woa_s +
                                #treat:I(woa_s^2)+
                                + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.tiw,Model_perch34.tiw.iw) # we need to keep I(woa^2) in the dataset

Model_perch34.tw <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+
                            #treat:woa_s +
                            treat:I(woa_s^2)+
                            + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.tw,Model_perch34)

Model_perch34.main <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+
                              #treat:woa_s +
                              #treat:I(woa_s^2)+
                              + (1|pen), family=binomial, data=less_woa)
anova(Model_perch34.tiw,Model_perch34.main)

library(parameters)
#Ergibt eine schöne Übersicht über die Werte, welche ich brauche
model_parameters(Model_perch34.tiw, exponentiate=T)
names(Distribution)

library(emmeans)
Model_perch34.tiw <- glmer(cbind((topP+midP), (20-(topP+midP))) ~ treat + woa_s + I(woa_s^2)+
                             treat:woa_s +
                             #treat:I(woa_s^2)+
                             + (1|pen), family=binomial, data=less_woa)

emmeans::emmip(Model_perch34.tiw,  ~ treat | woa_s)
emm_interaction <- emmeans(Model_perch34.tiw, ~ treat * woa_s, type="response") #This calculates the estimated marginal means for treat at different values of woa (by default, at the mean of woa).
# Let's define a few values of 'woa' (mean, 25th percentile, 75th percentile)
woa_values <- quantile(less_woa$woa_s, probs = c(0,0.25, 0.5, 0.75,1))

# Obtain estimated marginal means for 'treat' at those specific values of 'woa'
emm_interaction_custom <- emmeans(Model_perch34.tiw, ~ treat | woa_s, at = list(woa_s = woa_values))
# Perform pairwise comparisons between levels of 'treat' at those values of 'woa'
interaction_comparisons_custom <- pairs(emm_interaction_custom)
print(interaction_comparisons_custom)



#check assumptions of poisson?
# Pearson's goodness-of-fit
library(DHARMa)
testDispersion(topP1.wtp)
simulationOutput <- simulateResiduals(fittedModel = topP1.wtp, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

### Litter use  #### 
#### logistic regression ####
#nur 4 bis 16 uhr, da der Rest 0

# Datenstruktur
all_woa <- Distribution  %>% filter((woa %in% c("1","3","5", "7","9","11","13"))) 
all_woa$woa_s <- scale(all_woa$woa)
View(all_woa)
str(all_woa)
less_tp <- all_woa  %>% filter((timepoint %in% c("1","3","4","5", "7","8","9","10","13"))) # Verteilung alle 2 h
less_tp$timepoint <- droplevels(less_tp$timepoint)
str(less_tp)
View(less_tp)

# 3 Phasen erstellen
less_tp_light <- less_tp %>%
  mutate(
    timepoint_cat = case_when(
      timepoint %in% c("1") ~ "dawn",           # morning
      timepoint %in% c("3", "4","5","7","8","9","10") ~ "light", # time of day between dusk and dawn
      timepoint %in% c("13") ~ "dusk",         # evening
      TRUE ~ "Other"                                   # Keep other values as 'Other'
    )
  )
#daytime <- Distribution %>% filter(!(timepoint %in% c("1","2","6", "11", "12","13")))

less_tp_light$timepoint <- as.integer(less_tp_light$timepoint)
less_tp_light$timepoint_cat <- as.factor(less_tp_light$timepoint_cat)
less_tp_light_only <- subset(less_tp_light, timepoint_cat =="light")
less_tp_light_only$woa_s <- scale(less_tp_light_only$woa)

Model_litter <- glmer(cbind(litter, (20-litter)) ~ treat + woa_s + I(woa_s)^2+ 
                        treat:woa_s +
                        treat:I(woa_s^2)+
                        (1|pen), family=binomial, data=less_tp_light_only)
summary(Model_litter)

library(DHARMa)
testDispersion(Model_litter)
simulationOutput <- simulateResiduals(fittedModel = Model_litter, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

#high dispersion values therefore: test the probability of getting litter use:
str(less_tp_light_only)
Model_litter <- glmer(litter>0 ~ treat + woa + I(woa)^2+ 
                        treat:woa +
                        treat:I(woa^2)+
                        (1|pen), family=binomial, data=less_tp_light_only)
summary(Model_litter)
library(lmerTest)
anova(Model_litter)

Model_litter.tiw <- glmer(litter>0 ~ treat + woa + I(woa)^2+ 
                            treat:woa +
                            #treat:I(woa^2)+
                            (1|pen), family=binomial, data=less_tp_light_only)
anova(Model_litter.tiw,Model_litter)

Model_litter.tw <- glmer(litter>0 ~ treat + woa + I(woa)^2+ 
                           #treat:woa +
                           treat:I(woa^2)+
                           (1|pen), family=binomial, data=less_tp_light_only)
anova(Model_litter.tw,Model_litter)


# step 2 of zero-inflated model:
less_tp_light_only_litter <- subset(less_tp_light_only, litter !=0)
Model_litter.full_nonzero <-glmer(cbind(litter,(20-litter) ) ~ treat + woa + I(woa)^2+ 
                                    treat:woa +
                                    treat:I(woa^2)+
                                    (1|pen), family=binomial, data=less_tp_light_only_litter)
library(DHARMa)
testDispersion(Model_litter.full_nonzero)
simulationOutput <- simulateResiduals(fittedModel = Model_litter.full_nonzero, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

# still huge overdispersion: 
library(glmmTMB)
negbinom.Model_litter <- glmmTMB(litter ~ treat + woa + I(woa)^2+ 
                                   treat:woa +
                                   treat:I(woa^2)+ (1 | pen), family = nbinom2, data = less_tp_light_only_litter)

testDispersion(negbinom.Model_litter)
simulationOutput <- simulateResiduals(fittedModel = negbinom.Model_litter, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

# improved a lot

#### END EINSCHUB ####




Model_litter.tiw <- glmer(cbind(litter, (20-litter)) ~ treat + woa_s + I(woa_s)^2+ 
                            treat:woa_s +
                            #treat:I(woa_s^2)+
                            (1|pen), family=binomial, data=less_tp_light_only)
anova(Model_litter.tiw,Model_litter)

Model_litter.tw <- glmer(cbind(litter, (20-litter)) ~ treat + woa_s + I(woa_s)^2+ 
                           #treat:woa_s +
                           treat:I(woa_s^2)+
                           (1|pen), family=binomial, data=less_tp_light_only)
anova(Model_litter.tw,Model_litter)

# without timepoint ## ???
Model_litter.w2t <- glmer(cbind(litter, (20-litter)) ~ treat + woa + I(woa^2)+ timepoint+
                            treat:woa +
                            treat:I(woa^2)+
                            treat:timepoint+
                            woa:timepoint+
                            #I(woa^2):timepoint+
                            (1|pen), family=binomial, data=daytime)
summary(Model_litter.w2t)
anova(Model_litter,Model_litter.w2t)

Model_litter.wt <- glmer(cbind(litter, (20-litter)) ~ treat + woa + I(woa^2)+ timepoint+
                           treat:woa +
                           treat:I(woa^2)+
                           treat:timepoint+
                           #woa:timepoint+
                           I(woa^2):timepoint+
                           (1|pen), family=binomial, data=daytime)
summary(Model_litter.wt)
anova(Model_litter,Model_litter.wt)

Model_litter.tt <- glmer(cbind(litter, (20-litter)) ~ treat + woa + I(woa^2)+ timepoint+
                           treat:woa +
                           treat:I(woa^2)+
                           #treat:timepoint+
                           woa:timepoint+
                           #I(woa^2):timepoint+
                           (1|pen), family=binomial, data=daytime)
summary(Model_litter.tt)
anova(Model_litter,Model_litter.tt)


## Second floor incl second floor perch use ####
#Datenstruktur
#Distribution$woa <- as.factor(Distribution$woa)
#Distribution$timepoint <- as.integer(Distribution$timepoint)
Distribution$timepoint <- factor(Distribution$timepoint, ordered=T)

all_woa <- Distribution  %>% filter((woa %in% c("1","3","5", "7","9","11","13")))
all_woa$secondfloor_use <- all_woa$second_floor+all_woa$second_floor_perch
hist(all_woa$secondfloor_use)
all_woa$pen <- as.factor(all_woa$pen)
all_woa$woa <- as.numeric(all_woa$woa)
all_woa <- all_woa  %>% filter(!(timepoint %in% c("2","6", "11", "12")))
all_woa$woa_s <- scale(all_woa$woa)
str(all_woa)

### logistic regression with ceiling effect second floor + second floor perch####
secondfloor.fit <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+
                           treat:woa_s+
                           treat:I(woa_s^2)+
                           (1|pen), family=binomial, data=all_woa)

secondfloor.fit.tiw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+
                               treat:woa_s+
                               #treat:I(woa_s^2)+
                               (1|pen), family=binomial, data=all_woa)
anova(secondfloor.fit.tiw,secondfloor.fit)

secondfloor.fit.tw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+
                              #treat:woa_s+
                              treat:I(woa_s^2)+
                              (1|pen), family=binomial, data=all_woa)
anova(secondfloor.fit.tw,secondfloor.fit)

ggplot(all_woa , aes(timepoint,secondfloor_use/20,fill=treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_wrap(~as.factor(woa))

all_woa %>% 
  group_by(treat, woa) %>%
  summarise(value=mean(secondfloor_use/20,na.rm=T), sd(secondfloor_use/20,na.rm=T)) %>%
  ggplot(aes(x=as.numeric(woa),y=value,group=treat,color=treat))+
  geom_line()+
  geom_point()+
  xlab('woa')+
  labs(color='Ano')

# based on this plot: we pool timepoints between dusk and dawn: like for litter: getting here three timepoints: dusk, light, dawn
# Combine factor levels into 3 new levels
all_woa_sub <- all_woa %>%
  mutate(
    timepoint_cat = case_when(
      timepoint %in% c("1") ~ "morning",           # forgot whether dusk or dawn ;-)
      timepoint %in% c("3", "4","5","7","8","9","10") ~ "light", # time of day between dusk and dawn
      timepoint %in% c("13") ~ "evening", 
      TRUE ~ "Other"                                   # Keep other values as 'Other'
    )
  )

secondfloor.fit <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+timepoint_cat+
                           treat:timepoint_cat+
                           treat:woa_s+
                           treat:I(woa_s^2)+
                           (1|pen), family=binomial, data=all_woa_sub)
secondfloor.fit.tiw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+timepoint_cat+
                               treat:timepoint_cat+
                               treat:woa_s+
                               #treat:I(woa_s^2)+
                               (1|pen), family=binomial, data=all_woa_sub)

anova(secondfloor.fit.tiw,secondfloor.fit)

secondfloor.fit.tw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+timepoint_cat+
                              treat:timepoint_cat+
                              #treat:woa_s+
                              treat:I(woa_s^2)+
                              (1|pen), family=binomial, data=all_woa_sub)

anova(secondfloor.fit.tw,secondfloor.fit)

secondfloor.fit.tt <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+timepoint_cat+
                              #treat:timepoint_cat+
                              treat:woa_s+
                              treat:I(woa_s^2)+
                              (1|pen), family=binomial, data=all_woa_sub)

anova(secondfloor.fit.tt,secondfloor.fit)

ggplot(all_woa_sub , aes(timepoint_cat,secondfloor_use/20,fill=treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_wrap(~as.factor(woa))

# to keep everything the same: litter was zero for dusk and dawn therefore both excluded, perches34 it was zero for day: 
#so keep dusk and dawn and pool it and secondflooruse: all values are available so pool all or combine dusk/dawn in one stats and daytime in one stats

#### 27.01.2025 NEW EINSCHUB ####
# dusk/dawn pooled
str(all_woa_sub_dimming$timepoint_cat)
all_woa_sub_dimming <- subset(all_woa_sub, timepoint_cat !="light")
str(all_woa_sub_dimming)
all_woa_sub_dimming$timepoint_cat <- as.factor(all_woa_sub_dimming$timepoint_cat)
library(lme4)

secondfloor.fit <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa + I(woa^2)+ timepoint_cat+
                           treat:timepoint_cat+
                           treat:woa+
                           treat:I(woa^2)+
                           (1|pen), family=binomial, data=all_woa_sub)

summary(secondfloor.fit)
print(secondfloor.fit, correlation=T)

library(DHARMa)
testDispersion(secondfloor.fit)
simulationOutput <- simulateResiduals(fittedModel = secondfloor.fit, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

secondfloor.fit <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa + I(woa^2)+
                           treat:woa+
                           treat:I(woa^2)+
                           (1|pen)+(1|timepoint_cat), family=binomial, data=all_woa_sub)

summary(secondfloor.fit)
# random effects variance: 
# proportion of variance explained by timepoint_cat:
0.04394 + 0.06768 = 0.11162
# proportion of variance explained: 0.06768/0.11162 = 0.6063429 ==> 60.63 %!!!

secondfloor.fit.tt <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa + I(woa^2)+timepoint_cat+
                              #treat:timepoint_cat+
                              treat:woa+
                              treat:I(woa^2)+
                              (1|pen), family=binomial, data=all_woa_sub)

anova(secondfloor.fit.tt,secondfloor.fit)

secondfloor.fit.tw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa + I(woa^2)+timepoint_cat+
                              #treat:woa+
                              treat:timepoint_cat+
                              treat:I(woa^2)+
                              (1|pen), family=binomial, data=all_woa_sub)

anova(secondfloor.fit.tw,secondfloor.fit)

#### END EINSCHUB ####

# NZ: ADD EMMEANS

# daytime pooled
str(all_woa_sub_light$timepoint_cat)
all_woa_sub_light <- subset(all_woa_sub, timepoint_cat =="light")
str(all_woa_sub_light)
all_woa_sub_light$timepoint_cat <- as.factor(all_woa_sub_light$timepoint_cat)
secondfloor.fit <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+
                           treat:woa_s+
                           treat:I(woa_s^2)+
                           (1|pen), family=binomial, data=all_woa_sub_light)

secondfloor.fit.tiw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+
                               treat:woa_s+
                               #treat:I(woa_s^2)+
                               (1|pen), family=binomial, data=all_woa_sub_light)

anova(secondfloor.fit.tiw,secondfloor.fit)

secondfloor.fit.tw <- glmer((cbind(secondfloor_use, 20-secondfloor_use)) ~ treat + woa_s + I(woa_s^2)+
                              #treat:woa_s+
                              treat:I(woa_s^2)+
                              (1|pen), family=binomial, data=all_woa_sub_light)

anova(secondfloor.fit.tw,secondfloor.fit)

# NZ: ADD EMMEANS



#Access ####-------------------------------------------------------------------------------------------------------------------------------------
library(readxl)
#access <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/access_R.xlsx")
access <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/access_R.xlsx")

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

#Movement tot movers####
library(readxl)
library(ggplot2)
library(lme4)
moving <- read_excel("D:/Projekt_Bruderhaehne/Auswertung/Bruderhahn_Analysis/Moving_R.yg.xlsx")
moving <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-Moving_R.yg.xlsx")
moving <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Moving_R.yg.xlsx")
str(moving)

#NZ: Frage an dich: Wurde Falling während Morgendämmerung oder Abenddämmerung oder bei beidem angeschaut (und pro timepoint sowohl aufwärts und abwärtsbewegung oder nur entweder/oder)? 
# falls nicht unterscheidbar ob auf- oder abwärtsbewegung für die beiden dimming phasen: dann einfach im Excel beide Kolonnen zusammenzählen zu zb "total_movers" = number_birds_up + number_birds_down
moving$number_birds_down # number of birds going to perches 3 and or 4 if falling is observed during upwards movement (evening) only:
moving$number_birds_up # number of birds leaving perches 3 and or 4 if falling is observed during downwards movement (morning) only:
moving$total_movers # number of birds moving up or down in total per timepoint:

falling.fit <- glmer(cbind(falling,(total_movers-falling)) ~ treat + woa_s + I(woa_s^2) +
                       treat:woa_s+
                       treat:I(woa_s^2)+
                       (1|pen), family=binomial,data=moving)

anova(falling.fit)

library(DHARMa)
testDispersion(falling.fit)
simulationOutput <- simulateResiduals(fittedModel = falling.fit, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

##Übersicht falling ####

ggplot(moving, aes(time,falling/number_birds_down, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) +
  facet_grid(~woa) 

# NZ ADD THE SAME WAY BALANCING and "trail and fail"


# NZ was ist nochmals definition von trail and fail? sind diese abgestürzt (=falling)?


#Movement Hennenminuten ####------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(lme4)
library(readxl)
library(lme4)
library(lmerTest)
Henmin <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Hennenminuten_R.xlsx")
Henmin <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Hennenminuten_R.xlsx")

## Datenstruktur ####
Henmin$pen <- as.factor(Henmin$pen)
Henmin$treat <- as.factor(Henmin$treat)
Henmin$timepoint <- as.factor(Henmin$timepoint)
str(Henmin)

Henmin$latency <- ifelse(Henmin$totP_start>0, (Henmin$Hennensekunden / Henmin$totP_start),NA)
str(Henmin)
rm(latency)

## Visualisierung ####
ggplot(henmin.dawn, aes(x = as.factor(woa), y = latency, fill = treat)) +
  geom_boxplot(aes(color = treat), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() +
  labs(
    title = "mean duration per bird to leave perches",
    x = "week of age",
    y = "seconds",
    fill = "Verfahren",
    color = "Verfahren"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

##Model####
henmin.dawn <- subset(Henmin,timepoint == "dawn")
View(henmin.dawn)
henmin.dawn$timepoint<-droplevels(henmin.dawn$timepoint) #löscht die leren Zellen
latency.lme <- lmer(latency ~treat + woa_s + I(woa_s^2)+
                      treat:woa_s+
                      treat:I(woa_s^2)+
                      (1|pen), data = henmin.dawn,REML=T)
summary(latency.lme)      
anova(latency.lme)
print(latency.lme, correlation=TRUE)

# post hoc tests
library(emmeans)
library(pbkrtest)
#emm.woa_st <- emmeans(latency.lme, specs = pairwise ~ woa_s:treat,type = "response")
#emm.woa_s2t <- emmeans(latency.lme, specs = pairwise ~ treat:I(woa_s^2),type = "response")


emm.woat <- emmeans(latency.lme, ~ woa_s|treat, at = list(woa_s = c(7,9,11,13)))
pairs(emm.woat)
plot(emm.woat)
emm.woa2t <- emmeans(latency.lme, ~ I(woa_s^2)|treat, at = list(woa_s = c(7,9,11,13)))
pairs(emm.woa2t)
#Wieso sind die Sekunden im Verlaufe der woa negativ? Sie brauchen ja mehr Zeit für den Abstieg, nicht weniger?

#Model assumptions
## ----ResidClassLme, eval= FALSE-----------------------------------------------
par (mfrow= c (3, 3)) # alle Teilabbildungen gemeinsam anschauen

qqnorm (resid (latency.lme))

qqnorm (unlist (ranef (latency.lme, level= 1))) # für jeden geschachtelten zufäll. Effekt



scatter.smooth (fitted (latency.lme), resid (latency.lme))

boxplot (split (resid (latency.lme), Henmin [, 'treat'])) # für jede erklärende Variable
boxplot (split (resid (latency.lme), Henmin [, 'woa']))

dev.off ()





## ----ResidDHARMa, eval= FALSE-------------------------------------------------

## library ('DHARMa')

##

## sim.res <- simulateResiduals (model)

##

## plot (sim.res)

##

## plotResiduals (sim.res, data.df [, 'VarX'])
### Hennenminuten ####
#### Latency to move down from perches (3 & 4) #### (divided by topPStart)
henmin <- read_excel("H:/Projekt_Bruderhaehne/Auswertung/Bruderhahn_Analysis/Hennenminuten_R.xlsx")
str(henmin)

#### Average perch use in durations #### (divided by 20 birds)

### Statistics ####

balancing.fit <- lmer(sqrt(balancing) ~ treat + woa_s + I(woa_s^2) +
                        treat:woa_s+
                        treat:I(woa_s^2)+
                        (1|pen), data=henmin)

anova(balancing.fit)

library(DHARMa)
testDispersion(balancing.fit)
simulationOutput <- simulateResiduals(fittedModel = balancing.fit, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

#generate variable: number of birds having left perch
henmin$movedfromperch <- henmin$totP_start-henmin$totP_end
falling.fit <- glmer(cbind(falling,(totP_start-totP_end)-falling) ~ treat + woa_s + I(woa_s^2) +
                       treat:woa_s+
                       treat:I(woa_s^2)+
                       (1|pen), family=binomial,data=henmin)

anova(falling.fit)

library(DHARMa)
testDispersion(falling.fit)
simulationOutput <- simulateResiduals(fittedModel = falling.fit, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)




----------# Ramp use - M+ only ####---------------------------------------------------------------------------------------
male_move<- read_excel("D:/Projekt_Bruderhaehne/Auswertung/Bruderhahn_Analysis/Moving_R.yg.xlsx")
male_move<- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/ramp_use_males.nz.xlsx")
male_move <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/ramp_use_males.nz.xlsx")

# NZ: add in the excel file following two variables:
# total_movers_up (sum up number_birds of ramp use yes AND no of movement_direction = up)
# total_movers_down (sum up number_birds of ramp use yes AND no of movement_direction = down)
#Eventuell Direction usenäh
#Excel tabelle namal apasse
# dadurch kannst du die Werte prozentual resp. logistisch anschauen und sparst dir unten einen Faktor für die Analyse
## Datenstruktur ####
str(male_move)
male_move$pen <- as.factor(male_move$pen)
male_move$treat <- as.factor(male_move$treat)
male_move$time <- as.factor(male_move$time)
male_move$direction <- as.factor(male_move$direction)
male_move$ramp_use <- male_move$ramp / male_move$total
male_move$ramp_use <- as.numeric(male_move$ramp_use)
male_move$woa <- as.integer(male_move$woa)
male_move$woa_s <- scale(male_move$woa)
View(male_move)
maleR <- subset(male_move, treat =="M+") # subset dateset for M+ only
## Visulisierung ####

#GGPLOT WITH timepoint, rmap_use, movement_direction and woa 
# NA-Werte in ramp_use entfernen, Reihenfolge von Zeit definieren
library(ggplot2)
library(dplyr)
library(tidyr)

maleR_filtered <- maleR %>%
  filter(!is.na(direct), !is.na(ramp)) %>% # Entfernt NAs aus direct und rampe
  mutate(`time` = factor(`time`, levels = c("dawn", "mid", "dusk")))%>%   # Reihenfolge defin
  pivot_longer(cols = c(direct, ramp), names_to = "group", values_to = "value") # Daten umformen

# Plot mit beiden Gruppen (direct und rampe)
ggplot(maleR_filtered, aes(x = time, y = value, fill = interaction(group, direction))) +
  geom_boxplot(aes(color = interaction(group, direction)), outlier.shape = 16, outlier.size = 1.5, alpha = 0.5) + 
  facet_grid(~woa) + 
  scale_fill_brewer(palette = "Set1") + 
  scale_color_brewer(palette = "Set1") + 
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

# NA-Werte in ramp_use entfernen, Reihenfolge von Zeit definieren
maleR_filtered <- maleR %>%
  filter(!is.na(ramp_use)) %>%
  mutate(`time` = factor(`time`, levels = c("dawn", "mid", "dusk")))   # Reihenfolge defini

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

#Plot mit Linien und Standardabweichung



# NZ: ggplot of dusk-mid-dawn, woa and rmap use yes/no -> ?

## Model ####
library(lmerTest)
model_ramp_woatd <- lmer(ramp_use ~ (woa_s + direction + time)^3 + (1|pen), data=maleR)
summary (model_ramp_woatd)

model_ramp <- lmer(ramp_use ~ (woa_s + direction)^2 + (1|pen), data=maleR)
summary(model_ramp)

model_ramp_woa <- lmer(ramp_use ~ (woa_s) + (woa_s)^2 + (1|pen), data=maleR)

anova(model_ramp, model_ramp_woa)
#Keine Verbesserung des Models mit direction term drin 



#Feed ####-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Für das ganze Pen ####
##Visualisiserung ####
library(readxl)
library(ggplot2)
library(tidyr)
feed_all <- read_excel("C:/Users/nz24r283/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")
feed_all <- read_excel("C:/Users/nadja/OneDrive/ETH/Masterarbeit_Bruderhahn/Auswertung/Bruderhahn-/Futter_R_2.xlsx")

feed_tot <- feed_all[,c(-1)]
names(feed_tot)
table(feed_tot)
feed_tot= subset(feed_tot, woa !="13")
feed_tot$woa_s <- scale(feed_tot$woa)
feed_tot$feed_animal <- feed_tot$feed / 20 *1000
str(feed_tot)
summary(feed_tot)
View(feed_tot)
str(feed_tot)
feed_tot$woa <- as.numeric(feed_tot$woa) #evtl. kontinuierlich
feed_tot$pen <- as.factor(feed_tot$pen)
feed_tot$treat <- as.factor(feed_tot$treat)
feed_tot$feed <- as.numeric(feed_tot$feed)

# ganzes Pen
custom_labeller <- as_labeller(c(
  "2" = "1-2 woa",
  "4" = "3-4 woa",
  "6" = "5-6 woa",
  "8" = "7-8 woa",
  "10" = "9-10 woa",
  "12" = "11-12 woa"
  ))

ggplot(feed_tot, aes(treat,feed,)) +
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

# Auf das Tier runtergerechnet in gramm 
ggplot(feed_tot, aes(treat,feed,)) +
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



## model assumptions ####
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
                           
                           woa_s + # einmal weglassen: Haupteffekt VarB1
                           
                           I(woa_s^2) + # einmal weglassen: Haupteffekt VarC1
                           
                           treat:woa_s + # einmal weglassen: 1. Zweifachinteraktion
                           
                           treat:I(woa_s^2) + # einmal weglassen: 2. Zweifachinteraktion
                           
                           treat:I(woa_s^2) + # einmal weglassen: 3. Zweifachinteraktion
                           
                           treat:woa_s:I(woa_s^2) + # einmal weglassen: Dreifachinteraktion
                           
                           (1 | pen),
                         
                         feed_tot, REML= FALSE)

modell.XXX.p <- PBmodcomp (model_Futter, modell.XXX.blmer)
summary (modell.XXX.p)


# Modellschätzungen berechnen

#ACHTUNG hier zuerst lmerTest deinstallieren (oder einfach R neustarten, contrast funktioniert nicht mit lmerTest)
library (contrast)
modell.pred <- contrast (model_Futter, list (treat= levels (feed_tot [, 'treat']),
                                             
                                             woa_s= levels (feed_tot [, 'woa_s'])))

modell.pred [['Contrast']]
modell.pred [['Lower']]
modell.pred [['Upper']]

# diese Schätzungen werden dann im Plot eingezeichnet...zeige ich dir noch oder ergänze ich dir noch, einfach dieses Skript ins github pushen 

##Statistik ####
library(lme4)
library(lmerTest)
#PEn scheint kein relevanter Faktor gewesen zu sein 
model_Futter <- lmer(feed^2~(treat+woa_s+I(woa_s^2))^2 + (1|pen), data =feed_tot) # 3fach Interaktion ()^3 nicht sign. kann ersetzt werden durch ^2 because variance explained by pen = 0 ==> removal of random term:
model_Futter.1 <- lm(feed~(treat+woa_s+I(woa_s^2))^2, data =feed_tot)
anova(model_Futter, model_Futter.1) # yes, we can remove pen: does not explain anything 
summary(model_Futter)
summary(model_Futter.1) # in both cases estimates are very similar: e.g. compare intercept estimates...
plot(feed_tot$pen, feed_tot$feed) # varianz durch pen sehr klein, fast 0

model_Futter <- lm(feed^2~(treat+woa_s+ I(woa_s^2))^3 , data =feed_tot)

summary(model_Futter)
anova(model_Futter)
## vom Output her: du hast eine Interaktion zwischen treat und woa und treat mit woa^2 ==> also Interaktion sowohl linear als auch quadratisch (erkenntnlich an der abfallenden Kurve im Plot)

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
