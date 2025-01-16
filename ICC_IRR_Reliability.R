# install.packages("irr")
library(irr)
library(data.table)
library(readxl)

HA_bw_raters <- read_xlsx("G:/VPHI/Welfare/2- Research Projects/BruderHahn/Health assessment/Reliability_HA_kappa.xlsx")

View((HA_bw_raters))
kappa2(HA_bw_raters[, c("nz_kat", "yg_kat")], weight = "unweighted")
kappa2(HA_bw_raters[, c("nz_kat", "yg_kat")], weight = "equal")

ICCbare(DoeID, FussL, data = HA_bw_raters)

#health assessment training kont
install.packages("ICC")
install.packages("psych")
install.packages("tidyr")
install.packages("cli")
library(data.table)
library(psych)
library(ICC)
library(tidyverse)
library(dplyr)
library(tidyr)
library(cli)
HA_kont <- fread("//nas-vetsuisse/VETSUISSE/Benutzer/yg18q990/Project_Rabbits/20231222_20240123_healthassessment_training_kont.csv")

HA_kont <- read_xlsx("G:/VPHI/Welfare/2- Research Projects/BruderHahn/Health assessment/Relayability_ha.xlsx")

View(HA_kont)
names(HA_kont)
str(HA_kont)
remove.packages("cli")
install.packages("cli")
HA_kont$DoeID <- as.factor(HA_kont$DoeID)
HA_kont$ID <- as.factor(HA_kont$ID)

#rabbits
ICCbare(DoeID, FussL, data = HA_kont)

ICCbare(DoeID, FussR, data = HA_kont)

ICCbare(DoeID, AugeL, data = HA_kont)

ICCbare(DoeID, AugeR, data = HA_kont)

#bruderhahn
ICCbare(ID, Comb_thorat, data = HA_kont)
ICCbare(ID, Rhockburn, data = HA_kont)
ICCbare(ID, Lhockburn, data = HA_kont)
ICCbare(ID, RFootpad, data = HA_kont)
ICCbare(ID, LFootpad, data = HA_kont)
ICCbare(ID, RBumble, data = HA_kont)
ICCbare(ID, LBumble, data = HA_kont)
ICCbare(ID, RToe, data = HA_kont)
ICCbare(ID, LToe, data = HA_kont)

dat.AugeL <- HA_kont %>% 
  select(Assessor, ID, Comb_thorat)

dat.AugeL.1 <- dat.AugeL %>% 
  pivot_wider(., names_from = Assessor, values_from = AugeL)  

icc(dat.AugeL.1[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)

dat.AugeR <- HA_kont %>% 
  select(Assessor, DoeID, AugeR)
dat.AugeR.1 <- dat.AugeR %>% 
  pivot_wider(., names_from = Assessor, values_from = AugeR)

icc(dat.AugeR.1[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)

#Bruderhahn
dat.Comb <- HA_kont %>% 
  select(Assessor, ID, Comb_thorat)
dat.Comb.1 <- dat.Comb %>% 
  pivot_wider(., names_from = Assessor, values_from = Comb_thorat)  
icc(dat.Comb.1[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)

#test without out to see whether it is due to low variance
dat.new1 <- subset(HA_kont, out !=1)
dat.new.1 <- dat.new1 %>% 
  select(Assessor, ID, Rhockburn)
dat.new.2 <- dat.new.1 %>% 
  pivot_wider(., names_from = Assessor, values_from = Rhockburn)  
icc(dat.new.2[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)

dat.new <- HA_kont %>% 
  select(Assessor, ID, Rhockburn)
dat.new.1 <- dat.new %>% 
  pivot_wider(., names_from = Assessor, values_from = Rhockburn)  
icc(dat.new.1[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)

dat.new <- HA_kont %>% 
  select(Assessor, ID, Lhockburn)
dat.new.1 <- dat.new %>% 
  pivot_wider(., names_from = Assessor, values_from = Lhockburn)  
icc(dat.new.1[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)

dat.new <- HA_kont %>% 
  select(Assessor, ID, LBumble)
dat.new.1 <- dat.new %>% 
  pivot_wider(., names_from = Assessor, values_from = LBumble)  
icc(dat.new.1[,2:3], model = c("oneway"), type = c("agreement"), conf.level = 0.95)
