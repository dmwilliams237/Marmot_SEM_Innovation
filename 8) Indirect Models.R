####Date: February 14, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we use piecewiseSEM to run indirect models of the three trial-level 
#behavioral strategies (persistence, behavioral selectivity, and behavioral diveristy),
#and the one day-level behavioral strategy (neophobia) with relative rank 
#and both the affiliative and agonistic social network measures
####################################################################
####################################################################
#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

#Install relevant libraries
library(devtools) #Install from CRAN if you do not already have
#install_github("jslefche/piecewiseSEM") #Install the piecewiseSEM package directly from github
library(piecewiseSEM)
library(lmerTest)
library(nlme)

####The Data###
#aff_behavstrat is the previously combined dataset with behavioral diversity, relative rank and affiliative social principal components
#agr_behavstrat is the previously combined dataset with behavioral diversity, relative rank and agonistic social principal components
#aff_pers is the previously combined dataset with two behavioral strategies (persistence and behavioral selectivity), relative rank and affiliative social principal components
#agr_pers is the previously combined dataset with two behavioral strategies (persistence and behavioral selectivity), relative rank and agonistic social principal components
aff_behavstrat <- read.csv("aff_behavioral_strategies_cbi.csv", header=TRUE, row.names=1)
agr_behavstrat <- read.csv("agr_behavioral_strategies_cbi.csv", header=TRUE, row.names=1)
aff_pers <- read.csv("aff_behavioral_persistence_cbi.csv", header=TRUE, row.names=1)
agr_pers <- read.csv("agr_behavioral_persistence_cbi.csv", header=TRUE, row.names=1)

#Behavioral selectivity measurement is numeric
aff_behavstrat$PropIS_Door_Lid <- as.numeric(aff_behavstrat$PropIS_Door_Lid)
agr_behavstrat$PropIS_Door_Lid <- as.numeric(agr_behavstrat$PropIS_Door_Lid)
#Unique id of marmot is a factor 
aff_behavstrat$uid <- as.factor(aff_behavstrat$uid)
agr_behavstrat$uid <- as.factor(agr_behavstrat$uid)

#################Scale all variables###############
#Affiliative variables
aff_pers$PropIS_Door_Lid <- scale(aff_pers$PropIS_Door_Lid, center=TRUE, scale=TRUE)
aff_pers$prop_time <- scale(aff_pers$prop_time, center=TRUE, scale=TRUE)
aff_pers$rel.rank <- scale(aff_pers$rel.rank, center=TRUE, scale=TRUE)
aff_behavstrat$H <- scale(aff_behavstrat$H, center=TRUE, scale=TRUE)
aff_behavstrat$rel.rank <- scale(aff_behavstrat$rel.rank, center=TRUE, scale=TRUE)

#Agonistic variables
agr_pers$PropIS_Door_Lid <- scale(agr_pers$PropIS_Door_Lid, center=TRUE, scale=TRUE)
agr_pers$prop_time <- scale(agr_pers$prop_time, center=TRUE, scale=TRUE)
agr_pers$rel.rank <- scale(agr_pers$rel.rank, center=TRUE, scale=TRUE)
agr_behavstrat$H <- scale(agr_behavstrat$H, center=TRUE, scale=TRUE)
agr_behavstrat$rel.rank <- scale(agr_behavstrat$rel.rank, center=TRUE, scale=TRUE)

#################Check variable correlations within a dataset##############
#Affiliative variables
cor.test(aff_behavstrat$PC1, aff_behavstrat$rel.rank)#No correlation (-0.37)
cor.test(aff_behavstrat$PC2, aff_behavstrat$rel.rank)#No correlation (0.16)
cor.test(aff_behavstrat$PC3, aff_behavstrat$rel.rank)#No correlation (-0.46)

#Agonsitic variables
cor.test(agr_behavstrat$PC1, agr_behavstrat$rel.rank)#No correlation (-0.55)
cor.test(agr_behavstrat$PC2, agr_behavstrat$rel.rank)#No correlation (0.26)
cor.test(agr_behavstrat$PC3, agr_behavstrat$rel.rank)#No correlation (-0.25)

#####SEMs for persistence, relative rank and affiliative social measures######
# Affiliative PC1 - Friendliness
aff.pers.PC1.modlist=list(
  lme(prop_time~PC1 +rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_pers),
  glmer(success~prop_time+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_pers))

aff.pers.PC1.mod <- as.psem(aff.pers.PC1.modlist)
summary(aff.pers.PC1.mod, conditional=T)

#2 Affiliative PC2 - Bridging
aff.pers.PC2.modlist=list(
  lme(prop_time~PC2 +rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_pers),
  glmer(success~prop_time+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_pers))

aff.pers.PC2.mod <- as.psem(aff.pers.PC2.modlist)
summary(aff.pers.PC2.mod, conditional=T)

# Affiliative PC3- Initiated Closeness
aff.pers.PC3.modlist=list(
  lme(prop_time~PC3 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_pers),
  glmer(success~prop_time+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_pers))

aff.pers.PC3.mod <- as.psem(aff.pers.PC3.modlist)
summary(aff.pers.PC3.mod, conditional=T)

#####SEMs for persistence, relative rank and agonistic social measures######
#1 Agonistic PC1 - Direct Aggression Integration
agr.pers.PC1.modlist = list(
  lme(prop_time~PC1 + rel.rank , random = ~1|uid, na.action = na.omit, 
      data = agr_pers),
  glmer(success~prop_time+ (1|uid), na.action= na.omit, 
        family=binomial(link = "logit"), data = agr_pers) )

agr.pers.PC1.mod <- as.psem(agr.pers.PC1.modlist)
summary(agr.pers.PC1.mod, conditional=T)

#2 Agonistic PC2- Received Aggression
agr.pers.PC2.modlist = list(
  lme(prop_time~PC2 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_pers),
  glmer(success~prop_time + (1|uid),na.action=na.omit, 
        family=binomial(link = "logit"), data = agr_pers) )

agr.pers.PC2.mod <- as.psem(agr.pers.PC2.modlist)
summary(agr.pers.PC2.mod, conditional=T)

#3 Agonistic PC3 - Initiated Aggression Frequency
agr.pers.PC3.modlist = list(
  lme(prop_time~PC3 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_pers),
  glmer(success~prop_time+(1|uid), 
        family=binomial(link = "logit"), data = agr_pers) )

agr.pers.PC3.mod <- as.psem(agr.pers.PC3.modlist)
summary(agr.pers.PC3.mod, conditional=T)

#####SEMs for behavioral diversity, relative rank and affiliative social measures######
# Affiliative PC1 - Friendliness
aff.div.PC1.modlist=list(
  lme(H~PC1 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_behavstrat),
  glmer(success~H+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_behavstrat))

aff.div.PC1.mod <- as.psem(aff.div.PC1.modlist)
summary(aff.div.PC1.mod, conditional=T)

# Affiliative PC2 - Bridging
aff.div.PC2.modlist=list(
  lme(H~PC2 +rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_behavstrat),
  glmer(success~H+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_behavstrat))

aff.div.PC2.mod <- as.psem(aff.div.PC2.modlist)
summary(aff.div.PC2.mod, conditional=T)

# Affiliative PC3 - Initiated Closeness
aff.div.PC3.modlist=list(
  lme(H~PC3 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_behavstrat),
  glmer(success~H+(1|uid), 
        family=binomial(link = "logit"),control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data = aff_behavstrat))

aff.div.PC3.mod <- as.psem(aff.div.PC3.modlist)
summary(aff.div.PC3.mod, conditional=T)

####SEMs for behavioral diversity, relative rank and agonistic social measures####
# Agonistic PC1 - Direct Aggression Integration
agr.div.PC1.modlist=list(
  lme(H~PC1+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_behavstrat),
  glmer(success~H+(1|uid), 
        family=binomial(link = "logit"),control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data = agr_behavstrat))

agr.div.PC1.mod <- as.psem(agr.div.PC1.modlist)
summary(agr.div.PC1.mod, conditional=T)

# Agonistic PC2 - Received Aggression
agr.div.PC2.modlist=list(
  lme(H~PC2 +rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_behavstrat),
  glmer(success~H+(1|uid), 
        family=binomial(link = "logit"), data = agr_behavstrat))

agr.div.PC2.mod <- as.psem(agr.div.PC2.modlist)
summary(agr.div.PC2.mod, conditional=T)

# Agonistic PC3- Initiated Aggression Frequency
agr.div.PC3.modlist=list(
  lme(H~PC3 +rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_behavstrat),
  glmer(success~H+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = agr_behavstrat))

agr.div.PC3.mod <- as.psem(agr.div.PC3.modlist)
summary(agr.div.PC3.mod, conditional=T)

#####SEMs for behavioral selectivity, relative rank and affiliative social measures######
####27 individuals for affiliative 
#1 Affiliative PC1 - Friendliness
aff.sel.PC1.modlist = list(
  lme(PropIS_Door_Lid~PC1+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_pers),
  glmer(success~PropIS_Door_Lid+(1|uid), 
        family=binomial(link = "logit"), data = aff_pers) )

aff.sel.PC1.mod <- as.psem(aff.sel.PC1.modlist)
summary(aff.sel.PC1.mod, conditional=T)

#2 Affiliative PC2 - Bridging
aff.sel.PC2.modlist = list(
  lme(PropIS_Door_Lid~PC2 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_pers),
  glmer(success~PropIS_Door_Lid+(1|uid), 
        family=binomial(link = "logit"), data = aff_pers) )

aff.sel.PC2.mod <- as.psem(aff.sel.PC2.modlist)
summary(aff.sel.PC2.mod, conditional=T)

#3 Affiliative PC3 - Initiated Closeness
aff.sel.PC3.modlist = list(
  lme(PropIS_Door_Lid~PC3 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_pers),
  glmer(success~PropIS_Door_Lid+(1|uid), 
        family=binomial(link = "logit"), data = aff_pers) )

aff.sel.PC3.mod <- as.psem(aff.sel.PC3.modlist)
summary(aff.sel.PC3.mod, conditional=T)

#####SEMs for behavioral selectivity, relative rank and agonistic social measures######
#1 Agonistic PC1 - Direct Aggression Integration
agr.sel.PC1.modlist = list(
  lme(PropIS_Door_Lid~PC1 + rel.rank , random = ~1|uid, na.action = na.omit, 
      data = agr_pers),
  glmer(success~PropIS_Door_Lid+(1|uid), 
        family=binomial(link = "logit"), data = agr_pers) )

agr.sel.PC1.mod <- as.psem(agr.sel.PC1.modlist)
summary(agr.sel.PC1.mod, conditional=T)

#2 Agonistic PC2 - Received Aggression
agr.sel.PC2.modlist = list(
  lme(PropIS_Door_Lid~PC2 + rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_pers),
  glmer(success~PropIS_Door_Lid+(1|uid), 
        family=binomial(link = "logit"), data = agr_pers) )

agr.sel.PC2.mod <- as.psem(agr.sel.PC2.modlist)
summary(agr.sel.PC2.mod, conditional=T)

#3 Agonistic PC3 - Initiated Aggression Frequency
agr.sel.PC3.modlist = list(
  lme(PropIS_Door_Lid~PC3+ rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_pers),
  glmer(success~PropIS_Door_Lid+(1|uid), 
        family=binomial(link = "logit"), data = agr_pers) )

agr.sel.PC3.mod <- as.psem(agr.sel.PC3.modlist)
summary(agr.sel.PC3.mod, conditional=T)


####################################################################
####################################################################
#####Neophobia SEM####
#Remove all previous data and set work directory
rm(list=ls())

#The datasets####
#aff_neophobia is the previously compiled dataset with neophobia, relative rank and affiliative social principal component
#agr_neophobia is the previously compiled dataset with neophobia, relative rank and agonistic social principal component
aff_neophobia <- read.csv("aff_behavioral_neophobia_cbi.csv")
agr_neophobia <- read.csv("agr_behavioral_neophobia_cbi.csv")

####Modifications to dataset to ensure columns are reading in correctly####
#Neophobia measurement is numeric
aff_neophobia$lat.totouch.s.day <- as.numeric(aff_neophobia$lat.totouch.s.day)
agr_neophobia$lat.totouch.s.day <- as.numeric(agr_neophobia$lat.totouch.s.day)
#Unique id of marmot is a factor 
aff_neophobia$uid <- as.factor(aff_neophobia$uid)
agr_neophobia$uid <- as.factor(agr_neophobia$uid)
#Remove NAs from neophobia and success columns to ensure models are run on the same dataset
aff_neophobia <- aff_neophobia[!is.na(aff_neophobia$lat.totouch.s.day),]
agr_neophobia <- agr_neophobia[!is.na(agr_neophobia$lat.totouch.s.day),]
aff_neophobia <- aff_neophobia[!is.na(aff_neophobia$success),]
agr_neophobia <- agr_neophobia[!is.na(agr_neophobia$success),]

#################Scale all variables###############
#Affiliative variables
aff_neophobia$lat.totouch.s.day <- scale(aff_neophobia$lat.totouch.s.day, center=TRUE, scale=TRUE)
aff_neophobia$rel.rank <- scale(aff_neophobia$rel.rank, center=TRUE, scale=TRUE)

#Agonistic variables
agr_neophobia$lat.totouch.s.day <- scale(agr_neophobia$lat.totouch.s.day, center=TRUE, scale=TRUE)
agr_neophobia$rel.rank <- scale(agr_neophobia$rel.rank, center=TRUE, scale=TRUE)

#####Check new distributions####
#Affiliative variables
hist(aff_neophobia$lat.totouch.s.day)
hist(aff_neophobia$rel.rank)

#Agonistic variables
hist(agr_neophobia$lat.totouch.s.day)
hist(agr_neophobia$rel.rank)

#################Check variable correlations within dataset##############
#Affiliative variables
cor.test(aff_neophobia$PC1, aff_neophobia$rel.rank) #Not correlated (-0.29)
cor.test(aff_neophobia$PC2, aff_neophobia$rel.rank) #Not correlated (0.16)
cor.test(aff_neophobia$PC3, aff_neophobia$rel.rank) #Not correlated (-0.39)

#Agonsitic variables
cor.test(agr_neophobia$PC1, agr_neophobia$rel.rank)#Not correlated, (-0.41)
cor.test(agr_neophobia$PC2, agr_neophobia$rel.rank)#Not correlated (0.29)
cor.test(agr_neophobia$PC3, agr_neophobia$rel.rank)#Not correlated (-0.22)

#####SEMs for neophobia and affiliative social measures######
# Affiliative PC1 - Friendliness
aff.neo.PC1.modlist=list(
  lme(lat.totouch.s.day~PC1+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_neophobia),
  glmer(success~lat.totouch.s.day+(1|uid), 
        family=binomial(link = "logit"),control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_neophobia))

aff.neo.PC1.mod <- as.psem(aff.neo.PC1.modlist)
summary(aff.neo.PC1.mod, conditional=T)

# Affiliative PC2 - Bridging
aff.neo.PC2.modlist=list(
  lme(lat.totouch.s.day~PC2+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_neophobia),
  glmer(success~lat.totouch.s.day+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_neophobia))

aff.neo.PC2.mod <- as.psem(aff.neo.PC2.modlist)
summary(aff.neo.PC2.mod, conditional=T)

# Affiliative PC3 - Initiated Closeness
aff.neo.PC3.modlist=list(
  lme(lat.totouch.s.day~PC3+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = aff_neophobia),
  glmer(success~lat.totouch.s.day+(1|uid), 
        family=binomial(link = "logit"), control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)),data = aff_neophobia))

aff.neo.PC3.mod <- as.psem(aff.neo.PC3.modlist)
summary(aff.neo.PC3.mod, conditional=T)

#####SEMs for neophobia and agonistic social measures######
# Agonistic PC1 - Direct Aggression Integration
agr.neo.PC1.modlist=list(
  lme(lat.totouch.s.day~PC1+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_neophobia),
  glmer(success~lat.totouch.s.day+(1|uid), 
        family=binomial(link = "logit"), data = agr_neophobia))

agr.neo.PC1.mod <- as.psem(agr.neo.PC1.modlist)
summary(agr.neo.PC1.mod, conditional=T)

# Agonistic PC2 - Recevied Aggression
agr.neo.PC2.modlist=list(
  lme(lat.totouch.s.day~PC2+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_neophobia),
  glmer(success~lat.totouch.s.day+(1|uid), 
        family=binomial(link = "logit"), data = agr_neophobia))

agr.neo.PC2.mod <- as.psem(agr.neo.PC2.modlist)
summary(agr.neo.PC2.mod, conditional=T)

# Agonistic PC3 - Initiated Aggression Frequency
agr.neo.PC3.modlist=list(
  lme(lat.totouch.s.day~PC3+rel.rank, random = ~1|uid, na.action = na.omit, 
      data = agr_neophobia),
  glmer(success~lat.totouch.s.day+(1|uid), 
        family=binomial(link = "logit"), data = agr_neophobia))

agr.neo.PC3.mod <- as.psem(agr.neo.PC3.modlist)
summary(agr.neo.PC3.mod, conditional=T)

