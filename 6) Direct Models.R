####Date: February 8, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we calculate direct models of social PCA, relative rank and behavioral strategy on innovation
####################################################################
####################################################################
#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

####Load required packages####
library(lme4)
library(lmerTest)

####Affiliative Models####
#The data####
#behavstrat_innovation contains information on innovation and behavioral strategies (persistence, behavioral selectivity, shannon index behavioral diversity. 
#neo_innovation contains information on innovation and neophobia
#aff_innovation contains information on innovation and affiliative pcas
#agr_innovation contains information on innovation and agonistic pcas
#cbi_innovation contains information on innovation and relative rank 
behavstrat_innovation <- read.csv("behavioralstrategy_innovation.csv", header=TRUE, row.names=1)
neo_innovation <- read.csv("neophobia_innovation.csv", header=TRUE, row.names = 1)
aff_innovation<- read.csv("aff_innovation.csv", header=TRUE, row.names = 1)
agr_innovation <- read.csv("agr_innovation.csv", header=TRUE, row.names =1)
cbi_innovation <- read.csv("cbi_innovation.csv", header=TRUE, row.names =1)
pers_innovation <- subset(behavstrat_innovation, prop_time > 0)

#Ensure data is being read in properly
behavstrat_innovation$uid <- as.factor(behavstrat_innovation$uid)
neo_innovation$uid <- as.factor(neo_innovation$uid)
aff_innovation$uid <- as.factor(aff_innovation$uid)
agr_innovation$uid <- as.factor(agr_innovation$uid)
cbi_innovation$uid <- as.factor(cbi_innovation$uid)
behavstrat_innovation$success <- as.factor(behavstrat_innovation$success)
neo_innovation$lat.totouch.s.day <- as.numeric(neo_innovation$lat.totouch.s.day)

####Scale all variables for comparison####
pers_innovation$PropIS_Door_Lid <- scale(pers_innovation$PropIS_Door_Lid, center=TRUE, scale=TRUE)
pers_innovation$prop_time <- scale(pers_innovation$prop_time, center=TRUE, scale=TRUE)
behavstrat_innovation$H <- scale(behavstrat_innovation$H, center=TRUE, scale=TRUE)
neo_innovation$lat.totouch.s.day <- scale(neo_innovation$lat.totouch.s.day, center=TRUE, scale=TRUE)
cbi_innovation$rel.rank <- scale(cbi_innovation$rel.rank, center = TRUE, scale = TRUE)

#####Correlations between variables within a dataset####
cor.test(pers_innovation$PropIS_Door_Lid, pers_innovation$prop_time)#0.80

cor.test(aff_innovation$PC1, aff_innovation$PC2)#0.09
cor.test(aff_innovation$PC1, aff_innovation$PC3)#-0.04
cor.test(aff_innovation$PC2, aff_innovation$PC3)#-0.10

cor.test(agr_innovation$PC1, agr_innovation$PC2)#-0.14
cor.test(agr_innovation$PC1, agr_innovation$PC3)#-0.02
cor.test(agr_innovation$PC2, agr_innovation$PC3)#-0.01

####Direct model with persistence####
####33 individuals####
m.pers <- glmer(success~prop_time +(1|uid), family=binomial(link="logit"),control = glmerControl("bobyqa", optCtrl=list(maxfun=2e5)), data=pers_innovation)
summary(m.pers)

####Direct model with behavioral diversity####
####34 individuals####
m.div <- glmer(success~ H + (1|uid), family=binomial(link = "logit"), data = behavstrat_innovation)
summary(m.div)

####Direct model with behavioral selectivity####
####33 individuals####
m.behsl <- glmer(success~PropIS_Door_Lid +(1|uid), family=binomial(link="logit"), data=pers_innovation)
summary(m.behsl)

####Direct model with neophobia####
####35 individuals####
m.neo <- glmer(success~ lat.totouch.s.day +(1|uid), family=binomial(link = "logit"), data = neo_innovation)
summary(m.neo)

####Direct models with affiliative PCA####
####40 individuals####
#1 Affiliative PC1 - Friendliness
PC1.aff <- glm(success~PC1, family=binomial(link = "logit"), data = aff_innovation)
summary(PC1.aff)

#2 Affiliative PC2 - Bridging
PC2.aff <- glm(success~PC2, family=binomial(link = "logit"), data = aff_innovation)
summary(PC2.aff)

#3 Affiliative PC3 - Initiated Closeness
PC3.aff <- glm(success~PC3, family=binomial(link = "logit"),data = aff_innovation)
summary(PC3.aff)

####Direct model with agonistic PCA####
####28 individuals####
#1 Agonistic PC1 - Direct Aggression Integration
PC1.agr <- glm(success~PC1, family=binomial(link = "logit"), data = agr_innovation)
summary(PC1.agr)

#2 Agonistic PC2 - Received Aggression
PC2.agr <- glm(success~PC2, family=binomial(link = "logit"),data = agr_innovation)
summary(PC2.agr)

#3 Agonistic PC3 - Initiated Aggression Frequency
PC3.agr <- glm(success~PC3, family=binomial(link = "logit"),data = agr_innovation)
summary(PC3.agr)

#####Relative rank and innovation####
#####32 individuals####
m.rr <- glm(success~rel.rank, family=binomial(link = "logit"), data = cbi_innovation)
summary(m.rr)
