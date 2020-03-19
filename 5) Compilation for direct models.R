#####Compilation of datasets for indirect analysis####
####Date: February 3, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we compile innovation success, clutton-brock index and behavioral data
#extracted from videos and social measure PCAs calculated from code (calculation of affiliative PCA) 
#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

#The data####
#innovation contains the list of individuals who interacted with the puzzle box one or more times during 2019
#yearly_innovation contains the list of individuals who interacted with the puzzle box during 2018 and their overall success at opening the box during the year
#behav_strat contains the calculated behavioral strategies (behavioral selectivity, persistence, behavioral diversity)for each trial
#behav_neophobia contains the calculated behavioral strategy, neophobia, which is the latency to touch the box for the first trial of each day an individual interacted with the puzzle box
#cbi contains the 2018 yearly clutton-brock index rank, absolute rank and relative rank 
#aff_PCA contains the 2018 yearly social network PCAs
#agr_PCA contains the 2018 yearly social network PCAs
yearly_innovation <- read.csv("yearly_innovation.csv",header=TRUE)
behav_strat <- read.csv("behavioral_strategies.csv",header=TRUE, row.names =1)
behav_neophobia <- read.csv("behavioral_neophobia.csv",header=TRUE)
cbi <- read.csv("cbi.csv",header=TRUE, row.names =1)
aff_PCA <- read.csv("affiliative_PCA.csv",header=TRUE, row.names =1)
agr_PCA <- read.csv("agonistic_PCA.csv",header=TRUE, row.names =1)
innovation <- read.csv("innovation_2018.csv",header=TRUE)

#1. Behavioral selectivity, persistence and shannon index of behavioral diversity
behav_strat_innovation <- merge(innovation, behav_strat, by = c("uid","col"))
behav_strat_innovation$X <- NULL

write.csv(behav_strat_innovation,"behavioralstrategy_innovation.csv")

#2. Neophobia and innovation
neophobia_innovation <- merge(innovation, behav_neophobia, by =c("uid"))
write.csv(neophobia_innovation,"neophobia_innovation.csv")

#3. Affiliative and innovation
aff_innovation <- merge(innovation, aff_PCA, by = c("uid","col"))
aff_innovation_success <- merge(aff_innovation, yearly_innovation, by = c("uid","col"))
write.csv(aff_innovation_success,"aff_innovation.csv")

#4. Agonistic and innovation
agr_innovation <- merge(innovation, agr_PCA, by = c("uid","col"))
agr_innovation_success <- merge(agr_innovation, yearly_innovation, by = c("uid","col"))
write.csv(agr_innovation_success,"agr_innovation.csv")

#5. CBI and innovation
cbi_innovation <- merge(innovation, cbi, by = c("uid","col"))
cbi_innovation_success <- merge(cbi_innovation, yearly_innovation, by = c("uid","col"))
write.csv(cbi_innovation_success,"cbi_innovation.csv")


