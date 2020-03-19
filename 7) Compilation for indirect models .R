#####Compilation of datasets for indirect analysis####
####Date: February 3, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we compile innovation success, clutton-brock index and behavioral data
#extracted from videos and social measure PCAs calculated from code (calculation of affiliative PCA) 
#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

####################################################################
####################################################################
####Section 1: Compilation of affiliative datasets####
#Read in relevant datasets containing individual innovation information and affiliative PCAs
innovation <- read.csv("yearly_innovation.csv",header=TRUE)
behav_strat <- read.csv("behavioral_strategies.csv", header=TRUE, row.names =1)
behav_neophobia <- read.csv("behavioral_neophobia.csv",header=TRUE)
cbi <- read.csv("cbi.csv", header=TRUE, row.names =1)
aff_PCA <- read.csv("affiliative_PCA.csv",header=TRUE, row.names = 1)
innovation <- read.csv("innovation_2018.csv", header=TRUE)
behav_pers <- subset(behav_strat, prop_time > 0)


####Merge Affiliative Datasets####
#Create dataset of innovation, affiliative PCA values and behavioral diversity
innovation_affPCA <- merge(aff_PCA,innovation, by =c("uid","col"))
innovation_behstrat <- merge(behav_strat, innovation_affPCA, by = c("uid","col"))
innovation_behstrat_cbi <- merge(innovation_behstrat, cbi, by = c("col","uid"))


#Create dataset of innovation, affiliative PCA values and day-level behavioral strategy
innovation_neophobia <- merge(behav_neophobia, innovation_affPCA, by = "uid")
innovation_neophobia_cbi <- merge(innovation_neophobia, cbi, by = c("uid","col"))

#Create dataset of innovation, affiliative PCA values and persistence and behavioral selectivity
innovation_pers <- merge(behav_pers, innovation_affPCA, by = c("uid","col"))
innovation_pers_cbi <- merge(innovation_pers, cbi, by = c("uid","col"))


write.csv(innovation_behstrat_cbi,"aff_behavioral_strategies_cbi.csv")
write.csv(innovation_neophobia_cbi,"aff_behavioral_neophobia_cbi.csv")
write.csv(innovation_pers_cbi,"aff_behavioral_persistence_cbi.csv")


####################################################################
####################################################################
####Section 2: Compilation of agonistic datasets####
#Read in relevant datasets containing agonistic PCAs
agr_PCA <- read.csv("agonistic_PCA.csv",header=TRUE, row.names =1)

####Merge Agonistic Datasets####
#Create dataset of innovation and agonistic PCA values
innovation_agrPCA <- merge(agr_PCA,innovation, by =c("uid","col"))
innovation_agr_strategies <- merge(behav_strat, innovation_agrPCA, by = c("uid","col"))
innovation_agr_strategies_cbi <- merge(innovation_agr_strategies, cbi, by =c("uid","col"))


#Create dataset of innovation, agonistic PCA values and day-level behavioral strategy
innovation_agr_neophobia <- merge(behav_neophobia, innovation_agrPCA, by = "uid")
innovation_agr_neophobia_cbi <- merge(innovation_agr_neophobia, cbi, by = c("uid","col"))

#Create dataset of innovation, agonistic PCA values and persistence and behavioral selectivity
innovation_agr_pers <- merge(behav_pers, innovation_agrPCA, by = c("uid","col"))
innovation_agr_pers_cbi <- merge(innovation_agr_pers, cbi, by = c("uid","col"))

  
write.csv(innovation_agr_strategies_cbi,"agr_behavioral_strategies_cbi.csv")
write.csv(innovation_agr_neophobia_cbi,"agr_behavioral_neophobia_cbi.csv")
write.csv(innovation_agr_pers_cbi,"agr_behavioral_persistence_cbi.csv")
