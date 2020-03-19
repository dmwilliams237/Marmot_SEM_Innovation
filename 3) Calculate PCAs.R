####Date: February 2, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we reduce the affiliative social measures through principal component 
#analysis to avoid collinearity between measures

####################################################################
####################################################################
####Section 1: Affiliative PCA####

#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

####Required packages####
library(psych)
library(igraph)
library(stats)

#The data####
#The datasheet consists of the association matricies for agonistic interactions at 8 social groups.
#There are 60 total individuals with 13 yearling females, 23 adult females, 15 yearling males and 9 adult males. 
#Pups were excluded from the social network as they are not present for the majority of the data collection period. 


## To calculate PCA, remove any duplicate values so there is only one entry for each year.uid
PCA <- read.csv("social_measures_affiliative.csv", na.strings=c("NA"))
PCA$uid <- gsub("^X", '', PCA$uid) #remove the X in uid
PCA$X <- NULL
PCA <- na.omit(PCA)


datPCA <- PCA[c(3:11)]
names(datPCA) #check datPCA

summary(datPCA) #find which columns have NAs


# Calculate PCA using psych package. 
# Initial extraction of components. Determine number of components to retain Eigenvalue-one criteria

pca <- principal(datPCA, nfactors=9, rotate="none", scores=T) 
pca 

# Determine number of components to retain. Retain values >1.0 Eigenvalue
plot(pca$values, type="b", ylab="Eigenvalues", xlab="Component", lab=c(10,5,5))
abline(h=1)

#Generate a vector with number of PCAs that will be used
pca2.r <- principal(datPCA, nfactors=3, rotate="none", scores=T)
pca2.r

#Interpretation of principal components are highest loading components (>0.60)
#PC1 = indegree, outdegree, outstrength, instrength, eigenvector centrality
#PC2 = negative betweenness
#PC3 = outcloseness

# h2 (communality estimate) means %var in observed variable accounted for by retained components
# Obtain factor scores

pca2.sc <- pca2.r$r.scores

pca.new<-cbind(PCA,pca2.r$scores[,1:3]) #put PCA variables back into PCA dataset, change the 3 into nfactors
write.csv(pca.new, "affiliative_PCA.csv")

#End of affiliative PCA code#


####################################################################
####################################################################
####Section 2: Agonsitic PCA####

#Remove all previous data and set work directory
rm(list=ls())


## To calculate PCA, remove any duplicate values so there is only one entry for each year.uid
PCA <- read.csv("social_measures_agonistic.csv", na.strings=c("NA"))
PCA$uid <- gsub("^X", '', PCA$uid) #remove the X in uid
PCA$X <- NULL
PCA <- na.omit(PCA)
PCA$instrength <- as.numeric(PCA$instrength)

datPCA <- PCA[c(3:11)]
names(datPCA) #check datPCA

summary(datPCA) #find which columns have NAs


# Calculate PCA using psych package. 
# Initial extraction of components. Determine number of components to retain Eigenvalue-one criteria
pca <- principal(datPCA, nfactors=9, rotate="none", scores=T) 
pca 

# Determine number of components to retain. Retain values >1.0 Eigenvalues
plot(pca$values, type="b", ylab="Eigenvalues", xlab="Component", lab=c(10,5,5))
abline(h=1)


#Generate a vector with number of PCAs that will be used
pca2.r <- principal(datPCA, nfactors=3, rotate="none", scores=T)
pca2.r

#Interpretation of principal components are highest loading components (>0.60)
#PC1 = indegree, outdegree, outcloseness, incloseness, eigenvector centrality, betweenness
#PC2 = indegree, instrength
#PC3 = outcloseness


# h2 (communality estimate) means %var in observed variable accounted for by retained components
# Obtain factor scores

pca2.sc <- pca2.r$r.scores

pca.new<-cbind(PCA,pca2.r$scores[,1:3]) #put PCA variables back into PCA dataset, change the 3 into nfactors
write.csv(pca.new, "agonistic_PCA.csv")

#End of agonistic PCA code#
