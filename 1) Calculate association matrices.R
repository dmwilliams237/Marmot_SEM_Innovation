#####Compilation and calculation of datasets####
####Date: February 3, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we calculate association matricies for affiliative and agonistic interactions in 2018 for the examined colonies
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')
library(igraph)
library(stats)
library(XLConnect)
library(XLConnectJars)

####The Data####
#Soc_obs refers to a dataset with all social interactions from 2018 for the 8 colonies under analysis
#groupassignment_2018 is the dataset containing all social groups from 2018 for the 8 colonies under analysis
soc_obs <- read.csv("social_observations.csv", header = TRUE, na.strings=c("NA",""))
####################################################################
####################################################################

#Affiliative interactions, soc_obs_aff = affiliative interactions
soc_obs$interac_type <- as.character(soc_obs$interac_type) #coerce variable interaction type to character type
split <- strsplit(soc_obs$interac_type,'_') #split the variable interaction type using "_"
type <- rep(NA, length(soc_obs$interac_type))  #create a vector to hold the new variable

# create a loop to fill the vector that will hold the new variable
for (ii in 1:length(split)) { 
  type[ii] <- split[[ii]][1]
} 

# joint the vector type to the data frame
soc_obs$type <- as.factor(type)
head(soc_obs, 10); summary(soc_obs)

sum(length(which(soc_obs$type == "disp")), length(which(soc_obs$type == "agr")), length(which(soc_obs$type == "mount")), 
    length(which(soc_obs$type == "sex"))) #281

#take out all the non affiliative interactions
soc_obs_aff<- subset(soc_obs[!(soc_obs$type %in% c("agr", "disp" ,"mount","sex")),]) 
head(soc_obs_aff, 10); summary(soc_obs_aff); dim(soc_obs_aff) #check the data total data = 4038 

#take out all the NA's 
soc_obs_aff <- subset(soc_obs_aff, !is.na(soc_obs_aff$interac_type)) #taking out any rows that have interaction type of NA
length(soc_obs_aff $type) #5915
dim(soc_obs_aff) #5915 15 this is now the number of affiliative inteactions
View(soc_obs_aff)

# check that the number of affiliative data corresponds
length(soc_obs$type) - length(soc_obs_aff$type ) #247

#Affiliative Interactions in table
# pull out the individuals of each of the groups
dyad_aff <- na.omit(subset(soc_obs_aff, select=c(uid_ini, uid_rec, col))) # delete all NAs
summary(dyad_aff); dim(dyad_aff) #2442

# create a vector with the names of the social groups
groupsID <- as.character(unique(dyad_aff$col))

# create a list with the unique group ID
grouplist <-  vector("list", length(groupsID))

# name the elements in the list. To do that, ask for the names of the groups
groupsID
names(grouplist) <- groupsID

# create different objects for each group ID
for (ii in 1:(length(groupsID))){
  groupinter <- subset(dyad_aff, dyad_aff$col == groupsID[ii], select= c(uid_ini, uid_rec))
  grouplist[[ii]] <- groupinter     
  temp.name <-  groupsID[ii]
  names(grouplist)[ii] <- temp.name
}

#check we have the data in the list
grouplist

#Make into matrices

# create a list with the unique year.col
matrixlist <- vector("list", length(groupsID))
names(matrixlist) <- groupsID

#create a loop to read the list
for (ii in 1:(length(groupsID))) {
  el<-as.matrix(grouplist[[ii]]) #read the data that is stored in a list
  el[,1]<-as.character(el[,1])#Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems.
  el[,2]<-as.character(el[,2])
  g=graph.edgelist(el)
  
  a=as.matrix(get.adjacency(g,sparse=TRUE, type ="both"))
  rownames(a) <- colnames(a)
  
  matrixlist[[ii]] <- a
  temp.name <-  (groupsID[ii])
  names(matrixlist)[ii] <- temp.name
}

summary(matrixlist)
#Save to excel

matrices <- loadWorkbook("social_matrix_affiliative.xlsx", create = TRUE) #create workbook to store the results
createSheet(matrices, names(matrixlist)) # create a worksheet name as the social group contained in the matrixlist object
writeWorksheet(matrices, matrixlist, names(matrixlist), header = TRUE) # Write built-in dataset matrixlist names created above
saveWorkbook(matrices) # Save workbook - this actusocint_ad writes the output file to disk/computer


#################################

##Longevity social networks matrix calculations for agriliative interactions with all animals
rm(list=ls())

####Questions that need to be resolved: do we need to delete all NAs??####
#read in social observations from 2018 with sex added 
soc_obs <- read.csv("social_observations.csv", header = TRUE, na.strings=c("NA",""))
dim(soc_obs) #3595

#agonistic interactions, soc_obs_agr = agonistic interactions
soc_obs$interac_type <- as.character(soc_obs$interac_type) #coerce variable interaction type to character type
split <- strsplit(soc_obs$interac_type,'_') #split the variable interaction type using "_"
type <- rep(NA, length(soc_obs$interac_type))  #create a vector to hold the new variable

# create a loop to fill the vector that will hold the new variable
for (ii in 1:length(split)) { 
  type[ii] <- split[[ii]][1]
} 

# joint the vector type to the data frame
soc_obs$type <- as.factor(type)
head(soc_obs, 10); summary(soc_obs)
## Subset the dataset just the agressive interactions
# check how many agressive interactions in the data 
sum(length(which(soc_obs$type == "disp")), length(which(soc_obs$type == "agr"))) #236

#take out all the affiliative interactions, sex and mount
soc_obs_agr <- subset(soc_obs [(soc_obs$type %in% c("agr", "disp")),]) 
head(soc_obs_agr, 10); summary(soc_obs_agr); dim(soc_obs_agr) #check the data total data = 236   

# check that the number of agressive datacorresponds
length(soc_obs$type ) - length(soc_obs_agr$type ) #3359

#take out all the NA's 
soc_obs_agr <- subset(soc_obs_agr, !is.na(soc_obs_agr$interac_type)) #taking out any rows that have interaction type of NA
length(soc_obs_agr $type) #236
dim(soc_obs_agr) #236 15 

# check that the number of agriliative data corresponds
length(soc_obs$type) - length(soc_obs_agr$type ) #3359

#Agonistic Interactions in table
# pull out the individuals of each of the groups
dyad_agr <- na.omit(subset(soc_obs_agr, select=c(uid_ini, uid_rec, col))) # delete all NAs
summary(dyad_agr); dim(dyad_agr) #4038

# create a vector with the names of the social groups
groupsID <- as.character(unique(dyad_agr$col))

# create a list with the unique group ID
grouplist <-  vector("list", length(groupsID))

# name the elements in the list. To do that, ask for the names of the groups
groupsID
names(grouplist) <- groupsID

# create different objects for each group ID
for (ii in 1:(length(groupsID))){
  groupinter <- subset(dyad_agr, dyad_agr$col == groupsID[ii], select= c(uid_ini, uid_rec))
  grouplist[[ii]] <- groupinter     
  temp.name <-  groupsID[ii]
  names(grouplist)[ii] <- temp.name
}

#check we have the data in the list
grouplist

#Make into matrices

# create a list with the unique year.col
matrixlist <- vector("list", length(groupsID))
names(matrixlist) <- groupsID

#create a loop to read the list
for (ii in 1:(length(groupsID))) {
  el<-as.matrix(grouplist[[ii]]) #read the data that is stored in a list
  el[,1]<-as.character(el[,1])#Because the vertex IDs in this dataset are numbers, we make sure igraph knows these should be treated as characters. Otherwise, it'll create problems.
  el[,2]<-as.character(el[,2])
  g=graph.edgelist(el)
  
  a=as.matrix(get.adjacency(g,sparse=TRUE, type ="both"))
  rownames(a) <- colnames(a)
  
  matrixlist[[ii]] <- a
  temp.name <-  (groupsID[ii])
  names(matrixlist)[ii] <- temp.name
}

summary(matrixlist)

#Save to excel

matrices <- loadWorkbook("social_matrix_agonistic.xlsx", create = TRUE) #create workbook to store the results
createSheet(matrices, names(matrixlist)) # create a worksheet name as the social group contained in the matrixlist object
writeWorksheet(matrices, matrixlist, names(matrixlist), header = TRUE) # Write built-in dataset matrixlist names created above
saveWorkbook(matrices) # Save workbook - this actusocint_ad writes the output file to disk/computer
