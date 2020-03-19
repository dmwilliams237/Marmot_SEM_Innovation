#####Calculation of social measures from association matrices#####
####Date: February 2, 2020
####Code by: Adriana Maldonado-Chaparro 
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we calculate the social network measures for indegree, outdegree, betweenness, outcloseness,
#incloseness, local clustering, eigenvector centrality, outstrength and instrength for the two social network matrices, affiliative and agonisitic
####################################################################
####################################################################
####Section 1: Calculate affiliative social measures####
#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

####Required packages####
library(XLConnectJars)
library(XLConnect)
library(igraph)

#The data####
#The datasheet consists of the association matricies for agonistic interactions at 8 social groups.
#There are 60 total individuals with 13 yearling females, 23 adult females, 15 yearling males and 9 adult males. 
#Pups were excluded from the social network as they are not present for the majority of the data collection period. 

sheetnames <- getSheets(loadWorkbook("social_matrix_affiliative.xlsx"))

laasocmeasures <- length(snm <- c("indegree", "outdegree", "betweenness","outcloseness", "incloseness",
                                  "local clustering", "eigenvector centrality", "outstrength", "instrength"))

results_snm <- matrix(nrow = 0, ncol=laasocmeasures+2, byrow=F)

for(jj in 1:length(sheetnames)) {
  sheet <- readWorksheetFromFile("social_matrix_affiliative.xlsx",
                                 sheet=sheetnames[jj], header=TRUE) 
  
  mtx.w<-data.matrix(data.frame(sheet)) # convert the matrix into a matrix object
  rownames(mtx.w)<-colnames(mtx.w)
  diag(mtx.w) <- 0
  
  # binary matrix
  mtx.b <- as.matrix(ifelse(mtx.w[,]>0, 1, 0))
  print(mtx.b)
  
  colonyid <- sheetnames[jj]
  
  # retrive the year
  data <- as.data.frame(strsplit(sheetnames[jj],"_"))#split the name of the file by _
  year <- as.character(paste(as.character(data[1,])))
  colony <- as.character(paste(as.character(data[2,])))
  
  #create empty matrix with same length as the original matrix
  socialmeasures <- matrix(nrow=length(rownames(mtx.w)),ncol=laasocmeasures+2, byrow=F) 
  
  # assign the group ID of each individual in each matrix
  socialmeasures[,1] <- rep(colonyid, length(rownames(mtx.w)))
  # pull out the ID of the individuals in each matrix  
  socialmeasures[,2] <- rownames(mtx.w) # retrive individual ID from the original matrix
  print(socialmeasures)
  
  # convert the matrix in a directed, unweighted graph
  graphN <- graph.adjacency(mtx.b, mode="directed", weighted=NULL, diag=F)
  graph <- graph.adjacency(mtx.w, mode="directed", weighted=TRUE, diag=F)
  #1. Indegree
  measure1 <- as.numeric(degree(graph,  v=V(graph), mode = "in", loops = FALSE, normalized = T))
  print(measure1)
  # fill output matrix with the calculated social measure 
  socialmeasures[,3] <- measure1
  
  # 2. calculate the OUTDEGREE of each node of the matrix
  measure2 <- as.numeric(degree(graph,  v=V(graph), mode = "out", loops = FALSE, normalized = T))
  print(measure2)
  # fill output matrix with the calculated social measure 
  socialmeasures[,4] <- measure2
  
  # 3. calculate the BETWEENNESS of each node of the matrix
  measure3 <- as.numeric(betweenness(graph, v=V(graph), directed = F, weights = NULL, nobigint = TRUE, normalized = TRUE))
  print(measure3)
  # fill output matrix with the calculated social measure 
  socialmeasures[,5] <- measure3
  
  # 4. calculate the OUT CLOSENESS of each node of the matrix.calculated from a vertex
  measure4 <- as.numeric(closeness(graph, vids=V(graph), mode = "out", weights = NULL, normalized = T))
  print(measure4)
  # fill output matrix with the calculated social measure 
  socialmeasures[,6] <- measure4
  
  # 5. calculate the IN CLOSENESS of each node of the matrix
  measure5 <- as.numeric(closeness(graph, vids=V(graph), mode = "in", weights = NULL, normalized = T))
  print(measure5)
  # fill output matrix with the calculated social measure 
  socialmeasures[,7] <- measure5
  
  # 6. calculate the CLUSTERING COEFICIENT of each node of the matrix
  measure6 <- as.numeric(transitivity(graph, type= "local", vids=NULL))
  print(measure6)
  # fill output matrix with the calculated social measure 
  socialmeasures[,8] <- measure6
  
  # 7. calculate the EIGENVECTOR of each node of the matrix
  measure7 <- as.numeric(evcent(graphN, directed = FALSE, scale = TRUE, weights = NULL)$vector)
  print(measure7)
  # fill output matrix with the calculated social measure 
  socialmeasures[,9] <- measure7
  
  # 8. calculate the OUT STRENGTH of each node of the matrix
  measure8 <- as.numeric(graph.strength(graph, vids = V(graph), mode = c("out")))
  print(measure8)
  # fill output matrix with the calculated social measure 
  socialmeasures[,10] <- measure8
  
  # 9. calculate the IN STRENGTH of each node of the matrix
  measure9 <- as.numeric(graph.strength(graph, vids = V(graph), mode = c("in")))
  print(measure9)
  # fill output matrix with the calculated social measure 
  socialmeasures[,11] <- measure9
  
  # check
  print(socialmeasures)
  
  
  # append the results to the main list
  results_snm <- rbind(results_snm, socialmeasures, deparse.level = 1)
}
# name the variables (columns) of the output matrix
colnames(results_snm) <- c("col", "uid", "indegree", "outdegree", "betweenness","outcloseness", "incloseness",
                           "local_clustering", "eigenvector", "outstrength", "instrength")

# check
summary(results_snm)

# write the results to a csv file
write.csv(results_snm, "social_measures_affiliative.csv")

# end of affiliative social network measures


####################################################################
####################################################################
####Section 2: Calculate agonistic social measures####
#Remove all previous data
rm(list=ls())

#The data####
#The datasheet consists of the association matricies for agonistic interactions at 8 social groups.
#There are 48 total individuals with 12 yearling females, 19 adult females, 13 yearling males and 4 adult males. 
#Pups were excluded from the social network as they are not present for the majority of the data collection period. 

sheetnames <- getSheets(loadWorkbook("social_matrix_agonistic.xlsx"))

laasocmeasures <- length(snm <- c("indegree", "outdegree", "betweenness","outcloseness", "incloseness",
                                  "local clustering", "eigenvector centrality","outstrength", "instrength"))

results_snm <- matrix(nrow = 0, ncol=laasocmeasures+2, byrow=F)

for(jj in 1:length(sheetnames)) {
  sheet <- readWorksheetFromFile("social_matrix_agonistic.xlsx",
                                 sheet=sheetnames[jj], header=TRUE) 
  
  mtx.w<-data.matrix(data.frame(sheet)) # convert the matrix into a matrix object
  rownames(mtx.w)<-colnames(mtx.w)
  diag(mtx.w) <- 0
  
  # binary matrix
  mtx.b <- as.matrix(ifelse(mtx.w[,]>0, 1, 0))
  print(mtx.b)
  
  colonyid <- sheetnames[jj]
  
  # retrive the year
  data <- as.data.frame(strsplit(sheetnames[jj],"_"))#split the name of the file by _
  year <- as.character(paste(as.character(data[1,])))
  colony <- as.character(paste(as.character(data[2,])))
  
  #create empty matrix with same length as the original matrix
  socialmeasures <- matrix(nrow=length(rownames(mtx.w)),ncol=laasocmeasures+2, byrow=F) 
  
  # assign the group ID of each individual in each matrix
  socialmeasures[,1] <- rep(colonyid, length(rownames(mtx.w)))
  # pull out the ID of the individuals in each matrix  
  socialmeasures[,2] <- rownames(mtx.w) # retrive individual ID from the original matrix
  print(socialmeasures)
  
  # convert the matrix in a directed, unweighted graph
  graphN <- graph.adjacency(mtx.b, mode="directed", weighted=NULL, diag=F)
  graph <- graph.adjacency(mtx.w, mode="directed", weighted=TRUE, diag=F)
  #1. Indegree
  measure1 <- as.numeric(degree(graph,  v=V(graph), mode = "in", loops = FALSE, normalized = T))
  print(measure1)
  # fill output matrix with the calculated social measure 
  socialmeasures[,3] <- measure1
  
  # 2. calculate the OUTDEGREE of each node of the matrix
  measure2 <- as.numeric(degree(graph,  v=V(graph), mode = "out", loops = FALSE, normalized = T))
  print(measure2)
  # fill output matrix with the calculated social measure 
  socialmeasures[,4] <- measure2
  
  # 3. calculate the BETWEENNESS of each node of the matrix
  measure3 <- as.numeric(betweenness(graph, v=V(graph), directed = F, weights = NULL, nobigint = TRUE, normalized = TRUE))
  print(measure3)
  # fill output matrix with the calculated social measure 
  socialmeasures[,5] <- measure3
  
  # 4. calculate the OUT CLOSENESS of each node of the matrix.calculated from a vertex
  measure4 <- as.numeric(closeness(graph, vids=V(graph), mode = "out", weights = NULL, normalized = T))
  print(measure4)
  # fill output matrix with the calculated social measure 
  socialmeasures[,6] <- measure4
  
  # 5. calculate the IN CLOSENESS of each node of the matrix
  measure5 <- as.numeric(closeness(graph, vids=V(graph), mode = "in", weights = NULL, normalized = T))
  print(measure5)
  # fill output matrix with the calculated social measure 
  socialmeasures[,7] <- measure5
  
  # 6. calculate the CLUSTERING COEFICIENT of each node of the matrix
  measure6 <- as.numeric(transitivity(graph, type= "local", vids=NULL))
  print(measure6)
  # fill output matrix with the calculated social measure 
  socialmeasures[,8] <- measure6
  
  # 7. calculate the EIGENVECTOR of each node of the matrix
  measure7 <- as.numeric(evcent(graph, directed = FALSE, scale = TRUE, weights = NULL)$vector)
  print(measure7)
  # fill output matrix with the calculated social measure 
  socialmeasures[,9] <- measure7
  
  # 8. calculate the OUT STRENGTH of each node of the matrix
  measure10 <- as.numeric(graph.strength(graph, vids = V(graph), mode = c("out")))
  print(measure10)
  # fill output matrix with the calculated social measure 
  socialmeasures[,10] <- measure10
  
  # 9. calculate the IN STRENGTH of each node of the matrix
  measure9 <- as.numeric(graph.strength(graph, vids = V(graph), mode = c("in")))
  print(measure9)
  # fill output matrix with the calculated social measure 
  socialmeasures[,11] <- measure9
  
  # append the results to the main list
  results_snm <- rbind(results_snm, socialmeasures, deparse.level = 1)
}
# name the variables (columns) of the output matrix
colnames(results_snm) <- c("col", "uid", "indegree", "outdegree", "betweenness","outcloseness", "incloseness",
                           "local_clustering", "eigenvector","outstrength", "instrength")

# check
summary(results_snm)

# write the results to a csv file
write.csv(results_snm, "social_measures_agonistic.csv")

# end of agonistic social network measures

