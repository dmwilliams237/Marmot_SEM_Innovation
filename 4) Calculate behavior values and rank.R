#####Calculation of Behavior values####
####Date: February 4, 2020
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we calculate persistence, behavioral selectivity and shannon index of behavioral diveristy. 
#Definitions#
#Persistence is the proportion of time in sight spent on behaviors interacting with the puzzle box
#Behavioral selectivity is the proportion of time in sight spent on behaviors directed at the door or lid
#Behavioral diversity is the number of unique behaviors aimed at the puzzle box 
############################################################################
############################################################################
#Remove all previous data and set work directory
rm(list=ls())
setwd('~/Desktop/Marmot_SEM_Innovation-master/')

####The data####
#The dataset includes all output from Jwatcher analysis of 2018 puzzle box videos. Ethogram available in paper. 
behav <- read.csv("behaviors_2018.csv", header=TRUE)

####Calculate Persistence####
behav$prop_time <- with(behav, PropIS_Door.Nose + PropIS_Door.Paw + PropIS_Door.Bite + PropIS_Door.Other + PropIS_Lid.Nose + PropIS_Lid.Paw + PropIS_Lid.Bite + PropIS_Lid.Other + PropIS_Offside.Nose + PropIS_Offside.Paw + PropIS_Offside.Bite + PropIS_Offside.Other)


####Calculate Behavioral Selectivity####
behav$PropIS_Door_Lid <- with(behav, PropIS_Door.Nose + PropIS_Door.Paw + PropIS_Door.Bite + PropIS_Door.Other + PropIS_Lid.Nose + PropIS_Lid.Paw + PropIS_Lid.Bite + PropIS_Lid.Other )

####Calculate Behavioral Diversity- Shannon Index####
#Create new dataset with which to calculate
behav2 <- behav
###1) Calculate the log of each behavior. Behaviors that cannot be calculated are converted to 0s
behav2$logp_Door.Bite <- log(behav2$PropIS_Door.Bite)
behav2$logp_Door.Nose <- log(behav2$PropIS_Door.Nose)
behav2$logp_Door.Paw <- log(behav2$PropIS_Door.Paw)
behav2$logp_Door.Other <- log(behav2$PropIS_Door.Other)
behav2$logp_Lid.Bite <- log(behav2$PropIS_Lid.Bite)
behav2$logp_Lid.Nose <- log(behav2$PropIS_Lid.Nose)
behav2$logp_Lid.Paw <- log(behav2$PropIS_Lid.Paw)
behav2$logp_Lid.Other <- log(behav2$PropIS_Lid.Other)
behav2$logp_Other <- log(behav2$PropIS_Other)
behav2$logp_Offside.Bite <- log(behav2$PropIS_Offside.Bite)
behav2$logp_Offside.Nose <- log(behav2$PropIS_Offside.Nose)
behav2$logp_Offside.Paw <- log(behav2$PropIS_Offside.Paw)
behav2$logp_Offside.Other <- log(behav2$PropIS_Offside.Other)
behav2$logp_Rear.look <- log(behav2$PropIS_Rear.look)
behav2$logp_Stand.look <- log(behav2$PropIS_Stand.look)
behav2$logp_Walk <- log(behav2$PropIS_Walk)
behav2$logp_Stand.forage.near <- log(behav2$PropIS_Stand.forage.near)
behav2$logp_Stand.forage.at <- log(behav2$PropIS_Stand.forage.at)
behav2[is.na(behav2)] <- 0

####2) Calculate pi * log(pi)####
behav2$plogp_Door.Bite <- (behav2$PropIS_Door.Bite)*(behav2$logp_Door.Bite)
behav2$plogp_Door.Nose <- (behav2$PropIS_Door.Nose)*(behav2$logp_Door.Nose)
behav2$plogp_Door.Paw <- (behav2$PropIS_Door.Paw)*(behav2$logp_Door.Paw)
behav2$plogp_Door.Other <- (behav2$PropIS_Door.Other)*(behav2$logp_Door.Other)
behav2$plogp_Lid.Bite <- (behav2$PropIS_Lid.Bite)*(behav2$logp_Lid.Bite)
behav2$plogp_Lid.Nose <- (behav2$PropIS_Lid.Nose)*(behav2$logp_Lid.Nose)
behav2$plogp_Lid.Paw <- (behav2$PropIS_Lid.Paw)*(behav2$logp_Lid.Paw)
behav2$plogp_Lid.Other <- (behav2$PropIS_Lid.Other)*(behav2$logp_Lid.Other)
behav2$plogp_Other <- (behav2$PropIS_Other)*(behav2$logp_Other)
behav2$plogp_Offside.Bite <- (behav2$PropIS_Offside.Bite)*(behav2$logp_Offside.Bite)
behav2$plogp_Offside.Nose <- (behav2$PropIS_Offside.Nose)*(behav2$logp_Offside.Nose)
behav2$plogp_Offside.Paw <- (behav2$PropIS_Offside.Paw)*(behav2$logp_Offside.Paw)
behav2$plogp_Offside.Other <- (behav2$PropIS_Offside.Other)*(behav2$logp_Offside.Other)
behav2$plogp_Rear.look <- (behav2$PropIS_Rear.look)*(behav2$logp_Rear.look)
behav2$plogp_Stand.look <- (behav2$PropIS_Stand.look)*(behav2$logp_Stand.look)
behav2$plogp_Walk <- (behav2$PropIS_Walk)*(behav2$logp_Walk)
behav2$plogp_Stand.forage.near <- (behav2$PropIS_Stand.forage.near)*(behav2$logp_Stand.forage.near)
behav2$plogp_Stand.forage.at <- (behav2$PropIS_Stand.forage.at)*(behav2$logp_Stand.forage.at)

####3) Calculate log(plogp)####
behav2[behav2 == 0] <- NA
behav2$l_Door.Bite <- log(abs(behav2$plogp_Door.Bite))
behav2$l_Door.Nose <- log(abs(behav2$plogp_Door.Nose))
behav2$l_Door.Paw <- log(abs(behav2$plogp_Door.Paw))
behav2$l_Door.Other <- log(abs(behav2$plogp_Door.Other))
behav2$l_Lid.Bite <- log(abs(behav2$plogp_Lid.Bite))
behav2$l_Lid.Nose <- log(abs(behav2$plogp_Lid.Nose))
behav2$l_Lid.Paw <- log(abs(behav2$plogp_Lid.Paw))
behav2$l_Lid.Other <- log(abs(behav2$plogp_Lid.Other))
behav2$l_Offside.Bite <- log(abs(behav2$plogp_Offside.Bite))
behav2$l_Offside.Nose <- log(abs(behav2$plogp_Offside.Nose))
behav2$l_Offside.Paw <- log(abs(behav2$plogp_Offside.Paw))
behav2$l_Offside.Other <- log(abs(behav2$plogp_Offside.Other))
behav2$l_Other <- log(abs(behav2$plogp_Other))
behav2$l_Rear.look <- log(abs(behav2$plogp_Rear.look))
behav2$l_Stand.look <- log(abs(behav2$plogp_Stand.look))
behav2$l_Walk <- log(abs(behav2$plogp_Walk))
behav2$l_Stand.forage.near <- log(abs(behav2$plogp_Stand.forage.near))
behav2$l_Stand.forage.at <- log(abs(behav2$plogp_Stand.forage.at))
behav2[is.na(behav2)] <- 0

####4) Sum rows###
behav2$H<- -rowSums(behav2[,c(113:130)])

#Add to original dataframe 
behav$H <- behav2$H


####Write datafile####
write.csv(behav, "behavioral_strategies.csv")


  #####Calculation of Clutton-Brock Index####
####Date: February 6, 2020
#####Code by: Adriana Maldonado-Chaparro
####Contact: Dana M. Williams, dmwilliams@g.ucla.edu
#Purpose of Code###
#Here we calculate the Clutton-Brock Index, absolute rank and relative rank for all marmots in 2018. 
#Remove all previous data and set work directory
rm(list=ls())

####Below are the functions formerly contained in the compete package in R####
#get_wl_matrix
get_wl_matrix <- function(df, ties="remove"){
  mylevs = unique(c(as.character(df[,1]),as.character(df[,2])))
  
  if (ncol(df)==2){
    df <- as.data.frame(df)
    df[,1] <- factor(df[,1], levels=mylevs)
    df[,2] <- factor(df[,2], levels=mylevs)
    df$result<-1
    m1 = stats::xtabs(result ~ ., data = df)
    m1 <- m1[order(rownames(m1)), order(colnames(m1))]
    return(m1)
    
  }
  else
    if (ncol(df)>2 & ties=="remove"){
      df <- get_wl_df(df,ties="remove")
      df[,1] <- factor(df[,1], levels=mylevs)
      df[,2] <- factor(df[,2], levels=mylevs)
      m1 = stats::xtabs(result ~ ., data = df)
      m1 <- m1[order(rownames(m1)), order(colnames(m1))]
      return(m1)
    }
  
  else
    if (ncol(df)>2 & ties=="keep"){
      
      df <- get_wl_df(df,ties="keep")
      dfT <-df[(df[,3]==.5),]
      dfWL <-df[(df[,3]==1),]
      dfWL[,1] <- factor(dfWL[,1], levels=mylevs)
      dfWL[,2] <- factor(dfWL[,2], levels=mylevs)
      m1 = stats::xtabs(result ~ ., data = dfWL)
      m1 <- m1[order(rownames(m1)), order(colnames(m1))]
      #giving 0.5 for a win for ties
      dfT[ , c("winner", "loser")] <- lapply(dfT[ , c("winner", "loser")], function(x) factor(x, levels = mylevs))
      m2=stats::xtabs(result ~ ., data = dfT)
      m2 <- m2[order(rownames(m2)), order(colnames(m2))]
      mm <- m1 + m2 + t(m2)
      
      return(mm)
      
    }
}

#get_di_matrix

get_di_matrix <- function(m, type="wl"){
  
  mtxbinom<-mx<-m1<-m2<-m3<-NULL
  
  if(type=="wl") {
    m <- as.matrix(m)
    m <- (m > t(m)) + 0
    return(m)
  }
  
  if(type=="wlties") {
    m <- as.matrix(m)
    m <-((m > t(m)) + 0)  + ((m == t(m)) + 0)/2
    return(m)
  }
  
  if(type=="wlties0") {
    m <- as.matrix(m)
    mx <-((m > t(m)) + 0)  + ((m == t(m)) + 0)/2
    mx[(m==0 & t(m)==0)==T] <-0
    return(mx)
  }
  
  
  if(type=="wlbinom") {
    m <- as.matrix(m)
    mx <- mtxbinom(m)
    return(mx)
  }
  
  
  if(type=="wlbinomties") {
    m <- as.matrix(m)
    mx <- mtxbinom(m)
    mx <- mx + ((m == t(m)) + 0)/2
    return(mx)
  }
  
  
  if(type=="pa") {
    m <- as.matrix((m > 0) + 0)
    return(m)
  }
  
  if(type=="dom") {
    m <- as.matrix(m)
    
    m1 <- (m > t(m)) + 0     #put in +1s
    m2 <- -((m < t(m)) - 0)  #put in -1s
    
    m3 <- ((m == t(m)) + 0)/2   #put in 0.5s
    m3[(m==0 & t(m)==0)==T] <-0  #put in 0s to structural zeros
    
    mx <- m1 + m2 + m3
    
    return(mx)
  }
  
}
###################################################################

####The Data####
#All social interactions observed in 2018 that occurred at the 8 social groups analyzed in the paper.  
socnet <- read.csv("social_observations.csv", header=TRUE)


#Create empty dataset to store results
cbi.results_18 <- NULL

# use names of colony level: extract it from col_area 
socnet$col <- sapply(strsplit(as.character(socnet$col), "_"), "[", 1)

# subset: only aggressive interactions
all.soc.int <- unique(socnet$interac_type)
agg.int <- c("agr", "disp", "aggression", "agr_bite", "agr_box", "agr_chase", "agr_disp", "agr_grab/slap/push", "agr_mount",
             "agr_mouth spar", "agr_mouthspar", "agr_pounce", "agr_snap/snarl/hiss", "agr_wrestle", "disp_proximity", "disp_simple")

socint_agg <- socnet[socnet[, "interac_type"] %in% agg.int , , drop = FALSE]

# list of unique colonies in each year
colonies <-  as.character(unique(socint_agg$col))

# loop through each colony for each year
for (col in colonies) {
  print(col)
  
  
  # subset the data so we only work with one colony area at each time
  socint <- socint_agg[which(socint_agg["col"] ==  col), , drop = FALSE]
  
  # subset and rename data to be used in function
  soc.data.all <- socint[colnames(socint) %in% c("col", "uid_ini", "uid_rec", "uid_win", "interac_type")]
  
  # add loser
  soc.data.all = within(soc.data.all, {
    uid_lose = ifelse(as.character(uid_win) == as.character(soc.data.all$uid_ini), as.character(soc.data.all$uid_rec), as.character(soc.data.all$uid_ini))})
  
  soc.data <- soc.data.all[!is.na(soc.data.all["uid_win"]) | !is.na(soc.data.all["uid_lose"]), , drop = FALSE]
  
  # calculate nb of  individuals
  indivs <- length(unique(as.vector(rbind(as.character(soc.data$uid_win), as.character(soc.data$uid_rec))))) 
  
  # check there are enough observations in the colony
  if ((nrow(soc.data) > 2 & indivs > 2)) {
    
    # subset data
    intdata <- data.frame(soc.data[c("uid_lose", "uid_win")])
    
    # create the interaction frequency sociomatrices
    mat <- get_wl_matrix(intdata)
    
    # Transform frequency interaction sociomatrix (valued data) into a dichotomized 1/0 matrix
    # win/loss binary matrices: directed, asymmetric = adjacency matrices  
    bimat <- get_di_matrix(mat, type = "wl")
    
    # extract uids
    uids <- rownames(mat)
    
    # Clutton-Brock Index 
    
    # B represents the number of individuals that i defeated in one or more interactions,
    BB <- apply(bimat, 2, FUN = function(x) sum(x, na.rm = TRUE)) # sum over cols
    
    # L represents the number of individuals by which i was defeated 
    LL <- apply(bimat, 1, FUN = function(x) sum(x, na.rm = TRUE)) # sum over rows
    
    # Σb represents the total number of individuals (excluding i) that those represented in B defeated
    # Number of individuals that the individuals that i defeat, beat
    
    bb <- structure(rep(NA, nrow(bimat)), names=rownames(bimat))
    
    for (dd in rownames(bimat)){
      defeated <- uids[(bimat[,dd] > 0)]
      temp <- 0
      
      if (length(defeated) == 0) bb[dd] <- 0
      
      for (ii in defeated){
        temp0 <- BB[ii]
        temp <- temp0 + temp
        
        bb[dd] <- temp
      }
    }
    
    # Σl represents the total number of individuals (excluding i) by which those represented in L were defeated.  
    # Number of individuals that beaten i, beat
    
    ll <- structure(rep(NA, nrow(bimat)), names=rownames(bimat))
    
    for (dd in rownames(bimat)){
      defectors <- uids[(bimat[dd,] > 0)]
      temp <- 0
      
      if (length(defectors) == 0) ll[dd] <- 0
      
      for (ii in defectors){
        temp0 <- LL[ii]
        temp <- temp0 + temp
        
        ll[dd] <- temp
      }
    }
    
    
    
    # calculate CBI = BB + bb + 1 / LL + ll + 1
    cbi <- round((BB + bb + 1) / (LL + ll + 1), 2)
    
    # sort CBI hing to low
    scbi <- data.frame(sort(cbi))
    
    # assign absolute rank
    scbi$abs.rank <- seq(0, (nrow(scbi) - 1))
    
    # reassign absolute rank for ties
    for (jj in scbi$sort.cbi.){
      ids <- rownames(scbi)[scbi$sort.cbi. == (jj)]
      
      for (ii in ids){
        #rank.score <- scbi[ids[ii],]$abs.rank
        
        if (ids > 1) {
          temp.score <- round(mean(scbi[rownames(scbi) %in% ids,]$abs.rank), 2)
          scbi[rownames(scbi) %in% ids,]$abs.rank <- rep(temp.score, length(scbi[rownames(scbi) %in% ids,]$abs.rank))
        }
      }
    }
    
    # relative rank
    scbi$rel.rank <- round(scbi$abs.rank / max(scbi$abs.rank), 1)
    
    # reassign relative rank for ties
    for (jj in scbi$sort.cbi.){
      ids <- rownames(scbi)[scbi$sort.cbi. == (jj)]
      
      for (ii in ids){
        #rank.score <- scbi[ids[ii],]$abs.rank
        
        if (ids > 1) {
          temp.score <- round(mean(scbi[rownames(scbi) %in% ids,]$rel.rank), 2)
          scbi[rownames(scbi) %in% ids,]$rel.rank <- rep(temp.score, length(scbi[rownames(scbi) %in% ids,]$rel.rank))
        }
      }
    }
    
    # save the results
    temp <- data.frame(scbi)
    temp$uid <- names(cbi)
    temp$year <- rep(unique(soc.data$year), nrow(temp))
    temp$col <- rep(unique(soc.data$col), nrow(temp))
    
    cbi.results_18 <- rbind(cbi.results_18, temp, deparse.level = 1)
    
  }
}


summary(cbi.results_18)

# rename
names(cbi.results_18) <- c("cbi", "abs.rank", "rel.rank", "uid", "col")

# reorganize columns
cbi.results_18 <- cbi.results_18[c("col", "uid", "cbi", "abs.rank", "rel.rank")]

cbi.results_18 <- as.data.frame(cbi.results_18)
cbi.results_18 <- cbind(rownames(cbi.results_18), data.frame(cbi.results_18, row.names=NULL))


write.csv(cbi.results_18, "cbi.csv")

