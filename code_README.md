# Marmot_SEM_Innovation
Code repository for Williams et al. (2021) testing indirect social influence on innovation success mediated by behavioral strategy in yellow-bellied marmots
There are 9 code files. 
Contact: dmwilliams@g.ucla.edu

Code 1 - 3: Calculate social networks, network measures and principal component analysis of social variables

1) Calculate association matrices 
This code uses all social observations of adults and yearlings seen over 5 times at colonies studied and creates two separate association matricies for affiliative and agonistic interactions. 
Input: social_observations.csv
Outputs: social_matrix_affiliative.xlsx, social_matrix_agonistic.xlsx

2) Calculate social measures
This code extracts social network measures listed in Table 1 from the two association matrices. 
Inputs: social_matrix_affiliative.xlsx, social_matrix_agonistic.xlsx
Outputs: social_measures_affiliative.csv, social_measures_agonistic.csv

3) Calculate PCAs
This code uses principal component analysis on the social network measures to reduce possible correlation between variables
Inputs: social_measures_affiliative.csv, social_measures_agonistic.csv
Outputs: affiliative_PCA.csv, agonsitic_PCA.csv

Code 4 - 6: Calculate values for behavioral strategies, compile spreadsheets and run direct models

4) Calculate behavioral values and rank
This code takes values extracted from jwatcher analysis of puzzle box videos and calculates the behavioral strategies (persistence, behavioral selectivity and behavioral diversity). This code additionally calculates clutton-brock index dominance rank from known behaviors
Inputs: Behaviors_2018.csv, social_observations.csv
Outputs: Behavioral_strategies.csv, cbi.csv 

5) Compilation for direct models
This code combines innovation success, affiliative and agonsitic PCAs, rank and behavioral strategies into six datasets on which direct models can be run
Inputs: yearly_innovation.csv, affiliative_PCA.csv, agonistic_PCA.csv, cbi.csv, Behavioral_strategies.csv, Behavioral_neophobia.csv
Outputs: affiliative_innovation.csv, agonistic_innovation.csv, cbi_innovation.csv, behavioralstrategy_innovation.csv, neophobia_innovation.csv

6) Direct models
This code contains all direct genrealized linear mixed effects and generalized linear models of behavioral or social factor on innovation success
Inputs: affiliative_innovation.csv, agonistic_innovation.csv, cbi_innovation.csv, behavioralstrategy_innovation.csv, neophobia_innovation.csv
Outputs: No saved files: Model numbers in Supplemental Table S1 

Code 7 - 8 Compile datasets for indirect SEM models for behavioral strategies and behavioral neophobia

7) Compilation for indirect models
This code combines innovation success, affiliative and agonistic PCAs, rank and behavioral strategies into six datasets on which indirect SEM models can be run
Inputs: yearly_innovation.csv, affiliative_PCA.csv, agonistic_PCA.csv, cbi.csv, Behavioral_strategies.csv, Behavioral_neophobia.csv
Outputs: aff_behavioral_neophobia_cbi.csv, agr_behavioral_neophobia_cbi.csv, aff_behavioral_strategies_cbi.csv, agr_behavioral_strategies_cbi.csv, aff_behavioral_persistence_cbi.csv, agr_behavioral_persistence_cbi.csv

8) Indirect Models
This code runs structural equation models using piecewiseSEM for the indirect effects of affiliative and agonistic social position, all four behavioral strategies and innovation success
Inputs: aff_behavioral_strategies_cbi.csv, agr_behavioral_strategies_cbi.csv, aff_behavioral_persistence_cbi.csv, agr_behavioral_persistence_cbi.csv, aff_behavioral_neophobia.csv, agr_behavioral_neophobia.csv
Outputs: No saved files: Model numbers in Supplemental Table S2 and S3

