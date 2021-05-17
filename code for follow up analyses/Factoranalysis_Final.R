

library(psych)
library(psy)
library(nFactors)
library(ltm)
library(GPArotation)

library(naniar)
library(tidyverse)
library(rstatix)
library(ggpubr)
##########

OSQ_daten$PD01_01<-replace(OSQ_daten$PD01_01, 261,'43.68')
OSQ_daten$PD01_01<-as.numeric(OSQ_daten$PD01_01)


# Create subset for questions with multiple answer options 
DS13 <- subset(OSQ_daten, select = c(108:119))                # from OSQ_daten select rows 108:119

PR07 <- subset(OSQ_daten, select = c(87:96))

DS02<- subset(OSQ_daten,select = c(120:125))

# Combines questions into one dataframe
Factoranalysis<- cbind(DS02, DS13, PR07)


# Bartletts test 
cortest.bartlett(Factoranalysis)                              # tests whether items correlate with each other (should be significant)

#Kaiser Meyer Olkin (MSA= Measure of sample adequacy)
KMO(Factoranalysis)                                           # should be bigger than 0,5 

#Determines the number of factors or components to retain in a factor analysis
nfactors(Factoranalysis, rotate = "varimax", fm="mle")        #rotation method varimax, factoring method Maximum likelihood

nfactors(Factoranalysis, rotate = "oblimin", fm="mle")        #rotation method oblimin, factoring method Maximum likelihood

##Number of Factors with parallel analysis
Eigenvalue<- eigen(cor(Factoranalysis))                       # computes Eigenvalue 
ap<- parallel(subject=nrow(Factoranalysis), var=ncol(Factoranalysis), rep=100, quantile=.05) #number of subjects, number of variables, number of replications of the correlation matrix,quantile of the distributio non which the decision is made 
nS<- nScree(x=Eigenvalue$values, aparallel=ap$eigen$qevpea)    

plotnScree(nS)                                                # Scree plot

# maximumlikelihood analysis (best if you want to follow up with a confirmatory factor analyses)

fit1<- factanal(Factoranalysis, 4, rotation="varimax")        # 4 factors to be fitted, rotation method "varimax"

print(fit1, digits=2, cutoff=.3) 

fit2<- factanal(Factoranalysis, 4, rotation="oblimin")        # 4 factors to be fitted, rotation method "oblimin"

print(fit2, digits=2, cutoff=.3)                              # print results of ML analysis, 2 digits after the comma, only display factor loadings higher than 0,3 

#Communality
1-fit1$uniquenesses                                    

1-fit2$uniquenesses