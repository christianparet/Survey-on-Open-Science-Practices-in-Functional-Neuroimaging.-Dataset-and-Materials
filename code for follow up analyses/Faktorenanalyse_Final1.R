


library(psych)
library(psy)
library(nFactors)
library(ltm)
library(GPArotation)



##########

OSQ_daten$PD01_01 <- replace(OSQ_daten$PD01_01, 261, '43.68')
OSQ_daten$PD01_01 <- as.numeric(OSQ_daten$PD01_01)


# Create subset for questions with multiple answer options
DS13 <-
  subset(OSQ_daten, select = c(108:119))                # from OSQ_daten select rows 108:119

PR07 <- subset(OSQ_daten, select = c(87:96))
PR07 <-
  subset(OSQ_daten, select = c(87:95))                  # excluding item PR07_01 because of poor fit
DS02 <- subset(OSQ_daten, select = c(120:125))

# Combines questions into one dataframe
Factoranalysis <- cbind(DS02, DS13, PR07)


# Bartletts test
cortest.bartlett(Factoranalysis)                              # tests whether items correlate with each other (should be significant)

#Kaiser Meyer Olkin (MSA= Measure of sample adequacy)
KMO(Factoranalysis)                                           # should be bigger than 0,5

#Determines the number of factors or components to retain in a factor analysis
nfactors(Factoranalysis, rotate = "varimax", fm = "mle")        #rotation method varimax, factoring method Maximum likelihood

nfactors(Factoranalysis, rotate = "oblimin", fm = "mle")        #rotation method oblimin, factoring method Maximum likelihood

##Number of Factors with parallel analysis
Eigenvalue <-
  eigen(cor(Factoranalysis))                       # computes Eigenvalue
ap <-
  parallel(
    subject = nrow(Factoranalysis),
    var = ncol(Factoranalysis),
    rep = 100,
    quantile = .05
  ) #number of subjects, number of variables, number of replications of the correlation matrix,quantile of the distributio non which the decision is made
nS <- nScree(x = Eigenvalue$values, aparallel = ap$eigen$qevpea)

plotnScree(nS)                                                # Scree plot

# maximumlikelihood analysis
#(best if you want to follow up with a confirmatory factor analyses)

four_factor <-
  factanal(Factoranalysis, 4, rotation = "oblimin")        # 4 factors to be fitted, rotation method "oblimin"

print(four_factor, digits = 2, cutoff = .4)

five_factor <-
  factanal(Factoranalysis, 5, rotation = "oblimin")        # 4 factors to be fitted, rotation method "oblimin"

print(five_factor, digits = 2, cutoff = .4)

six_factor <-
  factanal(Factoranalysis, 6, rotation = "oblimin")        # 4 factors to be fitted, rotation method "oblimin"

print(six_factor, digits = 2, cutoff = .35)

seven_factor <-
  factanal(Factoranalysis, 7, rotation = "oblimin")        # 4 factors to be fitted, rotation method "oblimin"

print(seven_factor, digits = 2, cutoff = .4)

eight_factor <-
  factanal(Factoranalysis, 8, rotation = "oblimin")        # 4 factors to be fitted, rotation method "oblimin"

print(eight_factor, digits = 2, cutoff = .4)


# print results of ML analysis, 2 digits after the comma, only display factor loadings higher than 0,3

#Communality
1 - fit1$uniquenesses

1 - fit2$uniquenesses


