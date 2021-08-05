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


# prepare data and packages

# load packages (installs if not available)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, foreign, psych, reshape, ggplot2, corrplot, lavaan)

library(psych)
library(data.table)

#4 factors
esem_efa_4 <- fa(Factoranalysis, nfactors =4,rotate = "oblimin",
               fm = 'ml')

fa.diagram(esem_efa_4, digits=2) # note: factor correlations not shown
x<-fa.parallel(Factoranalysis, fa = "fa") # parallel analysis suggests 8 factors!

# transform EFA results to lavaan code
esem_efa_4.loadmat <- zapsmall(matrix(round(esem_efa_4$loadings, 2), nrow = 27, ncol = 4))
rownames(esem_efa_4.loadmat) <- colnames(Factoranalysis)
terms <- vector()
for (i in 1:4) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(esem_efa_4.loadmat[,i]), "*", names(esem_efa_4.loadmat[,1]), collapse = "+"))
}
osq.esem4 <- paste(terms, collapse = "\n")

osq.cfa4 <- lavaan::cfa(osq.esem4, data = Factoranalysis, verbose = F, estimator = "MLR")
fitmeasures(osq.cfa4, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(osq.cfa4)




#5 factors 

esem_efa_5 <- fa(Factoranalysis, nfactors =5,rotate = "oblimin",
                 fm = 'ml')

fa.diagram(esem_efa_5, digits=2) # note: factor correlations not shown


# transform EFA results to lavaan code
esem_efa_5.loadmat <- zapsmall(matrix(round(esem_efa_5$loadings, 2), nrow = 27, ncol = 5))
rownames(esem_efa_5.loadmat) <- colnames(Factoranalysis)
terms <- vector()
for (i in 1:5) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(esem_efa_5.loadmat[,i]), "*", names(esem_efa_5.loadmat[,1]), collapse = "+"))
}
osq.esem5 <- paste(terms, collapse = "\n")

osq.cfa5 <- lavaan::cfa(osq.esem5, data = Factoranalysis, verbose = F, estimator = "MLR")
fitmeasures(osq.cfa5, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(osq.cfa5)

# 6 factors

esem_efa_6 <- fa(Factoranalysis, nfactors =6,rotate = "oblimin",
                 fm = 'ml')
fa.diagram(esem_efa_6, digits=2) # note: factor correlations not shown


# transform EFA results to lavaan code
esem_efa_6.loadmat <- zapsmall(matrix(round(esem_efa_6$loadings, 2), nrow = 27, ncol = 6))
rownames(esem_efa_6.loadmat) <- colnames(Factoranalysis)
terms <- vector()
for (i in 1:6) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(esem_efa_6.loadmat[,i]), "*", names(esem_efa_6.loadmat[,1]), collapse = "+"))
}
osq.esem6 <- paste(terms, collapse = "\n")

osq.cfa6 <- lavaan::cfa(osq.esem6, data = Factoranalysis, verbose = F, estimator = "MLR")
fitmeasures(osq.cfa6, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(osq.cfa6)



#7 factors
esem_efa_7 <- fa(Factoranalysis, nfactors =7,rotate = "oblimin",
                 fm = 'ml')

fa.diagram(esem_efa_7, digits=2) # note: factor correlations not shown


# transform EFA results to lavaan code
esem_efa_7.loadmat <- zapsmall(matrix(round(esem_efa_7$loadings, 2), nrow = 27, ncol = 7))
rownames(esem_efa_7.loadmat) <- colnames(Factoranalysis)
terms <- vector()
for (i in 1:7) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(esem_efa_7.loadmat[,i]), "*", names(esem_efa_7.loadmat[,1]), collapse = "+"))
}
osq.esem7 <- paste(terms, collapse = "\n")

osq.cfa7 <- lavaan::cfa(osq.esem7, data = Factoranalysis, verbose = F, estimator = "MLR")
fitmeasures(osq.cfa7, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(osq.cfa7)


#8 factors

esem_efa_8 <- fa(Factoranalysis, nfactors =8,rotate = "oblimin",
                 fm = 'ml')
fa.diagram(esem_efa_8) # note: factor correlations not shown

# transform EFA results to lavaan code
esem_efa_8.loadmat <- zapsmall(matrix(round(esem_efa_8$loadings, 2), nrow = 27, ncol = 8))
rownames(esem_efa_8.loadmat) <- colnames(Factoranalysis)
terms <- vector()
for (i in 1:8) {
  terms[i] <-
    paste0("F",i,"=~ ", paste0(c(esem_efa_8.loadmat[,i]), "*", names(esem_efa_8.loadmat[,1]), collapse = "+"))
}
osq.esem8 <- paste(terms, collapse = "\n")

osq.cfa8 <- lavaan::cfa(osq.esem8, data = Factoranalysis, verbose = F, estimator = "MLR")
fitmeasures(osq.cfa8, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
summary(osq.cfa8)

osq.cfa8$latentvariables



