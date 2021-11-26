#################################################################
# Loading packages (installs if necessary)
#################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(psych,
               nFactors,
               ltm,
               GPArotation,
               foreign,
               corrplot,
               lavaan)
               


#################################################################
# Create dataframe "Factoranalysis"
#################################################################

# Create subset for questions with multiple answer options
DS13 <-
  subset(OSQ_daten, select = c(108:119))                       # from OSQ_daten select rows 108:119

PR07 <- subset(OSQ_daten, select = c(87:96))

DS02 <- subset(OSQ_daten, select = c(120:125))

# Combines questions into one dataframe
Factoranalysis <- cbind(DS02, DS13, PR07)

#################################################################
# Test suitability of data for factoranalysis
#################################################################

# Bartletts test
cortest.bartlett(Factoranalysis)                              # tests whether items correlate with each other (should be significant)
# ??²(378) = 3135.445, p= 0

#Kaiser Meyer Olkin (MSA= Measure of sample adequacy)
KMO(Factoranalysis)                                           # should be bigger than 0,5
#--> Overall MSA=0.81

PR07 <-
  subset(OSQ_daten, select = c(87:95))                        # exclude item PR07_01 because of poor fit
#-> MSA for item PR07=0.46

#################################################################
# Exploratory structural equation modelling 
#################################################################

#Parallel ananylsis
fa.parallel(Factoranalysis, fa = "fa")                        # Determining number of suggested factors 
#->parallel analysis suggests 8 factors

#While parallel analysis recommended the eight-factor solution, 
#we decided to choose a seven-factor model, for reasons of parsimony,
#as it already provided good model fit according to CFI and RMSEA 

##########################
#Chosen Model: 7 factors #
##########################
esem_efa_7 <- fa(Factoranalysis, nfactors =7,rotate = "oblimin",
                 fm = 'ml')

fa.diagram(esem_efa_7, digits=2) # note: factor correlations not shown


# transform EFA results to lavaan code
esem_efa_7.loadmat <- zapsmall(matrix(round(esem_efa_7$loadings, 2), nrow = 28, ncol = 7))
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



#####################################################
#Non-chosen solutions including 4, 5, 6 and 8 factors#
#####################################################

############
#4 factors #
############
# 
# esem_efa_4 <- fa(Factoranalysis, nfactors =4,rotate = "oblimin",
#                fm = 'ml')
# 
# fa.diagram(esem_efa_4, digits=2) # note: factor correlations not shown
# 
# 
# # transform EFA results to lavaan code
# esem_efa_4.loadmat <- zapsmall(matrix(round(esem_efa_4$loadings, 2), nrow = 28, ncol = 4))
# rownames(esem_efa_4.loadmat) <- colnames(Factoranalysis)
# terms <- vector()
# for (i in 1:4) {
#   terms[i] <-
#     paste0("F",i,"=~ ", paste0(c(esem_efa_4.loadmat[,i]), "*", names(esem_efa_4.loadmat[,1]), collapse = "+"))
# }
# osq.esem4 <- paste(terms, collapse = "\n")
# 
# osq.cfa4 <- lavaan::cfa(osq.esem4, data = Factoranalysis, verbose = F, estimator = "MLR")
# fitmeasures(osq.cfa4, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# summary(osq.cfa4)
# 
# 
# 
# 
# 
############
#5 factors #
############
# 
# esem_efa_5 <- fa(Factoranalysis, nfactors =5,rotate = "oblimin",
#                  fm = 'ml')
# 
# fa.diagram(esem_efa_5, digits=2) # note: factor correlations not shown
# 
# 
# # transform EFA results to lavaan code
# esem_efa_5.loadmat <- zapsmall(matrix(round(esem_efa_5$loadings, 2), nrow = 28, ncol = 5))
# rownames(esem_efa_5.loadmat) <- colnames(Factoranalysis)
# terms <- vector()
# for (i in 1:5) {
#   terms[i] <-
#     paste0("F",i,"=~ ", paste0(c(esem_efa_5.loadmat[,i]), "*", names(esem_efa_5.loadmat[,1]), collapse = "+"))
# }
# osq.esem5 <- paste(terms, collapse = "\n")
# 
# osq.cfa5 <- lavaan::cfa(osq.esem5, data = Factoranalysis, verbose = F, estimator = "MLR")
# fitmeasures(osq.cfa5, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# summary(osq.cfa5)
# 
############
#6 factors #
############
# 
# esem_efa_6 <- fa(Factoranalysis, nfactors =6,rotate = "oblimin",
#                  fm = 'ml')
# fa.diagram(esem_efa_6, digits=2) # note: factor correlations not shown
# 
# 
# # transform EFA results to lavaan code
# esem_efa_6.loadmat <- zapsmall(matrix(round(esem_efa_6$loadings, 2), nrow = 28, ncol = 6))
# rownames(esem_efa_6.loadmat) <- colnames(Factoranalysis)
# terms <- vector()
# for (i in 1:6) {
#   terms[i] <-
#     paste0("F",i,"=~ ", paste0(c(esem_efa_6.loadmat[,i]), "*", names(esem_efa_6.loadmat[,1]), collapse = "+"))
# }
# osq.esem6 <- paste(terms, collapse = "\n")
# 
# osq.cfa6 <- lavaan::cfa(osq.esem6, data = Factoranalysis, verbose = F, estimator = "MLR")
# fitmeasures(osq.cfa6, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
#summary(osq.cfa6)



# 
############
#8 factors #
############
# 
# esem_efa_8 <- fa(Factoranalysis, nfactors =8,rotate = "oblimin",
#                  fm = 'ml')
# fa.diagram(esem_efa_8) # note: factor correlations not shown
# 
# # transform EFA results to lavaan code
# esem_efa_8.loadmat <- zapsmall(matrix(round(esem_efa_8$loadings, 2), nrow = 28, ncol = 8))
# rownames(esem_efa_8.loadmat) <- colnames(Factoranalysis)
# terms <- vector()
# for (i in 1:8) {
#   terms[i] <-
#     paste0("F",i,"=~ ", paste0(c(esem_efa_8.loadmat[,i]), "*", names(esem_efa_8.loadmat[,1]), collapse = "+"))
# }
# osq.esem8 <- paste(terms, collapse = "\n")
# 
# osq.cfa8 <- lavaan::cfa(osq.esem8, data = Factoranalysis, verbose = F, estimator = "MLR")
# fitmeasures(osq.cfa8, c("cfi.robust","tli.robust","rmsea.robust","srmr"))
# summary(osq.cfa8)

#################################################################
# sessionInfo()
#################################################################

# R version 4.0.5 (2021-03-31)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] lavaan_0.6-8          corrplot_0.87         foreign_0.8-81        GPArotation_2014.11-1 ltm_1.1-1             polycor_0.7-10       
# [7] msm_1.6.8             MASS_7.3-53.1         nFactors_2.4.1        lattice_0.20-41       psych_2.1.3           pacman_0.5.1         
# 
# loaded via a namespace (and not attached):
#   [1] mvtnorm_1.1-1   grid_4.0.5      nlme_3.1-152    stats4_4.0.5    Matrix_1.3-2    pbivnorm_0.6.0  splines_4.0.5   tools_4.0.5     survival_3.2-10
# [10] parallel_4.0.5  compiler_4.0.5  mnormt_2.0.2    tmvnsim_1.0-2   expm_0.999-6   
# 
