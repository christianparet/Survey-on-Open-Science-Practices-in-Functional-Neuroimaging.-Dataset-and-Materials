

#################################################################
# Loading packages (installs if necessary)
#################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               naniar,
               ggplot2,
               psych,
               factoextra,
               clValid,
               profileR,
               effects,
               caret,
               svglite,
               dendextend)


#################################################################
#Specifying demographic variables for comparison
#################################################################


#Research experience
researchexp<- cut(OSQ_daten$PD05_01, c(0,16, 55), labels=c(1:2))                #-> (cut off median= 16 years)
#researchexp<-as.data.frame(researchexp)                                        #-> zum kontrollieren
#researchexp<- cbind(OSQ_daten$PD05_01, researchexp)
#table(researchexp$researchexp)

#Primary affiliation with medical faculty
PD08<-OSQ_daten$PD08
PD08<-as.data.frame(PD08)

University <- PD08 %>%                                                         #-> collapse 6 factors in to 2 (University hospital/medical faculty vs University psychology/other faculty)
  mutate(type = as.factor(case_when(
    PD08 %in% c(1) ~ 1,                                                        # 1 = With university hospital/medical faculty
    PD08 %in% c(2, 3) ~ 2,                                                     # 2 = With university/psychology faculty or similar 3 = With university/other faculty
    PD08 %in% c(4, 5, 6) ~ 3                                                   # 4 = With industry 5 = Other 6 = With governmental institution
  )))

University<-University%>%                                                      # replace level 3 with missing values
  replace_with_na(replace = list(type = c(3)))
University<-droplevels(University$type)                                        # drop unused level
#table(University)                                                             # zum kontrollieren

#EU residency
EU<-OSQ_daten$PD10                                                             # 1 = in the EU. 2 = outside of the EU.
EU<-as.factor(EU)                                                              # create factor of question regarding country of origin
#table(EU)                                                                     # zum kontrollieren

#Current position professor/associate professor or not
PD07<-OSQ_daten$PD07
PD07<-as.data.frame(PD07)

Professor <- PD07 %>%                                                          # collapse 10 levels into 2
  mutate(type = as.factor(case_when(
    PD07 %in% c(5, 10) ~ 1,                                                    # 5 = Full Professor 10 = Associate Professor/Reader/Lecturer
    PD07 %in% c(1, 2, 7, 8, 3, 6, 9) ~ 2,                                      # 1 = Research Assistant 2 = PhD Student 7 = Technical Assistant 8 = Lab Manager 3 = Post-Doc (1-3 years) 9 = Asisstant Professor/Post-Doc (4 years or longer) PD07_06 Other
    PD07 %in% c(-9) ~ 3
  )))

Professor<- as.factor(Professor$type)                                          # create factor
#table(Professor)                                                              # zum kontrollieren


prev_prereg<- subset(OSQ_daten, select = c(79:83, 85))
prev_prereg<-mutate(prev_prereg, prev_prereg = ifelse(PR01_01 | PR01_02 | PR01_03 | PR01_04 | PR01_05 == 'TRUE', "1", "0"))
prev_prereg$prev_prereg<- as.factor(prev_prereg$prev_prereg)

#################################################################
#Construction of factors
#################################################################

#Factor1 Lack of experience preregistration (DS13_05, DS13_06, DS13_07, DS13_08, !PR07_02!, PR07_04, !PR07_05!, PR07_06, PR07_07, PR07_10)

Training_prereg<-as.data.frame(cbind(OSQ_daten$PR07_02,
                                     OSQ_daten$PR07_04,
                                     OSQ_daten$PR07_05,
                                     OSQ_daten$PR07_06,
                                     OSQ_daten$PR07_07))

#calculate factor total score
Training_prereg$Training_preregTotal <- rowSums(subset(Training_prereg, select = c(1:5)))              # summing variables to get Total Score for this factor
Training_prereg$Training_preregTotal <- (Training_prereg$Training_preregTotal/5)



#Factor2 [fear of being transparent](DS13_09, DS13_10, DS13_11, DS13_12, PR07_08, PR07_09)

Fear<- as.data.frame(cbind(OSQ_daten$DS13_10,
                           OSQ_daten$DS13_11,
                           OSQ_daten$PR07_08,
                           OSQ_daten$PR07_09))

Fear$FearTotal <- rowSums(subset(Fear, select = c(1:4)))                         # summing variables to get Total Score for this factor
Fear$FearTotal <- (Fear$FearTotal/4)
#Factor3 [complexity/burden of making data accessible] (DS13_01, DS13_02, DS13_03, DS13_04, PR07_03)

Complexity<- as.data.frame(cbind(OSQ_daten$DS13_03,
                                 OSQ_daten$DS13_04,
                                 OSQ_daten$PR07_03))
#OSQ_daten$PR07_02,            #suits theoretically, doesn't load high on factor
#OSQ_daten$PR07_05))           #suits theoretically, doesn't load high on factor

Complexity$ComplexityTotal <- rowSums(subset(Complexity, select = c(1:3)))
Complexity$ComplexityTotal <- (Complexity$ComplexityTotal/3)

#Factor4 Need for datagovernance (DS02_01, DS02_03, DS02_04, DS02_05)

Control<- as.data.frame(cbind(OSQ_daten$DS02_01,
                              OSQ_daten$DS02_03,
                              OSQ_daten$DS02_04,
                              OSQ_daten$DS02_05))

Control_recoded <- Control %>%
  mutate_at(vars(1),
            ~ifelse(. == 1, 7,
             ifelse(. == 2, 6,
             ifelse(. == 3, 5,
             ifelse(. == 4, 4,
             ifelse(. == 5, 3,
             ifelse(. == 6, 2,
             ifelse(. == 7, 1, .))))))))                                             # recode reversely coded item


Control_recoded$ControlTotal <- rowSums(subset(Control_recoded, select = c(1:4)))    # summing variables to get Total Score for this factor
Control_recoded$ControlTotal <- (Control_recoded$ControlTotal/4)

#Factor 5 Unsupportive supervisor

Boss<-as.data.frame(cbind(OSQ_daten$DS13_08,
                          OSQ_daten$PR07_10))

Boss$BossTotal <- rowSums(subset(Boss, select = c(1:2)))                             # summing variables to get Total Score for this factor
Boss$BossTotal <- (Boss$BossTotal/2)

#Faktor 6 Lack of training Datasharing

Training_ds<-as.data.frame(cbind(OSQ_daten$DS13_05,
                                 OSQ_daten$DS13_06,
                                 OSQ_daten$DS13_07))

Training_ds$Training_dsTotal <- rowSums(subset(Training_ds, select = c(1:3)))        # summing variables to get Total Score for this factor
Training_ds$Training_dsTotal <- (Training_ds$Training_dsTotal/3)

#Lack of resources for Datasharing

NoResources_DS<- as.data.frame(cbind(OSQ_daten$DS13_01,
                               OSQ_daten$DS13_02))

NoResources_DS$NoResources_DS_Total <- rowSums((subset(NoResources_DS, select= c(1:2))))
NoResources_DS$NoResources_DS_Total <- (NoResources_DS$NoResources_DS_Total/2)


#################################################################
#Create dataframe for analysis
#################################################################

Follow_up<- as.data.frame(cbind(researchexp,
                                University,
                                EU,
                                Professor))


Follow_up$DS10 <-OSQ_daten$DS10
Follow_up$DS09 <-OSQ_daten$DS09
Follow_up$Training_preregTotal<- Training_prereg$Training_preregTotal
Follow_up$Training_dsTotal<- Training_ds$Training_dsTotal
Follow_up$FearTotal<- Fear$FearTotal
Follow_up$ComplexityTotal<-Complexity$ComplexityTotal
Follow_up$ControlTotal<-Control_recoded$ControlTotal
Follow_up$BossTotal<-Boss$BossTotal
Follow_up$NoResources_DS<-NoResources_DS$NoResources_DS_Total
Follow_up$researchexp<-as.factor(Follow_up$researchexp)
Follow_up$University<- as.factor(Follow_up$University)
Follow_up$Professor<- as.factor(Follow_up$Professor)
Follow_up$EU<- as.factor(Follow_up$EU)
Follow_up$DS09<- OSQ_daten$DS09
Follow_up$prev_prereg<- prev_prereg$prev_prereg
Follow_up$PD05_01<- OSQ_daten$PD05_01
Follow_up$BI02<- OSQ_daten$BI02
Follow_up$BI07<- OSQ_daten$BI07


#################################################################
#Clusteranalysis
#################################################################



clusteranalysis<- as.data.frame(cbind(Training_preregTotal=Follow_up$Training_preregTotal,
                                      Training_dsTotal=Follow_up$Training_dsTotal,
                                      FearTotal=Follow_up$FearTotal,
                                      ComplexityTotal=Follow_up$ComplexityTotal,
                                      ControlTotal=Follow_up$ControlTotal,
                                      BossTotal=Follow_up$BossTotal,
                                      NoResources_DS=Follow_up$NoResources_DS))


#determine number of cluster
fviz_nbclust(clusteranalysis, FUN = hcut, method = "wss")          #elbow method
fviz_nbclust(clusteranalysis, FUN = hcut, method = "silhouette")   #silhouette method
#->suggests best number of clusters = 2

#
seeds_df_sc <- as.data.frame(scale(clusteranalysis))
summary(seeds_df_sc)

dist_mat <- dist(seeds_df_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D2')

#create dendogram that shows the two clusters in different colours
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 2 , border = 2:6)
abline(h = 20 , col = 'red')
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 20)

#save dendrogramm
svg("dendrogram_final.svg")
plot(avg_col_dend,  xlab= "Respondents", ylab= "Euclidean Distance", leaflab="none")
dev.off()


#shows how many people are in each cluster
seeds_df_cl <- mutate(clusteranalysis, cluster = cut_avg)
count(seeds_df_cl,cluster)


#Calculate dunn index
dunn(dist_mat, cut_avg)
#->dunn index= 0,155



#################################################################
#Follow-up analyses: Cluster charactersitics
#################################################################

clusteranalysis$cluster<- as.factor(cut_avg)                               # add cluster variable to dataframe "clusteranalysis"
levels(clusteranalysis$cluster)

Follow_up$cluster<- as.factor(cut_avg)                                     # add cluster variable to dataframe "Follow_up"
levels(Follow_up$cluster)
#descriptive statistics of factors per cluster
desc.stats<-describeBy(clusteranalysis, group = Follow_up$cluster)

#descriptive statistics of BI07, DS09, BI02 per cluster
describeBy(Follow_up$BI07, group = Follow_up$cluster)
describeBy(Follow_up$DS09, group = Follow_up$cluster)
describeBy(Follow_up$BI02, group = Follow_up$cluster)

################################################################
#Profile plots of clusters based on factors from factoranalysis
################################################################


profileplot<-data.frame(TR_reg = c(4.30, 2.97),                                 # create dataframe with scores of the clusters on each factor (taken from descriptive statistics)
                        TR_ds = c(4.03, 2.07),
                        Fear = c(3.95, 2.55),
                        Compl= c( 4.04, 3.00),
                        Contr= c( 4.41, 3.83),
                        Boss = c( 3.20, 1.64),
                        NoResources_DS = c( 5.34, 3.67),
                        cluster= as.factor(c(1, 2)))
profileplot <- melt(profileplot, id.vars = "cluster")



profileplot$SD<-c(1.09, 1.20, 1.14, 1.06, 1.13, 1.13, 1.26, 1.30, 1.03, 1.48, 1.45, 0.91, 1.05, 1.53)  #add variable SD to profileplot dataframe
profileplot$min<- profileplot$value-profileplot$SD                                                     #calculate mininmum for profileplot by subtracting SD from mean
profileplot$max<- profileplot$value+profileplot$SD                                                     #calculate maximum for profileplot by sadding SD to mean


profileplot$SE<-c(0.09,0.11,0.09,0.10, 0.09,0.10,0.10,0.12,0.08,0.13,0.11,0.08, 0.08, 0.14)            #add SE to profileplotdataframe
profileplot$ci_max<- profileplot$value+(1.96*profileplot$SE)                                           #calculate 95 % confidence interval by adding SE to mean
profileplot$ci_min<- profileplot$value- (1.96*profileplot$SE)                                          #calculate 95 % confidence interval by subtracting SE from mean

#profile plot with SD
ggplot(profileplot, aes(x = variable, y = value, group = cluster)) +
  geom_line() +
  geom_ribbon(aes(ymin = min, ymax = max, fill = cluster), alpha = 0.3, color = NA) +
  scale_fill_manual(values = c("skyblue", "coral"), aesthetics = c("color", "fill"))

#profile plot with confidence interval
ggplot(profileplot, aes(x = variable, y = value, group = cluster)) +
  scale_y_continuous(limits=c(1,7), breaks=1:7)+
  geom_line() +
  geom_ribbon(aes(ymin = ci_min, ymax = ci_max, fill = cluster), alpha = 0.3, color = NA) +
  scale_fill_manual(values = c("skyblue", "coral"), aesthetics = c("color", "fill"))
ggsave("profileplot_ci.svg")

################################################################
#Logistic regression
#Trying to predict cluster belongingness by demographic variables
################################################################

##logistic regression

Follow_up<- Follow_up%>% drop_na(researchexp, Professor, EU, University, PD05_01)

model0<- glm(cluster~1, data=Follow_up, family=binomial())

#Model with research exp as dichotomous variable
model1 <- glm(cluster ~ researchexp+ Professor+ EU+ University,family=binomial(),data=Follow_up)
summary(model1)
#-> no significance, Trend for Professor and University

#95% Confidence intervals for betas
confint(model1)

##Additionally to the dichotomous predictor we explored "research experience" as a continuously scaled predictor variable. 
##For reasons of conciseness, this analysis is not included in the publication.All analyses with Model 2 are therefore commented out in the following.
# Model with research exp as continuous variable
# model2 <- glm(cluster ~ PD05_01+ Professor+ EU+ University,family=binomial(),data=Follow_up)       #adding research experience as scale
# summary(model2)
# -->University is significant

#plot of effects
svg("log_reg.svg")
plot(allEffects(model1))
dev.off()

#Omnibus test
modelchi<-model1$null.deviance - model1$deviance
chidf<- model1$df.null-model1$df.residual
chisqp <- 1-pchisq(modelchi, chidf)
#->p=0.003

# modelchi<-model2$null.deviance - model2$deviance
# chidf<- model2$df.null-model2$df.residual
# chisqp <- 1-pchisq(modelchi, chidf)
# #-> p=0.013

#Odds Ratio
exp(cbind(OR= coef(model1),confint(model1)))
# exp(cbind(OR= coef(model2),confint(model2)))

#coefficient of determination
n<-length(model1$residuals)
R2cs<-1-exp((model1$deviance-model1$null.deviance)/n)
R2n<- R2cs/(1-exp(-(model1$null.deviance/n)))
#->0.0536

# n<-length(model2$residuals)
# R2cs<-1-exp((model2$deviance-model2$null.deviance)/n)
# R2n<- R2cs/(1-exp(-(model2$null.deviance/n)))
# ->0.0667

#accuracy
logmodel1 <- train(cluster ~ researchexp+ Professor+EU+ University,
                   data = Follow_up,
                   trControl = trainControl(method = "cv", number = 10),
                   method = "glm",
                   family=binomial())
confusionMatrix(logmodel1)
#->0.5992

# logmodel2 <- train(cluster ~ PD05_01+ Professor+EU+ University,
#                    data = Follow_up,
#                    trControl = trainControl(method = "cv", number = 10),
#                    method = "glm",
#                    family=binomial())
# confusionMatrix(logmodel2)
# ->0.587


##################################################################
#sessionInfo()
##################################################################

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
#   [1] dendextend_1.15.1  svglite_2.0.0      caret_6.0-90       lattice_0.20-41    effects_4.2-0      carData_3.0-4      profileR_0.3-5     lavaan_0.6-8      
# [9] reshape_0.8.8      RColorBrewer_1.1-2 clValid_0.7        cluster_2.1.2      factoextra_1.0.7   psych_2.1.3        naniar_0.6.0       forcats_0.5.1     
# [17] stringr_1.4.0      dplyr_1.0.5        purrr_0.3.4        readr_1.4.0        tidyr_1.1.3        tibble_3.1.1       ggplot2_3.3.3      tidyverse_1.3.1   
# [25] pacman_0.5.1      
# 
# loaded via a namespace (and not attached):
#   [1] minqa_1.2.4          colorspace_2.0-1     ggsignif_0.6.1       rio_0.5.26           ellipsis_0.3.2       class_7.3-18         visdat_0.5.3        
# [8] estimability_1.3     fs_1.5.0             proxy_0.4-26         rstudioapi_0.13      farver_2.1.0         ggpubr_0.4.0         listenv_0.8.0       
# [15] ggrepel_0.9.1        prodlim_2019.11.13   fansi_0.4.2          lubridate_1.7.10     xml2_1.3.2           codetools_0.2-18     splines_4.0.5       
# [22] mnormt_2.0.2         jsonlite_1.7.2       nloptr_1.2.2.2       pROC_1.18.0          broom_0.7.6          dbplyr_2.1.1         compiler_4.0.5      
# [29] httr_1.4.2           backports_1.2.1      assertthat_0.2.1     Matrix_1.3-2         survey_4.1-1         cli_2.5.0            tools_4.0.5         
# [36] gtable_0.3.0         glue_1.4.2           reshape2_1.4.4       Rcpp_1.0.6           cellranger_1.1.0     vctrs_0.3.8          nlme_3.1-152        
# [43] iterators_1.0.13     insight_0.14.2       timeDate_3043.102    gower_0.2.2          globals_0.14.0       openxlsx_4.2.3       lme4_1.1-26         
# [50] rvest_1.0.0          lifecycle_1.0.0      rstatix_0.7.0        statmod_1.4.35       future_1.23.0        MASS_7.3-53.1        scales_1.1.1        
# [57] ipred_0.9-12         hms_1.0.0            parallel_4.0.5       curl_4.3.1           gridExtra_2.3        rpart_4.1-15         stringi_1.5.3       
# [64] foreach_1.5.1        e1071_1.7-9          zip_2.1.1            boot_1.3-27          lava_1.6.10          rlang_0.4.11         pkgconfig_2.0.3     
# [71] systemfonts_1.0.1    labeling_0.4.2       recipes_0.1.17       tidyselect_1.1.1     parallelly_1.28.1    plyr_1.8.6           magrittr_2.0.1      
# [78] R6_2.5.0             generics_0.1.0       DBI_1.1.1            foreign_0.8-81       pillar_1.6.0         haven_2.4.1          withr_2.4.2         
# [85] abind_1.4-5          survival_3.2-10      nnet_7.3-15          future.apply_1.8.1   car_3.0-10           modelr_0.1.8         crayon_1.4.1        
# [92] utf8_1.2.1           tmvnsim_1.0-2        viridis_0.6.1        grid_4.0.5           readxl_1.3.1         data.table_1.14.0    pbivnorm_0.6.0      
# [99] ModelMetrics_1.2.2.2 reprex_2.0.0         digest_0.6.27        stats4_4.0.5         munsell_0.5.0        viridisLite_0.4.0    mitools_2.4 
