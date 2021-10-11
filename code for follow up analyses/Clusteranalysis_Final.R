library(tidyverse)
library(naniar)
library(ggplot2)
library(ggmosaic)
#Constructing binary variables from Personal Data questions to compare items on 
BI07<- as.data.frame(OSQ_daten$BI07)
BI07<-replace_with_na(BI07, replace = list('OSQ_daten$BI07' = -9))


#Research experience 
researchexp<- cut(OSQ_daten$PD05_01, c(0,16, 55), labels=c(1:2)) #-> (cut off median= 16 years)
#researchexp<-as.data.frame(researchexp)                         #-> zum kontrollieren
#researchexp<- cbind(OSQ_daten$PD05_01, researchexp)
#table(researchexp$researchexp)

#Primary affiliation with medical faculty 
PD08<-OSQ_daten$PD08
PD08<-as.data.frame(PD08)

University <- PD08 %>%                                           #-> collapse 6 factors in to 2 (University hospital/medical faculty vs University psychology/other faculty)
  mutate(type = as.factor(case_when(
    PD08 %in% c(1) ~ 1,                                          # 1 = With university hospital/medical faculty
    PD08 %in% c(2, 3) ~ 2,                                       # 2 = With university/psychology faculty or similar 3 = With university/other faculty 
    PD08 %in% c(4, 5, 6) ~ 3                                     # 4 = With industry 5 = Other 6 = With governmental institution
  )))

University<-University%>%                                        # replace level 3 with missing values 
  replace_with_na(replace = list(type = c(3)))
University<-droplevels(University$type)                          # drop unused level 
#table(University)                                               # zum kontrollieren

#EU residency
EU<-OSQ_daten$PD10                                               # 1 = in the EU. 2 = outside of the EU.
EU<-as.factor(EU)                                                # create factor of question regarding country of origin
#table(EU)                                                       # zum kontrollieren

#Current position professor/associate professor or not 
PD07<-OSQ_daten$PD07
PD07<-as.data.frame(PD07)

Professor <- PD07 %>%                                            # collapse 10 levels into 2 
  mutate(type = as.factor(case_when(                           
    PD07 %in% c(5, 10) ~ 1,                                      # 5 = Full Professor 10 = Associate Professor/Reader/Lecturer 
    PD07 %in% c(1, 2, 7, 8, 3, 6, 9) ~ 2,                        # 1 = Research Assistant 2 = PhD Student 7 = Technical Assistant 8 = Lab Manager 3 = Post-Doc (1-3 years) 9 = Asisstant Professor/Post-Doc (4 years or longer) PD07_06 Other
    PD07 %in% c(-9) ~ 3
  )))


Professor<- as.factor(Professor$type)                            # create factor 
#table(Professor)                                                # zum kontrollieren


##Construction of Factors                                        

#Factor1 Training experience preregistration (DS13_05, DS13_06, DS13_07, DS13_08, !PR07_02!, PR07_04, !PR07_05!, PR07_06, PR07_07, PR07_10)

Training_prereg<-as.data.frame(cbind(OSQ_daten$PR07_02,
                                     OSQ_daten$PR07_04, 
                                     OSQ_daten$PR07_05,
                                     OSQ_daten$PR07_06,
                                     OSQ_daten$PR07_07))
#recode factor training 
cols = c("V1", "V2", "V3", "V4", "V5")
Training_prereg[ ,cols] = 8 - Training_prereg[ ,cols]
#calculate factor total score
Training_prereg$Training_preregTotal <- rowSums(subset(Training_prereg, select = c(1:5)))              # summing variables to get Total Score for this factor                       
Training_prereg$Training_preregTotal <- (Training_prereg$Training_preregTotal/5)  



#Factor2 [fear of being transparent](DS13_09, DS13_10, DS13_11, DS13_12, PR07_08, PR07_09)

Fear<- as.data.frame(cbind(OSQ_daten$DS13_10,
                           OSQ_daten$DS13_11,                   
                           OSQ_daten$PR07_08,
                           OSQ_daten$PR07_09))

Fear$FearTotal <- rowSums(subset(Fear, select = c(1:4)))        # summing variables to get Total Score for this factor
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
                                                       ifelse(. == 7, 1, .))))))))                             # recode reversely coded item 


Control_recoded$ControlTotal <- rowSums(subset(Control_recoded, select = c(1:4)))   # summing variables to get Total Score for this factor
Control_recoded$ControlTotal <- (Control_recoded$ControlTotal/4)

#Factor 5 Restrictive environment
Boss<-as.data.frame(cbind(OSQ_daten$DS13_08,
                          OSQ_daten$PR07_10))

Boss$BossTotal <- rowSums(subset(Boss, select = c(1:2)))              # summing variables to get Total Score for this factor                       
Boss$BossTotal <- (Boss$BossTotal/2)           

#Faktor 6 Training experience Datasharing

Training_ds<-as.data.frame(cbind(OSQ_daten$DS13_05,
                                 OSQ_daten$DS13_06,
                                 OSQ_daten$DS13_07))
#recode factor 
cols = c("V1", "V2", "V3")
Training_ds[ ,cols] = 8 - Training_ds[ ,cols]

Training_ds$Training_dsTotal <- rowSums(subset(Training_ds, select = c(1:3)))              # summing variables to get Total Score for this factor                       
Training_ds$Training_dsTotal <- (Training_ds$Training_dsTotal/3)

#Lack of resources for Datasharing

NoResources_DS<- as.data.frame(cbind(OSQ_daten$DS13_01,
                               OSQ_daten$DS13_02))

NoResources_DS$NoResources_DS_Total <- rowSums((subset(NoResources_DS, select= c(1:2))))
NoResources_DS$NoResources_DS_Total <- (NoResources_DS$NoResources_DS_Total/2)

#Create dataframe for analysis
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
Follow_up$BI07<- OSQ_daten$BI07
Follow_up$DS09<- OSQ_daten$DS09
Follow_up$BI02<- OSQ_daten$BI02

prev_prereg<- subset(OSQ_daten, select = c(79:83, 85))
prev_prereg<-mutate(prev_prereg, prev_prereg = ifelse(PR01_01 | PR01_02 | PR01_03 | PR01_04 | PR01_05 == 'TRUE', "1", "0"))
prev_prereg$prev_prereg<- as.factor(prev_prereg$prev_prereg)
Follow_up$prev_prereg<- prev_prereg$prev_prereg


###################
# CLUSTERANALYSIS #
###################
library(psych)
library(factoextra)
library(psych)
library(clValid)

clusteranalysis<- subset(Follow_up, select = c(8:14)) 

#determine number of cluster
fviz_nbclust(clusteranalysis, FUN = hcut, method = "wss")          #elbow method
fviz_nbclust(clusteranalysis, FUN = hcut, method = "silhouette")   #silhouette method
#->suggests best number of clusters = 2 

#
seeds_df_sc <- as.data.frame(scale(clusteranalysis))
summary(seeds_df_sc)

dist_mat <- dist(seeds_df_sc, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D2')

plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 2)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 2 , border = 2:6)
abline(h = 20 , col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 20)
plot(avg_col_dend)

#suppressPackageStartupMessages(library(dplyr)) [NEEDED?]
seeds_df_cl <- mutate(clusteranalysis, cluster = cut_avg)
count(seeds_df_cl,cluster)


library(clValid)
dunn(dist_mat, cut_avg)
#->dunn index= 0,155


###


#Follow up analyses: Cluster charactersitics 
Follow_up$cluster<- as.factor(cut_avg)


#descriptive statistics of factors per cluster 
describeBy(Follow_up[,8:14], group = Follow_up$cluster)

#descriptive statistics of BI07, DS09, BI02 per cluster 
describeBy(Follow_up$BI07, group = Follow_up$cluster)
describeBy(Follow_up$DS09, group = Follow_up$cluster)
describeBy(Follow_up$BI02, group = Follow_up$cluster)

########################
#Profile plots of clusters based on factors from factoranalysis
library(profileR)
profileplot<-data.frame(TR_reg = c(3.70, 5.03), 
                        TR_ds = c(3.97, 5.93),
                        Fear = c(3.95, 2.55),
                        Compl= c( 4.04, 3.00),
                        Contr= c( 4.41, 3.83),
                        Boss = c( 3.20, 1.64),
                        NoResources_DS = c( 5.34, 3.67),
                        cluster= as.factor(c(1, 2)))
profileplot <- melt(profileplot, id.vars = "cluster")


profileplot(profileplot, person.id= cluster, standardize = FALSE, interval = 7,
            by.pattern = TRUE, original.names = TRUE)


ggplot(profileplot) +
  scale_y_continuous(breaks = seq(0,7, by=1))+
  geom_line(aes(variable, value, group = cluster,
                color = cluster))


#radarchart of clusters based on factors from factoranalysis
library(fmsb)
radarchart<-data.frame(TR_reg = c(7, 0, 3.70, 5.03), 
                       TR_ds = c(7, 0, 3.97, 5.93),
                       Fear = c(7, 0, 3.95, 2.55),
                       Complexity= c(7, 0, 4.04, 3.00),
                       Control= c(7, 0, 4.41, 3.83),
                       Boss = c(7, 0, 3.20, 1.64),
                       NoRes_DS = c(7, 0, 5.34, 3.67),
                       row.names = c("max", "min", "Cluster1", "Cluster2"))
radarchart(radarchart, seg= 7)


##logistic regression

model0<- glm(cluster~1, data=Follow_up, family=binomial())

model1 <- glm(cluster ~ Follow_up$researchexp+ Follow_up$Professor+Follow_up$EU+ Follow_up$University,family=binomial(),data=Follow_up)
summary(model1)
#-> only Professor variable is a significant predictor 

model2 <- glm(cluster ~ Follow_up$researchexp+ Follow_up$Professor+Follow_up$EU+Follow_up$University+ Follow_up$DS10+ Follow_up$DS09+ Follow_up$prev_prereg,family=binomial(link='logit'),data=Follow_up)
summary(model2)

#Omnibus test
modelchi<-model1$null.deviance - model1$deviance
chidf<- model1$df.null-model1$df.residual
chisqp <- 1-pchisq(modelchi, chidf)
#->p=0,116 not significant 

modelchi<-model2$null.deviance - model2$deviance
chidf<- model2$df.null-model2$df.residual
chisqp <- 1-pchisq(modelchi, chidf)

#Odds Ratio
exp(cbind(OR= coef(model1),confint(model1)))

#Gütemaße
n<-length(model1$residuals)
R2cs<-1-exp((model1$deviance-model1$null.deviance)/n)
R2n<- R2cs/(1-exp(-(model1$null.deviance/n)))
