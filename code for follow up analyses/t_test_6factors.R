library(tidyverse)
library(naniar)



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

#Factor1 [Lack of training](DS13_05, DS13_06, DS13_07, DS13_08, !PR07_02!, PR07_04, !PR07_05!, PR07_06, PR07_07, PR07_10)

Training_prereg<-as.data.frame(cbind(OSQ_daten$PR07_02,
                              OSQ_daten$PR07_04, 
                              OSQ_daten$PR07_05,
                              OSQ_daten$PR07_06,
                              OSQ_daten$PR07_07))

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

#Factor4 [Control over Sharing] (DS02_01, DS02_03, DS02_04, DS02_05)

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

#Factor 5 Boss
Boss<-as.data.frame(cbind(OSQ_daten$DS13_08,
                          OSQ_daten$PR07_10))
                                    
Boss$BossTotal <- rowSums(subset(Boss, select = c(1:2)))              # summing variables to get Total Score for this factor                       
Boss$BossTotal <- (Boss$BossTotal/2)           

#Faktor 6 training data sharing

Training_ds<-as.data.frame(cbind(OSQ_daten$DS13_05,
                                     OSQ_daten$DS13_06,
                                     OSQ_daten$DS13_07))

Training_ds$Training_dsTotal <- rowSums(subset(Training_ds, select = c(1:3)))              # summing variables to get Total Score for this factor                       
Training_ds$Training_dsTotal <- (Training_ds$Training_dsTotal/3)

#Faktor 7

Faktor_7<- as.data.frame(cbind(OSQ_daten$DS13_01,
                            OSQ_daten$DS13_02))

Faktor_7$Faktor7_Total <- rowSums((subset(Faktor_7, select= c(1:2))))
Faktor_7$Faktor7_Total <- (Faktor_7$Faktor7_Total/2)

#Create dataframe for analysis
Follow_up<- as.data.frame(cbind(researchexp,                         
                                University,
                                EU,
                                Professor)) 

Follow_up$researchexp_scale<-OSQ_daten$PD05_01
Follow_up$DS10 <-OSQ_daten$DS10 
Follow_up$DS09 <-OSQ_daten$DS09
Follow_up$Training_preregTotal<- Training_prereg$Training_preregTotal                                  
Follow_up$Training_dsTotal<- Training_ds$Training_dsTotal 
Follow_up$FearTotal<- Fear$FearTotal
Follow_up$ComplexityTotal<-Complexity$ComplexityTotal
Follow_up$ControlTotal<-Control_recoded$ControlTotal
Follow_up$BossTotal<-Boss$BossTotal
Follow_up$Faktor_7<-Faktor_7$Faktor7_Total


# Comparing the effect of research experience (dichotomous) on each factor
t.test(Follow_up$Training_preregTotal ~ researchexp)
t.test(Follow_up$FearTotal ~ researchexp)
t.test(Follow_up$ComplexityTotal ~ researchexp)
t.test(Follow_up$ControlTotal ~ researchexp)
t.test(Follow_up$Training_dsTotal ~ researchexp)
t.test(Follow_up$BossTotal ~ researchexp)

# Comparing the effect of research experience (continuous) on each factor
fit <- lm(TrainingTotal ~ researchexp_scale, data=Follow_up)              # regression model with Factor Total as predicted variable and research experience in years as predictor
summary(fit)
fit <- lm(FearTotal ~ researchexp_scale, data=Follow_up)
summary(fit)
fit <- lm(ComplexityTotal ~ researchexp_scale, data=Follow_up)
summary(fit)
fit <- lm(ControlTotal ~ researchexp_scale, data=Follow_up)
summary(fit)

# Comparing the effect of EU residency on each factor
t.test(Follow_up$Training_preregTotal ~ EU)
t.test(Follow_up$FearTotal ~ EU)
t.test(Follow_up$ComplexityTotal ~ EU)
t.test(Follow_up$ControlTotal ~ EU)
t.test(Follow_up$Training_dsTotal ~ EU)
t.test(Follow_up$BossTotal ~ EU)

# Comparing the effect of affiliation with medical faculty on each factor
t.test(Follow_up$Training_preregTotal ~ University)
t.test(Follow_up$FearTotal ~ University)
t.test(Follow_up$ComplexityTotal ~ University)
t.test(Follow_up$ControlTotal ~ University)
t.test(Follow_up$Training_dsTotal ~ University)
t.test(Follow_up$BossTotal ~ University)

# Comparing the effect of career level on each factor
t.test(Follow_up$Training_preregTotal ~ Professor)
t.test(Follow_up$FearTotal ~ Professor)
t.test(Follow_up$ComplexityTotal ~ Professor)
t.test(Follow_up$ControlTotal ~ Professor)
t.test(Follow_up$Training_dsTotal ~ Professor)
t.test(Follow_up$BossTotal ~ Professor)

# Differences in datasharing for people living inside or outside of the EU 
#(1 = in the EU. 2 = outside of the EU.)
t.test(Follow_up$DS09 ~ EU)
t.test(Follow_up$DS10 ~ EU)




