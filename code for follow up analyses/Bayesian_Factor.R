#################################################################
# Loading packages (installs if necessary)
#################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               naniar,
               BayesFactor) 

#################################################################
#Specifying demograhpic variables for comparison              
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

#Factor1 Lack of Training experience (PR07_02, PR07_04, PR07_05, PR07_06, PR07_07)

Training_prereg<-as.data.frame(cbind(OSQ_daten$PR07_02,
                                     OSQ_daten$PR07_04, 
                                     OSQ_daten$PR07_05,
                                     OSQ_daten$PR07_06,
                                     OSQ_daten$PR07_07))

#calculate factor total score
Training_prereg$Training_preregTotal <- rowSums(subset(Training_prereg, select = c(1:5)))              # summing variables to get Total Score for this factor                       
Training_prereg$Training_preregTotal <- (Training_prereg$Training_preregTotal/5)  



#Factor2 [fear of being transparent](DS13_10, DS13_11, PR07_08, PR07_09)

Fear<- as.data.frame(cbind(OSQ_daten$DS13_10,
                           OSQ_daten$DS13_11,                   
                           OSQ_daten$PR07_08,
                           OSQ_daten$PR07_09))

Fear$FearTotal <- rowSums(subset(Fear, select = c(1:4)))                         # summing variables to get Total Score for this factor
Fear$FearTotal <- (Fear$FearTotal/4)
#Factor3 [complexity/burden of making data accessible] (DS13_03, DS13_04, PR07_03)

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

#Factor 5 Restrictive environment
Boss<-as.data.frame(cbind(OSQ_daten$DS13_08,
                          OSQ_daten$PR07_10))

Boss$BossTotal <- rowSums(subset(Boss, select = c(1:2)))                             # summing variables to get Total Score for this factor                       
Boss$BossTotal <- (Boss$BossTotal/2)           

#Faktor 6 Lack of Training Datasharing (DS13_05, DS13_06, DS13_07)

Training_ds<-as.data.frame(cbind(OSQ_daten$DS13_05,
                                 OSQ_daten$DS13_06,
                                 OSQ_daten$DS13_07))

Training_ds$Training_dsTotal <- rowSums(subset(Training_ds, select = c(1:3)))       # summing variables to get Total Score for this factor                       
Training_ds$Training_dsTotal <- (Training_ds$Training_dsTotal/3)

#Lack of resources for Datasharing (DS13_01, DS13_02) 

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
Follow_up$NoResources_DS_Total<-NoResources_DS$NoResources_DS_Total
Follow_up$researchexp<-as.factor(Follow_up$researchexp)
Follow_up$University<- as.factor(Follow_up$University)
Follow_up$Professor<- as.factor(Follow_up$Professor)
Follow_up$EU<- as.factor(Follow_up$EU)
Follow_up$DS09<- OSQ_daten$DS09
Follow_up$prev_prereg<- prev_prereg$prev_prereg

#Create dataframe for University by excluding missing values
BF_university<- na.omit(Follow_up)                  # exlude missing observations 



#################################################################
#BF Factor training preregistration 
#################################################################
BF_Trainingprereg_researchexp<- ttestBF(x=Follow_up$Training_preregTotal[Follow_up$researchexp==1],
                                        y=Follow_up$Training_preregTotal[Follow_up$researchexp==2])
BF_Trainingprereg_researchexp

#
BF_Trainingprereg_professor<- ttestBF(x=Follow_up$Training_preregTotal[Follow_up$Professor==1],
                                      y=Follow_up$Training_preregTotal[Follow_up$Professor==2])
BF_Trainingprereg_professor
#
BF_Trainingprereg_university<- ttestBF(x=BF_university$Training_preregTotal[BF_university$University==1],
                                       y=BF_university$Training_preregTotal[BF_university$University==2])
BF_Trainingprereg_university
#EU
BF_Trainingprereg_EU<- ttestBF(x=Follow_up$Training_preregTotal[Follow_up$EU==1],
                               y=Follow_up$Training_preregTotal[Follow_up$EU==2])
BF_Trainingprereg_EU

#################################################################
#BF Factor training datasharing 
#################################################################

BF_Training_dsTotal_researchexp<- ttestBF(x=Follow_up$Training_dsTotal[Follow_up$researchexp==1],
                                          y=Follow_up$Training_dsTotal[Follow_up$researchexp==2])
BF_Training_dsTotal_researchexp

#
BF_Training_dsTotal_professor<- ttestBF(x=Follow_up$Training_dsTotal[Follow_up$Professor==1],
                                        y=Follow_up$Training_dsTotal[Follow_up$Professor==2])
BF_Training_dsTotal_professor
#
BF_Training_dsTotal_university<- ttestBF(x=BF_university$Training_dsTotal[BF_university$University==1],
                                         y=BF_university$Training_dsTotal[BF_university$University==2])
BF_Training_dsTotal_university
#EU
BF_Training_dsTotal_EU<- ttestBF(x=Follow_up$Training_dsTotal[Follow_up$EU==1],
                                 y=Follow_up$Training_dsTotal[Follow_up$EU==2])
BF_Training_dsTotal_EU


#################################################################
#BF Factor fear
#################################################################

BF_Fear_researchexp<- ttestBF(x=Follow_up$FearTotal[Follow_up$researchexp==1],
                              y=Follow_up$FearTotal[Follow_up$researchexp==2])
BF_Fear_researchexp
#
BF_Fear_professor<- ttestBF(x=Follow_up$FearTotal[Follow_up$Professor==1],
                            y=Follow_up$FearTotal[Follow_up$Professor==2])
BF_Fear_professor
#University
BF_Fear_university<- ttestBF(x=BF_university$FearTotal[BF_university$University==1],
                             y=BF_university$FearTotal[BF_university$University==2])
BF_Fear_university
#EU
BF_Fear_EU<- ttestBF(x=Follow_up$FearTotal[Follow_up$EU==1],
                     y=Follow_up$FearTotal[Follow_up$EU==2])
BF_Fear_EU

#################################################################
#BF Factor complexity
#################################################################

BF_Complexity_researchexp<- ttestBF(x=Follow_up$ComplexityTotal[Follow_up$researchexp==1],
                                    y=Follow_up$ComplexityTotal[Follow_up$researchexp==2])
BF_Complexity_researchexp
#
BF_Complexity_professor<- ttestBF(x=Follow_up$ComplexityTotal[Follow_up$Professor==1],
                                  y=Follow_up$ComplexityTotal[Follow_up$Professor==2])
BF_Complexity_professor

#University
BF_Complexity_university<- ttestBF(x=BF_university$ComplexityTotal[BF_university$University==1],
                                   y=BF_university$ComplexityTotal[BF_university$University==2])
BF_Complexity_university
#EU
BF_Complexity_EU<- ttestBF(x=Follow_up$ComplexityTotal[Follow_up$EU==1],
                           y=Follow_up$ComplexityTotal[Follow_up$EU==2])
BF_Complexity_EU

#################################################################
#BF Factor Control
#################################################################

BF_Control_researchexp<- ttestBF(x=Follow_up$ControlTotal[Follow_up$researchexp==1],
                                 y=Follow_up$ControlTotal[Follow_up$researchexp==2])
BF_Control_researchexp

#Professor
BF_Control_professor<- ttestBF(x=Follow_up$ControlTotal[Follow_up$Professor==1],
                               y=Follow_up$ControlTotal[Follow_up$Professor==2])
BF_Control_professor
#University
BF_Control_university<- ttestBF(x=BF_university$ControlTotal[BF_university$University==1],
                                y=BF_university$ControlTotal[BF_university$University==2])
BF_Control_university
#EU
BF_Control_EU<- ttestBF(x=Follow_up$ControlTotal[Follow_up$EU==1],
                        y=Follow_up$ControlTotal[Follow_up$EU==2])
BF_Control_EU


#################################################################
#BF Factor Boss
#################################################################

BF_Boss_researchexp<- ttestBF(x=Follow_up$BossTotal[Follow_up$researchexp==1],
                              y=Follow_up$BossTotal[Follow_up$researchexp==2])
BF_Boss_researchexp

#Professor
BF_Boss_professor<- ttestBF(x=Follow_up$BossTotal[Follow_up$Professor==1],
                            y=Follow_up$BossTotal[Follow_up$Professor==2])
BF_Boss_professor

#University
BF_Boss_university<- ttestBF(x=BF_university$BossTotal[BF_university$University==1],
                             y=BF_university$BossTotal[BF_university$University==2])
BF_Boss_university
#EU
BF_Boss_EU<- ttestBF(x=Follow_up$BossTotal[Follow_up$EU==1],
                     y=Follow_up$BossTotal[Follow_up$EU==2])
BF_Boss_EU


#################################################################
#BF Factor No resources datasharing
#################################################################

BF_noresources_researchexp<- ttestBF(x=Follow_up$NoResources_DS_TotalFollow_up$researchexp==1],
y=Follow_up$NoResources_DS_Total[Follow_up$researchexp==2])
BF_noresources_researchexp

#Professor
BF_noresources_professor<- ttestBF(x=Follow_up$NoResources_DS_Total[Follow_up$Professor==1],
                                   y=Follow_up$NoResources_DS_Total[Follow_up$Professor==2])
BF_noresources_professor

#University
BF_noresources_university<- ttestBF(x=BF_university$NoResources_DS_Total[BF_university$University==1],
                                    y=BF_university$NoResources_DS_Total[BF_university$University==2])
BF_noresources_university
#EU
BF_noresources_EU<- ttestBF(x=Follow_up$NoResources_DS_Total[Follow_up$EU==1],
                            y=Follow_up$NoResources_DS_Total[Follow_up$EU==2])
BF_noresources_EU
#################################################################
#BF DS09
#################################################################

BF_DS09_researchexp<- ttestBF(x=Follow_up$DS09[Follow_up$researchexp==1],
                              y=Follow_up$DS09[Follow_up$researchexp==2])
BF_DS09_researchexp

#Uni
BF_DS09_university<- ttestBF(x=BF_university$DS09[BF_university$University==1],
                             y=BF_university$DS09[BF_university$University==2])
BF_DS09_university
#
BF_DS09_Professor<- ttestBF(x=Follow_up$DS09[Follow_up$Professor==1],
                            y=Follow_up$DS09[Follow_up$Professor==2])
BF_DS09_Professor
#
BF_DS09_EU<- ttestBF(x=Follow_up$DS09[Follow_up$EU==1],
                     y=Follow_up$DS09[Follow_up$EU==2])
BF_DS09_EU

#################################################################
#BF DS10
#################################################################
BF_DS10_researchexp<- ttestBF(x=Follow_up$DS10[Follow_up$researchexp==1],
                              y=Follow_up$DS10[Follow_up$researchexp==2])
BF_DS10_researchexp
#
BF_DS10_university<- ttestBF(x=BF_university$DS10[BF_university$University==1],
                             y=BF_university$DS10[BF_university$University==2])
BF_DS10_university
#
BF_DS10_Professor<- ttestBF(x=Follow_up$DS10[Follow_up$Professor==1],
                            y=Follow_up$DS10[Follow_up$Professor==2])
BF_DS10_Professor
#
BF_DS10_EU<- ttestBF(x=Follow_up$DS10[Follow_up$EU==1],
                     y=Follow_up$DS10[Follow_up$EU==2])
BF_DS10_EU


