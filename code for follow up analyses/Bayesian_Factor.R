

#################################################################
# Loading packages (installs if necessary)
#################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(BayesFactor) 


BF_university<-cbind(Training,                          # create new dataframe for university to be able to exlude missing observations 
                     Fear, 
                     Complexity, 
                     Control_recoded, 
                     Follow_up$DS09,
                     Follow_up$DS10,
                     University)
BF_university<- na.omit(BF_university)                  # exlude missing observations 



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
BF_noresources_EU<- ttestBF(x=Follow_up$BossTotal[NoResources_DS_Total$EU==1],
                                y=Follow_up$NoResources_DS_Total[Follow_up$EU==2])
BF_noresources_EU

#################################################################
#BF DS09
#################################################################

BF_DS09_researchexp<- ttestBF(x=Follow_up$DS09[Follow_up$researchexp==1],
                                 y=Follow_up$DS09[Follow_up$researchexp==2])
BF_DS09_researchexp

#Uni
BF_DS09_university<- ttestBF(x=BF_university$`Follow_up$DS09`[BF_university$University==1],
                             y=BF_university$`Follow_up$DS09`[BF_university$University==2])
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
BF_DS10_university<- ttestBF(x=BF_university$`Follow_up$DS10`[BF_university$University==1],
                             y=BF_university$`Follow_up$DS10`[BF_university$University==2])
BF_DS10_university
#
BF_DS10_Professor<- ttestBF(x=Follow_up$DS10[Follow_up$Professor==1],
                            y=Follow_up$DS10[Follow_up$Professor==2])
BF_DS10_Professor
#
BF_DS10_EU<- ttestBF(x=Follow_up$DS10[Follow_up$EU==1],
                     y=Follow_up$DS10[Follow_up$EU==2])
BF_DS10_EU

