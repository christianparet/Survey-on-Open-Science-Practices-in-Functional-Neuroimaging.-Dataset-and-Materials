
##############

#install.packages("BayesFactor")
library(BayesFactor)


BF_university<-cbind(Training,                          # create new dataframe for university to be able to exlude missing observations 
                     Fear, 
                     Complexity, 
                     Control_recoded, 
                     Follow_up$DS09,
                     Follow_up$DS10,
                     University)
BF_university<- na.omit(BF_university)                  # exlude missing observations 


#BF Factor training 
BF_Training_researchexp<- ttestBF(x=Follow_up$TrainingTotal[Follow_up$researchexp==1],
                                  y=Follow_up$TrainingTotal[Follow_up$researchexp==2])
BF_Training_researchexp

#
BF_Training_professor<- ttestBF(x=Follow_up$TrainingTotal[Follow_up$Professor==1],
                                y=Follow_up$TrainingTotal[Follow_up$Professor==2])
BF_Training_professor
#
BF_Training_university<- ttestBF(x=BF_university$TrainingTotal[BF_university$University==1],
                                 y=BF_university$TrainingTotal[BF_university$University==2])
BF_Training_university
#EU
BF_Training_EU<- ttestBF(x=Follow_up$TrainingTotal[Follow_up$EU==1],
                        y=Follow_up$TrainingTotal[Follow_up$EU==2])
BF_Training_EU



## BF Factor Fear

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


##Factor complexity

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

##Factor Data governance

BF_Control_researchexp<- ttestBF(x=Follow_up$ControlTotal[Follow_up$researchexp==1],
                                 y=Follow_up$ControlTotal[Follow_up$researchexp==2])
BF_Control_researchexp

#
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

## DS09

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

##DS10
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

