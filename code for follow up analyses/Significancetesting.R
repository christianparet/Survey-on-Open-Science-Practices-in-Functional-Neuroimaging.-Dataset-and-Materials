#################################################################
# Loading packages (installs if necessary)
#################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               naniar,
               RColorBrewer,
               svglite) 

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
Follow_up$NoResources_DS<-NoResources_DS$NoResources_DS_Total
Follow_up$researchexp<-as.factor(Follow_up$researchexp)
Follow_up$University<- as.factor(Follow_up$University)
Follow_up$Professor<- as.factor(Follow_up$Professor)
Follow_up$EU<- as.factor(Follow_up$EU)
Follow_up$DS09<- OSQ_daten$DS09
Follow_up$prev_prereg<- prev_prereg$prev_prereg
Follow_up$researchexp_scale<-OSQ_daten$PD05_01
Follow_up$DS04<- OSQ_daten$DS04


#################################################################
#Significance testing           
#################################################################

# Comparing the effect of research experience (dichotomous) on each factor
t.test(Follow_up$Training_preregTotal ~ researchexp)
t.test(Follow_up$FearTotal ~ researchexp)
t.test(Follow_up$ComplexityTotal ~ researchexp)
t.test(Follow_up$ControlTotal ~ researchexp)
t.test(Follow_up$Training_dsTotal ~ researchexp)
t.test(Follow_up$BossTotal ~ researchexp)
t.test(Follow_up$NoResources_DS ~ researchexp)

##Additionally to the dichotomous predictor we explored "research experience" as a continuously scaled predictor variable. 
##For reasons of conciseness, this analysis is not included in the publication.
# # Comparing the effect of research experience (continuous) on each factor
# fit <- lm(Training_preregTotal ~ researchexp_scale, data=Follow_up)              # regression model with Factor Total as predicted variable and research experience in years as predictor
# summary(fit)
# fit <- lm(Training_dsTotal ~ researchexp_scale, data=Follow_up)              
# summary(fit)
# fit <- lm(FearTotal ~ researchexp_scale, data=Follow_up)
# summary(fit)
# fit <- lm(ComplexityTotal ~ researchexp_scale, data=Follow_up)
# summary(fit)
# fit <- lm(ControlTotal ~ researchexp_scale, data=Follow_up)
# summary(fit)
# fit <- lm(BossTotal ~ researchexp_scale, data=Follow_up)
# summary(fit)
# fit <- lm(NoResources_DS ~ researchexp_scale, data=Follow_up) 
# summary(fit)


# Comparing the effect of EU residency on each factor
t.test(Follow_up$Training_preregTotal ~ EU)
t.test(Follow_up$FearTotal ~ EU)
t.test(Follow_up$ComplexityTotal ~ EU)
t.test(Follow_up$ControlTotal ~ EU)
t.test(Follow_up$Training_dsTotal ~ EU)
t.test(Follow_up$BossTotal ~ EU)
t.test(Follow_up$NoResources_DS ~ EU)

# Comparing the effect of affiliation with medical faculty on each factor        
t.test(Follow_up$Training_preregTotal ~ University)
t.test(Follow_up$FearTotal ~ University)
t.test(Follow_up$ComplexityTotal ~ University)
t.test(Follow_up$ControlTotal ~ University)
t.test(Follow_up$Training_dsTotal ~ University)
t.test(Follow_up$BossTotal ~ University)
t.test(Follow_up$NoResources_DS ~ University)

# Comparing the effect of career level on each factor
t.test(Follow_up$Training_preregTotal ~ Professor)
t.test(Follow_up$FearTotal ~ Professor)
t.test(Follow_up$ComplexityTotal ~ Professor)
t.test(Follow_up$ControlTotal ~ Professor)
t.test(Follow_up$Training_dsTotal ~ Professor)
t.test(Follow_up$BossTotal ~ Professor)
t.test(Follow_up$NoResources_DS ~ Professor)

# Differences in datasharing for people living inside or outside of the EU 
#(1 = in the EU. 2 = outside of the EU.)
t.test(Follow_up$DS09 ~ EU)
t.test(Follow_up$DS10 ~ EU)
chisq.test(Follow_up$DS04, Follow_up$EU)


#Comparing current BIDS usage based on data analysis software preferences

#BI02 Do you use BIDS to structure your neuroimaging datasets?
Follow_up$BI02<- OSQ_daten$BI02
Follow_up$BI02<-recode(Follow_up$BI02, "1" = "Yes", "2" = "No")

#NA02 "What is your preferred neuroimaging data analysis software?"
table(OSQ_daten$NA02)                                                           #1=160, 2=42, 3=24, 4=14, 5=2, 6=41

NA02<-OSQ_daten$NA02
NA02<- as.data.frame(NA02)

NA02 <- NA02 %>%                                                                #-> collapse 6 factors in to 2 (SPM and other preferred neuroimaging data analysis software)
  mutate(type = as.factor(case_when(
    NA02 %in% c(1) ~ 1,                                                         # 1 = SPM
    NA02 %in% c(2, 3, 4, 5, 6) ~ 2)))                                           # 2 = FSL, AFNI, BrainVoyager, ANTS, Other

NA02<- as.factor(NA02$type)
Follow_up$NA02<- NA02
Follow_up$NA02<-recode(Follow_up$NA02, '1' = "SPM", '2' = "Other")

#Differences in BIDS usage based on preference of neuroimaging data analysis software
BI02_NA02<-table(Follow_up$NA02, Follow_up$BI02)

BI02_NA02_chisq<-chisq.test(BI02_NA02)
BI02_NA02_chisq$expected
BI02_NA02_chisq$observed
BI02_NA02_chisq$p.value

#plot BI02_

dt<- as.data.frame(cbind(NA02 = OSQ_daten$NA02, BI02 = OSQ_daten$BI02))
dt$NA02<-recode(dt$NA02, "1" = "SPM", "2" = "FSL", "3" = "AFNI", "4" = "BrainVoyager", "5" = "ANTs", "6" = "Other")
dt$NA02 <- as.factor(dt$NA02)
dt$BI02<-recode(dt$BI02, "1" = "Yes", "2" = "No")
dt$BI02 <- as.factor(dt$BI02)

x<-table(dt)
coul<- brewer.pal(5, "Set1") 

svg("BIDSuse.svg")
plot(x, xlab= "Preferred Imaging Software", ylab="Using BIDS", main="", col=coul)
dev.off()

#NA07 "I prefer to operate neuroimaging analysis software..."
#1 = ...via graphical user interface
#2 = ...via command/batch interface
#3 = I don't operate such software myself.
table(OSQ_daten$NA07)                                                           #1=87, 2=168, 3=28

NA07<- OSQ_daten$NA07
NA07<- as.data.frame(NA07)

NA07<-NA07%>%                                                                   # replace level 3 with missing values 
  replace_with_na(replace = list(NA07 = c(3)))

NA07<- as.factor(NA07$NA07)
Follow_up$NA07<- NA07
Follow_up$NA07<-recode(Follow_up$NA07, '1' = "GUI", '2' = "command")


# Differences in BIDS usage based on preference to work with graphical user or commmand/batch interface
BI02_NA07<-table(Follow_up$NA07, Follow_up$BI02)

chisq.test(BI02_NA07)

BI02_NA07_chisq<-chisq.test(BI02_NA07)

BI02_NA07_chisq$observed
BI02_NA07_chisq$expected
BI02_NA07_chisq$p.value



#################################################################
#SessionInfo()        
#################################################################


# R version 4.0.5 (2021-03-31)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252
# [4] LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] svglite_2.0.0          RColorBrewer_1.1-2     naniar_0.6.0           forcats_0.5.1         
# [5] stringr_1.4.0          dplyr_1.0.5            purrr_0.3.4            readr_1.4.0           
# [9] tidyr_1.1.3            tibble_3.1.1           ggplot2_3.3.3          tidyverse_1.3.1       
# [13] pacman_0.5.1           BayesFactor_0.9.12-4.2 Matrix_1.3-2           coda_0.19-4           
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.6         lubridate_1.7.10   mvtnorm_1.1-1      lattice_0.20-41    gtools_3.9.2      
# [6] assertthat_0.2.1   utf8_1.2.1         R6_2.5.0           cellranger_1.1.0   backports_1.2.1   
# [11] MatrixModels_0.5-0 reprex_2.0.0       visdat_0.5.3       httr_1.4.2         pillar_1.6.0      
# [16] rlang_0.4.11       readxl_1.3.1       rstudioapi_0.13    munsell_0.5.0      broom_0.7.6       
# [21] compiler_4.0.5     modelr_0.1.8       systemfonts_1.0.1  pkgconfig_2.0.3    tidyselect_1.1.1  
# [26] fansi_0.4.2        crayon_1.4.1       dbplyr_2.1.1       withr_2.4.2        grid_4.0.5        
# [31] jsonlite_1.7.2     gtable_0.3.0       lifecycle_1.0.0    DBI_1.1.1          magrittr_2.0.1    
# [36] scales_1.1.1       cli_2.5.0          stringi_1.5.3      pbapply_1.5-0      fs_1.5.0          
# [41] xml2_1.3.2         ellipsis_0.3.2     generics_0.1.0     vctrs_0.3.8        tools_4.0.5       
# [46] glue_1.4.2         hms_1.0.0          parallel_4.0.5     colorspace_2.0-1   rvest_1.0.0       
# [51] haven_2.4.1    
