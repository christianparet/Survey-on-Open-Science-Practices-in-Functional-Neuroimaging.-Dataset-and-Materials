#################################################################
# Open Science Projekt
#################################################################

#################################################################
# Working directory
#################################################################
# Set working directory -----------------------------------------------------------------------------------------------------------------------------------------------------------
getwd()                                                           # Getting working directory
setwd(getwd())                                                    # Setting working directory

#################################################################
# Installing R packages
#################################################################

# Install and load packages -----------------------------------------------------------------------------------------------------------------------------------------------------------
# use install.packages(c("dplyr", "psych")) if not already installed
library(dplyr)
library(psych)

# Sample description: General information -----------------------------------------------------------------------------------------------------------------------------------------------------------
# How long did people take to fill in questionnaire?
describe(OSQ_daten$TIME_SUM) # in seconds
#   vars   n   mean  sd median trimmed    mad min  max range skew kurtosis    se
#X1    1 283 577.46 190    555   562.7 180.88 229 1345  1116 0.86        1 11.29

mean((OSQ_daten$TIME_SUM)/60) # in minutes
# [1] 9.624382

sd((OSQ_daten$TIME_SUM)/60) # in minutes
#[1] 3.16671

# Sample description: Personal data -----------------------------------------------------------------------------------------------------------------------------------------------------------

##PD01 "What is your age?"
describe(OSQ_daten$PD01_01)#-> mean=43,89 years, SD= 9,74 years

##PD02 "Gender:"
sum(OSQ_daten$PD02=="1") #-> Male=186
sum(OSQ_daten$PD02== "2")#-> Female=96
sum(OSQ_daten$PD02== "3")#-> Divers=1 

##PD10 "My country of residence is"
sum(OSQ_daten$PD10== "1")#-> in the EU = 161
sum(OSQ_daten$PD10== "2")#-> outside of the EU= 122

##PD04 "I have been trained as"
sum(OSQ_daten$PD04== "1")#-> Biologist = 27
sum(OSQ_daten$PD04== "2")#-> Enigneer = 21
sum(OSQ_daten$PD04== "3")#-> Medical Doctor = 42
sum(OSQ_daten$PD04== "4")#-> Physicist = 23
sum(OSQ_daten$PD04== "5")#-> Clinical Psychologist = 13
sum(OSQ_daten$PD04== "6")#-> Psychologist = 113
sum(OSQ_daten$PD04== "7")#-> Other = 44

##PD05 "For how long have you been working in research?"
describe(OSQ_daten$PD05)#-> mean= 17,58 years, SD= 8,49 years

##PD06 "What is your field of study? Please select the best match."
sum(OSQ_daten$PD06== "1")#-> Psychiatry = 39
sum(OSQ_daten$PD06== "3")#-> Clinical Psychology= 13
sum(OSQ_daten$PD06== "9")#-> Cognitive Neuroscience = 170
sum(OSQ_daten$PD06== "4")#-> Neurology = 19
sum(OSQ_daten$PD06== "5")#-> Biology = 1
sum(OSQ_daten$PD06== "6")#-> Physics = 8
sum(OSQ_daten$PD06== "7")#-> Medicine, other discipline = 19
sum(OSQ_daten$PD06== "8")#-> Psychology, other discipline = 14

##PD07 "What is your current position?"
sum(OSQ_daten$PD07== "1")#-> Research Assistant = 2
sum(OSQ_daten$PD07== "2")#-> Phd Student= 6
sum(OSQ_daten$PD07== "7")#-> Lab Manager = 1
sum(OSQ_daten$PD07== "8")#-> Post-Doc = 5
sum(OSQ_daten$PD07== "9")#-> Assistant Progessor/Post-Doc = 86
sum(OSQ_daten$PD07== "10")#-> Associate Professor/Reader/Lecturer = 68
sum(OSQ_daten$PD07== "5")#-> Full professor = 71
sum(OSQ_daten$PD07== "6")#-> Other, specify = 20

##PD08 "What is your primary affiliation?"
sum(OSQ_daten$PD08== "1")#-> With university hospital/medical faculty = 10
sum(OSQ_daten$PD08== "2")#-> With university/psychology faculty or similar= 95
sum(OSQ_daten$PD08== "3")#-> With university/other faculty = 29
sum(OSQ_daten$PD08== "6")#-> With governmental institution  = 21
sum(OSQ_daten$PD08== "4")#-> With industry= 5
sum(OSQ_daten$PD08== "5")#-> Other = 10


# Sample description: Technical data -----------------------------------------------------------------------------------------------------------------------------------------------------------

##DS04: "Have you shared raw neuroimaging data with researchers outside your department?"
table(OSQ_daten$DS04) # Yes = 186; No = 96
(table(OSQ_daten$DS04)/length(OSQ_daten$DS04))*100 # Percentages

##NA02: "What is your preferred neuroimaging data analysis software?"
table(OSQ_daten$NA01) # Windows = 98; Linux = 120; Mac/ Apple = 63; Other = 2
(table(OSQ_daten$NA01)/length(OSQ_daten$NA01))*100 # Percentages

##NA03: "How would you describe your knowledge level in working with your preferred neuroimaging data analysis software?"
table(OSQ_daten$NA03) # Expert user = 97; Advanced user = 106; Practically experienced user = 76; Novice = 13
(table(OSQ_daten$NA03)/length(OSQ_daten$NA03))*100 # Percentages

##NA07: "I prefer to operate neuroimaging analysis software..."
table(OSQ_daten$NA07) # ..via graphical user interface. = 87; ...via command/batch interface. = 168; I don’t operate such software myself. = 28
(table(OSQ_daten$NA07)/length(OSQ_daten$NA07))*100 # Percentages

##BI01: "Have you heard about the Brain Imaging Data Structure (BIDS) before?"
table(OSQ_daten$BI01) # Yes = 203; No = 80
(table(OSQ_daten$BI01)/length(OSQ_daten$BI01))*100 # Percentages

##BI02: "Do you use BIDS to structure your neuroimaging data sets?"
table(OSQ_daten$BI02) # Yes, I used at least in one of my projects. = 100; No = 183
(table(OSQ_daten$BI02)/length(OSQ_daten$BI02))*100 # Percentages

##BI04: "How many subjects have you converted to BIDS format? Please enter a rough estimate:"
describe(OSQ_daten$BI04_01) # n = 102; mean = 208.32; sd = 351.15; median = 97; min = 0; max = 2500

##BI06: "How long have you been using BIDS?"
describe(OSQ_daten$BI06_01) # n = 102; mean = 2.26; sd = 1.78; median = 2; min = 0; max = 15
table(OSQ_daten$BI06_01)
