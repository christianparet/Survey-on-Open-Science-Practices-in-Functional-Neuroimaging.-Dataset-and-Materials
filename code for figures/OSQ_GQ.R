#################################################################
# Open Science Projekt
#################################################################



#################################################################
# Working directory
#################################################################

# Getting and setting working directory

getwd()

setwd(getwd())



#################################################################
# Installing R packages
#################################################################

# Installing packages

# Installing packages

install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("colorspace")
install.packages("scales")
install.packages("likert")
install.packages("reshape2")
install.packages("ggthemes")
install.packages("surveydata")
install.packages("data.table")
install.packages("tidyr")
install.packages("svglite")


# Loading required packages

library(ggplot2)

library(RColorBrewer)

library(colorspace)

library(scales)

library(likert)

library(reshape2)

library(ggthemes)

library(dplyr)

library(surveydata)

library(data.table)

library(tidyr)

library(svglite)



#################################################################
# Importing the Data Frame:
#
# Load file "OSQ_import_open-science-practices_2021-01-25_18-49.r"
# and choose CSV "rdata_open-science-practices_2021-01-27_15-49.csv"
#################################################################



#---------------------------------------------------------------
  ######## General Questions########

##GQ2 "What modalities do you collect?"

# Create subset for current question
GQ02 <- subset(OSQ_daten, select = c(7:15))

#Changes subset from logical to numeric (i.e. False/True to 0/1)
Cols <-  which(sapply(GQ02, is.logical))
setDT(GQ02)
for(j in Cols){
  set(GQ02, i=NULL, j=j, value= as.numeric(GQ02[[j]]))
}

#Changes GQ02 format so that the answer option is in the rows and the columns are the observations.
GQ02<- as.data.frame(t(GQ02))

#Adds Column with the number of people that chose a certain answer option.
GQ02$NumberofPeople <- rowSums (subset(GQ02, select = c(1:283)))

#Adds Column that specifies which answer option corresponds to which modality.

modality<-c("fMRI-BOLD",
            "MRI anatomical data",
            "fMRI_ASL",
            "EEG",
            "MEG",
            "fNIRS",
            "DTI",
            "PET",
            "Other")
GQ02$modality <- modality

# Adds factor levels to previous new variable, so that we can order the levels manually.
GQ02$modality<-factor(GQ02$modality,                                  
                      levels = c(
                        "fMRI-BOLD",
                        "MRI anatomical data",
                        "DTI",
                        "EEG",
                        "fMRI_ASL",
                        "PET",
                        "MEG",
                        "fNIRS",
                        "Other"))
GQ02$modality <- factor(GQ02$modality, levels = GQ02$modality [order(GQ02$modality, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
GQ02_prop <- (GQ02$NumberofPeople)/283
GQ02_perc <- as.data.frame(round(GQ02_prop * 100, digits = 2)) # save as data frame
GQ02$perc_labels <- paste(GQ02_perc$`round(GQ02_prop * 100, digits = 2)`, "%", sep = " ", collapse = NULL)


#Plot bar plot
GQ01plot<-ggplot(GQ02, aes(x= modality, y=NumberofPeople, fill=modality)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("What modalities do you collect?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  geom_text(aes(y = 293, label = perc_labels), color = "black", size = 3)+
  ylab("Number of Responses")+
  xlab("")+
  scale_fill_brewer(palette = "BrBG") +
  theme(legend.position="none")+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")
  )+
  scale_y_continuous(breaks=waiver())

# Print plot
GQ01plot

# Save plot 
ggsave(file="GQ01.svg", plot=GQ01plot)
