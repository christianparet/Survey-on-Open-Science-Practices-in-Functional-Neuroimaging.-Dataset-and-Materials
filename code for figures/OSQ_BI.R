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
  ######## #Brain Imaging Data Structure (BIDS) ########

##BI01 "Have you heard about the Brain Imaging Data Structure (BIDS) before?"

sum(OSQ_daten$BI01=="1") #->Yes = 203
sum(OSQ_daten$BI01=="2") #->No = 80

# Create subset of question
dat_BI01_NA <- subset(OSQ_daten, select = BI01)
table(OSQ_daten$BI01)
dat_BI01 <- na.omit(dat_BI01_NA)

# Specify variable as factor
dat_BI01$BI01 <- as.factor(dat_BI01$BI01)


# Specify levels according to response options
levels(dat_BI01$BI01) <- c("Yes",
                           "No")

levels(dat_BI01$BI01)
#[1] "Yes" "No"   

# Data frame with proportion, percentages, and number of responses
tab_BI01 <- as.data.frame(table(dat_BI01$BI01))
BI01_prop <- table(dat_BI01$BI01)/length(dat_BI01$BI01)
dat_BI01_perc <- as.data.frame(round(BI01_prop * 100, digits = 2)) # save as data frame
dat_BI01_perc$nrresp <- tab_BI01[,2] # add variable for number of responses ("nrresp")

# Add rounded %-Variable for labelling
dat_BI01_perc$perc_labels <- paste(dat_BI01_perc$Freq, "%", sep = " ", collapse = NULL)

# For pie chart: Compute position of labels
dat_BI01_perc <- dat_BI01_perc %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_BI01_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

# Plot pie
plot_BI01_pie <- ggplot(dat_BI01_perc, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  #scale_fill_brewer("", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual("", values = c("#BF812D", "#35978F")) +
  ggtitle("Have you heard about the Brain Imaging Data Structure \r\n(BIDS) before?") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

plot_BI01_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_BI01_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_BI01_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



##BI02 "Do you use BIDS to structure your neuroimaging data sets?"

sum(OSQ_daten$BI02=="1") #->Yes, I used it at least in one of my projects = 100
sum(OSQ_daten$BI02=="2") #->No = 183

# Create subset of question
dat_BI02_NA <- subset(OSQ_daten, select = BI02)
table(OSQ_daten$BI02)
dat_BI02 <- na.omit(dat_BI02_NA)

# Specify variable as factor
dat_BI02$BI02 <- as.factor(dat_BI02$BI02)

# Specify levels according to response options
levels(dat_BI02$BI02) <- c("Yes, I used it  \r\nat least in one \r\nof my projects.",
                           "No")

levels(dat_BI02$BI02)
#[1] "Yes, I used it at least in one of my projects." "No" 

# Data frame with proportion, percentages, and number of responses
tab_BI02 <- as.data.frame(table(dat_BI02$BI02))
BI02_prop <- table(dat_BI02$BI02)/length(dat_BI02$BI02)
dat_BI02_perc <- as.data.frame(round(BI02_prop * 100, digits = 2)) # save as data frame
dat_BI02_perc$nrresp <- tab_BI02[,2] # add variable for number of responses ("nrresp")

# Add rounded %-Variable for labelling
dat_BI02_perc$perc_labels <- paste(dat_BI02_perc$Freq, "%", sep = " ", collapse = NULL)

# For pie chart: Compute position of labels
dat_BI02_perc <- dat_BI02_perc %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_BI02_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

# Plot pie
plot_BI02_pie <- ggplot(dat_BI02_perc, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  #scale_fill_brewer("", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual("", values = c("#BF812D", "#35978F")) +
  ggtitle("Do you use BIDS to structure \r\nyour neuroimaging data sets?") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

plot_BI02_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_BI02_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_BI02_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



##BI04 "How many subjects have you converted to BIDS format? Please enter a rough estimate:"
#-> answered by subsample of n = 102

describe(OSQ_daten$BI04_01)#-> n=102, mean=208 subjects, SD= 351 subjects
##Nicht wirklich sinnvoll mean zu berechenn: median?= 97, n= 102, range= 2500


##BI05 "What BIDS converter, if any, did you use to convert neuroimaging data to BIDS format?"
#-> answered by subsample of n = 104, thereof 3 that did not choose any item

# Create subset for current question
BI05 <- subset(OSQ_daten, select = c(21:46, 48))

# Create new data frame where TRUE/FALSE are changed to 0/1
Cols3 <-  which(sapply(BI05, is.logical))
setDT(BI05)
for(j in Cols3){
  set(BI05, i=NULL, j=j, value= as.numeric(BI05[[j]]))
}
# Change format of data set, so that each row pertains to one answer option (rows are now columns, columns are rows)
BI05<- as.data.frame(t(BI05))

# Add new column that counts how many people responded with TRUE for each row
BI05$NumberofPeople <- rowSums(BI05, na.rm = TRUE)

# Add new column in which each row is assigned its answer option 
BIDSconvert <- c("AFNI BIDS-tools", 
                 "BIDSISATab", 
                 "BIDSto3col", 
                 "BIDS2NDA",
                 "Bidsify", 
                 "Bidskit", 
                 "Data2Bids", 
                 "DCm2Bids", 
                 "DCM2NIIx", 
                 "DICM2NII", 
                 "HeuDiConv", 
                 "OpenfMRI2BIDS",
                 "Reproln", 
                 "Bids2xar",
                 "XNAT2BIDS", 
                 "Horos export plugin",
                 "BIDS2NIDM", 
                 "BIDScoin", 
                 "MNE-BIDS", 
                 "EEGLAB plugin",
                 "Dac2bids", 
                 "Autobids", 
                 "Biscuit", 
                 "BiDirect_BIDS_Converter", 
                 "Custom code", 
                 "I have not used any converter yet",
                 "Other")

# Add factor levels to previous new variable, so that we can order the levels manually. 
BI05$BIDSconvert <- as.factor(BIDSconvert)
BI05$BIDSconvert <- factor(BI05$BIDSconvert, 
                           levels = c("Custom code",
                                      "DCM2NIIx",
                                      "DICM2NII", 
                                      "AFNI BIDS-tools",
                                      "DCm2Bids",
                                      "Bidsify",
                                      "XNAT2BIDS",
                                      "HeuDiConv",
                                      "EEGLAB plugin",
                                      "OpenfMRI2BIDS",
                                      "MNE-BIDS",
                                      "BIDSto3col",
                                      "BIDS2NDA",
                                      "Reproln",
                                      "Data2Bids",
                                      "Bidskit",
                                      "BIDScoin",
                                      "Horos export plugin",
                                      "Dac2bids",
                                      "Biscuit",
                                      "BIDSISATab", 
                                      "Bids2xar",
                                      "BIDS2NIDM", 
                                      "BiDirect_BIDS_Converter",
                                      "Autobids",
                                      "Other",
                                      "I have not used any converter yet"))


# Order factors in decreasing order 
BI05$BIDSconvert <- factor(BI05$BIDSconvert, levels = BI05$BIDSconvert [order(BI05$BIDSconvert, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
BI05_prop <- (BI05$NumberofPeople)/100
BI05_perc <- as.data.frame(round(BI05_prop*100, digits = 2)) # save as data frame
BI05$perc_labels <- paste(BI05_perc$`round(BI05_prop * 100, digits = 2)`, "%", sep = " ", collapse = NULL)

# Extend colour scheme of BrBG 
nb.cols <- 27
mycolors <- colorRampPalette(brewer.pal(8, "BrBG"))(nb.cols)

# Plot as bar graph 
plotBI05<-ggplot(BI05, aes(x= BIDSconvert, y=NumberofPeople, fill=BIDSconvert))+ 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("What BIDS converter, if any, have you used?")+
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  geom_text(aes(y = 42, label = perc_labels), color = "black", size = 3)+
  ylab("Number of Responses")+
  xlab("")+
  scale_fill_manual(values = mycolors)+
  #scale_fill_brewer(palette = "BrBG") +
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
    axis.line = element_line(colour = "grey"))

# Print plot
plotBI05

# Save plot 
ggsave(file="BI05.svg", plot=plotBI05)

##BI06 "How long have you been using BIDS?"
#-> answered by subsample of n = 101

describe(OSQ_daten$BI06_01)#-> n= 101, mean= 2,26 years, SD= 1,78 years

##BI07 "Are you going to use BIDS in the future?"
#-> answered by subsample of n = 104, thereof 3 that did not choose any item

# Create subset for current question
BI07<- subset(OSQ_daten, select = c(50))

# Rename Variable into Question 
BI07<- BI07%>%
  rename("Are you going to use BIDS in the future?" = BI07)

# Add vector with answer options 
likelihoodchoices  = c("Extremely Unlikely", 
                       "Unlikely", 
                       "Neither likely nor Unlikely", 
                       "Likely",
                       "Extremely Likely")

# Replace levels 1:5 with answer options 
for(i in 1:ncol(BI07)) {
  BI07[,i] = factor(BI07[,i], levels=1:5, labels=likelihoodchoices, ordered=TRUE)
}

# Plot likert plot
plotBI07 <- plot(likert(BI07), wrap=25,legend = "", legend.position = "bottom" )

# Print plot
plotBI07

# Save plot
ggsave(file="BI07.svg", plot=plotBI07)

######################
#--->in likelihooditem#
###################### 

##BI08 "What BIDS-compatible software, if any, have you used before?"
#-> answered by subsample of n = 104, thereof 3 that did not choose any item

# Create subset for current question
BI08 <- subset(OSQ_daten, select = c(52:62, 64))

# Create new data frame where TRUE/FALSE are changed to 0/1
Cols4 <-  which(sapply(BI08, is.logical))
setDT(BI08)
for(j in Cols4){
  set(BI08, i=NULL, j=j, value= as.numeric(BI08[[j]]))}

# Change format of data set, so that each row pertains to one answer option (rows are now columns, columns are rows)
BI08<- as.data.frame(t(BI08))

# Adds new column that counts how many people responded with TRUE for each row
BI08$NumberofPeople <- rowSums(BI08, na.rm = TRUE)

# Adds new column in which each row is assigned its answer option
BIDScompatible <- c("MRIQC",
                    "QAP",
                    "Automatic Analysis",
                    "BIDSHandler",
                    "Brainstorm",
                    "C-Pac",
                    "FMRIPREP",
                    "OpenNeuro",
                    "PyBIDS",
                    "Bids-matlab",
                    "Other",
                    "I have not used any of the above tools")

# Adds factor levels to previous new variable, so that we can order the levels manually.
BI08$BIDScompatible<-  as.factor(BIDScompatible)
BI08$BIDScompatible <- factor(BI08$BIDScompatible, 
                           levels = c("FMRIPREP",
                                      "MRIQC",
                                      "OpenNeuro",
                                      "C-Pac",
                                      "Bids-matlab",
                                      "PyBIDS",
                                      "Brainstorm",
                                      "BIDSHandler",
                                      "Automatic Analysis",
                                      "QAP",
                                      "Other",
                                      "I have not used any of the above tools"))


# Order factors in decreasing order
BI08$BIDScompatible <- factor(BI08$BIDScompatible, levels = BI08$BIDScompatible [order(BI08$BIDScompatible, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
BI08_prop <- (BI08$NumberofPeople)/100
BI08_perc <- as.data.frame(round(BI08_prop * 100, digits = 2)) # save as data frame
BI08$perc_labels <- paste(BI08_perc$`round(BI08_prop * 100, digits = 2)`, "%", sep = " ", collapse = NULL)

# Plot bar plot
BI08plot<-ggplot(BI08, aes(x= BIDScompatible, y=NumberofPeople, fill=BIDScompatible)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("What BIDS compatible software, if any, have you used?")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(size=10, hjust = 0.5),
          axis.text=element_text(size=8),
          axis.title=element_text(size=8))+
  geom_text(aes(y = 54, label = perc_labels), color = "black", size = 3)+
  ylab("Number of Responses")+
  xlab("")+
  scale_fill_manual(values = mycolors)+
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
BI08plot

# Save plot
ggsave(file="BI08.svg", plot=BI08plot)

##BI09 "Why did you not use BIDS?"
#-> answered by subsample of n = 184, thereof 1 that did not choose any item

# Create subset for current question
BI09<- subset(OSQ_daten, select = c(66, 68:74))

# Create new data frame where TRUE/FALSE are changed to 0/1
Cols5 <-  which(sapply(BI09, is.logical))
setDT(BI09)
for(j in Cols5){
  set(BI09, i=NULL, j=j, value= as.numeric(BI09[[j]]))}

# Change format of data set, so that each row pertains to one answer option (rows are now columns, columns are rows)
BI09<- as.data.frame(t(BI09))

# Adds new column that counts how many people responded with TRUE for each row
BI09$NumberofPeople <- rowSums(BI09, na.rm = TRUE)

# Adds new column in which each row is assigned its answer option
NoBIDS <- c("I use a different standard format than BIDS",
            "Did not know about it before",
            "Had no time to learn more about it",
            "Had no time to implement it in my lab",
            "Currently implementing it",
            "Lacking technical expertise to get BIDS conversion running",
            "Not relevant for me and my lab",
            "Other")


# Adds factor levels to previous new variable, so that we can order the levels manually.
BI09$NoBIDS <- as.factor(NoBIDS)
BI09$NoBIDS <- factor(BI09$NoBIDS, 
                      levels = c("Did not know about it before",
                                 "Had no time to implement it in my lab",
                                 "Had no time to learn more about it",
                                 "Lacking technical expertise to get BIDS conversion running",
                                 "Currently implementing it",
                                 "I use a different standard format than BIDS",
                                 "Not relevant for me and my lab",
                                 "Other"))

# Order factors in decreasing order
BI09$NoBIDS <- factor(BI09$NoBIDS, levels = BI09$NoBIDS [order(BI09$NoBIDS, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
BI09_prop <- (BI09$NumberofPeople)/183
BI09_perc <- as.data.frame(round(BI09_prop * 100, digits = 2)) # save as data frame
BI09$perc_labels <- paste(BI09_perc$`round(BI09_prop * 100, digits = 2)`, "%", sep = " ", collapse = NULL)

# Plot bar plot
BI09plot<-ggplot(BI09, aes(x= NoBIDS , y=NumberofPeople, fill=NoBIDS)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Why did you not use BIDS?")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.title = element_text(size=10, hjust = 0.5),
          axis.text=element_text(size=8),
          axis.title=element_text(size=8),
          aspect.ratio=1)+
    geom_text(aes(y = 96, label = perc_labels), color = "black", size = 3)+
   ylab("Number of People")+
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
BI09plot

# Save plot
ggsave(file="BI09.svg", plot=BI09plot)

##BI10 "Let's say that a new tool for conversion of neuroimaging data into BIDS would become available that is direc..."

# Create subset for current question
BI10<- subset(OSQ_daten, select = c(76))

# Rename Variable into Question
BI10<- BI10%>%
  rename("Let's say that a new tool for conversion of neuroimaging data into BIDS would become available that is directly operated through your preferred neuroimagin data analysis: Would this increase your interest to use BIDS in the future?"
         = BI10)

# Add vector with answer options
likelihoodchoices  = c("Extremely Unlikely", 
                       "Unlikely", 
                       "Neither likely nor Unlikely", 
                       "Likely",
                       "Extremely Likely")

# Replace levels 1:5 with answer options
for(i in 1:ncol(BI10)) {
  BI10[,i] = factor(BI10[,i], levels=1:5, labels=likelihoodchoices, ordered=TRUE)
}

# Plot likert plot
BI10plot <- plot(likert(BI10), wrap=25,legend = "", legend.position = "bottom" )

# Print plot
BI10plot

# Save plot
ggsave(file="BI10.svg", plot=BI10plot)



##BI11 "Why would you not be interested?"
#-> subsample of n = 28 answered this question, thereof 7 that did not give a response, plot is based on n = 21

sum(OSQ_daten$BI11== "1", na.rm = TRUE)#-> I am not interested in using BIDS at all = 9
sum(OSQ_daten$BI11== "2", na.rm=TRUE)#-> The existing BIDS tools work good enough for me = 4
sum(OSQ_daten$BI11== "3", na.rm=TRUE)#-> Other reason = 8  

# Create subset of question
dat_BI11_NA <- subset(OSQ_daten, select = BI11)
table(OSQ_daten$BI11)
# 7 "no response"

dat_BI11_noresp <- na.omit(dat_BI11_NA)
dat_BI11 <- subset(dat_BI11_NA, BI11 != -9)

# Specify variable as factor
dat_BI11$BI11 <- as.factor(dat_BI11$BI11)


# Specify levels according to response options
levels(dat_BI11$BI11) <- c("I am not interested in using BIDS at all.",
                           "The existing BIDS tools work good enough for me.", # w?re "well" besser? --> auch überlegt, dann gegoogelt, Internet meint, beides ginge
                           "Other reason")

# Reorder levels
table(dat_BI11$BI11)
dat_BI11$BI11 <- factor(dat_BI11$BI11, levels = c("Other reason",
                                                  "The existing BIDS tools work good enough for me.",
                                                  "I am not interested in using BIDS at all."))

levels(dat_BI11$BI11)
#[1] "Other reason"                                     "The existing BIDS tools work good enough for me."
#[3] "I am not interested in using BIDS at all." 


# Data frame with proportion, percentages, and number of responses
tab_BI11 <- as.data.frame(table(dat_BI11$BI11))
BI11_prop <- table(dat_BI11$BI11)/length(dat_BI11$BI11)
dat_BI11_perc <- as.data.frame(round(BI11_prop * 100, digits = 2)) # save as data frame
dat_BI11_perc$nrresp <- tab_BI11[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_BI11_perc$perc_labels <- paste(dat_BI11_perc$Freq, "%", sep = " ", collapse = NULL)


# Plot bar plot
plot_BI11_bar <- ggplot(dat_BI11_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,9+0.3*9)) +
  geom_text(aes(y = 9+0.2*9, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("Why would you not be interested in using BIDS  \r\ngiven there would be a new conversion tool?") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        aspect.ratio = 1) +
  labs(x = "", y = "Number of Responses") +
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey")) +   scale_x_discrete(labels = wrap_format(25))


# Print plot
plot_BI11_bar


# Save plot
dev.new(width = 4.72, height = 2.04, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_BI11_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 2.04, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_BI11_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



