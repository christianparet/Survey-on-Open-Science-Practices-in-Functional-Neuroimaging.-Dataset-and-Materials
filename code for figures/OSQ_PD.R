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
  ######## Personal Data ########


##PD01 "What is your age?"

describe(OSQ_daten$PD01_01)#-> mean=43,89 years, SD= 9,74 years


##PD02 "Gender:"

sum(OSQ_daten$PD02=="1") #->Male=186
sum(OSQ_daten$PD02== "2")#->Female=96
sum(OSQ_daten$PD02== "3")#-> Divers=1 

##PD04 "I have been trained as"

sum(OSQ_daten$PD04== "1")#-> Biologist = 27
sum(OSQ_daten$PD04== "2")#-> Enigneer = 21
sum(OSQ_daten$PD04== "3")#-> Medical Doctor = 42
sum(OSQ_daten$PD04== "4")#-> Physicist = 23
sum(OSQ_daten$PD04== "5")#-> Clinical Psychologist = 13
sum(OSQ_daten$PD04== "6")#-> Psychologist = 113
sum(OSQ_daten$PD04== "7")#-> Other = 44

# Create subset of question
dat_PD04 <- subset(OSQ_daten, select = PD04)
table(OSQ_daten$PD04)
# No missings

# Open category "other"
PD04_specified_others <- na.omit(subset(OSQ_daten, select = PD04_07))
# Neuroscientists/ or combination which includes neuroscience: 24
# Other: Computer scientist, Biochemist, Engineer, Mathematician, Cognitive scientist, combination of many fields, Biomedical informatician/scientist,
#        Philosopher (and Neuroscientist, counts in above list), Movement scientist, Speech scientist, Psychiatrist, Optometrist and researcher, Physical Educator, ...


# Specify variable as factor
dat_PD04$PD04 <- as.factor(dat_PD04$PD04)


# Specify levels according to response options
levels(dat_PD04$PD04) <- c("Biologist", "Engineer", "Medical doctor", "Physicist", "Clinical psychologist (psychotherapy training)", "Psychologist (other)", "Other")


# Reorder levels
table(dat_PD04$PD04)
dat_PD04$PD04 <- factor(dat_PD04$PD04, levels = c("Other", "Clinical psychologist (psychotherapy training)", "Engineer", "Physicist", "Biologist", "Medical doctor", "Psychologist (other)"))

levels(dat_PD04$PD04)
#[1] "Other"                                          "Clinical psychologist (psychotherapy training)"
#[3] "Engineer"                                       "Physicist"                                     
#[5] "Biologist"                                      "Medical doctor"                                
#[7] "Psychologist (other)"                         


# Data frame with proportion, percentages, and number of responses
tab_PD04 <- as.data.frame(table(dat_PD04$PD04))

PD04_prop <- table(dat_PD04$PD04)/length(dat_PD04$PD04)

dat_PD04_perc <- as.data.frame(round(PD04_prop * 100, digits = 2)) # save as data frame

dat_PD04_perc$nrresp <- tab_PD04[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_PD04_perc$perc_labels <- paste(dat_PD04_perc$Freq, "%", sep = " ", collapse = NULL)


# Plot bar plot
plot_PD04_bar <- ggplot(dat_PD04_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,113+0.3*113)) +
  geom_text(aes(y = 113+0.2*113, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("I have been trained as...") +
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
plot_PD04_bar


# Save plot
dev.new(width = 4.72, height = 3.54, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD04_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 3.54, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD04_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


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

# Create subset of question
dat_PD06 <- subset(OSQ_daten, select = PD06)
table(OSQ_daten$PD06)
# No missings

# Open category "Medicine, other"
PD06_Medicine_specified_others <- na.omit(subset(OSQ_daten, select = PD06_07))

# Open category "Psychology, other"
PD06_Psychology_specified_others <- na.omit(subset(OSQ_daten, select = PD06_08))


# Specify variable as factor
dat_PD06$PD06 <- as.factor(dat_PD06$PD06)


# Specify levels according to response options
levels(dat_PD06$PD06) <- c("Psychiatry", "Clinical psychology", "Neurology", "Biology", "Physics", "Medicine (other discipline)", "Psychology (other discipline)", "Cognitive neuroscience")

levels(dat_PD06$PD06)
#[1] "Psychiatry"                    "Clinical psychology"           "Neurology"                     "Biology"                      
#[5] "Physics"                       "Medicine (other discipline)"   "Psychology (other discipline)" "Cognitive neuroscience"                     "Medicine, other discipline"   "Psychology, other discipline" "Cognitive neuroscience"


# Data frame with proportion, percentages, and number of responses
tab_PD06 <- as.data.frame(table(dat_PD06$PD06))

PD06_prop <- table(dat_PD06$PD06)/length(dat_PD06$PD06)

dat_PD06_perc <- as.data.frame(round(PD06_prop * 100, digits = 2)) # save as data frame

dat_PD06_perc$nrresp <- tab_PD06[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_PD06_perc$perc_labels <- paste(dat_PD06_perc$Freq, "%", sep = " ", collapse = NULL)


# For bar chart: Reorder data frame according to frequency of factor levels
dat_PD06_perc$Var1 <- factor(dat_PD06_perc$Var1, levels = dat_PD06_perc$Var1[order(dat_PD06_perc$Freq, decreasing = FALSE)])


# Plot bar plot
plot_PD06_bar <- ggplot(dat_PD06_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,170+0.3*170)) +
  geom_text(aes(y = 170+0.2*170, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("What is your field of study?") +
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
plot_PD06_bar


# Save plot
dev.new(width = 4.72, height = 4.08, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD06_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.08, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD06_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



##PD07 "What is your current position?"

sum(OSQ_daten$PD07== "1")#-> Research Assistant = 2
sum(OSQ_daten$PD07== "2")#-> Phd Student= 6
sum(OSQ_daten$PD07== "7")#-> Lab Manager = 1
sum(OSQ_daten$PD07== "8")#-> Post-Doc = 5
sum(OSQ_daten$PD07== "9")#-> Assistant Professor/Post-Doc = 86
sum(OSQ_daten$PD07== "10")#-> Associate Professor/Reader/Lecturer = 68
sum(OSQ_daten$PD07== "5")#-> Full professor = 71
sum(OSQ_daten$PD07== "6")#-> Other, specify = 20

# Create subset of question
dat_PD07 <- subset(OSQ_daten, select = PD07)
table(OSQ_daten$PD07)
# No missings

# Open category "other"
PD07_specified_others <- na.omit(subset(OSQ_daten, select = PD07_06))

# Specify variable as factor
dat_PD07$PD07 <- as.factor(dat_PD07$PD07)


# Specify levels according to response options
levels(dat_PD07$PD07) <- c("Research assistant", "PhD student", "Post-Doc (1-3 years)", "Full professor", "Other", "Technical assistant", "Lab manager", "Asisstant professor/ Post-Doc (4 years or longer)", "Associate Professor/ Reader/ Lecturer")

# Reorder levels
table(dat_PD07$PD07)
dat_PD07$PD07 <- factor(dat_PD07$PD07, levels = c("Other", "Research assistant", "PhD student", "Technical assistant", "Post-Doc (1-3 years)", "Lab manager", "Asisstant professor/ Post-Doc (4 years or longer)", "Associate Professor/ Reader/ Lecturer", "Full professor"))


levels(dat_PD07$PD07)
#[1] "Other"                                             "Research assistant"                               
#[3] "PhD student"                                       "Technical assistant"                              
#[5] "Post-Doc (1-3 years)"                              "Lab manager"                                      
#[7] "Asisstant professor/ Post-Doc (4 years or longer)" "Associate Professor/ Reader/ Lecturer"            
#[9] "Full professor"   

# Data frame with proportion, percentages, and number of responses
tab_PD07 <- as.data.frame(table(dat_PD07$PD07))

PD07_prop <- table(dat_PD07$PD07)/length(dat_PD07$PD07)

dat_PD07_perc <- as.data.frame(round(PD07_prop * 100, digits = 2)) # save as data frame

dat_PD07_perc$nrresp <- tab_PD07[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_PD07_perc$perc_labels <- paste(dat_PD07_perc$Freq, "%", sep = " ", collapse = NULL)


# Plot bar plot
plot_PD07_bar <- ggplot(dat_PD07_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,86+0.3*86)) +
  geom_text(aes(y = 86+0.2*86, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("What is your current position?") +
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
plot_PD07_bar


# Save plot
dev.new(width = 4.72, height = 4.59, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD07_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.59, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD07_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


##PD08 "What is your primary affiliation?"

sum(OSQ_daten$PD08== "1")#-> With university hospital/medical faculty = 10
sum(OSQ_daten$PD08== "2")#-> With university/psychology faculty or similar= 95
sum(OSQ_daten$PD08== "3")#-> With university/other faculty = 29
sum(OSQ_daten$PD08== "6")#-> With governmental institution  = 21
sum(OSQ_daten$PD08== "4")#-> With industry= 5
sum(OSQ_daten$PD08== "5")#-> Other = 10

# Create subset of question
dat_PD08 <- subset(OSQ_daten, select = PD08)
table(OSQ_daten$PD08)
# No missings


# Specify variable as factor
dat_PD08$PD08 <- as.factor(dat_PD08$PD08)


# Specify levels according to response options
levels(dat_PD08$PD08) <- c("With university hospital/ medical faculty",
                           "With university/ psychology faculty or similar",
                           "With university/ other faculty",
                           "With industry",
                           "Other",
                           "With governmental institution")

# Reorder levels
table(dat_PD08$PD08)
dat_PD08$PD08 <- factor(dat_PD08$PD08, levels = c("Other",
                                                  "With industry",
                                                  "With governmental institution",
                                                  "With university/ other faculty",
                                                  "With university/ psychology faculty or similar",
                                                  "With university hospital/ medical faculty"))


levels(dat_PD08$PD08)
#[1] "Other"                                          "With industry"                                 
#[3] "With governmental institution"                  "With university/ other faculty"                
#[5] "With university/ psychology faculty or similar" "With university hospital/ medical faculty"  


# Data frame with proportion, percentages, and number of responses
tab_PD08 <- as.data.frame(table(dat_PD08$PD08))

PD08_prop <- table(dat_PD08$PD08)/length(dat_PD08$PD08)

dat_PD08_perc <- as.data.frame(round(PD08_prop * 100, digits = 2)) # save as data frame

dat_PD08_perc$nrresp <- tab_PD08[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_PD08_perc$perc_labels <- paste(dat_PD08_perc$Freq, "%", sep = " ", collapse = NULL)


# Plot bar plot
plot_PD08_bar <- ggplot(dat_PD08_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,123+0.3*123)) +
  geom_text(aes(y = 123+0.2*123, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("What is your primary affiliation?") +
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
plot_PD08_bar


# Save plot
dev.new(width = 4.72, height = 3.06, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD08_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 3.06, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD08_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


##PD09 "My current research involves"

# Create subset of question
dat_PD09 <- subset(OSQ_daten, select = PD09)
table(OSQ_daten$PD09)
# No missings

# Subset
dat_PD09_010203 <- subset(OSQ_daten, select = c(PD09_01, PD09_02, PD09_03))


dat_PD09_010203$PD09_01 <- as.numeric(dat_PD09_010203$PD09_01)
dat_PD09_010203$PD09_02 <- as.numeric(dat_PD09_010203$PD09_02)
dat_PD09_010203$PD09_03 <- as.numeric(dat_PD09_010203$PD09_03)


nrresp <- colSums(dat_PD09_010203) # nrresp

Freq <- round((colSums(dat_PD09_010203)/283)*100, digits = 2) # proportion of people

Var1 <- c("healthy participant data.",
          "data from individuals affected with a mental disorder.",
          "data from individuals affected with a physical disorder.")

dat_PD09_010203_perc <- cbind.data.frame(Var1, Freq, nrresp)


# Add rounded %-Variable for labelling
dat_PD09_010203_perc$perc_labels <- paste(dat_PD09_010203_perc$Freq, "%", sep = " ", collapse = NULL)


# For bar chart: Reorder data frame according to frequency of factor levels
dat_PD09_010203_perc$Var1 <- factor(dat_PD09_010203_perc$Var1, levels = dat_PD09_010203_perc$Var1[order(dat_PD09_010203_perc$Freq, decreasing = FALSE)])


levels(dat_PD09_010203_perc$Var1)
#[1] "data from individuals affected with a physical disorder." "data from individuals affected with a mental disorder."  
#[3] "healthy participant data." 


# Choose colors manually
HEX_BrBG <- brewer.pal(n = 11, name = "BrBG")
# [1] "#543005" "#8C510A" "#BF812D" "#DFC27D" "#F6E8C3" "#F5F5F5" "#C7EAE5" "#80CDC1" "#35978F" "#01665E" "#003C30"

display.brewer.pal(n = 11, name = "BrBG")

HEX_BrBG_3 <- c("#8C510A", "#C7EAE5", "#01665E")

# Plot bar plot
plot_PD09_010203_bar <- ggplot(dat_PD09_010203_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,247+0.3*247)) +
  geom_text(aes(y = 247+0.2*247, label = perc_labels), color = "black", size = 2.5) +
  #  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  scale_fill_manual(values = c("#DFC27D", "#C7EAE5", "#35978F"), guide = FALSE) +
  ggtitle("My current research involves...") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        aspect.ratio = 1) +
  labs(x = "", y = "Number of Responses") +
  theme(
    panel.border = element_blank(),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey")) +
  scale_x_discrete(labels = wrap_format(25))

# Print plot
plot_PD09_010203_bar

# Save plot
dev.new(width = 4.72, height = 1.53, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_PD09_010203_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 1.53, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_PD09_010203_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()

##PD10 "My country of residence is"

sum(OSQ_daten$PD10== "1")#-> in the EU = 161
sum(OSQ_daten$PD10== "2")#-> outside of the EU= 122

# Create subset of question
dat_PD10 <- subset(OSQ_daten, select = PD10)
table(dat_PD10$PD10)
# No missings

# Specify variable as factor
dat_PD10$PD10 <- as.factor(dat_PD10$PD10)


# Specify levels according to response options
levels(dat_PD10$PD10) <- c("in the EU", "outside of the EU")


# Data frame with proportion, percentages, and number of responses
tab_PD10 <- as.data.frame(table(dat_PD10$PD10))

PD10_prop <- table(dat_PD10$PD10)/length(dat_PD10$PD10)

dat_PD10_perc <- as.data.frame(round(PD10_prop * 100, digits = 2)) # save as data frame

dat_PD10_perc$nrresp <- tab_PD10[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_PD10_perc$perc_labels <- paste(dat_PD10_perc$Freq, "%", sep = " ", collapse = NULL)


# For pie chart: Compute position of labels
dat_PD10_perc <- dat_PD10_perc %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_PD10_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


# Plot as pie chart
plot_PD10_pie <- ggplot(dat_PD10_perc, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
#  scale_fill_brewer("Country of residence", palette="BrBG") +
  scale_fill_manual("Country of residence", values = c("#BF812D", "#35978F")) +
  ggtitle("My country of residence is...") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


# Print plot
plot_PD10_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD10_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_PD10_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


##PD12 "If you want to, you may indicate your country of residence here:"

################
#->open question#
################
#---------------------------------------------------------------
  ######## Feedback ########


##FB01 "If you want to, you can submit your comments to the authors via this field."
