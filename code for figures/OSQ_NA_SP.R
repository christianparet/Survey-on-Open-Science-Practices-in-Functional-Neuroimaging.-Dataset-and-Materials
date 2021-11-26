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
# Loading packages (installs if necessary)
#################################################################


if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, 
               RColorBrewer, 
               colorspace, 
               scales, 
               likert, 
               ggthemes, 
               dplyr, 
               data.table, 
               tidyr, 
               psych)

######## Neuroimaging Analysis Software and OS ########

###################################################################################
#NA01 "What operating system do you primarily use to work with neuroimaging data?" #
###################################################################################

sum(OSQ_daten$NA01== "1", na.rm = TRUE)#-> Windows = 98
sum(OSQ_daten$NA01== "2", na.rm=TRUE)#-> Linux = 120
sum(OSQ_daten$NA01== "3", na.rm=TRUE)#-> Mac/Apple = 63 
sum(OSQ_daten$NA01== "4", na.rm=TRUE)#-> Other = 2

# Create subset of question
dat_NA01 <- subset(OSQ_daten, select = NA01)
table(OSQ_daten$NA01)
# No missings

# Open category "other"
NA01_specified_others <- na.omit(subset(OSQ_daten, select = NA01_04))
# both mac/apple and linux
# desktops mac, servers linux

# Specify variable as factor
dat_NA01$NA01 <- as.factor(dat_NA01$NA01)


# Specify levels according to response options
levels(dat_NA01$NA01) <- c("Windows",
                           "Linux",
                           "Mac/ Apple",
                           "Other")

# Data frame with proportion, percentages, and number of responses
tab_NA01 <- as.data.frame(table(dat_NA01$NA01))

NA01_prop <- table(dat_NA01$NA01)/length(dat_NA01$NA01)

dat_NA01_perc <- as.data.frame(round(NA01_prop * 100, digits = 1)) # save as data frame

dat_NA01_perc$nrresp <- tab_NA01[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_NA01_perc$perc_labels <- paste(dat_NA01_perc$Freq, "%", sep = " ", collapse = NULL)


# Reorder levels according to frequency
dat_NA01_perc$Var1 <- factor(dat_NA01_perc$Var1, levels = dat_NA01_perc$Var1[order(dat_NA01_perc$Freq, decreasing = FALSE)])
levels(dat_NA01$NA01)

# Plot bar plot
plot_NA01_bar <- ggplot(dat_NA01_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,120+0.3*120)) +
  geom_text(aes(y = 120+0.2*120, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("What operating system do you primarily \r\nuse to work with neuroimaging data?") +
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
plot_NA01_bar

# Save plot
dev.new(width = 4.72, height = 2.04, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_NA01_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 2.04, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_NA01_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()

# For pie chart: Compute position of labels
dat_NA01_perc <- dat_NA01_perc %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_NA01_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


# Plot pie
plot_NA01_pie <- ggplot(dat_NA01_perc, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer("Operating system", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  ggtitle("What operating system do you primarily \r\nuse to work with neuroimaging data?") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


plot_NA01_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA01_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA01_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



#####################################################################
#NA02 "What is your preferred neuroimaging data analysis software?" #
#####################################################################

sum(OSQ_daten$NA02== "1", na.rm = TRUE)#-> SPM = 160
sum(OSQ_daten$NA02== "2", na.rm=TRUE)#-> FSL = 42
sum(OSQ_daten$NA02== "3", na.rm=TRUE)#-> AFNI = 24
sum(OSQ_daten$NA02== "4", na.rm=TRUE)#-> BrainVoyager = 14
sum(OSQ_daten$NA02== "5", na.rm = TRUE)#-> ANTs = 2
sum(OSQ_daten$NA02== "6", na.rm=TRUE)#-> Other = 41

# Create subset of question
dat_NA02 <- subset(OSQ_daten, select = NA02)
table(OSQ_daten$NA02)
# No missings


# Open category "other"
NA02_specified_others <- na.omit(subset(OSQ_daten, select = NA02_06))
# many: custom matlab code
# many: Freesurfer


# Specify variable as factor
dat_NA02$NA02 <- as.factor(dat_NA02$NA02)


# Specify levels according to response options
levels(dat_NA02$NA02) <- c("SPM",
                           "FSL",
                           "AFNI",
                           "BrainVoyager",
                           "ANTs",
                           "Other")

# Reorder levels according to frequency, but "Other" last
table(dat_NA02$NA02)
dat_NA02$NA02 <- factor(dat_NA02$NA02, levels = c("Other",
                                                  "ANTs",
                                                  "BrainVoyager",
                                                  "AFNI",
                                                  "FSL",
                                                  "SPM"))


# Data frame with proportion, percentages, and number of responses
tab_NA02 <- as.data.frame(table(dat_NA02$NA02))

NA02_prop <- table(dat_NA02$NA02)/length(dat_NA02$NA02)

dat_NA02_perc <- as.data.frame(round(NA02_prop * 100, digits = 1)) # save as data frame

dat_NA02_perc$nrresp <- tab_NA02[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_NA02_perc$perc_labels <- paste(dat_NA02_perc$Freq, "%", sep = " ", collapse = NULL)



# Plot bar plot
plot_NA02_bar <- ggplot(dat_NA02_perc, aes(x = Var1, y = nrresp, fill = Var1)) +
  geom_bar(stat = "identity", width = 0.8) +
  coord_flip(ylim = c(0,160+0.3*160)) +
  geom_text(aes(y = 160+0.2*160, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer(palette = "BrBG", guide = FALSE) +
  ggtitle("What is your preferred neuroimaging data analysis software?") +
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
plot_NA02_bar


# Save plot
dev.new(width = 4.72, height = 3.06, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA02_bar.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 3.06, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA02_bar.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()

# For pie
dat_NA02_perc_pie <- dat_NA02_perc

# Reorder levels according to frequency
dat_NA02_perc_pie$Var1 <- factor(dat_NA02_perc_pie$Var1, levels = dat_NA02_perc_pie$Var1[order(dat_NA02_perc_pie$Freq, decreasing = FALSE)])
levels(dat_NA02_perc_pie$Var1)


# For pie chart: Compute position of labels
dat_NA02_perc_pie <- dat_NA02_perc_pie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_NA02_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


# Plot pie
plot_NA02_pie <- ggplot(dat_NA02_perc_pie, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer("Operating system", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  ggtitle("What is your preferred neuroimaging \r\n data analysis software?") + # \r\n
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


plot_NA02_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA02_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA02_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



#########################################################################################################################
#NA03 "How would you describe your knowledge level in working with your preferred neuroimaging data analysis software?" #
#########################################################################################################################

sum(OSQ_daten$NA03== "1", na.rm = TRUE)#-> Expert User = 97
sum(OSQ_daten$NA03== "2", na.rm=TRUE)#-> Advanced User = 106
sum(OSQ_daten$NA03== "3", na.rm=TRUE)#-> Practically experienced = 67 
sum(OSQ_daten$NA03== "4", na.rm=TRUE)#-> Novice = 13

# Create subset of question
dat_NA03 <- subset(OSQ_daten, select = NA03)
table(OSQ_daten$NA03)
# No missings


# Specify variable as factor
dat_NA03$NA03 <- as.factor(dat_NA03$NA03)


# Specify levels according to response options
levels(dat_NA03$NA03) <- c("Expert user",
                           "Advanced user",
                           "Practically experienced user",
                           "Novice")

# Reorder levels according to hierarchy
table(dat_NA03$NA03)
dat_NA03$NA03 <- factor(dat_NA03$NA03, levels = c("Novice",
                                                  "Practically experienced user",
                                                  "Advanced user",
                                                  "Expert user"))


# Data frame with proportion, percentages, and number of responses
tab_NA03 <- as.data.frame(table(dat_NA03$NA03))

NA03_prop <- table(dat_NA03$NA03)/length(dat_NA03$NA03)

dat_NA03_perc <- as.data.frame(round(NA03_prop * 100, digits = 1)) # save as data frame

dat_NA03_perc$nrresp <- tab_NA03[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_NA03_perc$perc_labels <- paste(dat_NA03_perc$Freq, "%", sep = " ", collapse = NULL)


# For pie
dat_NA03_perc_pie <- dat_NA03_perc

# Reorder levels according to frequency # not here
#dat_NA03_perc_pie$Var1 <- factor(dat_NA03_perc_pie$Var1, levels = dat_NA03_perc_pie$Var1[order(dat_NA03_perc_pie$Freq, decreasing = FALSE)])
#levels(dat_NA03_perc_pie$Var1)


# For pie chart: Compute position of labels
dat_NA03_perc_pie <- dat_NA03_perc_pie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_NA03_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


# Plot pie
plot_NA03_pie <- ggplot(dat_NA03_perc_pie, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer("Knowledge level", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  ggtitle("How would you describe your knowledge level \r\nin working with your preferred neuroimaging \r\ndata analysis software?") + # \r\n
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


plot_NA03_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_NA03_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot() # change manually
ggsave("plot_NA03_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


##NA04 "Do you have practical experience with other neuroimaging analysis software?"

sum(OSQ_daten$NA04=="1") #->Yes = 230
sum(OSQ_daten$NA04== "2")#->No = 53

# Create subset of question
dat_NA04_NA <- subset(OSQ_daten, select = NA04)
table(OSQ_daten$NA04)
dat_NA04 <- na.omit(dat_NA04_NA)

# Specify variable as factor
dat_NA04$NA04 <- as.factor(dat_NA04$NA04)

# Specify levels according to response options
levels(dat_NA04$NA04) <- c("Yes",
                           "No")

levels(dat_NA04$NA04)
#[1] "Yes" "No" 

# Data frame with proportion, percentages, and number of responses
tab_NA04 <- as.data.frame(table(dat_NA04$NA04))
NA04_prop <- table(dat_NA04$NA04)/length(dat_NA04$NA04)
dat_NA04_perc <- as.data.frame(round(NA04_prop * 100, digits = 1)) # save as data frame
dat_NA04_perc$nrresp <- tab_NA04[,2] # add variable for number of responses ("nrresp")

# Add rounded %-Variable for labelling
dat_NA04_perc$perc_labels <- paste(dat_NA04_perc$Freq, "%", sep = " ", collapse = NULL)

# For pie chart: Compute position of labels
dat_NA04_perc <- dat_NA04_perc %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_NA04_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

# Plot pie
plot_NA04_pie <- ggplot(dat_NA04_perc, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  #scale_fill_brewer("", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual("", values = c("#BF812D", "#35978F")) +
  ggtitle("Do you have practical experience with other \r\nneuroimaging analysis software?") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

plot_NA04_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA04_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA04_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



#################################################################################################
#NA05 "On the previous slide you said you are experienced with other software. Which software?" #
#################################################################################################

#-> answered by subsample of n = 236, thereof 6 did not choose any item, plot is based on subsample of n = 230

# Create subset for current question
NA05<- subset(OSQ_daten, select=c(133:138))

# Create new data frame where TRUE/FALSE are changed to 0/1
Cols6 <-  which(sapply(NA05, is.logical))
setDT(NA05)
for(j in Cols6){
  set(NA05, i=NULL, j=j, value= as.numeric(NA05[[j]]))}

# Change format of data set, so that each row pertains to one answer option (rows are now columns, columns are rows)
NA05<- as.data.frame(t(NA05))

# Add new column that counts how many people responded with TRUE for each row
NA05$NumberofPeople<- rowSums(NA05, na.rm = TRUE)

# Add new column in which each row is assigned its answer option
Othersoftware<- c("SPM",
                  "FSL",
                  "AFNI",
                  "BrainVoyager",
                  "ANTs",
                  "Other")
NA05$Othersoftware <- Othersoftware

# Add factor levels to previous new variable, so that we can order the levels manually.
NA05$Othersoftware <- factor(NA05$Othersoftware,                                
                             levels = c("FSL",
                                        "SPM",
                                        "AFNI",
                                        "BrainVoyager",
                                        "ANTs",
                                        "Other"))

# Order factors in decreasing order
NA05$Othersoftware <- factor(NA05$Othersoftware, levels = NA05$Othersoftware [order(NA05$Othersoftware, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
NA05_prop <- (NA05$NumberofPeople)/230
NA05_perc <- as.data.frame(round(NA05_prop * 100, digits = 1)) # save as data frame
NA05$perc_labels <- paste(NA05_perc$`round(NA05_prop * 100, digits = 1)`, "%", sep = " ", collapse = NULL)

# Plot bar plot
NA05plot<-ggplot(NA05, aes(x= Othersoftware , y=NumberofPeople, fill=Othersoftware)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("On the previous slide you said you are experienced with other software. Which software?")+
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  geom_text(aes(y = 145, label = perc_labels), color = "black", size = 3)+
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
NA05plot

# Save plot
ggsave(file="NA05.svg", plot=NA05plot)

########################################################################################################################
NA06 "What is your knowledge level with this/these software(s)? If you selected more than one option, think about ..." #
########################################################################################################################

#-> answered by subsample of n = 230 (NA06 == -9 for n = 6)

sum(OSQ_daten$NA06== "1", na.rm=TRUE)#-> Expert user = 20
sum(OSQ_daten$NA06== "2", na.rm=TRUE)#-> Advanced user = 65 
sum(OSQ_daten$NA06== "3", na.rm=TRUE)#-> Practically experienced user = 100 
sum(OSQ_daten$NA06== "4", na.rm=TRUE)#-> Novice = 45


# Create subset of question
dat_NA06 <- subset(OSQ_daten, NA04 == 1, select = NA06)
table(OSQ_daten$NA06)
# No missings


# Specify variable as factor
dat_NA06$NA06 <- as.factor(dat_NA06$NA06)


# Specify levels according to response options
levels(dat_NA06$NA06) <- c("Expert user",
                           "Advanced user",
                           "Practically experienced user",
                           "Novice")


# Reorder levels according to hierarchy
table(dat_NA06$NA06)
dat_NA06$NA06 <- factor(dat_NA06$NA06, levels = c("Novice",
                                                  "Practically experienced user",
                                                  "Advanced user",
                                                  "Expert user"))


# Data frame with proportion, percentages, and number of responses
tab_NA06 <- as.data.frame(table(dat_NA06$NA06))

NA06_prop <- table(dat_NA06$NA06)/length(dat_NA06$NA06)

dat_NA06_perc <- as.data.frame(round(NA06_prop * 100, digits = 1)) # save as data frame

dat_NA06_perc$nrresp <- tab_NA06[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_NA06_perc$perc_labels <- paste(dat_NA06_perc$Freq, "%", sep = " ", collapse = NULL)


# For pie
dat_NA06_perc_pie <- dat_NA06_perc


# For pie chart: Compute position of labels
dat_NA06_perc_pie <- dat_NA06_perc_pie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_NA06_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


# Plot pie
plot_NA06_pie <- ggplot(dat_NA06_perc_pie, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  scale_fill_brewer("Knowledge level", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  ggtitle("What is your knowledge level with \r\nyour second favorite software?") + # \r\n
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


plot_NA06_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA06_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA06_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


##############################################################
#NA07 "I prefer to operate neuroimaging analysis software..." #
##############################################################

sum(OSQ_daten$NA07== "1")#-> ...via graphical user interface = 87
sum(OSQ_daten$NA07== "2")#-> ...via command/batch interface = 168
sum(OSQ_daten$NA07== "3")#-> I don't operate such software myself = 28

# Create subset of question
dat_NA07 <- subset(OSQ_daten, select = NA07)
table(OSQ_daten$NA07)
# No missings


# Specify variable as factor
dat_NA07$NA07 <- as.factor(dat_NA07$NA07)


# Specify levels according to response options
levels(dat_NA07$NA07) <- c("via graphical user interface.",
                           "via command/batch interface.",
                           "I don't operate such software myself.")



# Data frame with proportion, percentages, and number of responses
tab_NA07 <- as.data.frame(table(dat_NA07$NA07))

NA07_prop <- table(dat_NA07$NA07)/length(dat_NA07$NA07)

dat_NA07_perc <- as.data.frame(round(NA07_prop * 100, digits = 1)) # save as data frame

dat_NA07_perc$nrresp <- tab_NA07[,2] # add variable for number of responses ("nrresp")


# Add rounded %-Variable for labelling
dat_NA07_perc$perc_labels <- paste(dat_NA07_perc$Freq, "%", sep = " ", collapse = NULL)


# For pie
dat_NA07_perc_pie <- dat_NA07_perc

# Reorder levels according to frequency
dat_NA07_perc_pie$Var1 <- factor(dat_NA07_perc_pie$Var1, levels = dat_NA07_perc_pie$Var1[order(dat_NA07_perc_pie$Freq, decreasing = FALSE)])
levels(dat_NA07_perc_pie$Var1)


# For pie chart: Compute position of labels
dat_NA07_perc_pie <- dat_NA07_perc_pie %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_NA07_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)


# Plot pie
plot_NA07_pie <- ggplot(dat_NA07_perc_pie, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  #  scale_fill_brewer("Operating system", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual("Preferred way", values = c("#DFC27D", "#C7EAE5", "#35978F"), guide = guide_legend(reverse = TRUE)) +
  ggtitle("I prefer to operate neuroimaging \r\nanalysis software...") + # \r\n
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))


plot_NA07_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA07_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_NA07_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()


#---------------------------------------------------------------
  ########Stimulus Presentation Software ########


###########################################################################################################
#SP01 "What software do you use to present stimuli to study participants during functional neuroimaging?" #
###########################################################################################################

# Create subset for current question
SP01<- subset(OSQ_daten, select=c(143:148, 150))

# Create new data frame where TRUE/FALSE are changed to 0/1
Cols8 <-  which(sapply(SP01, is.logical))
setDT(SP01)
for(j in Cols8){
  set(SP01, i=NULL, j=j, value= as.numeric(SP01[[j]]))}

# Change format of data set, so that each row pertains to one answer option (rows are now columns, columns are rows)
SP01<- as.data.frame(t(SP01))
 
# Add new column that counts how many people responded with TRUE for each row                   
SP01$NumberofPeople<- rowSums(SP01, na.rm = TRUE)

# Add new column in which each row is assigned its answer option
Stimulipresent<- c("Presentation",
                   "E-prime", 
                   "Psychotoolbox",
                   "Open Sesame",
                   "PsychoPy",
                   "Other",
                   "I never present stimuli")
SP01$Stimulipresent<- Stimulipresent

# Add factor levels to previous new variable, so that we can order the levels manually.
SP01$Stimulipresent <- factor(SP01$Stimulipresent,                                
                             levels = c("Presentation",
                                        "E-prime", 
                                        "Psychotoolbox",
                                        "PsychoPy",
                                        "Open Sesame",
                                        "Other",
                                        "I never present stimuli"))
# Order factors in decreasing order
SP01$Stimulipresent <- factor(SP01$Stimulipresent, levels = SP01$Stimulipresent [order(SP01$Stimulipresent, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
SP01_prop <- (SP01$NumberofPeople)/230
SP01_perc <- as.data.frame(round(SP01_prop * 100, digits = 1)) # save as data frame
SP01$perc_labels <- paste(SP01_perc$`round(SP01_prop * 100, digits = 1)`, "%", sep = " ", collapse = NULL)



# Plot bar plot
SP01plot<-ggplot(SP01, aes(x= Stimulipresent , y=NumberofPeople, fill=Stimulipresent)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("What software do you use to present stimuli to study participants during functional imaging")+
  theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  geom_text(aes(y = 141, label = perc_labels), color = "black", size = 3)+
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

# Plot bar plot
SP01plot

# Save plot 
ggsave(file="SP01.svg", plot=SP01plot)

#################################################################
# SessionInfo()
#################################################################
# 
# R version 4.0.5 (2021-03-31)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] psych_2.1.3        tidyr_1.1.3        data.table_1.14.0  dplyr_1.0.5        ggthemes_4.2.4     likert_1.3.5       xtable_1.8-4       scales_1.1.1      
# [9] colorspace_2.0-1   RColorBrewer_1.1-2 ggplot2_3.3.3      pacman_0.5.1      
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.6        pillar_1.6.0      compiler_4.0.5    plyr_1.8.6        tools_4.0.5       digest_0.6.27     nlme_3.1-152      lattice_0.20-41  
# [9] lifecycle_1.0.0   tibble_3.1.1      gtable_0.3.0      pkgconfig_2.0.3   rlang_0.4.11      DBI_1.1.1         parallel_4.0.5    gridExtra_2.3    
# [17] withr_2.4.2       stringr_1.4.0     systemfonts_1.0.1 generics_0.1.0    vctrs_0.3.8       grid_4.0.5        tidyselect_1.1.1  svglite_2.0.0    
# [25] glue_1.4.2        R6_2.5.0          fansi_0.4.2       farver_2.1.0      purrr_0.3.4       reshape2_1.4.4    magrittr_2.0.1    ellipsis_0.3.2   
# [33] assertthat_0.2.1  mnormt_2.0.2      labeling_0.4.2    utf8_1.2.1        stringi_1.5.3     munsell_0.5.0     tmvnsim_1.0-2     crayon_1.4.1  
# 
