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

#---------------------------------------------------------------
  ######## Personal Data ########


########################### 
#PD01 "What is your age?" #
###########################

hist(OSQ_daten$PD01_01)                                                          #unlikely value of 99 years of age
OSQ_daten$PD01_01<-replace(OSQ_daten$PD01_01, 261,'43.68')                       #replace value with mean age calculated without outlier 
OSQ_daten$PD01_01<-as.numeric(OSQ_daten$PD01_01)
describe(OSQ_daten$PD01_01)#-> mean=43,69 years, SD= 9,16 years


#################
#PD02 "Gender:" #
#################

table(OSQ_daten$PD02)
#1->Male=186
#2->Female=96
#3-> Divers=1 

################################
#PD04 "I have been trained as" #
################################

table(OSQ_daten$PD04)
#1-> Biologist = 27
#2-> Enigneer = 21
#3-> Medical Doctor = 42
#4-> Physicist = 23
#5-> Clinical Psychologist = 13
#6-> Psychologist = 113
#7-> Other = 44
# No missings

# Create subset of question
dat_PD04 <- subset(OSQ_daten, select = PD04)


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

dat_PD04_perc <- as.data.frame(round(PD04_prop * 100, digits = 1)) # save as data frame

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

####################################################################
#PD06 "What is your field of study? Please select the best match." #
####################################################################

table(OSQ_daten$PD06)
#1-> Psychiatry = 39
#3-> Clinical Psychology= 13
#4-> Neurology = 19
#5-> Biology = 1
#6-> Physics = 8
#7-> Medicine, other discipline = 19
#8-> Psychology, other discipline = 14
#9-> Cognitive Neuroscience = 170
# No missings

# Create subset of question
dat_PD06 <- subset(OSQ_daten, select = PD06)



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
dat_PD06_perc <- as.data.frame(round(PD06_prop * 100, digits = 1)) # save as data frame
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


########################################
#PD07 "What is your current position?" #
########################################

table(OSQ_daten$PD07)
#1-> Research Assistant = 2
#2-> Phd Student= 6
#7-> Lab Manager = 1
#8-> Post-Doc = 5
#9-> Assistant Professor/Post-Doc = 86
#1-> Associate Professor/Reader/Lecturer = 68
#5-> Full professor = 71
#6-> Other, specify = 20
# No missings

# Create subset of question
dat_PD07 <- subset(OSQ_daten, select = PD07)

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
dat_PD07_perc <- as.data.frame(round(PD07_prop * 100, digits = 1)) # save as data frame
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


##########################################
#PD08 "What is your primary affiliation?" #
##########################################

table(OSQ_daten$PD08)
#1-> With university hospital/medical faculty = 10
#2-> With university/psychology faculty or similar= 95
#3-> With university/other faculty = 29
#6-> With governmental institution  = 21
#4-> With industry= 5
#5-> Other = 10
# No missings

# Create subset of question
dat_PD08 <- subset(OSQ_daten, select = PD08)

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
dat_PD08_perc <- as.data.frame(round(PD08_prop * 100, digits = 1)) # save as data frame
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


#####################################
#PD09 "My current research involves" #
#####################################

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

Freq <- round((colSums(dat_PD09_010203)/283)*100, digits = 1) # proportion of people

Var1 <- c("healthy participant data.",
          "data from individuals affected with a mental disorder.",
          "data from individuals affected with a physical disorder.")

dat_PD09_010203_perc <- cbind.data.frame(Var1, Freq, nrresp)


# Add rounded %-Variable for labeling
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

####################################
#PD10 "My country of residence is"  #
####################################

table(OSQ_daten$PD10)
#1-> in the EU = 161
#2-> outside of the EU= 122
# No missings

# Create subset of question
dat_PD10 <- subset(OSQ_daten, select = PD10)

# Specify variable as factor
dat_PD10$PD10 <- as.factor(dat_PD10$PD10)

# Specify levels according to response options
levels(dat_PD10$PD10) <- c("in the EU", "outside of the EU")

# Data frame with proportion, percentages, and number of responses
tab_PD10 <- as.data.frame(table(dat_PD10$PD10))
PD10_prop <- table(dat_PD10$PD10)/length(dat_PD10$PD10)
dat_PD10_perc <- as.data.frame(round(PD10_prop * 100, digits = 1)) # save as data frame
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


#########################################################################
#PD12 "If you want to, you may indicate your country of residence here:" #
#########################################################################


#->open question

#---------------------------------------------------------------
  ######## Feedback ########


##FB01 "If you want to, you can submit your comments to the authors via this field."



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