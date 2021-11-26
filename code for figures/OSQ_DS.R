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
# # Loading packages (installs if necessary)
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



######## Data Sharing ########


########################################################################################
#DS04 "Have you shared raw neuroimaging data with researchers outside your department?" #
########################################################################################

sum(OSQ_daten$DS04=="1") #->Yes = 187
sum(OSQ_daten$DS04== "2")#->No =  96

# Create subset of question
dat_DS04_NA <- subset(OSQ_daten, select = DS04)
table(OSQ_daten$DS04)
dat_DS04 <- na.omit(dat_DS04_NA)

# Specify variable as factor
dat_DS04$DS04 <- as.factor(dat_DS04$DS04)

# Specify levels according to response options
levels(dat_DS04$DS04) <- c("Yes",
                           "No")

levels(dat_DS04$DS04)
#[1] "Yes" "No" 

# Data frame with proportion, percentages, and number of responses
tab_DS04 <- as.data.frame(table(dat_DS04$DS04))
DS04_prop <- table(dat_DS04$DS04)/length(dat_DS04$DS04)
dat_DS04_perc <- as.data.frame(round(DS04_prop * 100, digits = 1)) # save as data frame
dat_DS04_perc$nrresp <- tab_DS04[,2] # add variable for number of responses ("nrresp")

# Add rounded %-Variable for labelling
dat_DS04_perc$perc_labels <- paste(dat_DS04_perc$Freq, "%", sep = " ", collapse = NULL)

# For pie chart: Compute position of labels
dat_DS04_perc <- dat_DS04_perc %>% 
  arrange(desc(Var1)) %>%
  mutate(prop = Freq / sum(dat_DS04_perc$Freq) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)

# Plot pie
plot_DS04_pie <- ggplot(dat_DS04_perc, aes(x = "", y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(y = ypos, label = perc_labels), color = "black", size = 2.5) +
  #scale_fill_brewer("", palette="BrBG", guide = guide_legend(reverse = TRUE)) +
  scale_fill_manual("", values = c("#BF812D", "#35978F")) +
  ggtitle("Have you shared raw neuroimaging data with \r\nresearchers outside your department?") +
  theme(plot.title = element_text(size = 10, hjust = 0.5),
        aspect.ratio = 1,
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8))

plot_DS04_pie

# Save plot
dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_DS04_pie.svg",width = dev.size()[1],height = dev.size()[2]);dev.off()

dev.new(width = 4.72, height = 4.72, unit="in", noRStudioGD = T);last_plot()
ggsave("plot_DS04_pie.tiff",width = dev.size()[1],height = dev.size()[2]);dev.off()



####################################################################################################################
#DS09 "How likely are you to share primary research data for your next neuroimaging paper in an online repository?" #
####################################################################################################################

# Create subset for current question
DS09 <- subset(OSQ_daten, select = c(98))

# Rename Variable into Question
DS09<- DS09 %>%
  rename("How likely are you to share primary research data for your next neuroimaging paper in an online repository?" = DS09)

# Add vector with answer options
likelihoodchoices  = c("Extremely Unlikely", 
                       "Unlikely", 
                       "Neither likely nor Unlikely", 
                       "Likely",
                       "Extremely Likely")

# Replace levels 1:5 with answer options
for(i in 1:ncol(DS09)) {
  DS09[,i] = factor(DS09[,i], levels=1:5, labels=likelihoodchoices, ordered=TRUE)}

# Plot likert plot
DS09plot<- plot(likert(DS09),wrap=20, legend = "", legend.position = "bottom")

# Print plot
DS09plot

# Save plot
ggsave(file="DS09.svg", plot=DS09plot)

##############################################################################
#DS10 "This question relates to legal constraints connected to data sharing." #
##############################################################################


sum(OSQ_daten$DS10== "1", na.rm = TRUE)#-> 73
sum(OSQ_daten$DS10== "2", na.rm = TRUE)#-> 51
sum(OSQ_daten$DS10== "3", na.rm = TRUE)#-> 58
sum(OSQ_daten$DS10== "4", na.rm = TRUE)#-> 76
sum(OSQ_daten$DS10== "5", na.rm = TRUE)#-> 25
sum(OSQ_daten$DS10== "6", na.rm = TRUE)#-> 0

# Create subset for current question
DS10<- subset(OSQ_daten, select = c(99))

# Rename Variable into Question
DS10 <- DS10 %>%
  rename("I am not allowed to share my primary neuroimaging research data" = DS10)

# Add vector with answer options
agreementchoices  = c("\"Strongly Disagree\"", 
                      "\"Disagree\"", 
                      "\"Somewhat disagree\"", 
                      "\"Neither agree nor disagree\"",
                      "\"Somewhat agree\"",
                      "\"Agree\"",
                      "\"Strongly agree\"")

# Replace levels 1:5 with answer options
for(i in 1:ncol(DS10)) {
  DS10[,i] = factor(DS10[,i], levels=1:7, labels=agreementchoices, ordered=TRUE)}

# Plot likert plot
DS10plot<- plot(likert(DS10),wrap=20, legend = "", legend.position = "bottom")

# Print plot
DS10plot

# Save plot
ggsave(file="DS10.svg", plot=DS10plot)


###################################################################################
#DS11 "Why are you not allowed to share your primary neuroimaging research data?" #
###################################################################################

#-> answered by subsample of n = 224, thereof 11 did not choose any item, plot is based on n = # hier noch bitte einfügen

# Create subset for current question
DS11<- subset(OSQ_daten, select = c(101:106))

# Create new data frame where TRUE/FALSE are changed to 0/1
Cols5<-  which(sapply(DS11, is.logical))
setDT(DS11)
for(j in Cols5){
  set(DS11, i=NULL, j=j, value= as.numeric(DS11[[j]]))}

# Change format of data set, so that each row pertains to one answer option (rows are now columns, columns are rows)
DS11<- as.data.frame(t(DS11))

# Adds new column that counts how many people responded with TRUE for each row
DS11$NumberofPeople<- rowSums(DS11, na.rm = TRUE)

# Adds new column in which each row is assigned its answer option
Nosharingallowed<- c("My Institutional Review Board does not allow me to share my data",
                     "The consent form states that data will not be shared", 
                     "Anonymity cannot be guaranteed if the data is shared",
                     "Stakeholder interests prohibit data from being shared", 
                     "My funder/advisor/boss does not allow me to share my data",
                     "Other")

# Adds factor levels to previous new variable, so that we can order the levels manually.
DS11$Nosharingallowed <- as.factor(Nosharingallowed)
DS11$Nosharingallowed<- factor(DS11$Nosharingallowed,
                               levels= c("Anonymity cannot be guaranteed if the data is shared",
                                         "The consent form states that data will not be shared",
                                         "My Institutional Review Board does not allow me to share my data",
                                         "Stakeholder interests prohibit data from being shared",
                                         "My funder/advisor/boss does not allow me to share my data",
                                         "Other"))

# Order factors in decreasing order
DS11$Nosharingallowed <- factor(DS11$Nosharingallowed, levels = DS11$Nosharingallowed [order(DS11$Nosharingallowed, decreasing = TRUE)])

# Calculate proportion and add percent labels as new variable in dataframe
DS11_prop <- (DS11$NumberofPeople)/210
DS11_perc <- as.data.frame(round(DS11_prop*100, digits = 1)) # save as data frame
DS11$perc_labels <- paste(DS11_perc$`round(DS11_prop * 100, digits = 1)`, "%", sep = " ", collapse = NULL)

# Plot bar plot
DS11plot<-ggplot(DS11, aes(x= Nosharingallowed, y=NumberofPeople, fill=Nosharingallowed)) + 
  geom_bar(stat = "identity")+
  coord_flip()+
  ggtitle("Why are you not allowed to share your primary neuroimaging research data?")+
    theme(plot.title = element_text(size=10, hjust = 0.5),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8))+
  geom_text(aes(y = 105, label = perc_labels), color = "black", size = 3)+
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
DS11plot

# Save plot
ggsave(file="DS11.svg", plot=DS11plot)

#####################################################################################
#DS13 "These statements relate to possible barriers for and fears of data sharing." #
#####################################################################################
 
# Create subset for current question 
DS13 <- subset(OSQ_daten, select = c(108:119))

# Add vector with answer options
agreementchoices  = c("\"Strongly Disagree\"", 
                      "\"Disagree\"", 
                      "\"Somewhat disagree\"", 
                      "\"Neither agree nor disagree\"",
                      "\"Somewhat agree\"",
                      "\"Agree\"",
                      "\"Strongly agree\"")

# Replace levels 1:7 with answer options
for(i in 1:ncol(DS13)) {
  DS13[,i] = factor(DS13[,i], levels=1:7, labels=agreementchoices, ordered=TRUE)
}

# Rename Variables into Answer Options
plotDS13 <- rename(DS13, c(
  "\"Preparing data to make it suitable for online sharing is too time consuming for me\"" =  DS13_01, 
  "\"I lack funding to make data suitable for online sharing\""= DS13_02, 
  "\"My data set is too big to share\"" = DS13_03,
  "\"My data set is too complex to share\"" = DS13_04,
  "\"I have never learned to share my research data online\""= DS13_05,
  "\"I have never thought about sharing my research data online\"" = DS13_06,
  "\"I know too little about suitable repositories\"" = DS13_07,
  "\"My boss does not support online data sharing\"" = DS13_08, 
  "\"I am afraid that I will not get proper recognition for sharing my data\"" = DS13_09,
  "\"I am afraid that other researchers will perform alternative analyses on my data 
             and argue that my conclusions are invalid\"" = DS13_10,
  "\"I am afraid that other researchers will discover errors in my data\"" = DS13_11,
  "\"I am afraid of being scooped: that other researchers may publish results from my data set before I can\""= DS13_12))

# Plot likert plot
plotDS13 <- plot(likert(DS13), wrap=50,legend = "", legend.position = "bottom" ) +
  ggtitle("Possible barriers for and fears of data sharing") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = wrap_format(20))

# Print plot
plotDS13 

# Save plot
ggsave(file="DS13.svg", plot=plotDS13)


##############################################################################################################################
#DS02 "Think of a typical neuroimaging experiment of yours: How much do you agree on statements regarding possible options   #
#of sharing primary research data?\"                                                                                         #
##############################################################################################################################

# Create subset for current question 
DS02 <- subset(OSQ_daten, select = c(120:125))

# Add vector with answer options
agreementchoices  = c("\"Strongly Disagree\"", 
                      "\"Disagree\"", 
                      "\"Somewhat disagree\"", 
                      "\"Neither agree nor disagree\"",
                      "\"Somewhat agree\"",
                      "\"Agree\"",
                      "\"Strongly agree\"")

# Replace levels 1:7 with answer options
for(i in 1:ncol(DS02)) {
  DS02[,i] = factor(DS02[,i], levels=1:7, labels=agreementchoices, ordered=TRUE)}


# Rename Variables into Answer Options
DS02 <- rename(DS02, c(
  "\"I prefer to share via an online repository with unrestricted open access\"" =  DS02_01, 
  "\"I prefer to share via a managed online repository with restricted access\""= DS02_02, 
  "\"I prefer to share upon personal request, e.g. via direct data transfer from my instiution's server to the server of the recipient\"" = DS02_03,
  "\"I prefer to share under a data sharing agreeement to be signed by the recipient\"" = DS02_04,
  "\"Researchers with reasonable interest can work with my raw data, but this work needs to be done on the server of my home institution\""= DS02_05,
  "\"I prefer not to give other researchers access to my raw data\"" = DS02_06))

# Plot likert plot
plotDS02 <- plot(likert(DS02), wrap=50,legend = "", legend.position = "bottom" ) +
  ggtitle("\"Think of a typical neuroimaging experiment of yours: 
          How much do you agree on statements regarding possible options of sharing primary research data?\"") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels = wrap_format(20))

# Print plot  
plotDS02 

# Save plot
ggsave(file="DS02.svg", plot=plotDS02)


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