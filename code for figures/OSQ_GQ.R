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
  ######## General Questions########

#######################################
#GQ2 "What modalities do you collect?" #
#######################################

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
GQ02_perc <- as.data.frame(round(GQ02_prop * 100, digits = 1)) # save as data frame
GQ02$perc_labels <- paste(GQ02_perc$`round(GQ02_prop * 100, digits = 1)`, "%", sep = " ", collapse = NULL)


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
