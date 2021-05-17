# Project: Open Science Practices in Neuroimaging

## General project information
*Description:* Functional Magnetic Resonance Imaging (fMRI) is widely used in the generation and testing of biological models of mental disorder, but reluctance in the field to adopt open science practices may threaten scientific progress. For reproducible science, it is pivotal to differentiate confirmatory and exploratory hypotheses via preregistration of studies, and to provide data for re-use by peer researchers. Addressing the neuroimaging community directly with an online survey, this study aims to give an overview of how much researchers commit to current open science practices.
This github project provides the code for data analysis.

*Authors:* Nike Unverhau, Madita Stirner, Christian Paret

*Contact:* christian.paret@zi-mannheim.de

______________________________________________________________________________________________________________________________________________

## Content
 
This directory contains:

	- four folders with R scripts
		1. code for figures (one for each question category)*
		2. code for follow up analyses (Bayesian Factor, Factoranalysis and Significance Testing) 
		3. code to analyze demographic data (Country of Residence, sample descriptives) 
		4. code to import data 
	- this README file with instructions
	- the datafile "rdata_open-science-practices_2021-01-25_18-48" comprising the surveydata gathered via socisurvey
	- a pdf of the administered questionnaire 

## How to start

	1. Download repository by clicking "Download ZIP" under "Code". 
	2. Open ZIP folder on your Computer. 
	3. Open "Code to import data".
	4. Open R-Script "import_open-science-practices_2021_02_26_12-20.r" and execute the whole script.
	5. A dialog window opens. Choose the data frame to import by clicking on file "rdata_open-science-practices_2021_02_26_12-20.csv".
	6. The data frame called "OSQ_daten" should be in R's global environment.
	7. Open the R Script that you want to use to produce plots or analyze the data.


## Figures and data analysis:
	Scripts for producing plots are named the following way:
	- "OSQ_AB", where "AB" is the abbreviation of the respective question category
	- Abbreviations of question categories:
		--> GQ = General Questions
		--> BI = BIDS
		--> PR = Preregistration
		--> DS = Data Sharing
		--> NA = Neuroimaging Data Analysis Software
		--> SP = Stimulus Presentation Software
		--> PD = Participant Demographics/ Sociodemographic Information
	
