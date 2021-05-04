*Project: Open Science Practices in Neuroimaging*

*Description:* Functional Magnetic Resonance Imaging (fMRI) is widely used in the generation and testing of biological models of mental disorder, but reluctance in the field to adopt open science practices may threaten scientific progress. For reproducible science, it is pivotal to differentiate confirmatory and exploratory hypotheses via preregistration of studies, and to provide data for re-use by peer researchers. Addressing the neuroimaging community directly with an online survey, this study aims to give an overview of how much researchers commit to current open science practices.
This github project provides the code for data analysis.

*Authors:* Nike Unverhau, Madita Stirner, Christian Paret

*Contact:* christian.paret@zi-mannheim.de

______________________________________________________________________________________________________________________________________________

*How to*: 

Data import:
	1. Open R-Script "import_open-science-practices_2021_02_26_12-20.r" and execute the whole script.
	2. A dialog window opens. Choose the data frame to import by clicking on file "rdata_open-science-practices_2021_02_26_12-20.csv".
	3. The data frame called "OSQ_daten" should be in R's global environment.
	4. Open the R Script that you want to use to produce plots. For names of R scripts, see below.

Figures and data analysis:
	Scripts for analysing data and producing plots are named the following way:
	- "OSQ_AB", where "AB" is the abbreviation of the respective question category
	- Abbreviations of question categories:
		--> GQ = General Questions
		--> BI = BIDS
		--> PR = Preregistration
		--> DS = Data Sharing
		--> NA = Neuroimaging Data Analysis Software
		--> SP = Stimulus Presentation Software
		--> PD = Participant Demographics/ Sociodemographic Information
	- Abbreviations of items:
		--> general form: ABxy
			--> AB: letter abbreviation which identifies the question category
			--> xy: number; "xy"th item of category "AB"
			(relates to development phase of questionnaire, f. e. GQ01 does not appear, since it was not included in final version of the online questionnaire)

______________________________________________________________________________________________________________________________________________