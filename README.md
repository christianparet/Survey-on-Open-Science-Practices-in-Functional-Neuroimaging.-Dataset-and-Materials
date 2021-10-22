# Welcome

Thank you for visiting this project page. Great, that you are interested to learn more about this research.
This github project is authored by Christian Paret, Nike Unverhau, Madita Stirner and Maurizio Sicorello, Central Institute of Mental Health in Mannheim, Germany. It has received valuable support from Franklin Feingold and Russ Poldrack, Center for Open and Reproducible Science in Stanford USA.

# Project description

Replicability and reproducibility of scientific findings is paramount for sustainable progress in neuroscience. Preregistration of the hypotheses and methods of an empirical study before analysis, the sharing of primary research data,  and compliance with data standards such as the Brain Imaging Data Structure (BIDS),  are effective practices to secure progress and to substantiate quality of research. We investigated the current level of adoption of open science practices in neuroimaging and the difficulties that prevent researchers from using them.
_____________________________________________________________________________________________________________________________________________

# Content of this repository 

This directory contains:

- This README file with an overview of the project and the files in this repository
- Four folders with R scripts
	1. Code for figures*
	2. Code for follow up analyses
	3. Code to analyze demographic data 
- The datafile OSQ_data.Rdata
- A pdf of the questionnaire (OSQ.pdf)
_____________________________________________________________________________________________________________________________________________

# Purpose and highlights of the project

## Aims
To investigate the current level of ADOPTION OF OPEN SCIENCE PRACTICES IN HUMAN NEUROIMAGING RESEARCH. Improve understanding of difficulties that prevent researchers from implementing these practices. The focus was on three aspects: preregistration, data sharing, and data structure.

## Materials, Methods
[283 persons](./plots/Flowchart.png), aged 44 y. on average and with 17 y. average research experience, completed the questionnaire. 40% of the [international sample](./plots/CountryofResidence.png) was [trained in psychology](./plots/PD/PD04.png) and one half held a [full or associate professorship or comparable position](./plots/PD/PD07.png). The [primary affiliation of most participants was with a university (77%)](./plots/PD/PD08.png) and most participants indicated [cognitive neuroscience their field of study (60%)](./plots/PD/PD06.png). 

## Highlights
>> Although half of the participants were experienced with PREREGISTRATION, [the willingness to preregister studies in the future was modest.](./plots/PR/PR03.png)
>> The majority of participants (66%) had experience with the SHARING OF PRIMARY RESEARCH DATA. [About half of the participants were positive about sharing data of the next paper online.](./plots/DS/DS09)
>> [Most of the participants were interested in implementing a standardized data structure such as BIDS (Brain Imaging Data Structure) in their labs.](./plots/BI/BI07.png)
>> Based on demographic variables, we compared participants on seven subscales, which had been generated through [factor analysis](./plots/Factoranalysis_Table.pdf).
>>   • Experienced researchers at lower career level had higher “fear of being transparent”.
>>   • Researchers with residence in the EU had a higher “need for data governance”.
>>   • Researchers at medical faculties as compared to other university faculties reported a higher “need for data governance” and a more “unsupportive environment”.
 _____________________________________________________________________________________________________________________________________________

# How to reproduce the results
1. Download repository by clicking "Download ZIP" under "Code". 
2. Unzip folder on your computer. 
3. Open R / RStudio. Load the file "OSQ_data.RData" and add it to the global environment.
4. Open the R Script that you want to use to produce plots or analyze the data.

## Software information
The analysis was programmed with R version 4.0.5. 
Library dependencies...

## Further information
- Code for producing plots is named the following way: "OSQ_XX", where "XX" is the abbreviation of the respective question category*
- We feel commited to publish as much data as necessary to reproduce our findings, and for future research to re-use it. To minimize the risk for re-identification of study participants, we removed gender information from the data.
	
## *Abbreviations of question categories
GQ = General Questions
BI = BIDS
PR = Preregistration
DS = Data Sharing
NA = Neuroimaging Data Analysis Software
SP = Stimulus Presentation Software
PD = Participant Demographics/ Sociodemographic Information
_____________________________________________________________________________________________________________________________________________
	
# Whom to contact for support
Issues related to the code can be submitted via Github's Issues function. Please address your project-related questions to christian.paret[at]zi-mannheim.de. We are all busy people, but we will do our best to respond quickly. 

# Further reading
Paret C., Unverhau N., Feingold F., Poldrack R.A., Stirner M., Schmahl C., Sicorello M. (2021). Survey on Open Science Practices in Functional Neuroimaging. BioRxiv. 
Watch out for original article coming out soon.
