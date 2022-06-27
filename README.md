[![DOI](https://zenodo.org/badge/364143830.svg)](https://zenodo.org/badge/latestdoi/364143830)
# Welcome
Thank you for visiting this project page. Great, that you are interested to learn more about this research.
This github repository is authored by Christian Paret, Nike Unverhau and Maurizio Sicorello, Central Institute of Mental Health in Mannheim, Germany. It has received valuable support from Franklin Feingold and Russ Poldrack, Center for Open and Reproducible Science in Stanford USA.
# Project description
Preregistration of the hypotheses and methods of an empirical study before analysis, the sharing of primary research data,  and compliance with data standards such as the Brain Imaging Data Structure (BIDS),  are considered effective practices to secure progress and to substantiate quality of research. We investigated the current level of adoption of open science practices in neuroimaging and the difficulties that prevent researchers from using them. A PubMed search with the search terms ("fMRI" OR "functional magnetic resonance imaging" OR "functional Magnetic Resonance Imaging") was done to collect email addresses from corresponding authors of scientific articles published between 2010/01/01 and 2020/08/28. An email was sent to 14,690 addresses on 2020/01/12 with an invitation to participate, including a personalized link to the survey. If the recipients did not click the link or did not complete the survey after 14 days, they received a single reminder email. The questionnaire was composed of five building blocks. The Blocks 1-3 focused on three areas of open science practices: data structure, preregistration and data sharing. The fourth block asked about technical expertise with software and the fifth part assessed sociodemographic data. 
# Content of this repository 
This directory contains:
- This README file with an overview of the project and the files in this repository
- Four folders with R scripts
  1. code for figures (i.e., code to generate the graphical representations of survey results)*
  2. code for follow up analyses (i.e. significance testing and Bayes factor analysis of post hoc group comparisons, factor analysis, cluster analysis)
  3. code to analyze demographic data
- A folder "plots" with all graphical outputs that were generated to present the data in publications and presentations
- A folder with "survey materials"
- The datafile OSQ_data.Rdata; load this data into your R environment to reproduce the analyses
- The datafile OSQ_data.csv; for those who prefer csv files, has the same data as above
- The file variables_OSQ_data.csv, which is the data dictionary explaining the variable codes in the data file
- The file values_OSQ_data.csv, which explains response options, their meaning and coding to each of the survey items
# Purpose and highlights of the project
## Aims
To investigate the current level of ADOPTION OF OPEN SCIENCE PRACTICES IN HUMAN NEUROIMAGING RESEARCH. Improve understanding of difficulties that prevent researchers from implementing these practices. The focus was on three aspects: preregistration, data sharing, and data structure.
## Materials, Methods
[283 persons](./plots/Flowchart.png), aged 44 y. on average and with 17 y. average research experience, completed the questionnaire. 40% of the [international sample](./plots/CountryofResidence.png) was [trained in psychology](./plots/PD/PD04.png) and one half held a [full or associate professorship or comparable position](./plots/PD/PD07.png). The [primary affiliation of most participants was with a university (77%)](./plots/PD/PD08.png) and most participants indicated [cognitive neuroscience their field of study (60%)](./plots/PD/PD06.png). 
## Highlights
Although half of the participants were experienced with PREREGISTRATION, [the willingness to preregister studies in the future was modest.](./plots/PR/PR03.png)<br />
The majority of participants (66%) had experience with the SHARING OF PRIMARY RESEARCH DATA. [About half of the participants were positive about sharing data of the next paper online.](./plots/DS/DS09.png)<br />
[Most of the participants were interested in implementing a standardized data structure such as BIDS (Brain Imaging Data Structure) in their labs.](./plots/BI/BI07.png)<br />
Based on demographic variables, we compared participants on seven subscales, which had been generated through [factor analysis](./plots/Factoranalysis_Table.pdf):
- Experienced researchers at lower career level had higher “fear of being transparent”.
- Researchers with residence in the EU had a higher “need for data governance”.
- Researchers at medical faculties as compared to other university faculties reported a higher “need for data governance” and a more “unsupportive supervisor” with regards to reproducibility practices.
# How to reproduce the results
1. Download repository by clicking "Download ZIP" under "Code". 
2. Unzip folder on your computer. 
3. Open R / RStudio. Load the file "OSQ_data.RData" and add it to the global environment.
4. Open the R Script that you want to use to produce plots or analyze the data.
## Software information
The analysis was programmed with R version 4.0.5. The code has been produced and tested in Windows 10 OS.
## Further information
- Code for producing plots is named the following way: "OSQ_XX", where "XX" is the abbreviation of the respective question category*
- We feel commited to publish as much data as necessary to reproduce our findings, and for future research to re-use it. To minimize the risk for re-identification of study participants, we removed gender information from the data.
### *Abbreviations of question categories
GQ = General Questions; BI = BIDS; PR = Preregistration; DS = Data Sharing; NA = Neuroimaging Data Analysis Software; SP = Stimulus Presentation Software; PD = Participant Demographics/ Sociodemographic Information
# Whom to contact for support
Issues related to the code can be submitted via Github's Issues function. Please address your project-related questions to christian.paret[at]zi-mannheim.de. We are all busy people, but we will do our best to respond quickly. 
# Cite this research
Please include this reference if you want to cite this dataset:

Paret, C., Unverhau, N., Sicorello, M., 2022. Survey on Open Science-Practices in Functional Neuroimaging. Dataset and Materials (Version v7). https://github.com/christianparet/Survey-on-Open-Science-Practices-in-Functional-Neuroimaging.-Dataset-and-Materials. doi:10.5281/zenodo.6400829.

Make sure that you are working with the latest release of this repository.

Please also cite the original article when you reference this repository in your work:

Paret, C., Unverhau, N., Feingold, F., Poldrack, R.A., Stirner, M., Schmahl, C., Sicorello, M., 2022. Survey on Open Science Practices in Functional Neuroimaging. NeuroImage 257, 119306. https://doi.org/10.1016/j.neuroimage.2022.119306