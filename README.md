# tradeoffs-working-commuting
## Research project: Understanding tradeoffs between working and commuting
This repository contains data and scripts associated with the "Understanding tradeoffs between working and commuting" research project, conducted by [Niranjan Poudel](https://www.linkedin.com/in/niranjan-poudel-65082119b/) and [Patrick Singleton](https://engineering.usu.edu/cee/people/faculty/singleton-patrick) of the [Singleton Transportation Lab](https://engineering.usu.edu/cee/research/labs/patrick-singleton/index) at Utah State University. 

* **Objectives**
  1. Quantify willingness-to-pay for travel time changes and work time changes. 
  2. Measure and identify factors explaining preference heterogeneity in these time values.
  3. Examine the marginal rate of substitution between two monetary attributes (travel cost and income) within the same choice context. 
* **Methods**
  - Survey data collection, using an online questionnaire, including a stated choice experiment about tradeoffs between daily commute travel time, travel cost, work time, and income. 
  - Discrete choice modeling of choices about tradeoffs between commuting, working, and money. 

[![DOI](https://zenodo.org/badge/688134386.svg)](https://zenodo.org/badge/latestdoi/688134386)

## Description of folders and files
* Many of these scripts were written in R. To use, [download R](https://cloud.r-project.org/) and then [download RStudio](https://posit.co/products/open-source/rstudio/).
* **tradeoffs-working-commuting.Rproj**: This [RStudio project](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects) file allows R users to use relative file paths from this root folder, if R scripts are opened after opening this R project file.

### Data
These folders contain data collected as part of this research project.
* **0_Survey**: Details about the survey questions, built/deployed via Qualtrics. 
* **1_Raw**: The raw data files (omitted for privacy reasons). 
* **2_Anonymous**: Anonymized data. 
* **3_Processed**: Processed data, including quality control. 
* **4_Final**: Final processed datasets, in long and wide formats. 
* **Descriptives**: Files and tables reporting descriptive statistics. 
* **Scripts**: R scripts used to anonymize, clean, and process data and to calculate descriptive statistics. 

### Analysis
These folders contain scripts and outputs associated with analyses of the research project data.
* **A_MNL**: Script and outputs from Model A multinomial logit. 
* **B_MMNL**: Script and outputs from Model B panel mixed multinomial logit. 
* **C_MMNL_het**: Script and outputs (including step-wise estimation process) from Model C panel mixed multinomial logit with systematic preference heterogeneity. 
* **Results**: Script to get results from previous model folders; figures generated by the script; spreadsheet summarizing results tables. 
