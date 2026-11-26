# Trends in recording of cardiovascular risk assessment among adults without prior cardiovascular disease: a population-based cohort study

## Table of Contents
- [About](#about)
- [Installation](#installation)
- [Usage](#usage)
- [Project Structure](#project-structure)
- [Dependencies](#dependencies)
- [Authors](#authors)
- [Acknowledgements](#acknowledgements)
- [Citation](#citation)
- [Licence](#licence)

## About
- In this population-based cohort study, we examined temporal trends in recorded ASSIGN scores among individuals without established cardiovascular disease, and assessed variation by sociodemographic groups. Additionally, we explored the provision of lipid-lowering therapy among individuals identified as high-risk.
- Related publications: https://doi.org/10.1186/s12872-026-05572-8

## Installation
Step by step instructions to get your code running:
1. Clone the repository
git clone https://github.com/iPREVENT-CVD/iPREVENT-ASSIGN.git

2. Install R
Download and install R from https://www.r-project.org/
Recommended version: R 4.0+

3. Install RStudio (Recommended)
Download from https://www.rstudio.com/

4. Install required packages. At the start of the script required packages are stated. 

## Usage
The R code provided include the following analysis 1) the annual prevalence of the primary outcome by financial year and by subgroups, 2) the adjusted logistic regression models with a generalized estimating equation approach where we evaluated subgroup comparisons and time trends, 3) provision of lipid-lowering therapy in high-risk individuals, and 4) the ASSIGN score recording among individuals who had an encounter with primary care services compared to those who did not in the relevant financial year. Additionally, six functions are provided that were used in the R scripts to run the models. For each analysis, R scripts are available to create the figures as presented in the manuscript.


## Project Structure
```
├── function_df_fincancial_years/                         # R scripts
├── function_financial_year_plot/                         # R scripts
├── function_fit_gee_model/                               # R scripts
├── function_forest_plot/                                 # R scripts
├── function_gp_interaction_plot/                         # R scripts
├── helper_functions/                                     # R scripts
├── 001_time_trends_plots/                                # R scripts
├── 002_gee_models/                                       # R scripts
├── 003_lipidlowering_therapy_plots/                      # R scripts
├── 004_GP_interaction_plot/                              # R scripts

└── README.md    # This file
```

## Dependencies
- R 4.0+
- RStudio (recommended)
- Packages required:
   - tidyverse
   - geepack     

## Authors R code
- Marie de Bakker - University of Edinburgh
- Iona Lyell - University of Edinburgh
- Dorien Kimenai - University of Edinburgh
- A full list of the authors of the manuscript can be found here: placeholder

## Acknowledgements
This work was funded by the British Heart Foundation Intermediate Basic Science Research Fellowship (FS/IBSRF/23/25161) held by DMK, and MdB and DMK are supported by a Research Excellence Award from the British Heart Foundation (RE/24/130012). IL is supported by a Clinical Research Training Fellowship from the British Heart Foundation (FS/CRTF/25/24788). NL is supported by an Health Data Research UK personal fellowship. This work is affiliated to Health Data Research UK (Big Data for Complex Disease- HDR-23012), which is funded by the Medical Research Council (UKRI), the National Institute for Health Research, the British Heart Foundation, Cancer Research UK, the Economic and Social Research Council (UKRI), the Engineering and Physical Sciences Research Council (UKRI), Health and Care Research Wales, Chief Scientist Office of the Scottish Government Health and Social Care Directorates, and Health and Social Care Research and Development Division (Public Health Agency, Northern Ireland). PW is supported by grant funding from the Chief Scientist Office, Scottish Government (HIPS/24/31). This work was supported by DataLoch (dataloch.org), which is core-funded by the Data-Driven Innovation programme within the Edinburgh and South East Scotland City Region Deal (ddi.ac.uk). 

## Citation
If you use this code in your research please cite:
Placeholder

## Licence
This project is licensed under the [MIT Licence](LICENSE)

## Contact
For questions or issues please contact:
- Dorien Kimenai - dorien.kimenai@ed.ac.uk

