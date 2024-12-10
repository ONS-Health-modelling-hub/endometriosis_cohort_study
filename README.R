# Characteristics of women with an endometriosis diagnosis in England: 27 March 2011 to 31 December 2021

Authors: Hannah Bunk and Isobel Ward

Copyrights: Crown Copyright

## Project details

In this project, we conduct a population-level analysis of the characteristics of women receiving an endometriosis diagnosis in an NHS hospital in England between 2011 and 2021. 

Link to ONS release: https://www.ons.gov.uk/releases/characteristicsofwomenwithanendometriosisdiagnosisinengland27march2011to31december2021

Project completed for: Labour Markets Evaluation and Pilots Fund (2024 to 2025) https://www.gov.uk/government/publications/new-labour-markets-evaluation-and-pilots-fund

## Data sources

- Census 2011
- Hospital Episode Statistics (HES) Admitted Patient Care (APC)
- ONS death registrations

## Study population

All women enumerated in the 2011 Census who were usual residents in England on Census Day (27 March 2011) and could be linked to an NHS number.
We defined two cohorts of women with an endometriosis diagnosis. 
For our main analysis, we estimated prevalence by identifying women with any endometriosis diagnosis (primary or secondary) during the study period between 27 March 2011 and 31 December 2021 and a control group of women enumerated in Census 2011 with no evidence of endometriosis during the study period. 
For our supplementary analysis, we estimated incidence by identifying a group of women with a primary endometriosis diagnosis only during our study period and used two years of HES data (1 April 2009 to 26 March 2011) prior to the study start to exclude instances where the diagnosis during the study period was not the first. 
The control group of women for the supplementary analysis had no diagnosis of endometriosis during the study period, or in the two years prior. All groups were filtered to restrict our cohort to people who self-reported as female in the 2011 Census.

## Code structure

- **_functions** - Holds all the project's supporting functions

- **parameters.py** - Define parameters for reading in deaths data in "02_get_census_data.py".

- **lookups** - Contains the ICD lookup used in "01_get_hes_data.py" and geography lookups used in "03_dataset_preparation.R".

- **00_session_setup.R** - Set up the Spark session and define variables.

- **01_get_hes_data.py** - Get episode-level HES data with flags by for endometriosis diagnoses

- **02_get_census_data.py** - Get 2011 Census data linked to deaths

- **03_dataset_preparation.R** - Make the HES data person-level, link the Census 2011, deaths and HES data, filter to the study population, calculate the sample flow at each stage of filtering and save into a CSV file, 
format the variables, save the linked person-level dataset into HIVE, and run checks.

- **04_data_checks.R** - Read in the linked dataset from HIVE and check counts for age at first diagnosis and method of hospital admission.

- **05_descriptives.R** - Read in the linked dataset from HIVE and produce counts and plots by characteristic. 

- **06_get_rates.R** - Read in the linked dataset from HIVE and produce crude and age-standardised rates and plots by characteristic.

- **07a_models_setup_main.R** - Read in the linked dataset from HIVE, filter to the main analysis study population, create the age spline, define reference categories and labels for each exposure, and define the logistic regression model function.

- **07b_models_unadj_main.R** - Run unadjusted logistic regression models for each exposure independently using the main analysis study population, and save the outputs into an Excel file.

- **07c_models_age_adj_main.R** - Run logistic regression models adjusted for age for each exposure independently using the main analysis study population, and save the outputs into an Excel file.

- **07d_models_age_health_adj_main.R** - Run logistic regression models adjusted for age and health for each exposure independently using the main analysis study population, and save the outputs into an Excel file.

- **07e_models_exploratory_main.R** - Run logistic regression models with additional ad hoc adjustments using the main analysis study population, and save the outputs into an Excel file.

- **08a_models_setup_supplementary.R** - Read in the linked dataset from HIVE, filter to the supplementary analysis study population, create the age spline, define reference categories and labels for each exposure, and define the logistic regression model function.

- **08b_models_unadj_main.R** - Run unadjusted logistic regression models for each exposure independently using the supplementary analysis study population, and save the outputs into an Excel file.

- **08c_models_age_adj_main.R** - Run logistic regression models adjusted for age for each exposure independently using the supplementary analysis study population, and save the outputs into an Excel file.

- **08d_models_age_health_adj_main.R** - Run logistic regression models adjusted for age and health for each exposure independently using the supplementary analysis study population, and save the outputs into an Excel file.

- **08e_models_exploratory_main.R** - Run logistic regression models with additional ad hoc adjustments using the supplementary analysis study population, and save the outputs into an Excel file.

- **09_models_plots.R** - Read in all the model outputs, bind together into one dataset and output into an Excel file. Create plots of odds ratios by exposure.

- **10_select_sample.R** - Select a simple random sample from the linked dataset, for use in "11_test_age_spline.R" and "12_test_multicollinearity.R" (due to memory constraints). Sample all people with an endometriosis diagnosis and a specified proportion (r_non_endo) of people without an endometriosis diagnosis. Save the sample into HIVE.

- **11_test_age_spline.R** - Choose number of knots for age spline using the Akaike information criterion (AIC). 

- **12_test_multicollinearity.R** - Check for multicollinearity using the sample selected in "10_select_sample.R".

- **13_test_linearity_log_odds.R** - Check linearity of log odds using the linked dataset.


