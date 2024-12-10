####################################################################################################################################
# Session setup
####################################################################################################################################

#--------------------------
# Load packages
#--------------------------

library(rms)
library(sparklyr)
library(tidyverse)
library(lubridate)
library(splines)
library(broom)
library(writexl)
library(readxl)
library(lsr)
library(stddiff)

options(scipen = 999)
options(max.print = 400)

#--------------------------
# Set up the spark connection
#--------------------------

session_config <- sparklyr::spark_config()

sc <- sparklyr::spark_connect(
    master = "yarn-client",
    app_name = "endo",
    config = session_config
)

#--------------------------
# define run type (to output to correct folder)
#--------------------------

run_type <- "10_year_follow_up"
#run_type <- "2_year_follow_up"

#--------------------------
# define dataset versions
#--------------------------

hes_dataset <- "..."
hes_dataset_primary_diag_only <- "..."
census_dataset <- "..."
linked_dataset <- "..."
sample_dataset <- "..."

#--------------------------
# define study start and end date
#--------------------------

study_start_date <- "2011-03-27"

if (run_type == "10_year_follow_up") {
  study_end_date <- "2021-12-31"  
} else if (run_type == "2_year_follow_up") {
  study_end_date <- "2013-12-31"
} else {
  print("Error: enter a valid run_type")
}

#--------------------------
# define paths
#--------------------------

path <- "..."
source(paste0(path,"_functions.R"))
source("...")
source("...")
out_path <- paste0("endo/", run_type, "/")
out_path_plots <- paste0("endo/", run_type, "/plots/")
import_path <- paste0("endo/", run_type, "/")

#--------------------------
# define exposures
#--------------------------

exposures <- c("age_group_5",
               "general_health",
               "disability",
               "ethnicity_detailed",
               "ethnicity_agg",
               "cob",
               "main_language",
               "region",
               "utla",
               "rural_urban",
               "imd_decile",
               "nssec_hh",
               "qualifications",
               "qualifications_25_plus")

#--------------------------
# define formatted exposure lookup for outputs
#--------------------------

exposure_formatted_lookup <- tibble(
  variable = c("age_group_5",
               "general_health",
               "disability",
               "num_epi_pre_study_grouped",
               "ethnicity_detailed",
               "ethnicity_agg",
               "cob",
               "main_language",
               "region",
               "utla",
               "rural_urban",
               "imd_decile",
               "nssec_hh",
               "qualifications",
               "qualifications_25_plus",
               "total", 
               "age_at_diag_final_5"),
  name = c("Age on Census day",
           "General health",
           "Disability",
           "Number of HES episodes pre-study",
           "Ethnicity (detailed)",
           "Ethnicity (aggregated)",
           "Country of birth",
           "Main language",
           "Region",
           "Upper tier local authority",
           "Rural urban classification",
           "IMD decile",
           "Household NS-SEC",
           "Highest level of qualification",
           "Highest level of qualification, 25 years and over",
           "Total",
           "Age at endometriosis diagnosis"))
  
#--------------------------
# define order of exposures for outputs
#--------------------------

order_age_group_5 <- c("<10", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                      "70-74", "75-79", "80 plus")

order_age_at_diag_final_5 <- c("<15", "15-19", "20-24", "25-29", "30-34", "35-39",
                               "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
                               "70-74", "75-79", "80 plus", "No value")

order_general_health <- c("Very good health",
                          "Good health",
                          "Fair health",
                          "Bad health",
                          "Very bad health")

order_disability <- c("Day to day activities not limited",
                      "Day to day activities limited a little",
                      "Day to day activities limited a lot")

order_num_epi_pre_study_grouped <- c("0", "1-3", "4-6", "7-9", "10-14", "15+")

order_ethnicity_detailed = c('White: English/Welsh/Scottish/Northern Irish/British',
                             'White: Irish',
                             'White: Gypsy or Irish Traveller',
                             'White: Other White',
                             'Mixed/multiple ethnic groups: White and Black Caribbean',
                             'Mixed/multiple ethnic groups: White and Black African',
                             'Mixed/multiple ethnic groups: White and Asian',
                             'Mixed/multiple ethnic groups: Other Mixed',
                             'Asian/Asian British: Indian',
                             'Asian/Asian British: Pakistani',
                             'Asian/Asian British: Bangladeshi',
                             'Asian/Asian British: Chinese',
                             'Asian/Asian British: Other Asian',
                             'Black/African/Caribbean/Black British: African',
                             'Black/African/Caribbean/Black British: Caribbean',
                             'Black/African/Caribbean/Black British: Other Black',
                             'Other ethnic group: Arab',
                             'Other ethnic group: Any other ethnic group')

order_ethnicity_agg <- c("White",
                         "Mixed/Multiple ethnic groups",
                         "Asian/Asian British",
                         "Black/African/Caribbean/Black British",
                         "Other ethnic group")

order_cob <- c("Born in the UK",
               "Born outside the UK")

order_main_language <- c("Main language is English",
                         "Main language is not English")

order_region <- c("North East", 
                  "North West", 
                  "Yorkshire and the Humber",
                  "East Midlands", 
                  "West Midlands", 
                  "East of England", 
                  "London", 
                  "South East", 
                  "South West")

order_utla <- scan("...", sep=',', what = "", skip = 1, quiet = TRUE)
order_utla <- order_utla[order_utla != "Outside England or unknown"]

order_rural_urban <- c("Urban",
                       "Rural")

order_imd_decile <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

order_nssec_hh <- c("Class 1",
                    "Class 2",
                    "Class 3",                          
                    "Class 4",                          
                    "Class 5",                          
                    "Class 6",                                                       
                    "Class 7",
                    "Class 8",
                    "Students", 
                    "Not classified")

order_qualifications <- c("No academic or professional qualifications",
                          "Level 1",
                          "Level 2",
                          "Apprenticeship",
                          "Level 3",
                          "Level 4 and above",
                          "Other",
                          "Not classified")

order_qualifications_25_plus <- c("No academic or professional qualifications",
								  "Level 1",
								  "Level 2",
								  "Apprenticeship",
								  "Level 3",
								  "Level 4 and above",
								  "Other",
								  "Aged 0 to 24 years on Census Day")

order_total <- "Total"

order_emergency_admission_flag <- c("Emergency admission",
                                    "Non-emergency admission",
                                    "Not applicable or not known")

#-------------#
# END OF FILE #
#-------------#
