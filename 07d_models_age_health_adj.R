####################################################################################################################################
# Adjusted for age and health
####################################################################################################################################

source(".../07a_models_setup.R")

# age_group + health
model_1 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(age_group_5_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "age_group_5", 
  model_num = 3, suffix = "all") 

# age spline + health
model_2 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(age_parameters, 
    num_epi_pre_study_grouped_parameters),
  exposure = "age_spline",
  model_num = 3, suffix = "all")

# ethnicity + age + health
model_3a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "ethnicity",  
  model_num = 3, suffix = "all") 

# ethnicity_agg + age + health
model_3b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "ethnicity_agg",  
  model_num = 3, suffix = "all") 

# ethnicity_detailed + age + health
model_3c <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_detailed_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "ethnicity_detailed",  
  model_num = 3, suffix = "all") 

# cob + age + health
model_4 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(cob_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "cob", 
  model_num = 3, suffix = "all") 

# main_language + age + health
model_5 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(main_language_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "main_language",  
  model_num = 3, suffix = "all") 

# region + age + health
model_6 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(region_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "region", 
  model_num = 3, suffix = "all") 

# rural_urban + age + health
model_7 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(rural_urban_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "rural_urban", 
  model_num = 3, suffix = "all") 

# imd_decile + age + health
model_8 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(imd_decile_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "imd_decile",  
  model_num = 3, suffix = "all") 

# nssec_hh + age + health
model_9 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(nssec_hh_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "nssec_hh", 
  model_num = 3, suffix = "all") 

# qualifications + age + health
model_10 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(qualifications_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "qualifications",  
  model_num = 3, suffix = "all")

# utla + age + health
model_11 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(utla_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "utla", 
  model_num = 3, suffix = "all")

# qualifications + imd_decile + nssec_hh
model_12 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(qualifications_parameters, imd_decile_parameters, nssec_hh_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters),  
  exposure = "qualifications_imd_hhnssec",
  model_num = 3, suffix = "all")


models_all <- rbind(model_1,
                    model_2,
                    model_3a,
                    model_3b,
                    model_3c,
                    model_4,
                    model_5,
                    model_6,
                    model_7,
                    model_8,
                    model_9,
                    model_10,
                    model_11,
                    model_12)

#--------------------------
# Save outputs
#--------------------------

List_of_dfs <- list(models_all)

names(List_of_dfs) <- c("models_all")

write_xlsx(List_of_dfs, paste0(out_path, "endo_models_main_age_health_adj.xlsx"))


#-------------#
# END OF FILE #
#-------------#

