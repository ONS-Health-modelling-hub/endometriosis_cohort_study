####################################################################################################################################
# Adjusted for age
####################################################################################################################################

source(".../07a_models_setup_main.R")

# general_health + age
model_1a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(general_health_parameters, age_parameters), 
  exposure = "general_health", 
  model_num = 2, suffix = "all")

# general_health_agg + age
model_1b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(general_health_agg_parameters, age_parameters), 
  exposure = "general_health_agg", 
  model_num = 2, suffix = "all")

# disability + age
model_2 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(disability_parameters, age_parameters), 
  exposure = "disability", 
  model_num = 2, suffix = "all") 

# ethnicity + age
model_3a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_parameters, age_parameters),  
  exposure = "ethnicity",
  model_num = 2, suffix = "all") 

# ethnicity_agg + age
model_3b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_agg_parameters, age_parameters),  
  exposure = "ethnicity_agg",
  model_num = 2, suffix = "all") 

# ethnicity_detailed + age
model_3c <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_detailed_parameters, age_parameters),  
  exposure = "ethnicity_detailed",
  model_num = 2, suffix = "all") 

# cob + age
model_4 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(cob_parameters, age_parameters),  
  exposure = "cob",
  model_num = 2, suffix = "all") 

# main_language + age
model_5 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(main_language_parameters, age_parameters), 
  exposure = "main_language", 
  model_num = 2, suffix = "all") 

# region + age
model_6 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(region_parameters, age_parameters),  
  exposure = "region",
  model_num = 2, suffix = "all") 

# rural_urban + age
model_7 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(rural_urban_parameters, age_parameters), 
  exposure = "rural_urban", 
  model_num = 2, suffix = "all") 

# imd_decile + age
model_8 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(imd_decile_parameters, age_parameters),  
  exposure = "imd_decile",
  model_num = 2, suffix = "all") 

# nssec_hh + age
model_9 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(nssec_hh_parameters, age_parameters),  
  exposure = "nssec_hh",
  model_num = 2, suffix = "all") 

# qualifications + age
model_10 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(qualifications_parameters, age_parameters),  
  exposure = "qualifications",
  model_num = 2, suffix = "all")

# utla + age
model_11 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(utla_parameters, age_parameters), 
  exposure = "utla", 
  model_num = 2, suffix = "all")

# qualifications + imd_decile + nssec_hh
model_12 <- create_model(
  df_relevel, 
  "diag_endo", 
  c(qualifications_parameters, imd_decile_parameters, nssec_hh_parameters, age_parameters),  
  exposure = "qualifications_imd_hhnssec",
  model_num = 2, suffix = "all")

models_all <- rbind(model_1a,
                    model_1b,
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
# Save output
#--------------------------

List_of_dfs <- list(models_all)

names(List_of_dfs) <- c("models_all")

write_xlsx(List_of_dfs, paste0(out_path, "endo_models_main_age_adj.xlsx"))


#-------------#
# END OF FILE #
#-------------#

