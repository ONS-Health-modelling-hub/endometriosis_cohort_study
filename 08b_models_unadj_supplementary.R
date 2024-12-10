####################################################################################################################################
# Unadjusted models
####################################################################################################################################

source(".../08a_models_setup_supplementary.R")

# age_group
model_1a <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(age_group_5_parameters), 
  exposure = "age_group_5",
  model_num = 1, suffix = "all") 

# age spline
model_1b <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(age_parameters), 
  exposure = "age_spline",
  model_num = 1, suffix = "all") 

# general_health
model_2a <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(general_health_parameters), 
  exposure = "general_health", 
  model_num = 1, suffix = "all")

# general_health_agg
model_2b <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(general_health_agg_parameters), 
  exposure = "general_health_agg", 
  model_num = 1, suffix = "all")

# disability
model_3 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(disability_parameters),  
  exposure = "disability",
  model_num = 1, suffix = "all") 

# ethnicity
model_4a <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(ethnicity_parameters),  
  exposure = "ethnicity",
  model_num = 1, suffix = "all") 

# ethnicity_agg
model_4b <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(ethnicity_agg_parameters),  
  exposure = "ethnicity_agg",
  model_num = 1, suffix = "all") 

# ethnicity_detailed
model_4c <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(ethnicity_detailed_parameters),  
  exposure = "ethnicity_detailed",
  model_num = 1, suffix = "all") 

# cob
model_5 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(cob_parameters),  
  exposure = "cob",
  model_num = 1, suffix = "all") 

# main_language
model_6 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(main_language_parameters),  
  exposure = "main_language",
  model_num = 1, suffix = "all") 

# region
model_7 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(region_parameters),  
  exposure = "region",
  model_num = 1, suffix = "all") 

# rural_urban
model_8 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(rural_urban_parameters),  
  exposure = "rural_urban",
  model_num = 1, suffix = "all") 

# imd_decile
model_9 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(imd_decile_parameters),  
  exposure = "imd_decile",
  model_num = 1, suffix = "all") 

# nssec_hh
model_10 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(nssec_hh_parameters),  
  exposure = "nssec_hh",
  model_num = 1, suffix = "all") 

# qualifications
model_11 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(qualifications_parameters),  
  exposure = "qualifications",
  model_num = 1, suffix = "all")

# qualifications + imd_decile + nssec_hh
model_12 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(qualifications_parameters, imd_decile_parameters, nssec_hh_parameters),  
  exposure = "qualifications_imd_hhnssec",
  model_num = 1, suffix = "all")

# utla
model_13 <- create_model(
  df_relevel, 
  "diag_endo_primary", 
  c(utla_parameters), 
  exposure = "utla", 
  model_num = 1, suffix = "all")


models_all <- rbind(model_1a,
                    model_1b,
                    model_2a,
                    model_2b,
                    model_3,
                    model_4a,
                    model_4b,
                    model_4c,
                    model_5,
                    model_6,
                    model_7,
                    model_8,
                    model_9,
                    model_10,
                    model_11,
                    model_12,
					model_13)


#--------------------------
# Save output
#--------------------------

List_of_dfs <- list(models_all)

names(List_of_dfs) <- c("models_all")

write_xlsx(List_of_dfs, paste0(out_path, "endo_models_supplementary_unadj.xlsx"))


#-------------#
# END OF FILE #
#-------------#

