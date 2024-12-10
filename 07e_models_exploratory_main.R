####################################################################################################################################
# Exploratory models - adhoc
####################################################################################################################################

source(".../07a_models_setup_main.R")

# region + age + health + imd_decile
model_1a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(region_parameters,
    age_parameters,
    num_epi_pre_study_grouped_parameters,
    imd_decile_parameters),  
  exposure = "region",
  model_num = 4, suffix = "all")

# region + age + health + imd_decile + highest_qualification
model_1b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(region_parameters,
    age_parameters,
    num_epi_pre_study_grouped_parameters,
    imd_decile_parameters,
    qualifications_parameters),  
  exposure = "region",
  model_num = 5, suffix = "all")


# ethnicity + age + health + cob
model_3a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    cob_parameters), 
  exposure = "ethnicity",  
  model_num = 4, suffix = "all") 

# ethnicity_agg + age + health + cob
model_3b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    cob_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 4, suffix = "all") 

# ethnicity_detailed + age + health + cob
model_3c <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_detailed_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    cob_parameters),  
  exposure = "ethnicity_detailed",  
  model_num = 4, suffix = "all") 


# ethnicity + age + health + main_language
model_4a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    main_language_parameters), 
  exposure = "ethnicity",  
  model_num = 5, suffix = "all") 

# ethnicity_agg + age + health + main_language
model_4b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    main_language_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 5, suffix = "all") 

# ethnicity_detailed + age + health + main_language
model_4c <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_detailed_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    main_language_parameters),  
  exposure = "ethnicity_detailed",  
  model_num = 5, suffix = "all")


# ethnicity + age + health + cob + main_language
model_5a <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    cob_parameters,
    main_language_parameters), 
  exposure = "ethnicity",  
  model_num = 6, suffix = "all") 

# ethnicity_agg + age + health + cob + main_language
model_5b <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    cob_parameters,
    main_language_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 6, suffix = "all") 

# ethnicity_detailed + age + health + cob + main_language
model_5c <- create_model(
  df_relevel, 
  "diag_endo", 
  c(ethnicity_detailed_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters,
    cob_parameters,
    main_language_parameters),  
  exposure = "ethnicity_detailed",  
  model_num = 6, suffix = "all")

# ethnicity_agg for people born in the UK
df_relevel_born_in_uk <- df_relevel %>%
  filter(cob == "Born in the UK")
sdf_dim(df_relevel_born_in_uk)

# ethnicity_agg + age + health
model_5d <- create_model(
  df_relevel_born_in_uk, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 7, suffix = "all")

# ethnicity_agg for people born outside the UK
df_relevel_born_outside_uk <- df_relevel %>%
  filter(cob == "Born outside the UK")
sdf_dim(df_relevel_born_outside_uk)

# ethnicity_agg + age + health
model_5e <- create_model(
  df_relevel_born_outside_uk, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters,
    num_epi_pre_study_grouped_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 8, suffix = "all")

# ethnicity_agg for people whose main language is English
df_relevel_main_lang_english <- df_relevel %>%
  filter(main_language == "Main language is English")
sdf_dim(df_relevel_main_lang_english)

# ethnicity_agg + age + health
model_5f <- create_model(
  df_relevel_main_lang_english, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters,
    num_epi_pre_study_grouped_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 9, suffix = "all")

df_relevel_main_lang_not_english <- df_relevel %>%
  filter(main_language == "Main language is not English")
sdf_dim(df_relevel_main_lang_not_english)

# ethnicity_agg + age + health
model_5g <- create_model(
  df_relevel_main_lang_not_english, 
  "diag_endo", 
  c(ethnicity_agg_parameters, 
    age_parameters,
    num_epi_pre_study_grouped_parameters),  
  exposure = "ethnicity_agg",  
  model_num = 10, suffix = "all")

# qualifications for ages 25 and over
df_relevel_25_plus <- df_relevel %>%
  filter(age_census >= 25)

#remove not classified since none in the population aged 25+
qualifications_parameters_25_plus <- "qualifications_1 + qualifications_3 + qualifications_4 + qualifications_5 + qualifications_6 + qualifications_7"

# qualifications
model_7 <- create_model(
  df_relevel_25_plus, 
  "diag_endo", 
  c(qualifications_parameters_25_plus), 
  exposure = "qualifications",  
  model_num = 4, suffix = "all")

# qualifications + age
model_8 <- create_model(
  df_relevel_25_plus, 
  "diag_endo", 
  c(qualifications_parameters_25_plus, 
    age_parameters), 
  exposure = "qualifications",  
  model_num = 5, suffix = "all")

# qualifications + age + health
model_9 <- create_model(
  df_relevel_25_plus, 
  "diag_endo", 
  c(qualifications_parameters_25_plus, 
    age_parameters, 
    num_epi_pre_study_grouped_parameters), 
  exposure = "qualifications",  
  model_num = 6, suffix = "all")


models_exploratory <- rbind(model_1a,
                            model_1b,
                            #model_2a,
                            #model_2b,
                            model_3a,
                            model_3b,
                            model_3c,
                            model_4a,
                            model_4b,
                            model_4c,
                            model_5a,
                            model_5b,
                            model_5c,
                            model_5d,
                            model_5e,
                            model_5f,
                            model_5g,
                            #model_6a,
                            #model_6b,
                            model_7,
                            model_8,
                            model_9)


#--------------------------
# Save output
#--------------------------

List_of_dfs <- list(models_exploratory)

names(List_of_dfs) <- c("models_exploratory")

write_xlsx(List_of_dfs, paste0(out_path, "endo_models_main_exploratory.xlsx"))


#-------------#
# END OF FILE #
#-------------#