####################################################################################################################################
# Models setup - supplementary analysis
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Load data
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset)) %>%
  mutate(utla = case_when(utla == "Bristol" ~ "Bristol, City of",
                          utla == "Herefordshire" ~ "Herefordshire, County of",
                          utla == "Kingston upon Hull" ~ "Kingston upon Hull, City of",
                          TRUE ~ utla))

df <- filter(df, population_supplementary %in% c("endo", "no_endo"))

#--------------------------
# Prepare data for restricted cubic spline function
#--------------------------

df <- mutate(df, age_census_num = as.integer(age_census))

# define knots
knots_pctiles <- c(0.01, 0.17, 0.33, 0.5, 0.67, 0.83, 0.99)

spline_reg_cols <- purrr::map_chr(1:(length(knots_pctiles) - 2), function(e) {
  paste0("age_census_num", "_spline_part_", e)
})

formula <- as.formula(paste0("diag_endo_primary ~ age_census_num + ",
                             paste(spline_reg_cols, collapse = " + ")))

formula

#--------------------------
# Call restricted cubic spline function
#--------------------------

df <- sdf_rcs_fun(x = "age_census_num", data = df, knots_pctiles = knots_pctiles)


#--------------------------
# Create custom reference categories for categorical variables
#--------------------------

# Call relevel function from HALE reusable function to set custom reference category
# n_cat (Number of unique categories in variable) must be determined by the user before running function
# Produce a table of reference categories
ref_cat <- tibble(
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
               "qualifications"),
  ref_level = c("35-39",
                "Very good health",
                "Day to day activities not limited",
                "0",
                "White: English/Welsh/Scottish/Northern Irish/British",
                "White",
                "Born in the UK",
                "Main language is English",
                "London",
                "City of London and Hackney",
                "Urban",
                "10",
                "Class 1",
                "Level 4 and above"),
  n_cat = c(summarize(df, dist = n_distinct(age_group_5)) %>% pull(),
            summarize(df, dist = n_distinct(general_health)) %>% pull(),
            summarize(df, dist = n_distinct(disability)) %>% pull(),
            summarize(df, dist = n_distinct(num_epi_pre_study_grouped)) %>% pull(),
            summarize(df, dist = n_distinct(ethnicity_detailed)) %>% pull(),
            summarize(df, dist = n_distinct(ethnicity_agg)) %>% pull(),
            summarize(df, dist = n_distinct(cob)) %>% pull(), 
            summarize(df, dist = n_distinct(main_language)) %>% pull(),
            summarize(df, dist = n_distinct(region)) %>% pull(),
            summarize(df, dist = n_distinct(utla)) %>% pull(),
            summarize(df, dist = n_distinct(rural_urban)) %>% pull(), 
            summarize(df, dist = n_distinct(imd_decile)) %>% pull(), 
            summarize(df, dist = n_distinct(nssec_hh)) %>% pull(), 
            summarize(df, dist = n_distinct(qualifications)) %>% pull())

ref_cat

write.csv(ref_cat, paste0(out_path, "ref_cat.csv"), row.names = FALSE)


# Loop function to relevel variables
df_relevel <- df
for (i in 1:nrow(ref_cat)) { 
  print(ref_cat$variable[i])
  df_relevel <- relevel_sparkml(
    df_relevel, 
    variable = ref_cat$variable[i],
    ref_level = ref_cat$ref_level[i], 
    n_cat = ref_cat$n_cat[i])
}

# Check the output column names for the regression formula
colnames(df_relevel)

#--------------------------
# Create labels
#--------------------------

# Note that the terms do not have their category names, just numbers
# Use the HALE reusable function to create labels and join these onto the model dataframe
# This function uses a loop and the ref_cat table to apply labels

df_labels <- NULL
for (i in 1:nrow(ref_cat)) {
  print(ref_cat$variable[i])
  df_labels_prelim <- label_categories(
    df_relevel, 
    variable = ref_cat$variable[i],
    ref_level = ref_cat$ref_level[i]) %>%
    select(modelterm = !!sym(paste0(ref_cat$variable[i], "_modelterm")),
           label = !!sym(paste0(ref_cat$variable[i], "_label"))) %>%
    filter(label != paste0(ref_cat$variable[i], "_", ref_cat$ref_level[i]))
  
  df_labels <- bind_rows(df_labels, df_labels_prelim)
}

print(df_labels)


#--------------------------
# Create parameters for models
#--------------------------

# create vectors of parameters
age_params_sep <- c("age_census_num", spline_reg_cols)
age_group_5_params_sep <- paste0("age_group_5_", 1:(ref_cat$n_cat[ref_cat$variable == "age_group_5"] - 1))
general_health_params_sep <- paste0("general_health_", 1:(ref_cat$n_cat[ref_cat$variable == "general_health"] - 1))
disability_params_sep <- paste0("disability_", 1:(ref_cat$n_cat[ref_cat$variable == "disability"] - 1))
num_epi_pre_study_grouped_params_sep <- paste0("num_epi_pre_study_grouped_", 1:(ref_cat$n_cat[ref_cat$variable == "num_epi_pre_study_grouped"] - 1))
ethnicity_detailed_params_sep <- paste0("ethnicity_detailed_", 1:(ref_cat$n_cat[ref_cat$variable == "ethnicity_detailed"] - 1))
ethnicity_agg_params_sep <- paste0("ethnicity_agg_", 1:(ref_cat$n_cat[ref_cat$variable == "ethnicity_agg"] - 1))
cob_params_sep <- paste0("cob_", 1:(ref_cat$n_cat[ref_cat$variable == "cob"] - 1))
main_language_params_sep <- paste0("main_language_", 1:(ref_cat$n_cat[ref_cat$variable == "main_language"] - 1))
region_params_sep <- paste0("region_", 1:(ref_cat$n_cat[ref_cat$variable == "region"] - 1))
utla_params_sep <- paste0("utla_", 1:(ref_cat$n_cat[ref_cat$variable == "utla"] - 1))
rural_urban_params_sep <- paste0("rural_urban_", 1:(ref_cat$n_cat[ref_cat$variable == "rural_urban"] - 1))
imd_decile_params_sep <- paste0("imd_decile_", 1:(ref_cat$n_cat[ref_cat$variable == "imd_decile"] - 1))
nssec_hh_params_sep <- paste0("nssec_hh_", 1:(ref_cat$n_cat[ref_cat$variable == "nssec_hh"] - 1))
qualifications_params_sep <- paste0("qualifications_", 1:(ref_cat$n_cat[ref_cat$variable == "qualifications"] - 1))

# join together in a string
age_parameters <- paste0(age_params_sep, collapse = " + ")
age_group_5_parameters <- paste0(age_group_5_params_sep, collapse = " + ")
general_health_parameters <- paste0(general_health_params_sep, collapse = " + ")
disability_parameters <- paste0(disability_params_sep, collapse = " + ")
num_epi_pre_study_grouped_parameters <- paste0(num_epi_pre_study_grouped_params_sep, collapse = " + ")
ethnicity_detailed_parameters <- paste0(ethnicity_detailed_params_sep, collapse = " + ")
ethnicity_agg_parameters <- paste0(ethnicity_agg_params_sep, collapse = " + ")
cob_parameters <- paste0(cob_params_sep, collapse = " + ")
main_language_parameters <- paste0(main_language_params_sep, collapse = " + ")
region_parameters <- paste0(region_params_sep, collapse = " + ")
utla_parameters <- paste0(utla_params_sep, collapse = " + ")
rural_urban_parameters <- paste0(rural_urban_params_sep, collapse = " + ")
imd_decile_parameters <- paste0(imd_decile_params_sep, collapse = " + ")
nssec_hh_parameters <- paste0(nssec_hh_params_sep, collapse = " + ")
qualifications_parameters <- paste0(qualifications_params_sep, collapse = " + ")


#--------------------------
# Run logistic regression using generated variables
#--------------------------

# Function for running the logistic regression model
create_model <- function(dataset, outcome, parameters, exposure, model_num, suffix) {
  
  model <- ml_generalized_linear_regression(x = dataset,
    formula = as.formula(paste0(outcome, " ~ ", paste0(parameters, collapse = " + "))), family = "binomial", link = "logit")
  
  print(summary(model))
  
  df_model <- tidy_model(model, model_n = model_num, exp = TRUE)
  
  df_model <- left_join(df_model, df_labels, by = c("term" = "modelterm")) %>% 
    mutate(label = ifelse(is.na(label), term, label)) %>%
    select(!term) %>%
    relocate(label, .before = everything()) %>%
    rename(term = label) %>%
    mutate(exposure = !!exposure) %>%
    relocate(model_id, .before = everything()) %>%
    relocate(exposure, .before = everything()) %>%
    mutate(population = !!suffix) %>%
    relocate(population, .before = everything()) %>%
    mutate(AIC = model$summary$aic())
  
  #model_stats <- model_statistics(model = model, model_n = model_num)

  print(df_model)
  return(df_model)
   
}


#--------------------------
# Remove diag_endo as a precaution to not use this as the outcome by mistake
#--------------------------

df_relevel <- df_relevel %>%
  select(-diag_endo)

# Check counts
df_relevel %>% group_by(population_supplementary_split, diag_endo_primary) %>% count() %>% print()


#-------------#
# END OF FILE #
#-------------#
