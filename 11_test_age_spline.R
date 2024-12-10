1####################################################################################################################################
# Test age spline
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Load data
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset))

df <- mutate(df, age_census_num = as.integer(age_census))

#--------------------------
# Plot proportion of endo diagnoses by age_census
#--------------------------

plots_dataset <- plots_data(df, var1 = "age_census_num") %>% filter(!is.na(percentage))

ggplot(data = plots_dataset, 
       aes(x = age_census_num, y = percentage)) +
  geom_point() +
  geom_line()

ggsave(paste0(out_path_plots, "plot_perc_endo_age_census.pdf"),
       width = 10, height = 4, units = "in")


#--------------------------
# Load sample of data to use glm function
#--------------------------

sample_r <- sdf_sql(sc, paste0("SELECT * FROM ", sample_dataset)) %>%
  collect()

nrow(sample_r)


#--------------------------
# Test number of df for age spline in the model
#--------------------------

max_df = 15

formula_list <- list()

for (i in 1:max_df){
  
  formula <- paste0('diag_endo ~ splines::ns(age_census_num, df = ', 
                    i, 
                    ', Boundary.knots = quantile(age_census_num, c(.01, .99)))')

#  formula <- paste0("diag_endo ~ splines::ns(age_census_num, df = 6, knots = quantile(age_census_num, c(0.25, 0.5, 0.75)), Boundary.knots = quantile(age_census_num, c(0.1, 0.9)))")
#  formula <- paste0("diag_endo ~ splines::ns(age_census_num, df = 6, knots = quantile(age_census_num, c(0.25, 0.5, 0.75)), Boundary.knots = quantile(age_census_num, c(0.01, 0.99)))")
#  formula <- paste0("diag_endo ~ splines::ns(age_census_num, df = 8, knots = quantile(age_census_num, c(0.17, 0.33, 0.5, 0.67, 0.83)), Boundary.knots = quantile(age_census_num, c(0.01, 0.99)))")
  
  formula_list <- c(formula_list, formula)
  
}

df_aic <- data.frame(df = integer(), 
                     aic = integer())  

for (i in 1:max_df) {
  
  print(i)
  
  model <- glm(formula = formula_list[[i]],
               data = sample_r,
               family = "binomial",
               weights = sampling_weight)

  aic <- summary(model)$aic
  
  print(aic)
  
  df_aic <- add_row(df_aic, df = i, aic = aic)
  
}

print(df_aic)

# inspect df by aic
ggplot(data = df_aic, 
       aes(x = df, y = aic)) +
  geom_point() +
  geom_line()

ggsave(paste0(out_path_plots, "plot_age_spline_aic.pdf"),
       width = 10, height = 4, units = "in")


#--------------------------
# Check AICs of models on full dataset
#--------------------------

# define knots
knots_pctiles <- c(0.01, 0.17, 0.33, 0.5, 0.67, 0.83, 0.99)
#knots_pctiles <- c(0.1, 0.17, 0.33, 0.5, 0.67, 0.83, 0.9)
#knots_pctiles <- c(0.01, 0.2, 0.4, 0.6, 0.8, 0.99)
#knots_pctiles <- c(0.01, 0.25, 0.5, 0.75, 0.99)

spline_reg_cols <- purrr::map_chr(1:(length(knots_pctiles) - 2), function(e) {
  paste0("age_census_num", "_spline_part_", e)
})

formula <- as.formula(paste0("diag_endo ~ age_census_num + ",
                             paste(spline_reg_cols, collapse = " + ")))

formula

# Call restricted cubic spline function
df2 <- sdf_rcs_fun(x = "age_census_num", data = df, knots_pctiles = knots_pctiles)

age_params_sep <- c("age_census_num", spline_reg_cols)
age_parameters <- paste0(age_params_sep, collapse = " + ")
age_parameters

# Function for running the logistic regression model
create_model <- function(dataset, outcome, parameters, exposure, model_num, suffix) {
  
  model <- ml_generalized_linear_regression(x = dataset,
    formula = as.formula(paste0(outcome, " ~ ", paste0(parameters, collapse = " + "))), family = "binomial", link = "logit")
  
  print(summary(model))
  
  df_model <- tidy_model(model, model_n = model_num, exp = TRUE) %>%
    mutate(AIC = model$summary$aic())
  
  print(df_model)
  
  return(df_model)
   
}

# run model
model <- create_model(
  df2, 
  "diag_endo", 
  c(age_parameters), 
  exposure = "age_spline",
  model_num = 1, suffix = "all") 


#-------------#
# END OF FILE #
#-------------#

