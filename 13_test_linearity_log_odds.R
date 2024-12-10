####################################################################################################################################
# Test linearity of continuous variables (age) with log odds
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Load dataset 
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset))

#--------------------------
# Create numeric version of age_census
# Create age squared and age cubed variables
#--------------------------

df <- df %>%
  mutate(age_census_num = as.integer(age_census)) %>%
  mutate(age2 = age_census_num^2) %>%
  mutate(age3 = age_census_num^3)

#--------------------------
# Prepare data for restricted cubic spline function
#--------------------------

knots_pctiles <- c(0.01, 0.17, 0.33, 0.5, 0.67, 0.83, 0.99)

spline_reg_cols <- purrr::map_chr(1:(length(knots_pctiles) - 2), function(e) {
  
  paste0("age_census_num", "_spline_part_", e)
  
})

formula_spline <- as.formula(paste0("diag_endo ~ age_census_num + ",
                             paste(spline_reg_cols, collapse = " + ")))

formula_spline

#--------------------------
# Call restricted cubic spline function
#--------------------------

df <- sdf_rcs_fun(x = "age_census_num", data = df, knots_pctiles = knots_pctiles)

#--------------------------
# Test linearity of age + age2 + age3
#--------------------------

model_1a <- ml_generalized_linear_regression(x = df,
  formula = as.formula("diag_endo ~ age_census_num + age2 + age3"),
  family = "binomial",
  link = "logit")

model_1a_res <- model_1a$summary$predictions %>%
  mutate(logitpred = log(prediction/(1-prediction))) 

model_1a_plot <- model_1a_res %>% 
  select(age_census_num, age2, age3, logitpred) %>%
  distinct() 

ggplot(model_1a_plot, 
       aes(x = logitpred, 
           y = age3)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth() 

ggsave(paste0(out_path_plots, "plot_linearity_age_polynomial.pdf"),
       width = 10, height = 4, units = "in")

#--------------------------
# Test linearity of age spline
#--------------------------

# Model of death by age_census_spline using sample
model_2a <- ml_generalized_linear_regression(
  x = df,
  formula = as.formula("diag_endo ~ 
  age_census_num +
  age_census_num_spline_part_1 +
  age_census_num_spline_part_2 +
  age_census_num_spline_part_3 +
  age_census_num_spline_part_4 +
  age_census_num_spline_part_5"),
  family = "binomial",
  link = "logit")

model_2a_res <- model_2a$summary$predictions %>%
  mutate(logitpred = log(prediction/(1-prediction)))

model_2a_plot <- model_2a_res %>% 
  select(age_census_num, 
         age_census_num_spline_part_1, 
         age_census_num_spline_part_2,
         age_census_num_spline_part_3, 
         age_census_num_spline_part_4, 
         age_census_num_spline_part_5,          
         logitpred) %>%
  distinct() 

ggplot(model_2a_plot, 
       aes(x = logitpred, 
           y = age_census_num_spline_part_5)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth()

ggsave(paste0(out_path_plots, "plot_linearity_age_spline.pdf"),
       width = 10, height = 4, units = "in")


#-------------#
# END OF FILE #
#-------------#
