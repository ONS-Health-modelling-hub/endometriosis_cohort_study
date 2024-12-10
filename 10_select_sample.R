####################################################################################################################################
# Select sample for tests of logistic regression assumptions
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Load dataset 
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset))


#--------------------------
# Prepare data for restricted cubic spline function
#--------------------------

df <- mutate(df, age_census_num = as.integer(age_census))

knots_pctiles <- c(0.01, 0.17, 0.33, 0.5, 0.67, 0.83, 0.99)

spline_reg_cols <- purrr::map_chr(1:(length(knots_pctiles) - 2), function(e) {
  paste0("age_census_num", "_spline_part_", e)
})

formula <- as.formula(paste0("diag_endo ~ age_census_num + ",
                             paste(spline_reg_cols, collapse = " + ")))

formula


#--------------------------
# Call restricted cubic spline function
#--------------------------

df <- sdf_rcs_fun(x = "age_census_num", data = df, knots_pctiles = knots_pctiles)


#--------------------------
# Create a sample of data to use glm function
#--------------------------

# Select all people with an endometriosis diagnosis
endo <- df %>%
  filter(diag_endo == 1) %>%
  mutate(sampling_weight = 1)

# Select a sample of people without an endo diagnosis
r_non_endo <- 0.01
non_endo <- df %>%
  filter(diag_endo == 0) %>% 
  sdf_sample(fraction = r_non_endo, seed = 1423, replacement = FALSE) %>%
  mutate(sampling_weight = 1 / r_non_endo)

sample <- sdf_bind_rows(endo, non_endo)

sdf_nrow(sample)


#--------------------------
# Check sample is small enough to be collected in R memory
#--------------------------

sample_r <- collect(sample)

nrow(sample_r)


#--------------------------
# Save dataset
#--------------------------

# Delete dataset in SQL if a version already exists
sql <- paste0('DROP TABLE IF EXISTS ...')
DBI::dbExecute(sc, sql)

# Save the dataset in SQL
sdf_register(sample, 'sample')
sql <- paste0('CREATE TABLE ... AS SELECT * FROM sample')
DBI::dbExecute(sc, sql)


#-------------#
# END OF FILE #
#-------------#

