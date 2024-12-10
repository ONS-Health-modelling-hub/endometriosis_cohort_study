####################################################################################################################################
# Test multicollinearity
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Load sample of data to use glm function
#--------------------------

sample_r <- sdf_sql(sc, paste0("SELECT * FROM ", sample_dataset)) %>%
  collect()

nrow(sample_r)

#--------------------------
# Run models
#-------------------------

# full model
model1 <- glm(diag_endo ~ 
              splines::ns(age_census_num, 
                          df = 8, 
                          knots = quantile(age_census_num, c(0.17, 0.33, 0.5, 0.67, 0.83)),
                          Boundary.knots = quantile(age_census_num, c(0.01, 0.99))) +
              ethnicity + disability + num_epi_pre_study_grouped + 
              region + rural_urban + nssec_hh + imd_decile + qualifications + general_health + 
              cob + main_language + religion,
              sample_r, 
              family = "binomial",
              weights = sampling_weight)

vif_res1 <- rms::vif(model1)

df_vif1 <- data.frame(name = attr(vif_res1, "names"),
                     vif = vif_res1) %>% 
  arrange(desc(vif)) %>%
  mutate(formula = paste0(model1$formula[3])) %>%
  mutate(model = 1) %>%
  mutate(population = "all") %>%
  relocate(formula, .before = everything()) %>%
  relocate(model, .before = everything()) %>%
  relocate(population, .before = everything())
df_vif1

# with aggregated general health
model2 <- glm(diag_endo ~ 
              splines::ns(age_census_num, 
                          df = 8, 
                          knots = quantile(age_census_num, c(0.17, 0.33, 0.5, 0.67, 0.83)),
                          Boundary.knots = quantile(age_census_num, c(0.01, 0.99))) +
              ethnicity + disability + num_epi_pre_study_grouped + 
              region + rural_urban + nssec_hh + imd_decile + qualifications + general_health_agg + 
              cob + main_language + religion,
              sample_r, 
              family = "binomial",
              weights = sampling_weight)

vif_res2 <- rms::vif(model2)

df_vif2 <- data.frame(name = attr(vif_res2, "names"),
                     vif = vif_res2) %>% 
  arrange(desc(vif)) %>%
  mutate(formula = paste0(model2$formula[3])) %>%
  mutate(model = 2) %>%
  mutate(population = "all") %>%
  relocate(formula, .before = everything()) %>%
  relocate(model, .before = everything()) %>%
  relocate(population, .before = everything())
df_vif2

# without religion
model3 <- glm(diag_endo ~ 
              splines::ns(age_census_num, 
                          df = 8, 
                          knots = quantile(age_census_num, c(0.17, 0.33, 0.5, 0.67, 0.83)),
                          Boundary.knots = quantile(age_census_num, c(0.01, 0.99))) +
              ethnicity + disability + num_epi_pre_study_grouped + 
              region + rural_urban + nssec_hh + imd_decile + qualifications + general_health_agg + 
              cob + main_language, #religion +
              sample_r, 
              family = "binomial",
              weights = sampling_weight)

vif_res3 <- rms::vif(model3)

df_vif3 <- data.frame(name = attr(vif_res3, "names"),
                     vif = vif_res3) %>% 
  arrange(desc(vif)) %>%
  mutate(formula = paste0(model3$formula[3])) %>%
  mutate(model = 3) %>%
  mutate(population = "all") %>%
  relocate(formula, .before = everything()) %>%
  relocate(model, .before = everything()) %>%
  relocate(population, .before = everything())
df_vif3

# without religion and qualifications
model4  <- glm(diag_endo ~ 
              splines::ns(age_census_num, 
                          df = 8, 
                          knots = quantile(age_census_num, c(0.17, 0.33, 0.5, 0.67, 0.83)),
                          Boundary.knots = quantile(age_census_num, c(0.01, 0.99))) +
              ethnicity + disability + num_epi_pre_study_grouped + 
              region + rural_urban + nssec_hh + imd_decile #+ qualifications 
              + general_health_agg + 
              cob + main_language, #religion +
              sample_r, 
              family = "binomial",
              weights = sampling_weight)

vif_res4 <- rms::vif(model4)

df_vif4 <- data.frame(name = attr(vif_res4, "names"),
                     vif = vif_res4) %>% 
  arrange(desc(vif)) %>%
  mutate(formula = paste0(model4$formula[3])) %>%
  mutate(model = 4) %>%
  mutate(population = "all") %>%
  relocate(formula, .before = everything()) %>%
  relocate(model, .before = everything()) %>%
  relocate(population, .before = everything())
df_vif4

# with age groups instead of age spline
model5  <- glm(diag_endo ~ 
               age_group_5 +
               ethnicity + disability + num_epi_pre_study_grouped + 
               region + rural_urban + nssec_hh + imd_decile + 
               + general_health_agg + #qualifications +
               cob + main_language, #religion +
               sample_r, 
               family = "binomial",
               weights = sampling_weight)

vif_res5 <- rms::vif(model5)
df_vif5 <- data.frame(name = attr(vif_res5, "names"),
                     vif = vif_res5) %>% 
  arrange(desc(vif)) %>%
  mutate(formula = paste0(model5$formula[3])) %>%
  mutate(model = 5) %>%
  mutate(population = "all") %>%
  relocate(formula, .before = everything()) %>%
  relocate(model, .before = everything()) %>%
  relocate(population, .before = everything())
df_vif5

# age and health only
model6 <- glm(diag_endo ~ 
              splines::ns(age_census_num, 
                          df = 8, 
                          knots = quantile(age_census_num, c(0.17, 0.33, 0.5, 0.67, 0.83)),
                          Boundary.knots = quantile(age_census_num, c(0.01, 0.99))) +
              general_health_agg + 
              disability + 
              num_epi_pre_study_grouped, 
              sample_r, 
              family = "binomial",
              weights = sampling_weight)

vif_res6 <- rms::vif(model6)
df_vif6 <- data.frame(name = attr(vif_res6, "names"),
                     vif = vif_res6) %>% 
  arrange(desc(vif)) %>%
  mutate(formula = paste0(model6$formula[3])) %>%
  mutate(model = 6) %>%
  mutate(population = "all") %>%
  relocate(formula, .before = everything()) %>%
  relocate(model, .before = everything()) %>%
  relocate(population, .before = everything())
df_vif6


# bind outputs
df_vif <- rbind(df_vif1,
                df_vif2,
                df_vif3,
                df_vif4,
                df_vif5,
                df_vif6)

# save outputs
write.csv(df_vif, paste0(out_path, 'vif.csv'))


#-------------#
# END OF FILE #
#-------------#
