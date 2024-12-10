####################################################################################################################################
# Models plots
####################################################################################################################################

source(".../00_session_setup.R")

ref_cat <- read.csv(paste0(import_path, "ref_cat.csv"))

standard_models <- c("unadj",
                     "age_adj", 
                     "age_health_adj")

analysis_types <- c("main_10_year",
                    "supplementary_10_year",
                    "main_2_year",
                    "supplementary_2_year")

analysis_type_labeller <- c("main_10_year"      = "Main, 10 year follow up",
                            "supplementary_10_year" = "Supplementary, 10 year follow up",
                            "main_2_year"       = "Main, 2 year follow up",
                            "supplementary_2_year"  = "Supplementary, 2 year follow up")

#--------------------------
# Load data
#--------------------------

df_list <- list()

for (m in standard_models) {

    df_all_main <- read_xlsx(
      paste0("endo/10_year_follow_up/", "endo_models_main_", m, ".xlsx"), "models_all") %>% 
      mutate(model = m) %>%
      mutate(analysis_type = "main_10_year")
    
    df_all_supplementary <- read_xlsx(
      paste0("endo/10_year_follow_up/", "endo_models_supplementary_", m, ".xlsx"), "models_all") %>% 
      mutate(model = m) %>%
      mutate(analysis_type = "supplementary_10_year")
  
    df_all_main_2_year <- read_xlsx(
      paste0("endo/2_year_follow_up/", "endo_models_main_", m, ".xlsx"), "models_all") %>% 
      mutate(model = m) %>%
      mutate(analysis_type = "main_2_year")
    
    df_all_supplementary_2_year <- read_xlsx(
      paste0("endo/2_year_follow_up/", "endo_models_supplementary_", m, ".xlsx"), "models_all") %>% 
      mutate(model = m) %>%
      mutate(analysis_type = "supplementary_2_year")
 
    df_list[[m]] <- bind_rows(df_all_main, df_all_supplementary,
                              df_all_main_2_year, df_all_supplementary_2_year)
}

# bind model outputs
df_bind <- bind_rows(df_list)


# bind to exploratory models
df_exploratory_main <- read_xlsx(
  paste0(import_path, "endo_models_main_exploratory.xlsx"), "models_exploratory") %>% 
  mutate(model = "exploratory") %>%
  mutate(analysis_type = "main_10_year")

df_exploratory_supplementary <- read_xlsx(
  paste0(import_path, "endo_models_supplementary_exploratory.xlsx"), "models_exploratory") %>% 
  mutate(model = "exploratory") %>%
  mutate(analysis_type = "supplementary_10_year")

df_exploratory_main_2_year <- read_xlsx(
  paste0(import_path, "endo_models_main_exploratory.xlsx"), "models_exploratory") %>% 
  mutate(model = "exploratory") %>%
  mutate(analysis_type = "main_2_year")

df_exploratory_supplementary_2_year <- read_xlsx(
  paste0(import_path, "endo_models_supplementary_exploratory.xlsx"), "models_exploratory") %>% 
  mutate(model = "exploratory") %>%
  mutate(analysis_type = "supplementary_2_year")


# Bind datasets
df_bind <- bind_rows(df_bind, df_exploratory_main, df_exploratory_supplementary,
                     df_exploratory_main_2_year, df_exploratory_supplementary_2_year)


#--------------------------
# Apply secondary suppression
#--------------------------

df_bind_suppressed <- df_bind %>%
  mutate(across(.cols = contains(c("estimate", "std.error", "statistic", "p.value", "lower_ci", "upper_ci", "exp_est", "exp_lower_ci", "exp_upper_ci")),
                .fns = ~ ifelse(analysis_type %in% c("main_2_year", "supplementary_2_year") &
                                term %in% c("age_group_5_<10", "age_group_5_10-14"),
                                -10, .)))

#--------------------------
# Save output
#--------------------------

# save
write_xlsx(df_bind_suppressed, paste0(out_path, "endo_models_bind.xlsx"))


#--------------------------
# Create plots
#--------------------------

df_bind <- read_xlsx(paste0(out_path, "endo_models_bind.xlsx")) %>%
  filter(estimate != -10 &
         std.error != -10 &
         statistic != -10 &
         p.value != -10 &
         lower_ci != -10 &
         upper_ci != -10 &
         exp_est != -10 &
         exp_lower_ci != -10 &
         exp_upper_ci != -10)

plot_lim_min <- 0
plot_lim_max <- 2.5

df_bind <- df_bind %>% 
  mutate(exploratory_flag = ifelse(model == "exploratory" | exposure == "qualifications_imd_hhnssec", 1, 0)) %>%
  mutate(model = case_when(
    model == "age_health_adj" & exposure == "age_group_5" ~ "health_adj",    
    model == "exploratory" & exposure == "qualifications" & model_id == "Model_4" ~ "25+_unadj",
    model == "exploratory" & exposure == "qualifications" & model_id == "Model_5" ~ "25+_age_adj",
    model == "exploratory" & exposure == "qualifications" & model_id == "Model_6" ~ "25+_age_health_adj",
    model == "exploratory" & exposure == "region" & model_id == "Model_4" ~ "age_health_imd_adj",
    model == "exploratory" & exposure == "region" & model_id == "Model_5" ~ "age_health_imd_qual_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_4" ~ "age_health_cob_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_5" ~ "age_health_main_lang_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_6" ~ "age_health_cob_main_lang_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_7" ~ "born_in_uk_age_health_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_8" ~ "born_outside_uk_age_health_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_9" ~ "main_lang_eng_age_health_adj",
    model == "exploratory" & exposure %in% c("ethnicity", "ethnicity_detailed", "ethnicity_agg") & model_id == "Model_10" ~ "main_lang_not_eng_age_health_adj",
    TRUE ~ model))

# add separate rows for qualifications_imd_hhnssec models
df_bind_qualifications_imd_hhnssec <- df_bind %>%
  filter(exposure == "qualifications_imd_hhnssec") %>%
  mutate(model = case_when(model_id == "Model_1" ~ "imd_hhnssec_adj",
                           model_id == "Model_2" ~ "age_imd_hhnssec_adj",
                           model_id == "Model_3" ~ "age_health_imd_hhnssec_adj"))

df_bind_qual <- mutate(df_bind_qualifications_imd_hhnssec, exposure = "qualifications")
df_bind_imd <- mutate(df_bind_qualifications_imd_hhnssec, exposure = "imd_decile")
df_bind_hhnssec <- mutate(df_bind_qualifications_imd_hhnssec, exposure = "nssec_hh")

# bind to full dataset
df_bind <- df_bind %>% 
  bind_rows(df_bind_qual) %>%
  bind_rows(df_bind_imd) %>%
  bind_rows(df_bind_hhnssec)

# get all model names
models <- df_bind %>% 
  select(model) %>%
  distinct() %>%
  collect() %>%
  pull()


for (e in analysis_types) {
  
  for (i in exposures) {

    df_filtered <- df_bind %>% 
      filter(population == "all") %>%
      filter(analysis_type == e) %>%
      filter(exposure == i) %>%
      filter(str_detect(term, i)) %>%
      mutate(term = str_remove(term, paste0(i, "_")))

    plot <- plot_odds_ratios(df_filtered, 
                             variable = i, 
                             order = paste0("order_", i),
                             populations = "all")

    ggsave(paste0(out_path_plots, "plot_odds_ratios_", e, "_", i, ".pdf"),
           width = 10, height = 4, units = "in")

    # with unadjusted ORs removed
    df_filtered_adj <- filter(df_filtered, !str_detect(model, "unadj") & model != "imd_hhnssec_adj")

    # plot exploratory models
    df_filtered_adj_exploratory <- df_filtered_adj %>%
      filter(exploratory_flag == 1)

    if (nrow(df_filtered_adj_exploratory) > 0) {

      plot <- plot_odds_ratios(df_filtered_adj, 
                               variable = i, 
                               order = paste0("order_", i),
                               populations = "all")

      ggsave(paste0(out_path_plots, "plot_odds_ratios_adj_only_", e, "_", i, "_exploratory.pdf"),
             width = 10, height = 4, units = "in")

    }

    # plot standard models
    df_filtered_adj_standard <- df_filtered_adj %>%
      filter(exploratory_flag == 0)

    plot <- plot_odds_ratios(df_filtered_adj_standard, 
                             variable = i, 
                             order = paste0("order_", i),
                             populations = "all")

    ggsave(paste0(out_path_plots, "plot_odds_ratios_adj_only_", e, "_", i, ".pdf"),
           width = 10, height = 4, units = "in") 

  }

}

#--------------------------
# Create plots for all analysis types combined
#--------------------------

for (i in exposures) {

  df_filtered <- df_bind %>% 
    filter(population == "all") %>%
    filter(exposure == i) %>%
    filter(str_detect(term, i)) %>%
    mutate(term = str_remove(term, paste0(i, "_")))

  plot <- plot_odds_ratios_grid(df_filtered, 
                           variable = i, 
                           order = paste0("order_", i),
                           populations = "all")

  ggsave(paste0(out_path_plots, "plot_odds_ratios_bind_", i, ".pdf"),
         width = 10, height = 4, units = "in")

  # with unadjusted ORs removed
  df_filtered_adj <- filter(df_filtered, !str_detect(model, "unadj") & model != "imd_hhnssec_adj")

  # plot exploratory models
  df_filtered_adj_exploratory <- df_filtered_adj %>%
    filter(exploratory_flag == 1)

  if (nrow(df_filtered_adj_exploratory) > 0) {

    plot <- plot_odds_ratios_grid(df_filtered_adj, 
                             variable = i, 
                             order = paste0("order_", i),
                             populations = "all")

    ggsave(paste0(out_path_plots, "plot_odds_ratios_bind_adj_only_", i, "_exploratory.pdf"),
           width = 10, height = 4, units = "in")

  }

  # plot standard models
  df_filtered_adj_standard <- df_filtered_adj %>%
    filter(exploratory_flag == 0)

  plot <- plot_odds_ratios_grid(df_filtered_adj_standard, 
                           variable = i, 
                           order = paste0("order_", i),
                           populations = "all")

  ggsave(paste0(out_path_plots, "plot_odds_ratios_bind_adj_only_", i, ".pdf"),
         width = 10, height = 4, units = "in") 

}


#--------------------------
# Inspect AICs
#--------------------------

for (i in exposures) {
  
  df_filtered <- df_bind %>%
    group_by(exposure, model, population) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    filter(exposure == i) %>%
    select(analysis_type, population, exposure, model, AIC) %>%
    arrange(analysis_type, population)
  
  print(df_filtered)
  
}


#-------------#
# END OF FILE #
#-------------#


