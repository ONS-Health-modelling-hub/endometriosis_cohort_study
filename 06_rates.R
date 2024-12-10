####################################################################################################################################
# Rates
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Read in the linked dataset 
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset)) 

# Add dummy variable to count all people by endo diagnosis
df <-  df %>%
  mutate(total = "Total") %>%
  mutate(utla = case_when(utla == "Bristol" ~ "Bristol, City of",
                          utla == "Herefordshire" ~ "Herefordshire, County of",
                          utla == "Kingston upon Hull" ~ "Kingston upon Hull, City of",
                          TRUE ~ utla)) %>%
  mutate(qualifications_25_plus = ifelse(age_census >= 25, qualifications, "Aged 0 to 24 years on Census Day"))

# main analysis
df_main <- filter(df, population_main %in% c("endo", "no_endo"))

# supplementary analysis
df_supplementary <- filter(df, population_supplementary %in% c("endo", "no_endo"))

exposures <- c("total", exposures)

#--------------------------
# Read in the Poisson data
#--------------------------

poisson <- spark_read_csv(
  sc, 
  name = "poisson", 
  path = "...", 
  header = TRUE, 
  delimiter = ",")

# Poisson editing
poisson <- poisson %>%
  sdf_coalesce(1) %>%
  collect() %>%
  rename(count = Deaths)

#--------------------------
# Get weights for age-standardised rates
#--------------------------

# main analysis

# Calculate weighted populations
df_main_unweighted_population <- df_main %>%
  group_by(age_group_5) %>%
  summarise(pop = n()) %>%
  sdf_coalesce(1) %>%
  collect()

# Calculate total pop
total_df_main_unweighted <- sum(df_main_unweighted_population$pop)


# supplementary analysis

# Calculate weighted populations
df_supplementary_unweighted_population <- df_supplementary %>%
  group_by(age_group_5) %>%
  summarise(pop = n()) %>%
  sdf_coalesce(1) %>%
  collect()

# Calculate total pop
total_df_supplementary_unweighted <- sum(df_supplementary_unweighted_population$pop)

#--------------------------
# Crude and age-standardised rates
#--------------------------

# main analysis

rates_table_main <- data.frame()

for (e in exposures) {
  
  print(e)
  
  rates <- get_age_standardised_rates(
    data = df_main,
    ESP_data = df_main_unweighted_population,
    ESP_data_variable = "pop",
    poisson_data = poisson, 
    total_esp = total_df_main_unweighted,
    outcome = "diag_endo",
    type_of_outcome = "count",
    age_grp = "age_group_5",
    group_vars = e) %>%
    mutate(outcome = "diag_endo",
           domain = e) %>%
    rename(group = all_of(e)) %>%
    mutate(group = as.character(group)) %>%
    relocate(domain, .before = group) %>%
    relocate(outcome, .before = domain) %>%
    arrange(factor(group, levels = !!sym(paste0("order_", e)))) %>%
    mutate(population = "main") %>%
    relocate(population, .before = everything()) %>%
    select(-pops, -unweighted_count) %>%
    mutate(disclosive = case_when((count < 10 |
                                   sample < 10) ~ "1",                                  
                                   TRUE ~ "0")) %>%
    mutate(count = ifelse(count < 10, -10, count),
           sample = ifelse(sample < 10, -10, sample)) %>%
    mutate(across(.cols = contains("rate"),
                  .fns = ~ ifelse(disclosive == "1", -10, .x))) %>%
    mutate(across(.cols = c("count", "sample"),
                .fns = ~ round(.x / 5) * 5)) %>%
    mutate(across(.cols = contains("rate"),
                  .fns = ~ round(.x, 2))) %>%
    select(-disclosive)

  rates_table_main <- bind_rows(rates_table_main, rates)
  
}

print(rates_table_main)


# supplementary analysis

rates_table_supplementary <- data.frame()

for (e in exposures) {
  
  print(e)
  
  rates <- get_age_standardised_rates(
    data = df_supplementary,
    ESP_data = df_supplementary_unweighted_population,
    ESP_data_variable = "pop",
    poisson_data = poisson, 
    total_esp = total_df_supplementary_unweighted,
    outcome = "diag_endo_primary",
    type_of_outcome = "count",
    age_grp = "age_group_5",
    group_vars = e) %>%
    mutate(outcome = "diag_endo_primary",
           domain = e) %>%
    rename(group = all_of(e)) %>%
    mutate(group = as.character(group)) %>%
    relocate(domain, .before = group) %>%
    relocate(outcome, .before = domain) %>%
    arrange(factor(group, levels = !!sym(paste0("order_", e)))) %>%
    mutate(population = "supplementary") %>%
    relocate(population, .before = everything()) %>%
    select(-pops, -unweighted_count) %>%
    mutate(disclosive = case_when((count < 10 |
                                   sample < 10) ~ "1",                                  
                                   TRUE ~ "0")) %>%
    mutate(count = ifelse(count < 10, -10, count),
           sample = ifelse(sample < 10, -10, sample)) %>%
    mutate(across(.cols = contains("rate"),
                  .fns = ~ ifelse(disclosive == "1", -10, .x))) %>%
    mutate(across(.cols = c("count", "sample"),
                .fns = ~ round(.x / 5) * 5)) %>%
    mutate(across(.cols = contains("rate"),
                  .fns = ~ round(.x, 2))) %>%
    select(-disclosive)

  rates_table_supplementary <- bind_rows(rates_table_supplementary, rates)
  
}

print(rates_table_supplementary)


# Bind datasets
rates_bind <- bind_rows(rates_table_main, 
                        rates_table_supplementary)


#--------------------------
# Apply secondary suppression
#--------------------------

if (run_type == "2_year_follow_up") {  
  rates_bind_suppressed <- rates_bind %>%
    mutate(across(.cols = contains(c("count", "crude_rate")),
                  .fns = ~ ifelse(group %in% c("<10", "10-14"), -10, .)))  
} else {
  rates_bind_suppressed <- rates_bind  
}


#--------------------------
# Save output
#--------------------------

write_xlsx(rates_bind_suppressed, paste0(out_path, "endo_rates.xlsx"))


#--------------------------
# Plots for age-standardised rates
#--------------------------

rates_bind <- read_xlsx(paste0(out_path, "endo_rates.xlsx")) %>%
  mutate(across(.cols = contains("rate"),
                .fns = ~ as.numeric(.))) %>%
  filter(count != -10)

# remove "total"
exposures <- exposures[exposures != "total"]


# main analysis

for (i in exposures[exposures != "age_group_5"]) {

  rates_table_subset <- rates_bind %>%
    filter(domain == i,
           population == "main")
  
  ggplot(data = rates_table_subset,
         aes(x = fct_relevel(group, !!sym(paste0("order_", i))),
             y = age_standardised_rate)) +
   geom_bar(position = "dodge", stat = "identity", fill = "#206095") +
   geom_errorbar(aes(ymin = age_standardised_rate_lower_ci, 
                     ymax = age_standardised_rate_upper_ci),
                 width = 0.4, 
                 position = position_dodge(width = 0.9)) +
   labs(title = paste0("Age-standardised rates per 100,000 people, ", exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]),
        y = "Age-standardised rate per 100,000 people", 
        x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]) +
   scale_x_discrete(limits = rev) +
   #scale_y_continuous(limits = c(plot_lim_min, plot_lim_max)) +
   scale_y_continuous(labels = scales::comma) +
   theme_bw() + 
   theme(legend.position = "bottom") +
   guides(colour = guide_legend(title = NULL)) +
   guides(fill = guide_legend(reverse = TRUE)) +
   coord_flip()
  
  ggsave(paste0(out_path_plots, "plot_age_standardised_rates_main_", i, ".pdf"),
       width = 10, height = 4, units = "in")

}


# supplementary analysis

for (i in exposures[exposures != "age_group_5"]) {

  rates_table_subset <- rates_bind %>%
    filter(domain == i,
           population == "supplementary")
  
  ggplot(data = rates_table_subset,
         aes(x = fct_relevel(group, !!sym(paste0("order_", i))),
             y = age_standardised_rate)) +
   geom_bar(position = "dodge", stat = "identity", fill = "#206095") +
   geom_errorbar(aes(ymin = age_standardised_rate_lower_ci, 
                     ymax = age_standardised_rate_upper_ci),
                 width = 0.4, 
                 position = position_dodge(width = 0.9)) +
   labs(title = paste0("Age-standardised rates per 100,000 people, ", exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]),
        y = "Age-standardised rate per 100,000 people", 
        x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]) +
   scale_x_discrete(limits = rev) +
   #scale_y_continuous(limits = c(plot_lim_min, plot_lim_max)) +
   scale_y_continuous(labels = scales::comma) +
   theme_bw() + 
   theme(legend.position = "bottom") +
   guides(colour = guide_legend(title = NULL)) +
   guides(fill = guide_legend(reverse = TRUE)) +
   coord_flip()
  
  ggsave(paste0(out_path_plots, "plot_age_standardised_rates_supplementary_", i, ".pdf"),
       width = 10, height = 4, units = "in")

}


#--------------------------
# Plots for crude rates for age group exposure
#--------------------------

# main analysis

for (i in "age_group_5") {

  rates_table_subset <- rates_bind %>%
    filter(domain == i,
           population == "main")
  
  ggplot(data = rates_table_subset,
         aes(x = fct_relevel(group, !!sym(paste0("order_", i))),
             y = crude_rate)) +
   geom_bar(position = "dodge", stat = "identity", fill = "#206095") +
   geom_errorbar(aes(ymin = crude_rate_lower_ci, 
                     ymax = crude_rate_upper_ci),
                 width = 0.4, 
                 position = position_dodge(width = 0.9)) +
   labs(title = paste0("Crude rates per 100,000 people, ", exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]),
        y = "Crude rate per 100,000 people", 
        x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]) +
   scale_x_discrete(limits = rev) +
   #scale_y_continuous(limits = c(plot_lim_min, plot_lim_max)) +
   scale_y_continuous(labels = scales::comma) +
   theme_bw() + 
   coord_flip()
  
  ggsave(paste0(out_path_plots, "plot_crude_rates_main_", i, ".pdf"),
         width = 10, height = 4, units = "in")

}


# supplementary analysis

for (i in "age_group_5") {

  rates_table_subset <- rates_bind %>%
    filter(domain == i,
           population == "supplementary")
  
  ggplot(data = rates_table_subset,
         aes(x = fct_relevel(group, !!sym(paste0("order_", i))),
             y = crude_rate)) +
   geom_bar(position = "dodge", stat = "identity", fill = "#206095") +
   geom_errorbar(aes(ymin = crude_rate_lower_ci, 
                     ymax = crude_rate_upper_ci),
                 width = 0.4, 
                 position = position_dodge(width = 0.9)) +
   labs(title = paste0("Crude rates per 100,000 people, ", exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]),
        y = "Crude rate per 100,000 people", 
        x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]) +
   scale_x_discrete(limits = rev) +
   #scale_y_continuous(limits = c(plot_lim_min, plot_lim_max)) +
   scale_y_continuous(labels = scales::comma) +
   theme_bw() + 
   coord_flip()
  
  ggsave(paste0(out_path_plots, "plot_crude_rates_supplementary_", i, ".pdf"),
         width = 10, height = 4, units = "in")

}


#--------------------------
# Geographical heatmaps
#--------------------------

# main analysis

# region census
df_region <- rates_bind %>%
  filter(domain == "region",
         population == "main")

ggplot(data = df_region, 
       aes(x = domain, 
           y = fct_relevel(group, rev(order_region)), 
           fill = age_standardised_rate))  +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "#D9E9F7", high = "#206095", labels = scales::comma) +
  theme_minimal() +  
  geom_text(aes(label = paste0(age_standardised_rate, " (", age_standardised_rate_lower_ci, ", ", age_standardised_rate_upper_ci, ")")), size = 3) + 
  labs(title = "Age-standardised rates per 100,000 people: Region",
       x = "Region",
       y = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())

ggsave(paste0(out_path_plots, "plot_asrs_heatmap_main_region.pdf"),
       width = 10, height = 4, units = "in")

# UTLA census
df_utla <- rates_bind %>%
  filter(domain == "utla",
         population == "main")

ggplot(data = df_utla, 
       aes(x = domain, 
           y = fct_relevel(group, rev(order_utla)), 
           fill = age_standardised_rate))  +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "#D9E9F7", high = "#206095", labels = scales::comma) +
  theme_minimal() +  
  geom_text(aes(label = paste0(age_standardised_rate, " (", age_standardised_rate_lower_ci, ", ", age_standardised_rate_upper_ci, ")")), size = 3) + 
  labs(title = "Age-standardised rates per 100,000 people: UTLA",
       x = "UTLA",
       y = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())

ggsave(paste0(out_path_plots, "plot_asrs_heatmap_main_utla.pdf"),
       width = 10, height = 4, units = "in")


# supplementary analysis

# region census
df_region <- rates_bind %>%
  filter(domain == "region",
         population == "supplementary")

ggplot(data = df_region, 
       aes(x = domain, 
           y = fct_relevel(group, rev(order_region)), 
           fill = age_standardised_rate))  +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "#D9E9F7", high = "#206095", labels = scales::comma) +
  theme_minimal() +  
  geom_text(aes(label = paste0(age_standardised_rate, " (", age_standardised_rate_lower_ci, ", ", age_standardised_rate_upper_ci, ")")), size = 3) + 
  labs(title = "Age-standardised rates per 100,000 people: Region",
       x = "Region",
       y = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())

ggsave(paste0(out_path_plots, "plot_asrs_heatmap_supplementary_region.pdf"),
       width = 10, height = 4, units = "in")

# UTLA census
df_utla <- rates_bind %>%
  filter(domain == "utla",
         population == "supplementary")

ggplot(data = df_utla, 
       aes(x = domain, 
           y = fct_relevel(group, rev(order_utla)), 
           fill = age_standardised_rate))  +
  geom_tile(colour = "white") + 
  scale_fill_gradient(low = "#D9E9F7", high = "#206095", labels = scales::comma) +
  theme_minimal() +  
  geom_text(aes(label = paste0(age_standardised_rate, " (", age_standardised_rate_lower_ci, ", ", age_standardised_rate_upper_ci, ")")), size = 3) + 
  labs(title = "Age-standardised rates per 100,000 people: UTLA",
       x = "UTLA",
       y = "") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.title = element_blank())

ggsave(paste0(out_path_plots, "plot_asrs_heatmap_supplementary_utla.pdf"),
       width = 10, height = 4, units = "in")


#-------------#
# END OF FILE #
#-------------#

