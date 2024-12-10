####################################################################################################################################
# Data checks
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Load data
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset))

df <- df %>%
  mutate(utla = case_when(utla == "Bristol" ~ "Bristol, City of",
                          utla == "Herefordshire" ~ "Herefordshire, County of",
                          utla == "Kingston upon Hull" ~ "Kingston upon Hull, City of",
                          TRUE ~ utla)) %>%
  mutate(age_at_diag_final_5 = case_when(age_at_diag_final_5 == "<10" ~ "<15",
                          age_at_diag_final_5 == "10-14" ~ "<15",
                          TRUE ~ age_at_diag_final_5)) %>%
  mutate(qualifications_25_plus = ifelse(age_census >= 25, qualifications, "Aged 0 to 24 years on Census Day"))
  
# main analysis
df_main <- filter(df, population_main %in% c("endo", "no_endo"))

# supplementary analysis
df_supplementary <- filter(df, population_supplementary %in% c("endo", "no_endo"))

#--------------------------
# Check age at diagnosis variables
#--------------------------

#--------------------------
# Main analysis
#--------------------------

# Plot age at diagnosis

df_table <- df_main %>%
  filter(diag_endo == 1) %>%
  select(age_at_diag_final) %>%
  mutate(age_at_diag = case_when(age_at_diag_final < 15 ~ "<15",
                                 age_at_diag_final > 85 ~ ">85",
                                 TRUE ~ age_at_diag_final)) %>%
  group_by(age_at_diag) %>%
  count() %>%
  ungroup() %>%
  collect()

tot <- df_table %>% 
  summarise(n = sum(n)) %>% 
  pull()

df_table2 <- df_table %>%
  add_row(age_at_diag = "Total", n = tot) %>%
  mutate(n = round(n / 5) * 5) %>%
  arrange(age_at_diag)
print(df_table2)

write.csv(df_table2, 
          paste0(out_path, "age_at_first_endo_diag_main.csv"),
          row.names = FALSE)

df_plot <- df_main %>%
  filter(diag_endo == 1) %>%
  select(age_at_diag_final) %>%
  filter(age_at_diag_final >= 15 & age_at_diag_final <= 85) %>%
  group_by(age_at_diag_final) %>%
  count() %>%
  ungroup() %>%
  mutate(n = round(n / 5) * 5) %>%
  collect() %>%
  arrange(age_at_diag_final)

ggplot(data = df_plot, 
       aes(x = age_at_diag_final, 
           y = n)) +
  geom_bar(stat = "identity", fill = "#206095") +
  labs(title = "Distribution of age at first primary or secondary endometriosis diagnosis, ages 15-85",
       x = "Age at first primary or secondary endometriosis diagnosis (years)",
       y = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 40, size = 1.5) +
  theme_bw()

ggsave(paste0(out_path_plots, "plot_age_at_first_endo_diag_main.pdf"),
       width = 10, height = 4, units = "in") 

df_plot_grouped <- df_main %>%
  filter(diag_endo == 1) %>%
  select(age_at_diag_final_5) %>%
  group_by(age_at_diag_final_5) %>%
  count() %>%
  ungroup() %>%
  collect()

tot_grouped <- df_plot_grouped %>% 
  summarise(n = sum(n)) %>% 
  pull()

df_plot_grouped <- df_plot_grouped %>%
  mutate(n = round(n / 5) * 5) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  add_row(age_at_diag_final_5 = "Total", n = tot_grouped, Percentage = 100) %>%
  mutate(n = round(n / 5) * 5) %>%
  arrange(age_at_diag_final_5)
print(df_plot_grouped)

write.csv(df_plot_grouped, 
          paste0(out_path, "age_at_first_endo_diag_grouped_main.csv"),
          row.names = FALSE)

ggplot(data = df_plot_grouped %>% filter(age_at_diag_final_5 != "Total"), 
       aes(x = age_at_diag_final_5, 
           y = n)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), fill = "#206095") +
  labs(title = "Distribution of age at first primary or secondary endometriosis diagnosis",
       x = "Age at first primary or secondary endometriosis diagnosis (years)",
       y = "") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = "40-44", size = 1.5) +
  theme_bw()

ggsave(paste0(out_path_plots, "plot_age_at_first_endo_diag_grouped_main.pdf"),
       width = 10, height = 4, units = "in") 

df_age_at_diag_summary <- df_main %>%
  filter(diag_endo == 1) %>%
  summarise(mean_age_at_diag = mean(age_at_diag_final),
            min_age_at_diag = min(age_at_diag_final),
            Q1_age_at_diag = quantile(age_at_diag_final, probs = 0.25),
            median_age_at_diag = median(age_at_diag_final), 
            Q3_age_at_diag = quantile(age_at_diag_final, probs = 0.75),
            max_age_at_diag = max(age_at_diag_final))
print(df_age_at_diag_summary)

write.csv(df_age_at_diag_summary, 
          paste0(out_path, "age_at_first_endo_diag_summary_main.csv"),
          row.names = FALSE)


#--------------------------
# Supplementary analysis
#--------------------------

# Plot age at diagnosis

df_table <- df_supplementary %>%
  filter(diag_endo_primary == 1) %>%
  select(age_at_diag_final) %>%
  mutate(age_at_diag = case_when(age_at_diag_final < 15 ~ "<15",
                                 age_at_diag_final > 85 ~ ">85",
                                 TRUE ~ age_at_diag_final)) %>%
  group_by(age_at_diag) %>%
  count() %>%
  ungroup() %>%
  collect()

tot <- df_table %>% 
  summarise(n = sum(n)) %>% 
  pull()

df_table2 <- df_table %>%
  add_row(age_at_diag = "Total", n = tot) %>%
  mutate(n = round(n / 5) * 5) %>%
  arrange(age_at_diag)
print(df_table2)

write.csv(df_table2, 
          paste0(out_path, "age_at_first_endo_diag_supplementary.csv"),
          row.names = FALSE)

df_plot <- df_supplementary %>%
  filter(diag_endo_primary == 1) %>%
  select(age_at_diag_final) %>%
  filter(age_at_diag_final >= 15 & age_at_diag_final <= 85) %>%
  group_by(age_at_diag_final) %>%
  count() %>%
  ungroup() %>%
  mutate(n = round(n / 5) * 5) %>%
  collect() %>%
  arrange(age_at_diag_final)

ggplot(data = df_plot, 
       aes(x = age_at_diag_final, 
           y = n)) +
  geom_bar(stat = "identity", fill = "#206095") +
  labs(title = "Distribution of age at first primary endometriosis diagnosis, ages 15-85",
       x = "Age at first primary endometriosis diagnosis (years)",
       y = "") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_vline(xintercept = 35, size = 1.5) +
  theme_bw()

ggsave(paste0(out_path_plots, "plot_age_at_first_endo_diag_supplementary.pdf"),
       width = 10, height = 4, units = "in") 

df_plot_grouped <- df_supplementary %>%
  filter(diag_endo_primary == 1) %>%
  select(age_at_diag_final_5) %>%
  group_by(age_at_diag_final_5) %>%
  count() %>%
  ungroup() %>%
  collect()

tot_grouped <- df_plot_grouped %>% 
  summarise(n = sum(n)) %>% 
  pull()

df_plot_grouped <- df_plot_grouped %>%
  mutate(n = round(n / 5) * 5) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  add_row(age_at_diag_final_5 = "Total", n = tot_grouped, Percentage = 100) %>%
  mutate(n = round(n / 5) * 5) %>%
  arrange(age_at_diag_final_5)
print(df_plot_grouped)

write.csv(df_plot_grouped, 
          paste0(out_path, "age_at_first_endo_diag_grouped_supplementary.csv"),
          row.names = FALSE)

ggplot(data = df_plot_grouped %>% filter(age_at_diag_final_5 != "Total"), 
       aes(x = age_at_diag_final_5, 
           y = n)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), fill = "#206095") +
  labs(title = "Distribution of age at first primary endometriosis diagnosis",
       x = "Age at first primary endometriosis diagnosis (years)",
       y = "") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = "35-39", size = 1.5) +
  theme_bw()

ggsave(paste0(out_path_plots, "plot_age_at_first_endo_diag_grouped_supplementary.pdf"),
       width = 10, height = 4, units = "in") 

df_age_at_diag_summary <- df_supplementary %>%
  filter(diag_endo_primary == 1) %>%
  summarise(mean_age_at_diag = mean(age_at_diag_final),
            min_age_at_diag = min(age_at_diag_final),
            Q1_age_at_diag = quantile(age_at_diag_final, probs = 0.25),
            median_age_at_diag = median(age_at_diag_final), 
            Q3_age_at_diag = quantile(age_at_diag_final, probs = 0.75),
            max_age_at_diag = max(age_at_diag_final))
print(df_age_at_diag_summary)

write.csv(df_age_at_diag_summary, 
          paste0(out_path, "age_at_first_endo_diag_summary_supplementary.csv"),
          row.names = FALSE)


#--------------------------
# Emergency admission flag
#--------------------------

# main analysis
df_emerg_main <- df_main %>%
  group_by(population_main_split, emergency_admission_flag) %>%
  tally() %>%
  ungroup() %>%
  complete(population_main_split, emergency_admission_flag, fill = list(n = 0)) %>%
  mutate(n = round(n / 5) * 5) %>%
  group_by(population_main_split) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(population_main_split, emergency_admission_flag) %>%
  collect() %>%
  mutate(population = "main") %>%
  rename(population_split = population_main_split)

df_emerg_main %>% data.frame()

# supplementary analysis
df_emerg_supplementary <- df_supplementary %>%
  group_by(population_supplementary_split, emergency_admission_flag) %>%
  tally() %>%
  ungroup() %>%
  complete(population_supplementary_split, emergency_admission_flag, fill = list(n = 0)) %>%
  mutate(n = round(n / 5) * 5) %>%
  group_by(population_supplementary_split) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  arrange(population_supplementary_split, emergency_admission_flag) %>%
  collect() %>%
  mutate(population = "supplementary") %>%
  rename(population_split = population_supplementary_split)

df_emerg_supplementary %>% data.frame()

# bind datasets
df_emerg <- bind_rows(df_emerg_main, df_emerg_supplementary) %>%
  relocate(population, .before = everything()) %>%
  filter(population_split != "in_cen_only_no_endo")

# save
write.csv(df_emerg, 
          paste0(out_path, "emergency_admissions.csv"),
          row.names = FALSE)

# remove counts and round percentages to 1dp
df_emerg <- df_emerg %>%
  select(-n) %>%
  mutate(percentage = round(percentage, 1))

df_emerg %>% data.frame()

# save
write.csv(df_emerg, 
          paste0(out_path, "emergency_admissions_percentages_only.csv"),
          row.names = FALSE)


# Check individual ADMIMETH codes
admimeth <- df %>%
  group_by(emergency_admission_flag, ADMIMETH) %>%
  count() %>%
  ungroup() %>%
  arrange(emergency_admission_flag, ADMIMETH) %>%
  collect()

admimeth %>% data.frame()


#-------------#
# END OF FILE #
#-------------#
