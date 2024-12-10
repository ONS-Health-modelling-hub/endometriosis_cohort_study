####################################################################################################################################
# Descriptive tables and plots
####################################################################################################################################

source(".../00_session_setup.R")

#--------------------------
# Read in the linked dataset 
#--------------------------

df <- sdf_sql(sc, paste0("SELECT * FROM ", linked_dataset))

# Add dummy variable to count all people by endo diagnosis
df <- df %>%
  mutate(total = "Total") %>%
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


####################################################################################################################################
# Counts
####################################################################################################################################

#--------------------------
# Define breakdown variables
#--------------------------

demog_varsnames <- c("total", 
                     exposures, 
                     "num_epi_pre_study_grouped", 
                     "age_at_diag_final_5")
demog_varsnames


#--------------------------
# Get population counts
#--------------------------

# main analysis

demog_table <- data.frame()

demog_table <- purrr::pmap_dfr(.l = list(data = purrr::map(1:length(demog_varsnames), function(e) {return(df_main)}),
                                         demog_varsnames = demog_varsnames,
                                         grouping_var = "population_main"),
                                         .f = get_counts) %>%
    mutate(population = "main")

counts_main <- demog_table
print(counts_main)

# supplementary analysis

demog_table <- data.frame()

demog_table <- purrr::pmap_dfr(.l = list(data = purrr::map(1:length(demog_varsnames), function(e) {return(df_supplementary)}),
                                         demog_varsnames = demog_varsnames,
                                         grouping_var = "population_supplementary"),
                                         .f = get_counts) %>%
    mutate(population = "supplementary")

counts_supplementary <- demog_table
print(counts_supplementary)


#--------------------------
# Bind datasets
#--------------------------

counts_bind <- bind_rows(counts_main, 
                         counts_supplementary)

counts_bind <- counts_bind %>%
    relocate(population, .before = everything())

#--------------------------
# Apply secondary suppression
#--------------------------

# Age group - 2 year follow up
if (run_type == "2_year_follow_up") {  
  counts_bind_suppressed <- counts_bind %>%
    mutate(across(.cols = contains(c("n_endo", "percentage_endo", "n_no_endo", "percentage_no_endo", "rate_endo_diagnosis_per_100000_people")),
                  .fns = ~ ifelse(category == "age_group_5" & group %in% c("<10", "10-14"), -10, .)))
} else {
  counts_bind_suppressed <- counts_bind  
}

#--------------------------
# Save output
#--------------------------

write_xlsx(counts_bind_suppressed, paste0(out_path, "endo_counts.xlsx"))


####################################################################################################################################
# Plots
####################################################################################################################################

#--------------------------
# Define breakdown variables
#--------------------------

demog_varsnames_plots <- c(exposures, 
                           "num_epi_pre_study_grouped")
demog_varsnames_plots


#--------------------------
# Load population counts and proportions by characteristic
#--------------------------

counts_bind <- read_xlsx(paste0(out_path, "endo_counts.xlsx")) %>%
  filter(n_endo != -10 &
         percentage_endo != -10 &
         n_no_endo != -10 & 
         percentage_no_endo != -10 &
         rate_endo_diagnosis_per_100000_people != -10) %>%
  mutate(across(.cols = contains("percentage"),
                .fns = ~ as.numeric(.)))


# main analysis
counts_main <- filter(counts_bind, population == "main")

# supplementary analysis
counts_supplementary <- filter(counts_bind, population == "supplementary")


#--------------------------
# Plots of percentages by characteristic and population
#--------------------------

# main analysis 

for (i in demog_varsnames_plots) {
  
  print(i)
  
  df_counts <- counts_main %>%
    filter(category == i) %>%
    rename(!!sym(i) := group) %>% 
    select(-rate_endo_diagnosis_per_100000_people) %>%
    pivot_longer(
      cols = starts_with(c("n_", "percentage_")),
      names_to = c(".value", "sample"), 
      names_pattern = '(.*?)_(\\w+)') %>%
    filter(sample != "total")
      
  ggplot(data = df_counts, aes(x = fct_relevel(!!sym(i), !!sym(paste0("order_", i))), 
                               y = percentage, 
                               fill = sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Census sample",
         x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c("#27a0cc", "#206095")) +
    scale_x_discrete(limits = rev) +
    coord_flip()

  ggsave(paste0(out_path_plots, "plot_perc_main_", i, ".pdf"),
         width = 10, height = 4, units = "in") 

}

# supplementary analysis 

for (i in demog_varsnames_plots) {
  
  print(i)
  
  df_counts <- counts_supplementary %>%
    filter(category == i) %>%
    rename(!!sym(i) := group) %>% 
    select(-rate_endo_diagnosis_per_100000_people) %>%
    pivot_longer(
      cols = starts_with(c("n_", "percentage_")),
      names_to = c(".value", "sample"), 
      names_pattern = '(.*?)_(\\w+)') %>%
    filter(sample != "total")
      
  ggplot(data = df_counts, aes(x = fct_relevel(!!sym(i), !!sym(paste0("order_", i))), 
                               y = percentage, 
                               fill = sample)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Census sample",
         x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i]) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c("#27a0cc", "#206095")) +
    scale_x_discrete(limits = rev) +
    coord_flip()

  ggsave(paste0(out_path_plots, "plot_perc_supplementary_", i, ".pdf"),
         width = 10, height = 4, units = "in") 

}

#--------------------------
# Plots of percentages of endo diagnoses by characteristic and population
#--------------------------

# main analysis

for (i in demog_varsnames_plots) {
  
  print(i)
  
  df_counts <- counts_main %>%
    filter(category == i) %>% 
    select(category, group, rate_endo_diagnosis_per_100000_people) %>%
    rename(!!sym(i) := group)
  
  ggplot(data = df_counts, aes(x = fct_relevel(!!sym(i), !!sym(paste0("order_", i))), 
                               y = rate_endo_diagnosis_per_100000_people)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), fill = "#206095") +
    labs(title = "Census sample",
         x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i],
         y = "Rate of endometriosis diagnosis per 100,000 people") +
    scale_x_discrete(limits = rev) +
    theme_bw() +
    coord_flip()
  
  ggsave(paste0(out_path_plots, "plot_perc_endo_diag_main_", i, ".pdf"),
         width = 10, height = 4, units = "in") 

}


# supplementary analysis

for (i in demog_varsnames_plots) {
  
  print(i)
  
  df_counts <- counts_supplementary %>%
    filter(category == i) %>% 
    select(category, group, rate_endo_diagnosis_per_100000_people) %>%
    rename(!!sym(i) := group)
  
  ggplot(data = df_counts, aes(x = fct_relevel(!!sym(i), !!sym(paste0("order_", i))), 
                               y = rate_endo_diagnosis_per_100000_people)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), fill = "#206095") +
    labs(title = "Census sample",
         x = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == i],
         y = "Rate of endometriosis diagnosis per 100,000 people") +
    scale_x_discrete(limits = rev) +
    theme_bw() +
    coord_flip()
  
  ggsave(paste0(out_path_plots, "plot_perc_endo_diag_supplementary_", i, ".pdf"),
         width = 10, height = 4, units = "in") 

}


####################################################################################################
## Standardised differences
####################################################################################################

demog_varsnames_std_diff <- demog_varsnames[demog_varsnames != "total" & demog_varsnames != "age_at_diag_final_5"]

std_diff_main <- cov.dist.cat(vars = demog_varsnames_std_diff[demog_varsnames_std_diff != "utla"], 
                             dataset = df_main, 
                             exposure = "diag_endo") %>%
  mutate(population = "main")

std_diff_supplementary <- cov.dist.cat(vars = demog_varsnames_std_diff[demog_varsnames_std_diff != "utla"], 
                             dataset = df_supplementary, 
                             exposure = "diag_endo_primary") %>%
  mutate(population = "supplementary")

# Save output
List_of_dfs <- list(std_diff_main, std_diff_supplementary)
names(List_of_dfs) <- c("main", "supplementary")
write_xlsx(List_of_dfs, paste0(out_path, "standardised_differences.xlsx"))


## add to counts tables
std_diff_main_filtered <- std_diff_main %>%
  select(characteristic, abs_std_diff) %>%
  filter(!is.na(characteristic) & characteristic != "") %>%
  distinct() %>%
  mutate(abs_std_diff = round(as.numeric(paste(abs_std_diff)) * 100, 2))

counts_main_with_std_diff <- full_join(counts_main, 
                                  std_diff_main_filtered, 
                                  by = c("category" = "characteristic"))

std_diff_supplementary_filtered <- std_diff_supplementary %>%
  select(characteristic, abs_std_diff) %>%
  filter(!is.na(characteristic) & characteristic != "") %>%
  distinct() %>%
  mutate(abs_std_diff = round(as.numeric(paste(abs_std_diff)) * 100, 2))

counts_supplementary_with_std_diff <- full_join(counts_supplementary, 
                                  std_diff_supplementary_filtered, 
                                  by = c("category" = "characteristic"))

# Save output
List_of_dfs <- list(counts_main_with_std_diff,
                    counts_supplementary_with_std_diff)

names(List_of_dfs) <- c("counts_main", "counts_supplementary")

write_xlsx(List_of_dfs, paste0(out_path, "endo_counts_with_std_diff.xlsx"))


####################################################################################################
## 10 most common primary diagnoses for endo diagnoses
####################################################################################################

#--------------------------
# Define breakdown variables
#--------------------------

demog_varsnames <- c("total", 
                     exposures, 
                     "num_epi_pre_study_grouped", 
                     "age_at_diag_final_5")
demog_varsnames

#--------------------------
# read in ICD 10 lookup
#--------------------------

icd10_lookup <- read.csv("...") %>%
  rename_with(tolower) %>%
  select(code_x3, block) %>%
  distinct()

icd10_lookup_sdf <- copy_to(sc, icd10_lookup, "icd10_lookup_sdf", overwrite = TRUE)

# filter to people with an endo diagnosis (primary or secondary)
# link to the ICD 10 lookup for text descriptions of the primary diagnoses
df_top_10_primary <- df %>% 
  filter(population_main == "endo") %>%
  mutate(DIAG_01 = substr(DIAG_01, 1, 3)) %>%
  mutate(DIAG_4_01 = substr(DIAG_4_01, 1, 3)) %>%
  mutate(diag_code_primary = DIAG_01) %>%
  mutate(diag_code_primary = ifelse(is.na(diag_code_primary), DIAG_4_01, diag_code_primary)) %>%  
  left_join(icd10_lookup_sdf, by = c("diag_code_primary" = "code_x3")) %>%
  mutate(primary_diagnosis = paste(diag_code_primary, block))

#--------------------------
# Counts by exposure
#--------------------------

# initialise empty data frame
counts_primary_diag <- data.frame()

for (i in demog_varsnames) {
  
  print(i)

  # get 10 most common primary diagnoses by exposure subgroup
  # aggregate all other primary diagnoses into "Other" category
  df_top_10_primary_count <- df_top_10_primary %>%
    group_by(!!sym(i), primary_diagnosis) %>%
    tally() %>%
    ungroup() %>%
    group_by(!!sym(i)) %>%
    arrange(desc(n)) %>%
    mutate(primary_diagnosis = ifelse(row_number() <= 10 & n >= 3, primary_diagnosis, "Other primary diagnosis")) %>%
    ungroup()
  
  # get final counts with "Other" category now included
  # calculate percentages
  df_top_10_primary_count <- df_top_10_primary_count %>%
    group_by(!!sym(i), primary_diagnosis) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    mutate(n = round(n / 5) * 5) %>%
    group_by(!!sym(i)) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    ungroup() %>%
    collect()
  
  # format table
   df_top_10_primary_count <- df_top_10_primary_count %>% 
    mutate(domain = i) %>%
    rename(group = all_of(i)) %>%
    mutate(group = as.character(group)) %>%
    # create dummy variable for sorting
    mutate(id = ifelse(primary_diagnosis == "Other primary diagnosis", -999, n)) %>%
    arrange(factor(group, levels = !!sym(paste0("order_", i))), desc(id)) %>%
    select(-id)

  # bind to the main counts data frame
  counts_primary_diag <- bind_rows(counts_primary_diag, df_top_10_primary_count)
     
}

# format table
# apply suppression
counts_primary_diag_formatted <- counts_primary_diag %>%
  relocate(group, .before = everything()) %>%
  relocate(domain, .before = group) %>%
  mutate(across(.cols = c("n", "percentage"),
                .fns = ~ ifelse(n < 5, -10, .)))

# print
counts_primary_diag_formatted %>% data.frame()

#--------------------------
# save output
#--------------------------

write.csv(counts_primary_diag_formatted, 
          paste0(out_path, "counts_primary_diag_main.csv"),
          row.names = FALSE)

#--------------------------
# total counts
#--------------------------

# aggregate all small value primary diagnoses into "Other" category
df_primary_count <- df_top_10_primary %>%
    group_by(primary_diagnosis) %>%
    tally() %>%
    ungroup() %>%
    arrange(desc(n)) %>%
    mutate(primary_diagnosis = ifelse(row_number() <= 10 & n >= 3, primary_diagnosis, "Other primary diagnosis"))

total <- df_primary_count %>%
  summarise(n = sum(n)) %>%
  pull()

# get final counts with "Other" category now included
# calculate percentages
df_primary_count <- df_primary_count %>%
  group_by(primary_diagnosis) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  mutate(n = round(n / 5) * 5) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  collect() %>%
  # add total
  add_row(primary_diagnosis = "Total", n = round(total / 5) * 5, percentage = 100)

# format table
 df_primary_count <- df_primary_count %>% 
  # create dummy variable for sorting
  mutate(id = ifelse(primary_diagnosis == "Other primary diagnosis", -999, n)) %>%
  arrange(desc(id)) %>%
  select(-id) %>%  
  # apply suppression
  mutate(across(.cols = c("n", "percentage"),
                .fns = ~ ifelse(n < 5, -10, .)))

# print
df_primary_count %>% data.frame()

#--------------------------
# save output
#--------------------------

write.csv(df_primary_count, 
          paste0(out_path, "counts_primary_diag_total_main.csv"),
          row.names = FALSE)


#-------------#
# END OF FILE #
#-------------#

