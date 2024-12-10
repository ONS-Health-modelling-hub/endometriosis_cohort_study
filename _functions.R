#check_missing function 

#df = dataframe

check_missing <- function(df){
  
  missing <- df %>%
    group_by(population_main, population_main_split, population_supplementary, population_supplementary_split) %>%
    mutate_all(is.na) %>%
    mutate_all(as.numeric) %>%
    summarise_all(sum, na.rm=TRUE) %>%
    ungroup() %>%
    collect() %>%
    pivot_longer(cols = -c(population_main, population_main_split, population_supplementary, population_supplementary_split), 
                 names_to = "Variable", 
                 values_to = "Missing") %>%
    arrange(desc(Missing), Variable, population_main, population_main_split, population_supplementary, population_supplementary_split)

  return(missing)
  
}

#get_counts function 

#data = df
#grouping_var
#demog_varsnames

get_counts <- function(data, grouping_var, demog_varsnames) {
      
  print(demog_varsnames)
  
  table <- data %>%
    group_by(!!sym(grouping_var), .data[[demog_varsnames]]) %>%
    count() %>%
    mutate(n = ifelse(n < 10, -10, n)) %>%
    mutate(n = round(n / 5) * 5) %>%
    ungroup() %>%
    group_by(!!sym(grouping_var)) %>%
  	mutate(percentage = ifelse(n == -10, -10, n / sum(n) * 100)) %>%
    ungroup() %>%
    collect() %>%
		mutate(percentage = ifelse(percentage == -10, percentage, round(percentage, 2)))
  
  table_totals <- data %>%
    group_by(.data[[demog_varsnames]]) %>%
    count() %>%
    mutate(n = ifelse(n < 10, -10, n)) %>%
    mutate(n = round(n / 5) * 5) %>%
    ungroup() %>%
    mutate(percentage = ifelse(n == -10, -10, n / sum(n) * 100)) %>%
    mutate(!!sym(grouping_var) := "total") %>%
    collect() %>%
		mutate(percentage = ifelse(percentage == -10, percentage, round(percentage, 2)))

  table_fin <- bind_rows(table, table_totals)  %>%
    arrange(!!sym(grouping_var), 
            factor(.data[[demog_varsnames]], levels = !!sym(paste0("order_", demog_varsnames)))) %>%
		rename(group = all_of(demog_varsnames)) %>%
		mutate(category = demog_varsnames) %>%
    relocate(category, .before = everything()) %>% 
    pivot_wider(names_from = !!sym(grouping_var), 
                values_from = c(n, percentage), 
                names_vary = "slowest") %>%
    mutate(rate_endo_diagnosis_per_100000_people = ifelse(n_endo != -10 & 
                                                          n_no_endo != -10 &
                                                          n_total != -10, 
                                                          round(n_endo / n_total * 100000, 2),
                                                          -10))

	demog_table <- rbind(demog_table, table_fin)
  		
	return(demog_table)
  
}

# Calculate crude rates

get_crude_rates <- function(data, 
                            poisson_data, 
                            outcome,
                            group_vars,
                            weight = NULL) {
  
  if (missing(weight)) {
    
    data <- data %>%
      ungroup() %>%
      mutate(weight = 1)
    
    weight <- "weight"
    
  }
        
  df_rates <- data %>%
    mutate(outcome_weighted = .data[[outcome]] * .data[[weight]]) %>%
    group_by(across(all_of(!!group_vars))) %>%
    summarise(unweighted_count = sum(.data[[outcome]], na.rm = TRUE),
              count = sum(outcome_weighted, na.rm = TRUE),
              sample = n(),
              pops = sum(.data[[weight]], na.rm = TRUE),
              .groups = 'drop') %>%
    sdf_coalesce(1) %>%
    collect() %>%
    mutate(across(.cols = all_of(c("count", "pops")),
                  .fns = ~ as.integer(round(., 0)))) %>%
    mutate(crude_rate = 100000 * (count / pops)) %>%
    left_join(poisson_data, by = "count") %>%
    mutate(crude_rate_lower_ci = ifelse((count > 0 & count < 100), L * crude_rate,
                               crude_rate - (1.96 * (crude_rate / sqrt(count)))),
           crude_rate_upper_ci = ifelse((count > 0 & count < 100), U * crude_rate,
                               crude_rate + (1.96 * (crude_rate / sqrt(count))))) %>% 
    mutate(crude_rate_lower_ci = ifelse(crude_rate_lower_ci < 0, 0, crude_rate_lower_ci)) %>%  
    select(-L, -U)
    
    return(df_rates)
  
}
  

# Calculate age-standardised rates

get_age_standardised_rates <- function(
  data,
  ESP_data = ESP,
  ESP_data_variable,
  poisson_data = poisson, 
  total_esp,
  outcome,
  time_var,
  type_of_outcome,
  age_grp,
  group_vars,
  weight = NULL) {
  
  
  if(!(type_of_outcome %in% c("time to event", "count"))) {
    
    stop(cat("Must specify 'type_of_outcome' as one of:",
             "time to event", "count"))
    
  }
  
  if (missing(weight)) {
    
    data <- data %>%
      ungroup() %>%
      mutate(weight = 1)
    
    weight <- "weight"
    
  }
    
  if (type_of_outcome == "time to event") {
    
    if (group_vars == "age_group_5") {
    
      asmrs <- get_crude_rates(data = data, 
                               poisson_data = poisson, 
                               outcome = outcome,
                               group_vars = group_vars) %>%
        mutate(age_standardised_rate = NULL,
               age_standardised_rate_lower_ci = NULL,
               age_standardised_rate_upper_ci = NULL)
    
    } else {

      grp_vars = c(age_grp, group_vars)

      rates_by_age <- data %>%
        mutate(time_at_risk_years = .data[[time_var]] / 365.25,
               time_at_risk_years_weighted = time_at_risk_years * .data[[weight]],
               outcome_weighted = .data[[outcome]] * .data[[weight]]) %>%
        group_by(across(all_of(!!grp_vars))) %>%
        summarise(unweighted_count = sum(.data[[outcome]], na.rm = TRUE),
                  count = sum(outcome_weighted, na.rm = TRUE),
                  sample = n(),
                  pops = sum(.data[[weight]], na.rm = TRUE),
                  unweighted_person_years = sum(time_at_risk_years, na.rm = TRUE),
                  person_years = sum(time_at_risk_years_weighted, na.rm = TRUE),
                  .groups = 'drop') %>%
        mutate(crude_rate_100000_person_years = 100000 * (count / person_years)) %>%
        #rename(age_group_esp = all_of(age_grp)) %>%
        sdf_coalesce(1) %>%
        collect() %>%
        left_join(ESP_data, by = age_grp) %>%
        mutate(variance = (((crude_rate_100000_person_years^2) / count) * (.data[[ESP_data_variable]]^2)),
               stand_rate = crude_rate_100000_person_years * .data[[ESP_data_variable]])

      if (sum(is.na(rates_by_age[[ESP_data_variable]])) != 0) {

        stop("Missing population weights. Check the labels of the age groups")

      }

      asmrs <- rates_by_age %>%
            group_by(across(all_of(!!group_vars))) %>%
            summarise(across(.cols = all_of(!!c("unweighted_count",
                                                "count",
                                                "sample",
                                                "pops",
                                                "stand_rate",
                                                "variance",
                                                "unweighted_person_years",
                                                "person_years")),
                             .fns = ~ sum(.x, na.rm = TRUE)),
                      .groups = 'drop') %>%
            mutate(count = as.integer(round(count, 0)),
                   variance = (variance / (total_esp^2)),
                   SE = sqrt(variance),
                   rate = stand_rate / total_esp) %>%
            left_join(poisson_data, by = c("count")) %>%
            mutate(lower = if_else(count < 100,
                                   ((((L * count) - count) * ((variance/count)^0.5)) + rate),
                                   rate - (1.96 * SE)),
                   upper = if_else(count < 100,
                                   ((((U * count) - count) * ((variance/count)^0.5)) + rate),
                                   rate + (1.96 * SE))) %>%
      mutate(age_standardised_rate = rate,
             age_standardised_rate_lower_ci = lower,
             age_standardised_rate_upper_ci = upper) %>%
      mutate(across(.cols = all_of(c("count", "person_years")),
                    .fns = ~ as.integer(round(., 0)))) %>%
      mutate(crude_rate = map2_dbl(count, person_years, ~poisson.test(.x, .y)$estimate),
             crude_rate = 100000 * crude_rate,
             crude_rate_lower_ci = map2_dbl(count, person_years, ~poisson.test(.x, .y)$conf.int[1]),
             crude_rate_lower_ci = 100000 * crude_rate_lower_ci,
             crude_rate_upper_ci = map2_dbl(count, person_years, ~poisson.test(.x, .y)$conf.int[2]),
             crude_rate_upper_ci = 100000 * crude_rate_upper_ci) %>%
      mutate(crude_rate_lower_ci = ifelse(crude_rate_lower_ci < 0, 0, crude_rate_lower_ci),
             age_standardised_rate_lower_ci = ifelse(age_standardised_rate_lower_ci < 0, 0, age_standardised_rate_lower_ci)) %>%
      select(all_of(!!c(group_vars,
                        "unweighted_count", "count",
                        "sample", "pops",
                        "unweighted_person_years", "person_years",
                        "crude_rate",
                        "crude_rate_lower_ci",
                        "crude_rate_upper_ci",
                        "age_standardised_rate",
                        "age_standardised_rate_lower_ci",
                        "age_standardised_rate_upper_ci"))) %>%
      ungroup()

      asmrs
      
    }


  } else if (type_of_outcome == "count") {
    
    if (group_vars == "age_group_5") {
    
      asmrs <- get_crude_rates(data = data, 
                               poisson_data = poisson, 
                               outcome = outcome,
                               group_vars = group_vars) %>%
        mutate(age_standardised_rate = NULL,
               age_standardised_rate_lower_ci = NULL,
               age_standardised_rate_upper_ci = NULL)
    
    } else {

      grp_vars = c(age_grp, group_vars)

      rates_by_age <- data %>%
        mutate(outcome_weighted = .data[[outcome]] * .data[[weight]]) %>%
        group_by(across(all_of(!!grp_vars))) %>%
        summarise(unweighted_count = sum(.data[[outcome]], na.rm = TRUE),
                  count = sum(outcome_weighted, na.rm = TRUE),
                  sample = n(),
                  pops = sum(.data[[weight]], na.rm = TRUE),
                  .groups = 'drop') %>%
        mutate(crude_rate_100000_people = 100000 * (count / pops)) %>%
        #rename(age_group_esp = all_of(age_grp)) %>%
        sdf_coalesce(1) %>%
        collect() %>%
        left_join(ESP_data, by = age_grp) %>%
        mutate(variance = (((crude_rate_100000_people^2) / count) * (.data[[ESP_data_variable]]^2)),
               stand_rate = crude_rate_100000_people * .data[[ESP_data_variable]])

      if (sum(is.na(rates_by_age[[ESP_data_variable]])) != 0) {

        stop("Missing population weights. Check the labels of the age groups")

      }

      asmrs <- rates_by_age %>%
        group_by(across(all_of(!!group_vars))) %>%
        summarise(across(.cols = all_of(!!c("unweighted_count",
                                            "count",
                                            "sample",
                                            "pops",
                                            "stand_rate",
                                            "variance")),
                         .fns = ~ sum(.x, na.rm = TRUE)),
                  .groups = 'drop') %>%
        mutate(count = as.integer(round(count, 0)),,
               variance = (variance / (total_esp^2)),
               SE = sqrt(variance),
               rate = stand_rate / total_esp) %>%
        left_join(poisson_data, by = c("count")) %>%
        mutate(lower = if_else(count < 100,
                               ((((L * count) - count) * ((variance / count)^0.5)) + rate),
                               rate -(1.96 * SE)),
               upper = if_else(count < 100,
                               ((((U * count) - count) * ((variance / count)^0.5)) + rate),
                               rate + (1.96 * SE))) %>%
        mutate(age_standardised_rate = rate,
               age_standardised_rate_lower_ci = lower,
               age_standardised_rate_upper_ci = upper) %>%
        mutate(across(.cols = all_of(c("count", "pops")),
                      .fns = ~ as.integer(round(., 0)))) %>%
        mutate(crude_rate = map2_dbl(count, pops, ~poisson.test(.x, .y)$estimate),
               crude_rate = 100000 * crude_rate,
               crude_rate_lower_ci = map2_dbl(count, pops, ~poisson.test(.x, .y)$conf.int[1]),
               crude_rate_lower_ci = 100000 * crude_rate_lower_ci,
               crude_rate_upper_ci = map2_dbl(count, pops, ~poisson.test(.x, .y)$conf.int[2]),
               crude_rate_upper_ci = 100000 * crude_rate_upper_ci) %>%
        mutate(crude_rate_lower_ci = ifelse(crude_rate_lower_ci < 0, 0, crude_rate_lower_ci),
               age_standardised_rate_lower_ci = ifelse(age_standardised_rate_lower_ci < 0, 0, age_standardised_rate_lower_ci)) %>%
      select(all_of(!!c(group_vars,
                        "unweighted_count", "count", "sample", "pops",
                        "crude_rate",
                        "crude_rate_lower_ci",
                        "crude_rate_upper_ci",
                        "age_standardised_rate",
                        "age_standardised_rate_lower_ci",
                        "age_standardised_rate_upper_ci"))) %>%
      ungroup()
      
    }

    asmrs       
    
  }
  
}

           
# Age standardised rates plots

asr_plot <- function(data,
                     outcome_col,
                     domain_col,
                     weight_col,
                     xlab,
                     ylab,
                     save = FALSE,
                     directory,
                     file_name) {
  
  asr_plot <- data %>%
    filter(outcome == outcome_col, 
           domain == domain_col,
           weighted == weight_col,
           sex != "all") %>%
    mutate(group = as.factor(group),
           Sex = as.factor(sex)) %>%
    ggplot(aes(y = group,
               x = age_standardised_rate,
               fill = Sex)) +
    geom_col(position = position_dodge(0.9)) +
    geom_errorbar(aes(xmin = age_standardised_rate_lower_ci,
                      xmax = age_standardised_rate_upper_ci),
                  width = 0.25,
                  position = position_dodge(0.9)) +
   theme_bw() +
   scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
   scale_fill_manual(values = c("#009E73", "#F0E442")) +
   labs(x = xlab, y = ylab, title = NULL) +
   theme(legend.position = "top")
  
  if (save == TRUE) {
    
    if (missing(directory) | missing(file_name)) {
      
      stop("Must provide 'directory' and 'file_name' when 'save' = TRUE")
      
    } else {
      
      ggsave(plot = asr_plot,
             filename = file_name,
             path = directory,
             device = "png")
      
    }
    
  }
  
  return(asr_plot)
  
}


# sparklyr restricted cubic spline function

sdf_rcs_fun <- function(x, data, knots_pctiles) {
  
  if(!(sdf_schema(data[x])[[x]][["type"]] %in% paste0(c("Double",
                                                        "Integer",
                                                        "Float",
                                                        "Decimal",
                                                        "Long"),
                                                        "Type"))) {
    
    stop(paste0('"x"', " must be a numeric variable"))
    
  }
  
  n_knots <- length(knots_pctiles)
  
  # Need at least 3 knots
  if (n_knots < 3) {
    
    stop('"knots_pctiles" must be of length of at least 3')
    
  }
  
  if(is.unsorted(knots_pctiles)) {
    
    stop('"knots_pctiles" must be in ascending order')
    
  }
  
  # To return the only the minimum required vars at the end
  orig_cols <- colnames(data)
  new_cols <- purrr::map_chr(1:(n_knots - 2), function(e) {
    
    paste0(x, "_spline_part_", e)
    
  })
  
  cat(
    paste(
      paste0(
        'The following variables will be created by the function to use in a regression together with ',
        '"', x, '"', 
        ":"),
      paste(new_cols, collapse = ", "), sep = "\n"))
  
  # Get the knot values in terms of x
  pctiles <- sdf_quantile(x = data, column = x, probabilities = knots_pctiles)
  names(pctiles) <- NULL
  
  # Add the knot values in terms of x to the data
  for (i in 1:n_knots) {
    
    data <- data %>%
      mutate("t_{i}" := !!pctiles[i])
    
  }
  
  # Add the K-2 spline parts
  for (i in 1:(n_knots - 2)) {
    
    new_col_i <- new_cols[i]
    
    data <- data %>%
      # S3,1
      dplyr::mutate(s_1 = (.data[[x]] - .data[[paste0("t_", i)]])^3,
                    s_2_1 = ((.data[[paste0("t_", n_knots)]] - .data[[paste0("t_", i)]]) /
                               (.data[[paste0("t_", n_knots)]] - .data[[paste0("t_", (n_knots - 1))]])),
                    s_2_2 = (.data[[x]] - .data[[paste0("t_", (n_knots - 1))]])^3,
                    s_3_1 = ((.data[[paste0("t_", (n_knots - 1))]] - .data[[paste0("t_", i)]]) /
                               (.data[[paste0("t_", n_knots)]] - .data[[paste0("t_", (n_knots - 1))]])),
                    s_3_2 = (.data[[x]] - .data[[paste0("t_", n_knots)]])^3) %>%
      dplyr::mutate(across(c("s_1", "s_2_2", "s_3_2"), .fns = ~ ifelse(.x < 0, 0, .x))) %>%
      dplyr::mutate(s_2 = s_2_1 * s_2_2,
                    s_3 = s_3_1 * s_3_2,
                    "{new_col_i}" := s_1 - s_2 + s_3) %>%
      select(-starts_with("s_"))      
    
  }
  
  data <- data %>%
    select(all_of(!!c(orig_cols, new_cols)))
  
}

# Plots data function
# Creates frequencies for populations; deaths by a given variable; joins and calculates percentages

plots_data <- function(df, var1) {
  
  all <- df %>%
   group_by(.data[[var1]]) %>%
    summarise(all=n()) %>%
    collect()
  
  endo <- df %>%
    filter(diag_endo == 1) %>%
    group_by(.data[[var1]]) %>%
    summarise(endo=n()) %>%
    collect()
  
  percentages <- left_join(all, endo) %>%
    mutate(percentage = endo/all*100) %>%
    collect() %>%
    select(-all, -endo)
  
return(percentages)
  
}


# Plots data by age function
# Creates frequencies for populations; deaths by a given variable and age group; joins and calculates percentages

plots_data_by_age <- function(df, var1) {
  
  all <- df %>%
   group_by(.data[[var1]], age_group_10) %>%
    summarise(all=n()) %>%
    collect()
  
  endo <- df %>%
    filter(diag_endo == 1) %>%
    group_by(.data[[var1]], age_group_10) %>%
    summarise(endo=n()) %>%
    collect()
  
  percentages <- left_join(all, endo) %>%
    mutate(percentage = endo/all*100) %>%
    collect() %>%
    select(-all, -endo)

return(percentages)
  
}


# Create horizontal error bar plots for the odds ratios using ggplot

plot_odds_ratios <- function(df, variable, order, populations) {
      
  print(variable)
  
  plot <- ggplot(data = df,
                 aes(x = exp_est,
                     y = fct_relevel(term, rev(!!sym(order))),
                     colour = fct_relevel(model, rev(models)))) +
                 geom_errorbar(aes(xmin = exp_lower_ci, 
                                   xmax = exp_upper_ci),
                               width = 0.4, 
                               position = position_dodge(width = 0.5)) +
                 geom_point(position = position_dodge(width = 0.5), 
                            size = 2) +
                 geom_vline(xintercept = 1, linetype = 1, colour = "black", size = 1) +
                 labs(subtitle = paste0("Comparison group: ", 
                                        ref_cat$ref_level[ref_cat$variable == variable]),
                      y = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == variable],
                      x = "Odds ratio") +
                 scale_x_continuous(limits = c(plot_lim_min, plot_lim_max)) +
                 theme_bw() +
                 theme(legend.position = "bottom") +
                 guides(colour = guide_legend(title = NULL, reverse=TRUE)) +
                 {if(length(populations) > 1) facet_wrap(~ population)}  
  
  print(plot)
  
  return(plot)
  
}


# Create horizontal error bar plots for the odds ratios using ggplot by analysis type

plot_odds_ratios_grid <- function(df, variable, order, populations) {
      
  print(variable)
  
  plot <- ggplot(data = df,
                 aes(x = exp_est,
                     y = fct_relevel(term, rev(!!sym(order))),
                     colour = fct_relevel(model, rev(models)))) +
                 geom_errorbar(aes(xmin = exp_lower_ci, 
                                   xmax = exp_upper_ci),
                               width = 0.4, 
                               position = position_dodge(width = 0.5)) +
                 geom_point(position = position_dodge(width = 0.5), 
                            size = 2) +
                 geom_vline(xintercept = 1, linetype = 1, colour = "black", size = 1) +
                 labs(subtitle = paste0("Comparison group: ", 
                                        ref_cat$ref_level[ref_cat$variable == variable]),
                      y = exposure_formatted_lookup$name[exposure_formatted_lookup$variable == variable],
                      x = "Odds ratio") +
                 scale_x_continuous(limits = c(plot_lim_min, plot_lim_max)) +
                 theme_bw() +
                 theme(legend.position = "bottom") +
                 guides(colour = guide_legend(title = NULL, reverse=TRUE)) +
                 facet_wrap(~ fct_relevel(analysis_type, analysis_types),
                            labeller = as_labeller(analysis_type_labeller),
                            nrow = 1)
    
  print(plot)
  
  return(plot)
  
}

