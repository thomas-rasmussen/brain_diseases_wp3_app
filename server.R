
# Load data
codelist <- readRDS(here("data", "codelist.rds"))
patient_characteristics <- readRDS(here("data", "patient_characteristics.rds"))
cost_results <- readRDS(here("data", "cost_results.rds")) %>%
  group_by(
    var_pop, population, population_label, agegroup, agegroup_label,
    var_name, var_name_label, cost_component, cost_component_label
  ) %>%
  summarize(
    act_cost = sum(act_cost),
    att_cost = sum(att_cost),
    act_py = sum(act_py),
    att_py = sum(att_py),
    .groups = "keep"
  ) %>%
  ungroup() %>%
  mutate(
    act_cost_py = act_cost / act_py,
    att_cost_py = att_cost / att_py
  )

var_names_as_list <- codelist %>%
  filter(group == "bd_def") %>%
  select(var_name, var_label) %>%
  distinct()

var_names_as_list <- setNames(
  as.list(var_names_as_list$var_name),
  nm = var_names_as_list$var_label
)

# Find DKK to EUR exchange rate used in codelist
eur_dkk_rate <- codelist %>%
  filter(str_starts(var_name, "eur_to_dkk")) %>%
  select(code_include) %>%
  pull() %>%
  as.numeric()


function(input, output, session) {
    
  #### output$definitions_details_bd ####
  output$definitions_details_bd <- renderUI({
      div(HTML("
        <h2>Brain disease definitions</h2>
        
        <p>Codes used to define brain diseases. Note that if ATC indication codes
        are specified, the given ATC codes are required to be accompigned with
        one of the listed ATC indication codes.
      
        <p>
        Note that all ICD-10 and ATC subcodes are included/excluded.
        </p>
      "))
  })
    
  #### output$defintions_details_cci ####
  output$definitions_details_cci <- renderUI({
      div(HTML("
        <h2>Charlson Comorbidity index definitions</h2>
      
        <p>
        Note that all ICD-10 and ATC subcodes are included/excluded.
        </p>
      "))
  })

  #### output$definitions_table_bd ####
  output$definitions_table_bd <- renderTable(striped = TRUE, {
    
    codelist %>%
      filter(
        group == "bd_def" & var_name != "bd_00"
        & (!is.na(code_include) | !is.na(code_include))
      ) %>%
      select(var_label, code_type, code_include, code_exclude) %>%
      mutate(
        code_type = case_match(
          code_type,
          "icd10" ~ "ICD-10",
          "atc" ~ "ATC",
          "indo" ~ "ATC indication",
          "sssy" ~ "SSSY"
        )
      ) %>%
      rename(
        "Brain disorder" = var_label,
        "Code type" = code_type,
        "Codes included" = code_include,
        "Subcoodes excluded" = code_exclude
      )
  })
 
  #### output$definitions_table_cci ####
  output$definitions_table_cci <- renderTable(striped = TRUE, {
    
    codelist %>%
      filter(
        group == "cci"
        & (!is.na(code_include) | !is.na(code_include))
      ) %>%
      select(var_label, code_type, code_include, code_exclude) %>%
      mutate(
        code_type = case_match(
          code_type,
          "icd10" ~ "ICD-10",
          "icd8" ~ "ICD-8",
        )
      ) %>%
      rename(
        "CCI disease" = var_label,
        "Code type" = code_type,
        "Codes included" = code_include,
        "Subcoodes excluded" = code_exclude
      )
  })
  
  #### output$patient_characteristics_table ####
  output$patient_characteristics_table <- renderTable({
    patient_characteristics %>%
      filter(
        var_name == input$patient_characteristics_var_name_id
        & population ==  input$patient_characteristics_population_id
      ) %>%
      select(
        agegroup_label, closest_relative_type_label,
        label, case_relative, label_order, stat_char
      ) %>%
      mutate(
        stat_char = case_match(
          label,
          "__n" ~ word(stat_char, 1),
          .default = stat_char
        )
      ) %>%
      pivot_wider(
        names_from = case_relative,
        values_from = stat_char,
        names_prefix = "case"
      ) %>%
      mutate(
        label = case_match(
          label,
          "__n" ~ "Number of persons, n",
          "male" ~ "Male, n (%)",
          "age_index" ~ "Age, median (Q1;Q3)",
          "cci_g: title" ~ "CCI, n (%)",
          "cci_g: 0" ~ "  0",
          "cci_g: 1-2" ~ "  1-2",
          "cci_g: +3" ~ "  +3",
          .default = label
        )
      ) %>%
      rename(
        "Patient characteristic" = label,
        "Agegroup" = agegroup_label,
        "Closest relative type" = closest_relative_type_label,
        "Control relative" = case0,
        "Case relativce" = case1
      ) %>%
      select(-label_order)
  })

  #### output$cost_analyses_table ####
  output$cost_analyses_table <- renderTable(
    cost_results %>%
      filter(
        (var_name == input$cost_analyses_table_var_name_id
          | input$cost_analyses_table_var_name_id == "all"
        )
        & (population == input$cost_analyses_table_population_id
         | input$cost_analyses_table_population_id == "all"
        )
      ) %>%
      mutate(
        act_cost_mil_eur = act_cost / (eur_dkk_rate * 10**6),
        act_cost_py_eur = act_cost_py / eur_dkk_rate,
        att_cost_mil_eur = att_cost / (eur_dkk_rate * 10**6),
        att_cost_py_eur = att_cost_py / eur_dkk_rate
      ) %>%
      select(
        population_label, var_name_label, agegroup_label,
        cost_component_label,
        act_cost_mil_eur, act_cost_py_eur,
        att_cost_mil_eur, att_cost_py_eur
      ) %>%
      rename(
        "Population" = population_label,
        "Brain disease" = var_name_label,
        "Agegroup" = agegroup_label,
        "Cost component" = cost_component_label,
        "Total actual costs (million EUR)" = act_cost_mil_eur,
        "Actual cost per person (EUR)" = act_cost_py_eur,
        "Total attributable costs (million EUR)" = att_cost_mil_eur,
        "Attributable cost per person (EUR)" = att_cost_py_eur
      )
  )
  
  #### output$cost_analyses_plot ####
  output$cost_analyses_plot <- renderPlot({

    cost_type <- input$cost_analyses_plot_type
    
    cost_data <- cost_results %>%
      filter(
        population == input$cost_analyses_plot_population_id
        & var_name != "bd_00"
      )
    
    cost_data$cost_var <- cost_data[[cost_type]]

    if (cost_type %in% c("act_cost", "att_cost")) {
      labs_x <- "Cost in million EUR"
      convert_factor <- eur_dkk_rate * 10**6
    } else if (cost_type %in% c("act_cost_py", "att_cost_py")) {
      labs_x <- "Cost in EUR"
      convert_factor <- eur_dkk_rate
    }
    
    cost_data <- cost_data %>%
      mutate(cost_var = cost_var / {{ convert_factor }})

    # Look at total costs for each brain disease to determine y axis ordering
    var_name_ordering <- cost_data %>%
      select(var_name_label, cost_var) %>%
      group_by(var_name_label) %>%
      summarize(total_cost = sum(cost_var)) %>%
      arrange(total_cost)
    
    cost_data <- cost_data %>%
      mutate(
        var_name_label = factor(
          var_name_label,
          levels = var_name_ordering$var_name_label
        )
      )
    
    
    age0_24_right <- cost_data %>%
      filter(agegroup == "0-24" & cost_component != "lost_production_sickness") %>%
      make_costs_barplot(y_axis_labels = TRUE)
    
    age0_24_left <- cost_data %>%
      filter(agegroup == "0-24" & cost_component == "lost_production_sickness") %>%
      make_costs_barplot(y_axis_labels = FALSE, flip_plot = TRUE)
    
    age25_64_right <- cost_data %>%
      filter(agegroup == "25-64" & cost_component != "lost_production_sickness") %>%
      make_costs_barplot(y_axis_labels = TRUE)
    
    age25_64_left <- cost_data %>%
      filter(agegroup == "25-64" & cost_component == "lost_production_sickness") %>%
      make_costs_barplot(y_axis_labels = FALSE, flip_plot = TRUE)
    
    age65p_right <- cost_data %>%
      filter(agegroup == "65+" & cost_component != "lost_production_sickness") %>%
      make_costs_barplot(
        labs_x = {{ labs_x }},
        y_axis_labels = TRUE,
        include_legend = TRUE
      )
    
    age65p_left <- cost_data %>%
      filter(agegroup == "65+" & cost_component == "lost_production_sickness") %>%
      make_costs_barplot(
        labs_x = {{ labs_x }},
        y_axis_labels = FALSE,
        include_legend = TRUE,
        flip_plot = TRUE
      )
    
    if (cost_type %in% c("att_cost", "att_cost_py")) {
      tmp1 <- age0_24_left + age0_24_right + plot_annotation(
        title = "Agegroup 0-24"
      )
      
      tmp2 <- age25_64_left + age25_64_right + plot_annotation(
        title = "Agegroup 25-64"
      )
      
      tmp3 <- age65p_left + age65p_right + plot_annotation(
        title = "Agegroup 65+"
      )
    } else if (cost_type %in% c("act_cost", "act_cost_py")) {
      tmp1 <- age0_24_right + plot_annotation(
        title = "Agegroup 0-24"
      )
      
      tmp2 <- age25_64_right + plot_annotation(
        title = "Agegroup 25-64"
      )
      
      tmp3 <- age65p_right + plot_annotation(
        title = "Agegroup 65+"
      )
    }
    
    # Use wrap_elements() to preseve plot annotations
    wrap_elements(tmp1) / wrap_elements(tmp2) / wrap_elements(tmp3)


  })
  
}
  
 
