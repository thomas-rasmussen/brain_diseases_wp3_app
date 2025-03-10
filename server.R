
# Load data
codelist <- readRDS(here("data", "codelist.rds"))
patient_characteristics <- readRDS(here("data", "patient_characteristics.rds"))
cost_results <- readRDS(here("data", "cost_results.rds"))

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
      are specified, the given ATC codes are required to be accompanied with
      one of the listed ATC indication codes.
    
      <p>
      Note that all ICD-10 and ATC subcodes are included/excluded.
      </p>
    "))
  })
    
  #### output$definitions_details_cci ####
  output$definitions_details_cci <- renderUI({
    div(HTML("
      <h2>Charlson Comorbidity index definitions</h2>
    
      <p>
      Note that all ICD-10 and ATC subcodes are included/excluded.
      </p>
    "))
  })
  
  #### output$definitions_details_education ####
  output$definitions_details_education <- renderUI({
    div(HTML("
      <h2>Highest completed education definition</h2>
      
      <p>Highest completed education was defined the UDDA registry. The registry
      provided codes (HFAUDD) and dates (HF_VFRA) for completed educations.
      These educations were grouped into education levels of 'No education',
      'Short education', 'Medium education' and 'long education', and from the
      point a person completed an education at one education level, the person
      was regarded as being of that education level from that point onwards.</p>
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
  
  ####output$definitions_table_education ####
  output$definitions_table_education <- renderTable(striped = TRUE, {
    
    codelist %>%
      filter(group == "education") %>%
      select(var_label, code_type, code_include) %>%
      group_by(var_label) %>%
      mutate(code_include_group = paste(code_include, collapse = " ")) %>%
      select(-code_include) %>%
      distinct() %>%
      rename(
        "Education level" = var_label,
        "Code type" = code_type,
        "Codes included" = code_include_group
      )
  })
  
  
  #### output$patient_characteristics_table ####
  output$patient_characteristics_table <- renderTable({
    patient_characteristics %>%
      filter(
        var_name == input$patient_characteristics_var_name_id
        & population ==  input$patient_characteristics_population_id
        & same_education_req == input$patient_characteristics_same_education_req_id
      ) %>%
      select(
        agegroup_label, closest_relative_group_label,
        label, relative_group, label_order, stat_char
      ) %>%
      mutate(
        stat_char = case_match(
          label,
          "__n" ~ word(stat_char, 1),
          .default = stat_char
        )
      ) %>%
      pivot_wider(
        names_from = relative_group,
        values_from = stat_char,
        names_prefix = "relative_group_"
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
          "education_level: title" ~ "Education level, n (%)",
          "education_level: no_education" ~ "  No education",
          "education_level: short_education" ~ "  Short education",
          "education_level: medium_education" ~ "  Medium education",
          "education_level: long_education" ~ "  Long education",
          "closest_relative_type: title" ~ "Closest relative type, n (%)",
          "closest_relative_type: mother" ~ "  Mother",
          "closest_relative_type: father" ~ "  Father",
          "closest_relative_type: partner" ~ "  Partner",
          "closest_relative_type: parent" ~ "  Parent",
          "closest_relative_type: child" ~ "  Child",
          "closest_relative_type: sibling" ~ "  Sibling",
          .default = label
        )
      ) %>%
      rename(
        "Patient characteristic" = label,
        "Agegroup" = agegroup_label,
        "Closest relative strata" = closest_relative_group_label,
        "Control relatives" = relative_group_control,
        "Case relatives" = relative_group_case
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
        & same_education_req == input$cost_analyses_table_same_education_req_id
        & cost_period == input$cost_analyses_table_cost_period_id
      ) %>%
      mutate(
        act_cost_mil_eur = act_cost / (eur_dkk_rate * 10**6),
        act_cost_py_eur = act_cost_py / eur_dkk_rate,
        att_cost_mil_eur = att_cost / (eur_dkk_rate * 10**6),
        att_cost_py_eur = att_cost_py / eur_dkk_rate
      ) %>%
      select(
        cost_period_label, same_education_req_label,
        population_label, var_name_label, agegroup_label,
        closest_relative_type_label,
        cost_component_label,
        act_cost_mil_eur, act_cost_py_eur,
        att_cost_mil_eur, att_cost_py_eur
      ) %>%
      rename(
        "Cost period" = cost_period_label,
        "Same education requirement" = same_education_req_label,
        "Population" = population_label,
        "Brain disease" = var_name_label,
        "Agegroup" = agegroup_label,
        "Closest relative type" = closest_relative_type_label,
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
        & same_education_req == input$cost_analyses_plot_same_education_req_id
        & cost_period == input$cost_analyses_plot_cost_period_id
        & agegroup == input$cost_analyses_plot_agegroup_id
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

    # Make different output plot depending on the agegroup
    if (input$cost_analyses_plot_agegroup_id %in% c("25-64", "65+")) {
      # Look at total costs for each brain disease to determine y axis ordering
      var_name_ordering <- cost_data %>%
        select(var_name_label, cost_var) %>%
        group_by(var_name_label) %>%
        summarize(total_cost = sum(cost_var, na.rm = TRUE)) %>%
        arrange(total_cost)
      
      cost_data <- cost_data %>%
        mutate(
          var_name_label = factor(
            var_name_label,
            levels = var_name_ordering$var_name_label
          )
        )
      
      plot_right <- cost_data %>%
        filter(cost_component != "lost_production_sickness") %>%
        make_costs_barplot(
          labs_x = {{ labs_x }},
          y_axis_labels = TRUE,
          include_legend = TRUE
        )
      
      plot_left <- cost_data %>%
        filter(cost_component == "lost_production_sickness") %>%
        make_costs_barplot(
          labs_x = {{ labs_x }},
          y_axis_labels = FALSE,
          include_legend = TRUE,
          flip_plot = TRUE
        )
      
      if (cost_type %in% c("att_cost", "att_cost_py")) {
        tmp <- plot_left + plot_right 
      } else if (cost_type %in% c("act_cost", "act_cost_py")) {
        tmp <- plot_right
      }
      
      tmp + labs(title = "Closest relative type: Closest relative")
    } else if (input$cost_analyses_plot_agegroup_id == "0-24") {
      # Look at total costs for each brain disease to determine y axis ordering
      var_name_ordering <- cost_data %>%
        select(var_name_label, cost_var) %>%
        group_by(var_name_label) %>%
        summarize(total_cost = sum(cost_var, na.rm = TRUE)) %>%
        arrange(total_cost)
      
      cost_data <- cost_data %>%
        mutate(
          var_name_label = factor(
            var_name_label,
            levels = var_name_ordering$var_name_label
          )
        )
      
      plot_right_father <- cost_data %>%
        filter(
          closest_relative_type == "father"
          & cost_component != "lost_production_sickness"
        ) %>%
        make_costs_barplot(
          labs_x = {{ labs_x }},
          y_axis_labels = TRUE,
          include_legend = TRUE
        )
      
      plot_left_father <- cost_data %>%
        filter(
          closest_relative_type == "father"
          & cost_component == "lost_production_sickness"
        ) %>%
        make_costs_barplot(
          labs_x = {{ labs_x }},
          y_axis_labels = FALSE,
          include_legend = TRUE,
          flip_plot = TRUE
        )
      
      plot_right_mother <- cost_data %>%
        filter(
          closest_relative_type == "mother"
          & cost_component != "lost_production_sickness"
        ) %>%
        make_costs_barplot(
          labs_x = {{ labs_x }},
          y_axis_labels = TRUE,
          include_legend = TRUE
        )
      
      plot_left_mother <- cost_data %>%
        filter(
          closest_relative_type == "mother"
          & cost_component == "lost_production_sickness"
        ) %>%
        make_costs_barplot(
          labs_x = {{ labs_x }},
          y_axis_labels = FALSE,
          include_legend = TRUE,
          flip_plot = TRUE
        )
      
            
      if (cost_type %in% c("att_cost", "att_cost_py")) {
        tmp_father <- plot_left_father + plot_right_father + plot_annotation(title = "Closest relative type: Father")
        tmp_mother <- plot_left_mother + plot_right_mother + plot_annotation(title = "Closest relative type: Mother")
      } else if (cost_type %in% c("act_cost", "act_cost_py")) {
        tmp_father <- plot_right_father + plot_annotation(title = "Closest relative type: Father")
        tmp_mother <- plot_right_mother + plot_annotation(title = "Closest relative type: Mother")
      }
      
      wrap_elements(tmp_father) + wrap_elements(tmp_mother)
    }
  })
  
}
  
 
