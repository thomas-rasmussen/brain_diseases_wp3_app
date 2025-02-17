
# Load data
codelist <- readRDS(here("data", "codelist.rds"))
patient_characteristics <- readRDS(here("data", "patient_characteristics.rds"))
cost_results <- readRDS(here("data", "cost_results.rds")) %>%
  group_by(
    var_pop, population, population_label, var_name, var_name_label,
    cost_component, cost_component_label
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
        population_label, var_name_label,
        cost_component_label,
        act_cost_mil_eur, act_cost_py_eur,
        att_cost_mil_eur, att_cost_py_eur
      ) %>%
      rename(
        "Population" = population_label,
        "Brain disease" = var_name_label,
        "Cost component" = cost_component_label,
        "Total actual costs (million EUR)" = act_cost_mil_eur,
        "Actual cost per person (EUR)" = act_cost_py_eur,
        "Total attributable costs (million EUR)" = att_cost_mil_eur,
        "Attributable cost per person (EUR)" = att_cost_py_eur
      )
  )
  
  #### output$cost_analyses_plot_att_total ####
  output$cost_analyses_plot_att_total <- renderPlot({
    plot_dat <- cost_results %>%
      filter(
        population == input$cost_analyses_plot_att_total_population_id
        & var_name != "bd_00"
        & cost_component != "lost_production_sickness"
      ) %>%
      mutate(att_cost_mil_eur = att_cost / (eur_dkk_rate * 10**6)) 
    
    plot_dat %>%
      ggplot(aes(x = att_cost_mil_eur, y = var_name_label, fill = cost_component_label)) +
      geom_bar(position = "stack", stat = "identity") +    
      theme_bw() +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0),
        labels = function(x) formatC(abs(x), format = "f", big.mark = ",", digits = 1)
      ) +
      scale_y_discrete(expand = c(0, 0)) +
      # cvd-friendly qualitative color palette from "Fundamentals of Data
      # Visualization" figure 19.10
      scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
                                   "#0072B2", "#D55E00")) +
      labs(
        title = "test title",
        subtitle = "Test subtitle",
        x = "Cost in million EUR"
      ) +
      theme(legend.position = "bottom",
            panel.grid.major.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_text(size = 14),
            axis.text = element_text(size = 12, colour = "black"),
            axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
            axis.ticks.y = element_blank(),
            axis.line.x.bottom = element_line(colour = "black"),
            panel.border = element_blank(),
            legend.title = element_blank(),
            legend.direction = "vertical",
            plot.title = element_text(size = 20),
            plot.subtitle = element_text(size = 18)
      ) +
      guides(fill = guide_legend(nrow = 2, reverse = TRUE, byrow = TRUE))
  
  })
  
}
  
 
