
# Load data
codelist <- readRDS(here("data", "codelist.rds"))
patient_characteristics <- readRDS(here("data", "patient_characteristics.rds")) %>%
  mutate(stat_char = ifelse(is.na(stat_char), "", stat_char))
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


# Load helper functions
source("helpers.R")

function(input, output, session) {
    
  #### output$main_info ####
  output$main_info <- renderUI({
    HTML("
      <h3>Societal costs for closest family relatives of patients with brain
      disorders in Denmark: a population-based cohort study</h1>
      
      <p>Main and supplemental results from the study.</p>
    ")
  })
  
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
          "icd8" ~ "ICD-8"
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
  
  #### output$flowchart_text ####
  output$flowchart_text <- renderUI({
    HTML("
      <h3>Studypopulations flowchart </h3>
      
      <h4>Incident brain disorder cohorts</h4>
      
      <p> Using diagnoses from 2001-2021 we identified all incident diagnoses
      in 2016-2021 where the person was under follow-up at the time of
      incident diagnoses.</p>
    
      <h4>Prevalent brain disorder cohorts</h4>
      
      <p> Using diagnoses from 2001-2021 we identified all persons under
      follow-up with a prior diagnoses on 2021-01-01.</p>
      
      <h4>Matched population</h4>
      
      <p>For each prevalent/incident brain disorder population we made
      a matched population with up to 100 controls from the general population,
      matched on sexand birth year. Furthermore, the controls needed to be
      living in Denmark and free of the index disease as the time of
      matching.</p>
      
      <h4>Finding Closest relatives</h5>
      
      <p> A closest relative was determined for each case/control based
      on the definition on the right. If a case did not have any relevant
      relatives, the case (and matched controls) was removed. Likewise, all
      controls with no relevant relatives were also removed. Additionally, controls
      with a type of closest relative that was different from the closest
      relative of the corresponding case was also removed. Finally, if a case
      had multiple controls with a valid closest relative, one was chosen at
      random. </p>
    ")
  })
  
  #### output$flowchart_plot ####
  output$flowchart_plot <- renderImage({
    list(src = here("diagrams", "relative_cohort_flowchart.svg"),
         width = 600,
         height = 800,
         alt = "Flowchart of studypopulation creation"
    )
  }, deleteFile = FALSE)
  
  #### output$flowchart_closest_relative_def ####
  output$flowchart_closest_relative_def <- renderUI({
    HTML("
      <h3>Closest relative definition</h4>
      
      <p>Algorithm to determine a closest relative. Note that for people
      aged 0-24, we will include both the mother and the father, if
      available, whereas for the other agegroups, we will find the closest
      relative based on the given prioritization. </p>
      
      <h4>Children and young people 0-24 years</h5>

      <ul>
        <li>Mother</li>
        <li>Father</li>
      </ul>

      <h4>Young adults and adults 25-64 years</h5>
      
      <ol>
        <li>Spouse/cohabiting partner</li>
        <li>Youngest parent living in same municipality</li>
        <li>Youngest parent living in same region</li>
        <li>Youngest parent</li>
        <li>Oldest adult child living in same municipality</li>
        <li>Oldest adult child living in same region</li>
        <li>Oldest adult child </li>
        <li>Oldest adult sibling living in same municipality</li>
        <li>Oldest adult sibling living in same region</li>
        <li>Oldest adult sibling</li>
      </ol>

      <h4>Older people 65+ years</h5>
      
      <ol>
        <li>Spouse/cohabiting partner</li>
        <li>Oldest adult child living in same municipality</li>
        <li>Oldest adult child living in same region</li>
        <li>Oldest adult child </li>
      </ol>
      
    ")
  })
  
  #### output$patient_characteristics_table ####
  output$patient_characteristics_table <- render_gt({
    
    if (input$patient_characteristics_pool_relative_types_id == "yes") {
      tbl_dat <- patient_characteristics %>%
        filter(agegroup == "0-24" | closest_relative_type == "pooled")
    } else if (input$patient_characteristics_pool_relative_types_id == "no") {
      tbl_dat <- patient_characteristics %>%
        filter(agegroup == "0-24" | closest_relative_type != "pooled")
    }
    
    tbl_dat <- tbl_dat %>%
      filter(
        var_name == input$patient_characteristics_var_name_id
        & population ==  input$patient_characteristics_population_id
        & same_education_req == input$patient_characteristics_same_education_req_id
      ) %>%
      select(
        var_name_label, population_label, agegroup, agegroup_label,
        closest_relative_type, closest_relative_type_label,
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
        names_prefix = "case_relative_"
      ) %>%
      mutate(
        label = case_match(
          label,
          "__n" ~ "Number of persons, n",
          "male" ~ "Male, n (%)",
          "age_index" ~ "Age, median (Q1;Q3)",
          "cci_g: title" ~ "CCI, n (%)",
          "cci_g: 0" ~ "\u2800\u28000",
          "cci_g: 1-2" ~ "\u2800\u28001-2",
          "cci_g: +3" ~ "\u2800\u2800+3",
          "education_level: title" ~ "Education level, n (%)",
          "education_level: no_education" ~ "\u2800\u2800No education",
          "education_level: short_education" ~ "\u2800\u2800Primary",
          "education_level: medium_education" ~ "\u2800\u2800Secondary",
          "education_level: long_education" ~ "\u2800\u2800Tertiary",
          "closest_relative_relation: title" ~ "Relation to patient, n (%)",
          "closest_relative_relation: partner" ~ "\u2800\u2800Partner",
          "closest_relative_relation: mother" ~ "\u2800\u2800Mother",
          "closest_relative_relation: father" ~ "\u2800\u2800Father",
          "closest_relative_relation: child" ~ "\u2800\u2800Child",
          "closest_relative_relation: sibling" ~ "\u2800\u2800Sibling",
          .default = label
        )
      ) %>%
      select(-label_order) %>%
      relocate(
        var_name_label, population_label, agegroup_label,
        closest_relative_type_label, label, case_relative_1, case_relative_0
      )

      tbl_dat <- tbl_dat %>%
        group_by(agegroup_label) %>%
        mutate(agegroup_first_row = row_number() == 1L) %>%
        group_by(agegroup_label, closest_relative_type_label) %>%
        mutate(closest_relative_first_row = row_number() == 1L) %>%
        ungroup() %>%
        mutate(
          agegroup_label = ifelse(agegroup_first_row, agegroup_label, ""),
          closest_relative_type_label = ifelse(closest_relative_first_row, closest_relative_type_label, "")
        )
      
      if (input$patient_characteristics_population_id == "prev_2021") {
        tbl_title <- "Characteristics of closest family relatives with a prevalent brain disorder, by patient age strata"
      } else if (input$patient_characteristics_population_id == "inc_2016_2021") {
        tbl_title <- "Characteristics of closest family relatives with an incident brain disorder, by patient age strata"
      }
      
      tbl_dat <- tbl_dat %>%
        select(-c(var_name_label, population_label)) %>%
        gt() %>%
        tab_header(
          title = tbl_title,
          subtitle = paste0(tbl_dat$var_name_label[1], " - ", tbl_dat$population_label[1])
        ) %>%
        cols_label(
          agegroup_label = md("**Age strata of index patients**"),
          closest_relative_type_label = md("**Closest relative type**"),
          label = md("**Characteristic of family<br>relatives by age strata of<br>index patients**"),
          case_relative_0 = md("**Comparison relative**"),
          case_relative_1 = md("**Closest family relatives<br>of brain disorder patients**")
        ) %>%
        tab_style(
          style = list(cell_text(decorate = "underline")),
          locations = cells_row_groups()
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = c(agegroup_label, closest_relative_type_label))
        ) %>%
        tab_style(
          style = cell_borders(
            sides = "top",
            color = "black",
            weight = px(2),
            style = "solid"
          ),
          locations = cells_body(rows = closest_relative_first_row)
        ) %>%
        tab_options(
          data_row.padding = px(0),
          column_labels.padding = px(0),
          heading.padding = px(0)
        )
      
      if (input$patient_characteristics_same_education_req_id == "yes") {
        tbl_dat <- tbl_dat %>%
          tab_footnote(
            footnote = "Comparison relatives are additionally required to have the same type of education as the relative to the index patient",
            location = cells_column_labels(columns = case_relative_0)
          ) 
      }
      
      if (input$patient_characteristics_pool_relative_types_id == "yes") {
        tbl_dat <- tbl_dat %>%
          tab_footnote(
            footnote = "Results are pooled for all closest relative types: Children, parents, partners, and siblings",
            location = cells_body(
              columns = closest_relative_type_label,
              rows = closest_relative_type == "pooled"
                     & closest_relative_first_row == 1L
                     & agegroup == "25-64"
            )
          ) %>%
          tab_footnote(
            footnote = "Results are pooled for all closest relative types: Children and partners",
            location = cells_body(
              columns = closest_relative_type_label,
              rows = closest_relative_type == "pooled"
                     & closest_relative_first_row == 1L
                     & agegroup == "65+"
            )
          )
      }
      
      tbl_dat  %>%
        cols_hide(columns = c(
          agegroup_first_row, closest_relative_first_row,
          closest_relative_type,agegroup
        ))

  })

  #### output$cost_analyses_table ####
  output$cost_analyses_table <- render_gt({
    
    # Restrict results
    tbl_dat <- cost_results %>%
      filter(
        var_name == input$cost_analyses_table_var_name_id
        & population == input$cost_analyses_table_population_id
        & same_education_req == input$cost_analyses_table_same_education_req_id
        & cost_period == input$cost_analyses_table_cost_period_id
      )
    
    if (input$cost_analyses_table_pool_relative_types_id == "yes") {
      tbl_dat <- tbl_dat %>%
        filter(
          agegroup == "0-24"
          | (agegroup != "0-24" & closest_relative_type == "pooled")
        )
    } else if (input$cost_analyses_table_pool_relative_types_id == "no") {
      tbl_dat <- tbl_dat %>%
        filter(closest_relative_type != "pooled")
    }
    
    tbl_dat <- tbl_dat %>%
      select(-c(
        cost_period, cost_period_label,
        same_education_req_label, var_pop, cost_component_order,
        cost_component_f
      ))
    
    # Prepare data for table
    tbl_dat <- tbl_dat %>%
      mutate(
        act_cost_mil_eur = act_cost / (eur_dkk_rate * 10**6),
        act_cost_py_eur = act_cost_py / eur_dkk_rate,
        att_cost_mil_eur = att_cost / (eur_dkk_rate * 10**6),
        att_cost_py_eur = att_cost_py / eur_dkk_rate
      )
    
    # Find number of patients in each agegroup/closest_relative_type strata
    tbl_dat <- tbl_dat %>%
      left_join(
        patient_characteristics %>%
          filter(case_relative == 1 & var == "__n") %>%
          select("same_education_req", "population", "var_name",
                 "agegroup", "closest_relative_type", "stat_num1"),
        by = c("same_education_req", "population", "var_name",
               "agegroup", "closest_relative_type")
      ) %>%
      rename(n_cases_strata = stat_num1) %>%
      mutate(n_cases_strata = formatC(as.integer(n_cases_strata), digits = 0, format = "d", big.mark = ",")) %>%
      select(-same_education_req)
    
    tbl_dat <- tbl_dat %>%
        group_by(agegroup_label) %>%
        mutate(agegroup_first_row = row_number() == 1L) %>%
        group_by(agegroup_label, closest_relative_type_label) %>%
        mutate(closest_relative_first_row = row_number() == 1L) %>%
        ungroup() %>%
        mutate(
          agegroup_label = ifelse(agegroup_first_row, agegroup_label, ""),
          closest_relative_type_label = ifelse(closest_relative_first_row, closest_relative_type_label, ""),
          n_cases_strata = ifelse(closest_relative_first_row, n_cases_strata, "")
        )
    

    
    if (input$cost_analyses_table_cost_period_id == "before_index") {
      tbl_title_p1 <- "Cost of illness in the year prior to index date"
    } else if (input$cost_analyses_table_cost_period_id == "after_index") {
      tbl_title_p1 <- "Cost of illness in the year after index date"
    }
    
    tbl_title_p2 <- "for family relatives of patients with"
    
    if (input$cost_analyses_table_population_id == "prev_2021") {
      tbl_title_p3 <- "a prevalent"
    } else if (input$cost_analyses_table_population_id == "inc_2016_2021") {
      tbl_title_p3 <- "an incident"
    }
    
    tbl_title_p4 <- "brain disorder, by patient age strata"
    
    tbl_title <- paste(tbl_title_p1, tbl_title_p2, tbl_title_p3, tbl_title_p4)
    
    tbl_dat <- tbl_dat %>%
      relocate(
        agegroup_label, closest_relative_type_label, n_cases_strata,
        cost_component_label, act_cost_mil_eur, act_cost_py_eur,
        att_cost_mil_eur, att_cost_py_eur
      ) %>%
      gt() %>%
      tab_header(
        title = tbl_title,
        subtitle = paste0(tbl_dat$var_name_label[1], " - ", tbl_dat$population_label[1])
      ) %>%
      tab_spanner(
        label = "Actual costs",
        columns = c(act_cost_mil_eur, act_cost_py_eur)
      ) %>%
      tab_spanner(
        label = "Attributable costs",
        columns = c(att_cost_mil_eur, att_cost_py_eur)
      ) %>%
      cols_label(
        agegroup_label = md("**Age strata of index patients**"),
        closest_relative_type_label = md("**Closest relative type**"),
        n_cases_strata = md("**Number of index patients**"),
        cost_component_label = md("**Cost component**"),
        act_cost_mil_eur = md("**Total (million EUR)**"),
        act_cost_py_eur = md("**Per person (EUR)**"),
        att_cost_mil_eur = md("**Total (million EUR)**"),
        att_cost_py_eur = md("**Per person (EUR)**")
      ) %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = c(agegroup_label, closest_relative_type_label))
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "top",
          color = "black",
          weight = px(2),
          style = "solid"
        ),
        locations = cells_body(rows = closest_relative_first_row)
      ) %>%
      fmt_number(
        columns = c(act_cost_mil_eur, att_cost_mil_eur),
        decimals = 1
      ) %>%
      fmt_number(
        columns = c(act_cost_py_eur, att_cost_py_eur),
        decimals = 0
      ) %>%
      tab_options(
        data_row.padding = px(0),
        column_labels.padding = px(0),
        heading.padding = px(0)
      ) 
    
    
    tbl_dat <- tbl_dat%>%
      tab_footnote(
        footnote = "Only including family relatives aged 18-65 years",
        location = cells_body(
          columns = cost_component_label,
          rows = cost_component == "lost_production_sickness"
        )
      )
    
    if (input$patient_characteristics_pool_relative_types_id == "yes") {
      tbl_dat <- tbl_dat %>%
        tab_footnote(
          footnote = "Results are pooled for all closest relative types: Children, parents, partners, and siblings",
          location = cells_body(
            columns = closest_relative_type_label,
            rows = closest_relative_type == "pooled"
                   & closest_relative_first_row == 1L
                   & agegroup == "25-64"
          )
        ) %>%
        tab_footnote(
          footnote = "Results are pooled for all closest relative types: Children and partners",
          location = cells_body(
            columns = closest_relative_type_label,
            rows = closest_relative_type == "pooled"
                   & closest_relative_first_row == 1L
                   & agegroup == "65+"
          )
        )
    }

    tbl_dat %>%
      cols_hide(columns = c(
        population, population_label, var_name, var_name_label,
        closest_relative_type, agegroup, cost_component,
        act_cost, act_py, act_cost_py, att_cost, att_py, att_cost_py,
        agegroup_first_row, closest_relative_first_row
      ))

  })
  
  #### output$cost_analyses_plot ####
  output$cost_analyses_plot <- renderPlot({

    # Restrict data
    plot_dat <- cost_results %>%
      mutate(
        act_cost = act_cost / (eur_dkk_rate * 10**6),
        act_cost_py = act_cost_py / eur_dkk_rate,
        att_cost = att_cost / (eur_dkk_rate * 10**6),
        att_cost_py = att_cost_py / eur_dkk_rate
      )
    plot_dat[["cost_var"]] <- plot_dat[[input$cost_analyses_plot_cost_type]]
    plot_dat <- plot_dat %>%
      select(-c(act_cost, act_cost_py, att_cost, att_cost_py, act_py, att_py, var_pop))

    if (input$cost_analyses_plot_pool_relative_types_id == "yes") {
      plot_dat <- plot_dat %>%
        filter(
          agegroup == "0-24"
          | (agegroup != "0-24" & closest_relative_type == "pooled")
        )
    } else if (input$cost_analyses_plot_pool_relative_types_id == "no") {
      plot_dat <- plot_dat %>%
        filter(closest_relative_type != "pooled")
    }

    plot_dat <- plot_dat %>%
      filter(
        var_name == input$cost_analyses_plot_var_name_id
        & population == input$cost_analyses_plot_population_id
        & same_education_req == input$cost_analyses_plot_same_education_req_id
        & cost_period == input$cost_analyses_plot_cost_period_id
      )

    if (input$cost_analyses_plot_cost_type %in% c("act_cost", "att_cost")) {
      x_axis_label <- "cost in million EUR"
    } else if (input$cost_analyses_plot_cost_type %in% c("act_cost_py", "att_cost_py")) {
      x_axis_label <- "cost in EUR"
    }
    
    plot_title <- case_when(
      input$cost_analyses_plot_cost_type == "act_cost" ~ "Total actual costs",
      input$cost_analyses_plot_cost_type == "act_cost_py" ~ "Per person actual costs",
      input$cost_analyses_plot_cost_type == "att_cost" ~ "Total attributable costs",
      input$cost_analyses_plot_cost_type == "att_cost_py" ~ "Per person attributable costs"
    )
    
    brain_disorder_label <- plot_dat %>%
      select(var_name_label) %>%
      distinct() %>%
      pull()
    
    plot_title <- paste0(
      plot_title,
      ' for "',
      brain_disorder_label,
      '" in the year'
    )
    
    if (input$cost_analyses_plot_cost_period_id == "before_index") {
      plot_title <- paste0(plot_title, " before the index date")
    } else if (input$cost_analyses_plot_cost_period_id == "after_index") {
      plot_title <- paste0(plot_title, " after the index date")
    }
    
    plot_subtitle <- case_when(
      input$cost_analyses_plot_population_id == "prev_2021" ~ "Prevalent cohort 2021",
      input$cost_analyses_plot_population_id == "inc_2016_2021" ~ "Incident cohort 2016-2021"
    )

    if (input$cost_analyses_plot_cost_type %in% c("att_cost", "att_cost_py")) {
      include_lost_production <- TRUE
    } else if (input$cost_analyses_plot_cost_type %in% c("act_cost", "act_cost_py")) {
      include_lost_production <- FALSE
    }
    
    # Determine min and max values of costs to determine x axis limits across
    # plots
    x_axis_limits <- plot_dat %>%
      mutate(
        cost_group = ifelse(
          cost_component == "lost_production_sickness",
          "lost_prod",
          "other"
        ),
        cost_var = ifelse(cost_group == "lost_prod", - cost_var, cost_var)
      ) %>%
      group_by(agegroup, closest_relative_type, cost_group) %>%
      summarize(total_cost = sum(cost_var, na.rm = TRUE), .groups = "keep") %>%
      group_by(cost_group) %>%
      summarize(
        max_cost = max(total_cost),
        min_cost = min(total_cost),
        .groups = "keep"
      )  %>%
      mutate(
        min_cost = min(0, min_cost),
        max_cost = max(0, max_cost)
      )
    
    x_axis_limits_other <- x_axis_limits %>%
      filter(cost_group == "other")
    x_axis_limits_other <- c(x_axis_limits_other$min_cost, x_axis_limits_other$max_cost)
    
    x_axis_limits_lost_prod <- x_axis_limits %>%
      filter(cost_group == "lost_prod")
    x_axis_limits_lost_prod <- c(x_axis_limits_lost_prod$min_cost, x_axis_limits_lost_prod$max_cost)

    plot_agegroup_0_24 <- plot_dat %>%
      filter(agegroup == "0-24") %>%
      make_cost_plot(
        title = "Agegroup 0-24",
        include_lost_production_plot = include_lost_production,
        x_axis_label = x_axis_label,
        include_legend = FALSE,
        x_axis_limits_left_plot = x_axis_limits_lost_prod,
        x_axis_limits_right_plot = x_axis_limits_other
      )

    plot_agegroup_25_64 <- plot_dat %>%
      filter(agegroup == "25-64") %>%
      make_cost_plot(
        title = "Agegroup 25-64",
        include_lost_production_plot = include_lost_production,
        x_axis_label = x_axis_label,
        include_legend = FALSE,
        x_axis_limits_left_plot = x_axis_limits_lost_prod,
        x_axis_limits_right_plot = x_axis_limits_other
      )

    plot_agegroup_65p <- plot_dat %>%
      filter(agegroup == "65+") %>%
      make_cost_plot(
        title = "Agegroup 65+",
        include_lost_production_plot = include_lost_production,
        x_axis_label = x_axis_label,
        include_legend = TRUE,
        x_axis_limits_left_plot = x_axis_limits_lost_prod,
        x_axis_limits_right_plot = x_axis_limits_other
      )

    wrap_elements(plot_agegroup_0_24) /
      wrap_elements(plot_agegroup_25_64) /
      wrap_elements(plot_agegroup_65p) +
      plot_annotation(
        title = plot_title,
        subtitle = plot_subtitle,
        theme = theme(
          plot.title = element_text(size = 20, colour = "black"),
          plot.subtitle = element_text(size = 18, colour = "black")
        )
      )

  })
  
 
  
}