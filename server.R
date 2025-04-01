
# Load data
codelist <- readRDS(here("data", "codelist.rds"))
patient_characteristics <- readRDS(here("data", "patient_characteristics.rds"))
cost_results <- readRDS(here("data", "cost_results.rds"))
assess_relatives <- readRDS(here("data", "assess_relatives.rds"))

# Load helper functions
source("helpers.R")

function(input, output, session) {
    
  #### output$main_info ####
  output$main_info <- renderUI({
    HTML("
      <h3 style='text-align:center'>Societal costs for closest family relatives of patients with brain
      disorders in Denmark: a population-based cohort study</h1>
      
      <h4 style='text-align:center'>Version 0.1.1</h4>
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
  
  #### output$identify_relatives_table ####
  output$identify_relatives_table <- render_gt({
    tbl_dat <- assess_relatives %>%
      filter(
        var_name == input$identify_relatives_var_name_id
        & population == input$identify_relatives_population_id
        & additional_relative_req == input$identify_relatives_additional_relative_req_id
      ) %>%
      mutate(
        closest_relative_group_pct = 100 * closest_relative_group_n/n_agegroup,
        has_control_closest_rel_pct = 100 * has_control_closest_rel_prop,
        closest_relative_group_n_pct = paste0(
          formatC(closest_relative_group_n, digits = 0, format = "d", big.mark = ","),
          " (",
          formatC(round(closest_relative_group_pct), digits = 0, format = "d"),
          "%)"
        ),
        closest_relative_group_n_pct = ifelse(
          closest_relative_group_n_pct == "NA (NA%)",
          "n/a",
          closest_relative_group_n_pct
        ),
        has_control_closest_rel_pct = paste0(
          formatC(round(has_control_closest_rel_pct), digits = 0, format = "d")
        ),
        has_control_closest_rel_pct =ifelse(
          has_control_closest_rel_pct == "NA",
          "n/a",
          paste0(has_control_closest_rel_pct, "%")
        ),
        n_agegroup = formatC(n_agegroup, digits = 0, format = "d", big.mark = ","),
        agegroup_n_label = paste0(agegroup, "\nn = ", n_agegroup),
        closest_relative_group_label_n_pct = paste0(
          closest_relative_group, "\nn = ", closest_relative_group_n_pct
        )
      ) %>%
      select(
        agegroup_n_label, closest_relative_group_label_n_pct,
        has_control_closest_rel_pct, var_name, population
      )
    
    # Only show agegroup values in first row
    tbl_dat <- tbl_dat %>%
      group_by(agegroup_n_label) %>%
      mutate(
        first_agegroup_row = row_number() == 1L,
        agegroup_n_label = ifelse(first_agegroup_row, agegroup_n_label, ""),
      ) %>%
      ungroup()

    # Construct table title
    title_pop_type <- case_when(
        grepl("prevalent", ignore.case = TRUE, input$identify_relatives_population_id) ~ "prevalent",
        grepl("incident", ignore.case = TRUE, input$identify_relatives_population_id) ~ "incident",
      .default = "UNKNOWN POPULATION TYPE"
    )
    title_brain_disorder <- tbl_dat$var_name[1]

    tbl_title <- glue("
      Source population of patients with {title_pop_type} \\
      '{title_brain_disorder}', relatives identified and percentage with at \\
      lest one eligible comparator
    ")
    
    tbl_dat <- tbl_dat %>%
      gt() %>%
      tab_header(
          title = tbl_title,
          subtitle = paste0(tbl_dat$var_name[1], " - ", tbl_dat$population[1])
        ) %>%
      cols_label(
        agegroup_n_label = md("**Agegroup of index patients in matched population**"),
        closest_relative_group_label_n_pct = md("**Closest relative type identified**"),
        has_control_closest_rel_pct = md("**Available control with same type of closest relative**")
      ) %>%
      # Preserve whitespace, ie newline symbols, in table cells
      tab_style(style = cell_text(whitespace = "pre"), locations = cells_body()) %>%
      # Make parts of cell text bold
      text_transform(
        locations = cells_body(columns = c(closest_relative_group_label_n_pct, agegroup_n_label)),
        fn = function(x) {
          gsub("^(.+)(n = .+)$", "<span style='font-weight:bold'>\\1</span>\\2", x)
        }
      ) %>%
      tab_style(
        style = cell_borders(
          sides = "top",
          color = "black",
          weight = px(2),
          style = "solid"
        ),
        locations = cells_body(rows = first_agegroup_row)
      )
    
      if (input$identify_relatives_additional_relative_req_id == "Yes") {
        tbl_dat <- tbl_dat %>%
          tab_footnote(
            footnote = "
              Comparison relatives are additionally required to fulfill
              the following with respect to the closest relative of the
              associated brain disorder patient:
              1) Have the same level of education
              2) Being born within five years of the person",
              location = cells_column_labels(columns = has_control_closest_rel_pct)
          ) 
      }
    
      tbl_dat %>%
        cols_hide(columns = c(var_name, population, first_agegroup_row))

  })
  
  #### output$patient_characteristics_table ####
  output$patient_characteristics_table <- render_gt({
    
    if (input$patient_characteristics_pool_relative_types_id == "Yes") {
      tbl_dat <- patient_characteristics %>%
        filter(
          agegroup == "Children and young people (0-24 years)"
          | closest_relative_group == "Pooled"
        )
    } else if (input$patient_characteristics_pool_relative_types_id == "No") {
      tbl_dat <- patient_characteristics %>%
        filter(
        agegroup == "Children and young people (0-24 years)"
        | closest_relative_group != "Pooled"
      )
    }
    
    tbl_dat <- tbl_dat %>%
      filter(
        var_name == input$patient_characteristics_var_name_id
        & population ==  input$patient_characteristics_population_id
        & additional_relative_req == input$patient_characteristics_additional_relative_req_id
      ) %>%
      select(
        var_name, population, agegroup, closest_relative_group,
        characteristic_variable, characteristic_level, case_relative, stat_char
      ) %>%
      pivot_wider(
        names_from = case_relative,
        values_from = stat_char,
        names_prefix = "case_relative_"
      ) %>%
      relocate(
        var_name, population, agegroup, closest_relative_group,
        characteristic_level, case_relative_1, case_relative_0
      )

      tbl_dat <- tbl_dat %>%
        group_by(agegroup) %>%
        mutate(agegroup_first_row = row_number() == 1L) %>%
        group_by(agegroup, closest_relative_group) %>%
        mutate(closest_relative_first_row = row_number() == 1L) %>%
        ungroup() %>%
        mutate(
          agegroup = as.character(agegroup),
          agegroup = ifelse(agegroup_first_row, agegroup, ""),
          closest_relative_group = as.character(closest_relative_group),
          closest_relative_group = ifelse(closest_relative_first_row, closest_relative_group, "")
        )
      
      # Construct table title
      title_pop_type <- case_when(
        grepl("prevalent", ignore.case = TRUE, input$patient_characteristics_population_id) ~ "prevalent",
        grepl("incident", ignore.case = TRUE, input$patient_characteristics_population_id) ~ "incident",
        .default = "UNKNOWN POPULATION TYPE"
      )
      title_brain_disorder <- tbl_dat$var_name[1]
      tbl_title <- glue("
        Characteristics of closest family relatives with {title_pop_type} \\
        '{title_brain_disorder},' by patient agegroup
      ")
      
      tbl_dat <- tbl_dat %>%
        select(-c(var_name, population)) %>%
        gt() %>%
        tab_header(
          title = tbl_title,
          subtitle = paste0(tbl_dat$var_name[1], " - ", tbl_dat$population[1])
        ) %>%
        cols_label(
          agegroup = md("**Age strata of index patients**"),
          closest_relative_group = md("**Closest relative type**"),
          characteristic_level = md("**Characteristic of family<br>relatives by age strata of<br>index patients**"),
          case_relative_0 = md("**Comparison relative**"),
          case_relative_1 = md("**Closest family relatives<br>of brain disorder patients**")
        ) %>%
        cols_align("left") %>%
        tab_style(
          style = list(cell_text(decorate = "underline")),
          locations = cells_row_groups()
        ) %>%
        tab_style(
          style = list(cell_text(weight = "bold")),
          locations = cells_body(columns = c(agegroup, closest_relative_group))
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
      
      if (input$patient_characteristics_additional_relative_req_id == "Yes") {
        tbl_dat <- tbl_dat %>%
          tab_footnote(
            footnote = "
              Comparison relatives are additionally required to fulfill
              the following with respect to the closest relative of the
              associated brain disorder patient:
              1) Have the same level of education
              2) Being born within five years of the person",
              location = cells_column_labels(columns = case_relative_0)
          ) 
      }
      
      if (input$patient_characteristics_pool_relative_types_id == "yes") {
        tbl_dat <- tbl_dat %>%
          tab_footnote(
            footnote = "Results are pooled for all closest relative types: Children, parents, partners, and siblings",
            location = cells_body(
              columns = closest_relative_group,
              rows = closest_relative_group == "Pooled"
                     & closest_relative_first_row == 1L
                     & agegroup == "Young adults and adults (25-64 years)"
            )
          ) %>%
          tab_footnote(
            footnote = "Results are pooled for all closest relative types: Children and partners",
            location = cells_body(
              columns = closest_relative_group,
              rows = closest_relative_group == "Pooled"
                     & closest_relative_first_row == 1L
                     & agegroup == "Older people (65+ years)"
            )
          )
      }
      
      tbl_dat  %>%
        cols_hide(columns = c(
          agegroup_first_row, closest_relative_first_row,
          characteristic_variable
        ))

  })

  #### output$cost_analyses_table ####
  output$cost_analyses_table <- render_gt({
    
    # Restrict results
    tbl_dat <- cost_results %>%
      filter(
        var_name == input$cost_analyses_table_var_name_id
        & population == input$cost_analyses_table_population_id
        & additional_relative_req == input$cost_analyses_table_additional_relative_req_id
        & cost_period == input$cost_analyses_table_cost_period_id
      )
 
    if (input$cost_analyses_table_pool_relative_types_id == "Yes") {
      tbl_dat <- tbl_dat %>%
        filter(
          agegroup == "Children and young people (0-24 years)"
          | closest_relative_group == "Pooled"
        )
    } else if (input$cost_analyses_table_pool_relative_types_id == "No") {
      tbl_dat <- tbl_dat %>%
        filter(closest_relative_group != "Pooled")
    }

    # Find number of patients in each agegroup/closest_relative_group strata
    tbl_dat <- tbl_dat %>%
      left_join(
        patient_characteristics %>%
          filter(case_relative == 1 & characteristic_variable == "__n") %>%
          select("additional_relative_req", "population", "var_name",
                 "agegroup", "closest_relative_group", "stat_num1"),
        by = c("additional_relative_req", "population", "var_name",
               "agegroup", "closest_relative_group")
      ) %>%
      rename(n_cases_strata = stat_num1) %>%
      mutate(n_cases_strata = formatC(as.integer(n_cases_strata), digits = 0, format = "d", big.mark = ",")) %>%
      select(-additional_relative_req)

    tbl_dat <- tbl_dat %>%
        group_by(agegroup) %>%
        mutate(agegroup_first_row = row_number() == 1L) %>%
        group_by(agegroup, closest_relative_group) %>%
        mutate(closest_relative_first_row = row_number() == 1L) %>%
        ungroup() %>%
        mutate(
          agegroup = as.character(agegroup),
          agegroup = ifelse(agegroup_first_row, agegroup, ""),
          closest_relative_group = as.character(closest_relative_group),
          closest_relative_group = ifelse(closest_relative_first_row, closest_relative_group, ""),
          n_cases_strata = ifelse(closest_relative_first_row, n_cases_strata, ""),
          n_cases_included = formatC(n_cases_included, digits = 0, format = "d", big.mark = ",")
        )


    # Construct table title
    title_cost_period <- case_when(
      input$cost_analyses_table_cost_period_id == "Year after index date" ~ "prior to",
      input$cost_analyses_table_cost_period_id == "Year before index date" ~ "after",
      .default = "UNKNOWN COST PERIOD"
    )
    title_pop_type <- case_when(
        grepl("prevalent", ignore.case = TRUE, input$cost_analyses_table_population_id) ~ "prevalent",
        grepl("incident", ignore.case = TRUE, input$cost_analyses_table_population_id) ~ "incident",
      .default = "UNKNOWN POPULATION TYPE"
    )
    title_brain_disorder <- tbl_dat$var_name[1]

    tbl_title <- glue("
      Cost of illness in the year {title_cost_period} index date \\
      for family relatives of patients with \\
      {title_pop_type} '{title_brain_disorder}', by patient agegroup
    ")

    tbl_dat <- tbl_dat %>%
      relocate(
        agegroup, closest_relative_group, n_cases_strata,
        cost_component, n_cases_included, act_cost, act_cost_py,
        att_cost, att_cost_py
      ) %>%
      gt() %>%
      tab_header(
        title = tbl_title,
        subtitle = paste0(tbl_dat$var_name[1], " - ", tbl_dat$population[1])
      ) %>%
      tab_spanner(
        label = "Actual costs",
        columns = c(act_cost, act_cost_py)
      ) %>%
      tab_spanner(
        label = "Attributable costs",
        columns = c(att_cost, att_cost_py)
      ) %>%
      cols_label(
        agegroup = md("**Age strata of index patients**"),
        closest_relative_group = md("**Closest relative type**"),
        n_cases_strata = md("**Number of index patients**"),
        cost_component = md("**Cost component**"),
        n_cases_included = md("**Number of index patients included in cost component calculations**"),
        act_cost = md("**Total (million EUR)**"),
        act_cost_py = md("**Per person (EUR)**"),
        att_cost = md("**Total (million EUR)**"),
        att_cost_py = md("**Per person (EUR)**")
      ) %>%
      cols_align("left") %>%
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_body(columns = c(agegroup, closest_relative_group))
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
        columns = c(act_cost, att_cost),
        decimals = 1
      ) %>%
      fmt_number(
        columns = c(act_cost_py, att_cost_py),
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
          columns = cost_component,
          rows = cost_component == "Lost productivity (income loss)"
        )
      )

    if (input$patient_characteristics_pool_relative_types_id == "Yes") {
      tbl_dat <- tbl_dat %>%
        tab_footnote(
          footnote = "Results are pooled for all closest relative types: Children, parents, partners, and siblings",
          location = cells_body(
            columns = closest_relative_group,
            rows = closest_relative_group == "Pooled"
                   & closest_relative_first_row == 1L
                   & agegroup == "Young adults and adults (25-64 years)"
          )
        ) %>%
        tab_footnote(
          footnote = "Results are pooled for all closest relative types: Children and partners",
          location = cells_body(
            columns = closest_relative_group,
            rows = closest_relative_group == "Pooled"
                   & closest_relative_first_row == 1L
                   & agegroup == "Older people (65+ years)"
          )
        )
    }

    tbl_dat %>%
      cols_hide(columns = c(
        population, var_name, act_py, att_py, cost_period,
        agegroup_first_row, closest_relative_first_row
      ))

  })
  
  #### output$cost_analyses_plot ####
  output$cost_analyses_plot <- renderPlot({

    # Restrict data
    plot_dat <- cost_results
    plot_dat[["cost_var"]] <- plot_dat[[input$cost_analyses_plot_cost_type]]
    plot_dat <- plot_dat %>%
      select(-c(act_cost, act_cost_py, att_cost, att_cost_py, act_py, att_py))

    if (input$cost_analyses_plot_pool_relative_types_id == "Yes") {
      plot_dat <- plot_dat %>%
        filter(
          agegroup == "Children and young people (0-24 years)"
          | closest_relative_group == "Pooled"
        )
    } else if (input$cost_analyses_plot_pool_relative_types_id == "No") {
      plot_dat <- plot_dat %>%
        filter(closest_relative_group != "Pooled")
    }

    plot_dat <- plot_dat %>%
      filter(
        var_name == input$cost_analyses_plot_var_name_id
        & population == input$cost_analyses_plot_population_id
        & additional_relative_req == input$cost_analyses_plot_additional_relative_req_id
        & cost_period == input$cost_analyses_plot_cost_period_id
      )

    x_axis_label <- case_when(
      input$cost_analyses_plot_cost_type %in% c("act_cost", "att_cost") ~ "cost in million EUR",
      input$cost_analyses_plot_cost_type %in% c("act_cost_py", "att_cost_py") ~ "cost in EUR",
      .default = "UNKNOWN COST TYPE"
    )

    # Construct plot title
    title_cost_type <- case_when(
      input$cost_analyses_plot_cost_type == "act_cost" ~ "Total actual costs",
      input$cost_analyses_plot_cost_type == "act_cost_py" ~ "Per person actual costs",
      input$cost_analyses_plot_cost_type == "att_cost" ~ "Total attributable costs",
      input$cost_analyses_plot_cost_type == "att_cost_py" ~ "Per person attributable costs"
    )
    title_brain_disorder <- plot_dat$var_name[1]
    title_cost_period <- case_when(
      input$cost_analyses_plot_cost_period_id == "Year before index date" ~ "before",
      input$cost_analyses_plot_cost_period_id == "Year after index date" ~ "after",
      .default = "UNKNOWN COST PERIOD"
    )
    
    plot_title <- glue(
      "{title_cost_type} for '{title_brain_disorder}' in the year \\
      {title_cost_period} the index date"
    )
    
    plot_subtitle <- input$cost_analyses_plot_population_id

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
          cost_component == "Lost productivity (income loss)",
          "lost_prod",
          "other"
        ),
        cost_var = ifelse(cost_group == "lost_prod", -cost_var, cost_var)
      ) %>%
      group_by(agegroup, closest_relative_group, cost_group) %>%
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
      filter(agegroup == "Children and young people (0-24 years)") %>%
      make_cost_plot(
        title = "Agegroup 0-24",
        include_lost_production_plot = include_lost_production,
        x_axis_label = x_axis_label,
        include_legend = FALSE,
        x_axis_limits_left_plot = x_axis_limits_lost_prod,
        x_axis_limits_right_plot = x_axis_limits_other
      )

    plot_agegroup_25_64 <- plot_dat %>%
      filter(agegroup == "Young adults and adults (25-64 years)") %>%
      make_cost_plot(
        title = "Agegroup 25-64",
        include_lost_production_plot = include_lost_production,
        x_axis_label = x_axis_label,
        include_legend = FALSE,
        x_axis_limits_left_plot = x_axis_limits_lost_prod,
        x_axis_limits_right_plot = x_axis_limits_other
      )

    plot_agegroup_65p <- plot_dat %>%
      filter(agegroup == "Older people (65+ years)") %>%
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