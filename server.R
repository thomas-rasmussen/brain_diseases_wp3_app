
# Load data
codelist <- readRDS(here("data", "codelist.rds"))
patient_characteristics_relatives <- readRDS(here("data", "patient_characteristics_relatives.rds"))
patient_characteristics_cases <- readRDS(here("data", "patient_characteristics_cases.rds"))
cost_results <- readRDS(here("data", "cost_results.rds"))
assess_relatives <- readRDS(here("data", "assess_relatives.rds"))

# Load helper functions
source("helpers.R")

function(input, output, session) {
    
  #### Update input choices ####
  observe(updateSelectInput(
    inputId = "cost_analyses_plot_by_disorder_closest_relative_id",
    choices = {
      if (input$cost_analyses_plot_by_disorder_agegroup_id == "Children and young people (0-24 years)") {
        list("Fathers", "Mothers")
      } else {
        list("Pooled")
      }
    }
  ))

  
  #### output$main_info ####
  output$main_info <- renderUI({
    HTML("
      <h3 style='text-align:center'>Societal costs for closest family relatives of patients with brain
      disorders in Denmark: a population-based cohort study</h1>
      
      <h4 style='text-align:center'>Version 0.2.2</h4>
    ")
  })
  
  #### output$definitions_brain_disorders_table ####
  output$definitions_brain_disorders_table <- render_gt({
    
    # Extract definition and restructure data
    tbl_dat <- codelist %>%
      filter(
        group == "bd_def" & var_name != "bd_00"
        & (!is.na(code_include) | !is.na(code_include))
      ) %>%
      select(var_label, code_type, code_include, code_exclude) %>%
      pivot_wider(
        names_from = code_type,
        values_from = c(code_include, code_exclude)
      ) %>%
      select(-c(code_exclude_indo, code_exclude_atc)) %>%
      rename(
        brain_disorder = var_label,
        icd10_include = code_include_icd10,
        icd10_exclude = code_exclude_icd10,
        atc_include = code_include_atc,
        indo_include = code_include_indo
      ) %>%
      mutate(
        icd10_exclude = ifelse(is.na(icd10_exclude), "", icd10_exclude),
        atc_include = ifelse(is.na(atc_include), "", atc_include),
        indo_include = ifelse(is.na(indo_include), "", indo_include)
      ) %>%
      relocate(
        brain_disorder,
        icd10_include,
        icd10_exclude,
        atc_include,
        indo_include
      )
    
    # Make table
    tbl_dat %>%
      gt() %>%
      tab_header(title = "Codes used to define brain disorders") %>%
      tab_spanner(
        id = "spanner_icd10",
        label = md("**ICD-10 codes**"),
        columns = c(icd10_include, icd10_exclude)
      ) %>%
      tab_spanner(
        id = "spanner_atc",
        label = md("**ATC codes**"),
        columns = c(atc_include, indo_include )
      ) %>%
      cols_label(
        brain_disorder = md("**Brain disorder**"),
        icd10_include = md("**Include**"),
        icd10_exclude = md("**Exclude**"),
        atc_include = md("**Include**"),
        indo_include = md("**Indication**")
      ) %>%
      tab_options(
        data_row.padding = px(0),
        column_labels.padding = px(0),
        heading.padding = px(0)
      ) %>%
      tab_footnote(
        footnote = "All ICD-10 subcodes are included/excluded.",
        location = cells_column_spanners(spanners = "spanner_icd10")
      ) %>%
      tab_footnote(
        footnote = "All ATC subcodes are included/excluded.",
        location = cells_column_spanners(spanners = "spanner_atc")
      ) %>%
      tab_footnote(
        footnote = "
          If ATC indication codes are specified, the given included ATC
          codes are required to be accompanied with one of the listed
          indication codes.
        ",
        location = cells_column_labels(columns = indo_include)
      )
  })
 
  #### output$definitions_cci_table ####
  output$definitions_cci_table <- render_gt({
    
    # Extract definition and restructure data
    tbl_dat <- codelist %>%
      filter(
        group == "cci"
        & (!is.na(code_include) | !is.na(code_include))
      ) %>%
      select(var_label, code_type, code_include) %>%
      pivot_wider(names_from = code_type, values_from = code_include) %>%
      rename(
        cci_disease = var_label,
        icd8_include = icd8,
        icd10_include = icd10
      ) %>%
      relocate(cci_disease, icd8_include, icd10_include)

    # Make table
    tbl_dat %>%
      gt() %>%
      tab_header(title = "Charlson Comorbidity index definition") %>%
      tab_spanner(
        label = md("**Codes included**"),
        columns = c(icd8_include, icd10_include)
      ) %>%
      cols_align("left") %>%
      cols_label(
        cci_disease = md("**CCI disease**"),
        icd8_include = md("**ICD-8**"),
        icd10_include = md("**ICD-10**"),
      ) %>%
      tab_options(
        data_row.padding = px(0),
        column_labels.padding = px(0),
        heading.padding = px(0)
      ) %>%
      tab_footnote(
        footnote = "All ICD8 and ICD-10 subcodes are included",
        location = cells_column_spanners()
      )
    
  })
  
  ####output$definitions_education_table ####
  output$definitions_education_table <- render_gt({
    
    tbl_dat <- codelist %>%
      filter(group == "education") %>%
      select(var_label, code_type, code_include) %>%
      group_by(var_label) %>%
      mutate(code_include_group = paste(code_include, collapse = " ")) %>%
      select(-c(code_include, code_type)) %>%
      distinct() %>%
      rename(
        education_level = var_label,
        hfaudd_codes = code_include_group
      ) %>%
      ungroup()
    
    tbl_dat %>%
      gt() %>%
      tab_header(title = "Highest completed education definition") %>%
      cols_align("left") %>%
      cols_label(
        education_level = md("**Education level**"),
        hfaudd_codes = md("**HFAUDD codes**")
      ) %>%
      tab_options(
        data_row.padding = px(0),
        column_labels.padding = px(0),
        heading.padding = px(0)
      ) %>%
      tab_style(
        style = cell_text(size = "smaller",),
        locations = cells_body(columns = hfaudd_codes)
      ) %>%
      tab_footnote(
        footnote = "
          Highest completed education defined using the UDDA registry. The
          registry provides codes (HFAUDD) and dates (HF_VFRA) for completed
          educations. These educations were grouped into education levels of
          'No education', 'Short education', 'Medium education' and
          'Long education'. From the point a person completes an education at
          one level, he/she is considered to be at that education level until
          he/she completes an education at a higher level.
      "
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
      matched on sex and birth year. Furthermore, the controls needed to be
      living in Denmark and be free of the index disease at the time of
      matching.</p>
      
      <h4>Closest relatives</h4>
      
      <p> A closest relative was determined for each case and control based
      on the definition on the right. If a case did not have any relevant
      relatives, the case (and matched controls) was removed. Likewise, all
      controls with no relevant relatives were also removed.
      
      For each case's closest relative, we restricted the corresponding
      control closest relatives to the ones fulfilling the following criteria:
      
      <ol>
        <li>Control closest relative is the same type as the case closest relative
        <li>Control closest relative has same sex as case closest relative
        <li>Optional restriction in sensitivity analysis: control closest relative
            must have same sex and case closest relative, and must be born within
            five years of the case closest relative.
      </ol>
      
      Finally, if a case had multiple controls with a valid closest relative,
      one was chosen at random.</p>
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
      <small>
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
        <li>Youngest adult sibling living in same municipality</li>
        <li>Youngest adult sibling living in same region</li>
        <li>Youngest adult sibling</li>
      </ol>
      </small>
      
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
  
  #### output$patient_characteristics_relatives_table ####
  output$patient_characteristics_relatives_table <- render_gt({
    
    if (input$patient_characteristics_relatives_pool_relative_types_id == "Yes") {
      tbl_dat <- patient_characteristics_relatives %>%
        filter(
          agegroup == "Children and young people (0-24 years)"
          | closest_relative_group == "Pooled"
        )
    } else if (input$patient_characteristics_relatives_pool_relative_types_id == "No") {
      tbl_dat <- patient_characteristics_relatives %>%
        filter(
        agegroup == "Children and young people (0-24 years)"
        | closest_relative_group != "Pooled"
      )
    }
    
    tbl_dat <- tbl_dat %>%
      filter(
        var_name == input$patient_characteristics_relatives_var_name_id
        & population ==  input$patient_characteristics_relatives_population_id
        & additional_relative_req == input$patient_characteristics_relatives_additional_relative_req_id
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

      # Reorder closest_relative_group to follow the match the order of
      # priority in the closest relative definition
      tbl_dat <- tbl_dat %>% 
        mutate(
          row_order = row_number(),
          closest_relative_group_order = case_when(
            agegroup == "Children and young people (0-24 years)" & closest_relative_group == "Fathers" ~ 1.1,
            agegroup == "Children and young people (0-24 years)" & closest_relative_group == "Mothers" ~ 1.2,
            agegroup == "Young adults and adults (25-64 years)" & closest_relative_group == "Partners" ~ 2.1,
            agegroup == "Young adults and adults (25-64 years)" & closest_relative_group == "Parents" ~ 2.2,
            agegroup == "Young adults and adults (25-64 years)" & closest_relative_group == "Children" ~ 2.3,
            agegroup == "Young adults and adults (25-64 years)" & closest_relative_group == "Siblings" ~ 2.4,
            agegroup == "Older people (65+ years)" & closest_relative_group == "Partners" ~ 3.1,
            agegroup == "Older people (65+ years)" & closest_relative_group == "Children" ~ 3.2,
            agegroup == "Older people (65+ years)" & closest_relative_group == "Siblings" ~ 3.3
          )
        ) %>%
        arrange(closest_relative_group_order, row_order) %>%
        select(-closest_relative_group_order, -row_order)

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
        grepl("prevalent", ignore.case = TRUE, input$patient_characteristics_relatives_population_id) ~ "prevalent",
        grepl("incident", ignore.case = TRUE, input$patient_characteristics_relatives_population_id) ~ "incident",
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
      
      if (input$patient_characteristics_relatives_additional_relative_req_id == "Yes") {
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
      
      if (input$patient_characteristics_relatives_pool_relative_types_id == "yes") {
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
  
  #### output$patient_characteristics_cases_table ####
  output$patient_characteristics_cases_table <- render_gt({
    
    # Filter and restructure data
    tbl_dat <- patient_characteristics_cases %>%
      filter(
        var_name == input$patient_characteristics_cases_var_name_id
        & additional_relative_req == input$patient_characteristics_cases_additional_relative_req_id
      ) %>%
      mutate(
        pop_key = ifelse(
          grepl("incident", population, ignore.case = TRUE),
          "inc", 
          "prev"
        )
      ) %>%
      select(pop_key, agegroup, characteristic_level, stat_char) %>%
      pivot_wider(names_from = pop_key, values_from = stat_char) %>%
      relocate(agegroup, characteristic_level, prev, inc)

    # Remove repeated classification values
    tbl_dat <- tbl_dat %>%
      group_by(agegroup) %>%
      mutate(agegroup_first_row = row_number() == 1L) %>%
      ungroup() %>%
      mutate(
        agegroup = as.character(agegroup),
        agegroup = ifelse(agegroup_first_row, agegroup, ""),
      )

    # Construct table title
    title_brain_disorder <- input$patient_characteristics_cases_var_name_id
    tbl_title <- glue("
      Characteristics of index patients with '{title_brain_disorder},' by patient agegroup
    ")

  tbl_dat <- tbl_dat %>%
    gt() %>%
    tab_header(title = tbl_title) %>%
    cols_label(
      agegroup = md("**Age strata of index patients**"),
      characteristic_level = md("**Characteristic**"),
      prev = md("**Prevalent cohort 2021**"),
      inc = md("**Incident cohort 2016-2021**")
    ) %>%
    cols_align("left") %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(columns = c(agegroup))
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "top",
        color = "black",
        weight = px(2),
        style = "solid"
      ),
      locations = cells_body(rows = agegroup_first_row)
    ) %>%
    tab_options(
      data_row.padding = px(0),
      column_labels.padding = px(0),
      heading.padding = px(0)
    )

  tbl_dat  %>%
    cols_hide(columns = c(
      agegroup_first_row
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

    # Format columns to be included in table
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
        agegroup, closest_relative_group, n_cases_included,
        cost_component, act_cost_total, act_cost_per_person,
        att_cost_total, att_cost_per_person
      ) %>%
      gt() %>%
      tab_header(
        title = tbl_title,
        subtitle = paste0(tbl_dat$var_name[1], " - ", tbl_dat$population[1])
      ) %>%
      tab_spanner(
        label = "Actual costs",
        columns = c(act_cost_total, act_cost_per_person)
      ) %>%
      tab_spanner(
        label = "Attributable costs",
        columns = c(att_cost_total, att_cost_per_person)
      ) %>%
      cols_label(
        agegroup = md("**Age strata of index patients**"),
        closest_relative_group = md("**Closest relative type**"),
        n_cases_included = md("**Number of index patients included in cost component calculations**"),
        cost_component = md("**Cost component**"),
        act_cost_total = md("**Total (million EUR)**"),
        act_cost_per_person = md("**Per person (EUR)**"),
        att_cost_total = md("**Total (million EUR)**"),
        att_cost_per_person = md("**Per person (EUR)**")
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
        columns = c(act_cost_total, att_cost_total),
        decimals = 1
      ) %>%
      fmt_number(
        columns = c(act_cost_per_person, att_cost_per_person),
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

    if (input$cost_analyses_table_pool_relative_types_id == "Yes") {
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
        population, var_name, act_py, att_py, att_cost_prop, cost_period,
        agegroup_first_row, closest_relative_first_row,
        additional_relative_req
      ))

  })
  
  #### output$cost_analyses_plot_by_type ####
  output$cost_analyses_plot_by_type <- renderPlot({

    # Restrict data
    plot_dat <- cost_results
    plot_dat[["cost_var"]] <- plot_dat[[input$cost_analyses_plot_by_type_cost_type]]
    plot_dat <- plot_dat %>%
      select(-c(act_cost_total, act_cost_per_person, att_cost_total,
                att_cost_per_person, act_py, att_py, att_cost_prop))

    if (input$cost_analyses_plot_by_type_pool_relative_types_id == "Yes") {
      plot_dat <- plot_dat %>%
        filter(
          agegroup == "Children and young people (0-24 years)"
          | closest_relative_group == "Pooled"
        )
    } else if (input$cost_analyses_plot_by_type_pool_relative_types_id == "No") {
      plot_dat <- plot_dat %>%
        filter(closest_relative_group != "Pooled")
    }

    plot_dat <- plot_dat %>%
      filter(
        var_name == input$cost_analyses_plot_by_type_var_name_id
        & population == input$cost_analyses_plot_by_type_population_id
        & additional_relative_req == input$cost_analyses_plot_by_type_additional_relative_req_id
        & cost_period == input$cost_analyses_plot_by_type_cost_period_id
      )

    x_axis_label <- case_when(
      input$cost_analyses_plot_by_type_cost_type %in% c("act_cost_total", "att_cost_total") ~ "cost in million EUR",
      input$cost_analyses_plot_by_type_cost_type %in% c("act_cost_per_person", "att_cost_per_person") ~ "cost in EUR",
      .default = "UNKNOWN COST TYPE"
    )
    
    # Add number of patients to y-axis label variable
    n_patients <- plot_dat %>%
      filter(cost_component != "Lost productivity (income loss)") %>%
      select(agegroup, closest_relative_group, n_cases_included) %>%
      distinct()
      
    plot_dat <- plot_dat %>%
      select(-n_cases_included) %>%
      left_join(n_patients, by = c("agegroup", "closest_relative_group")) %>%
      mutate(
        closest_relative_group_label = paste0(
          closest_relative_group,
          " (",
          "n = ",
          formatC(n_cases_included, digits = 0, format = "d", big.mark = ","), 
          ")"
        )
      )

    # Construct plot title
    title_cost_type <- case_when(
      input$cost_analyses_plot_by_type_cost_type == "act_cost_total" ~ "Total actual costs",
      input$cost_analyses_plot_by_type_cost_type == "act_cost_per_person" ~ "Per person actual costs",
      input$cost_analyses_plot_by_type_cost_type == "att_cost_total" ~ "Total attributable costs",
      input$cost_analyses_plot_by_type_cost_type == "att_cost_per_person" ~ "Per person attributable costs"
    )
    title_brain_disorder <- plot_dat$var_name[1]
    title_cost_period <- case_when(
      input$cost_analyses_plot_by_type_cost_period_id == "Year before index date" ~ "before",
      input$cost_analyses_plot_by_type_cost_period_id == "Year after index date" ~ "after",
      .default = "UNKNOWN COST PERIOD"
    )
    
    plot_title <- glue(
      "{title_cost_type} for '{title_brain_disorder}' in the year \\
      {title_cost_period} the index date"
    )
    
    plot_subtitle <- input$cost_analyses_plot_by_type_population_id

    if (input$cost_analyses_plot_by_type_cost_type %in% c("att_cost_total", "att_cost_per_person")) {
      include_lost_production <- TRUE
    } else if (input$cost_analyses_plot_by_type_cost_type %in% c("act_cost_total", "act_cost_per_person")) {
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

    # Determine number of values of closest_relative_group in each agegroup
    n_groups <- plot_dat %>%
      select(agegroup, closest_relative_group) %>%
      distinct() %>%
      group_by(agegroup) %>%
      mutate(n_groups = row_number()) %>%
      summarize(n_groups = max(n_groups)) %>%
      ungroup()
  
    # Find maximum number of groups
    max_groups <- max(n_groups$n_groups)
    
    width_multiplier <- 0.9

    # Set bar widths in plots. it's not perfect, but it helps keep the
    # bar widths approximately even accros the plots for each agegroup
    bar_widths <- n_groups %>%
      mutate(
        width_multiplier = width_multiplier,
        width_fraction = width_multiplier * n_groups / max_groups
      )
    

  bar_width <- bar_widths %>%
    filter(agegroup == "Children and young people (0-24 years)") %>%
    select(width_fraction) %>%
    pull()
  plot_agegroup_0_24 <- plot_dat %>%
    filter(agegroup == "Children and young people (0-24 years)") %>%
    make_cost_plot(
      title = "Agegroup 0-24",
      include_lost_production_plot = include_lost_production,
      x_axis_label = x_axis_label,
      include_legend = FALSE,
      x_axis_limits_left_plot = x_axis_limits_lost_prod,
      x_axis_limits_right_plot = x_axis_limits_other,
      bar_width = bar_width
    )

  bar_width <- bar_widths %>%
    filter(agegroup == "Young adults and adults (25-64 years)") %>%
    select(width_fraction) %>%
    pull()
  plot_agegroup_25_64 <- plot_dat %>%
    filter(agegroup == "Young adults and adults (25-64 years)") %>%
    make_cost_plot(
      title = "Agegroup 25-64",
      include_lost_production_plot = include_lost_production,
      x_axis_label = x_axis_label,
      include_legend = FALSE,
      x_axis_limits_left_plot = x_axis_limits_lost_prod,
      x_axis_limits_right_plot = x_axis_limits_other,
      bar_width = bar_width
    )

  bar_width <- bar_widths %>%
    filter(agegroup == "Older people (65+ years)") %>%
    select(width_fraction) %>%
    pull()
  plot_agegroup_65p <- plot_dat %>%
    filter(agegroup == "Older people (65+ years)") %>%
    make_cost_plot(
      title = "Agegroup 65+",
      include_lost_production_plot = include_lost_production,
      x_axis_label = x_axis_label,
      include_legend = TRUE,
      x_axis_limits_left_plot = x_axis_limits_lost_prod,
      x_axis_limits_right_plot = x_axis_limits_other,
      bar_width = bar_width
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
  
  #### output$cost_analyses_plot_relative ####
  output$cost_analyses_plot_relative <- renderPlot({
    
    # Restrict data
    plot_dat <- cost_results %>%
      filter(cost_component == "Lost productivity (income loss)") %>%
      select(
        cost_period, additional_relative_req, population, var_name,
        agegroup, closest_relative_group, cost_component,
        n_cases_included, att_cost_prop
      )

    if (input$cost_analyses_plot_relative_pool_relative_types_id == "Yes") {
      plot_dat <- plot_dat %>%
        filter(
          agegroup == "Children and young people (0-24 years)"
          | closest_relative_group == "Pooled"
        )
    } else if (input$cost_analyses_plot_relative_pool_relative_types_id == "No") {
      plot_dat <- plot_dat %>%
        filter(closest_relative_group != "Pooled")
    }

    plot_dat <- plot_dat %>%
      filter(
        var_name == input$cost_analyses_plot_relative_var_name_id
        & population == input$cost_analyses_plot_relative_population_id
        & additional_relative_req == input$cost_analyses_plot_relative_additional_relative_req_id
        & cost_period == input$cost_analyses_plot_relative_cost_period_id
      )

    # Add number of patients to y-axis label variable
    plot_dat <- plot_dat %>%
      mutate(
        closest_relative_group_label = paste0(
          closest_relative_group,
          " (",
          "n = ",
          formatC(n_cases_included, digits = 0, format = "d", big.mark = ","), 
          ")"
        )
      )

    # Construct plot title
    title_cost_type <- "Relative attributable costs"
    title_brain_disorder <- plot_dat$var_name[1]
    title_cost_period <- case_when(
      input$cost_analyses_plot_relative_cost_period_id == "Year before index date" ~ "before",
      input$cost_analyses_plot_relative_cost_period_id == "Year after index date" ~ "after",
      .default = "UNKNOWN COST PERIOD"
    )
        
    plot_title <- glue(
      "{title_cost_type} for '{title_brain_disorder}' in the year \\
      {title_cost_period} the index date"
    )
        
    plot_subtitle <- input$cost_analyses_plot_relative_population_id
    
    # Determine min and max values of costs to determine x axis limits across
    # plots
    x_axis_limits <- plot_dat %>%
      summarize(
        max_cost = max(att_cost_prop, na.rm = TRUE),
        min_cost = min(att_cost_prop, na.rm = TRUE)
      )
    
    x_axis_limits <- c(
      min(x_axis_limits$min_cost, 0),
      max(x_axis_limits$max_cost, 0)
    )
    
    # Determine number of values of closest_relative_group in each agegroup
    n_groups <- plot_dat %>%
      select(agegroup, closest_relative_group) %>%
      distinct() %>%
      group_by(agegroup) %>%
      mutate(n_groups = row_number()) %>%
      summarize(n_groups = max(n_groups)) %>%
      ungroup()
  
    # Find maximum number of groups
    max_groups <- max(n_groups$n_groups)
    
    width_multiplier <- 0.9
    
    # Set bar widths in plots. it's not perfect, but it helps keep the
    # bar widths approximately even accros the plots for each agegroup
    bar_widths <- n_groups %>%
      mutate(
        width_multiplier = width_multiplier,
        width_fraction = width_multiplier * n_groups / max_groups
      )
    
    # agegroup 0-24
    bar_width <- bar_widths %>%
      filter(agegroup == "Children and young people (0-24 years)") %>%
      select(width_fraction) %>%
      pull()
    plot_agegroup_0_24 <- plot_dat %>%
      filter(agegroup == "Children and young people (0-24 years)") %>%
      make_cost_plot_relative(
        title = "Agegroup 0-24",
        include_legend = FALSE,
        x_axis_limits = x_axis_limits,
        bar_width = bar_width
      )

    # agegroup 25-64
    bar_width <- bar_widths %>%
      filter(agegroup == "Young adults and adults (25-64 years)") %>%
      select(width_fraction) %>%
      pull()
    plot_agegroup_25_64 <- plot_dat %>%
      filter(agegroup == "Young adults and adults (25-64 years)") %>%
      make_cost_plot_relative(
        title = "Agegroup 25-64",
        include_legend = FALSE,
        x_axis_limits = x_axis_limits,
        bar_width = bar_width
      )

    # agegroup 65+
    bar_width <- bar_widths %>%
      filter(agegroup == "Older people (65+ years)") %>%
      select(width_fraction) %>%
      pull()
    plot_agegroup_65p <- plot_dat %>%
      filter(agegroup == "Older people (65+ years)") %>%
      make_cost_plot_relative(
        title = "Agegroup 65+",
        include_legend = TRUE,
        x_axis_limits = x_axis_limits,
        bar_width = bar_width
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
  
  #### output$cost_analyses_plot_by_disorder ####
  output$cost_analyses_plot_by_disorder <- renderPlot({
    
    # Restrict data
    plot_dat <- cost_results %>%
      filter(
        agegroup == input$cost_analyses_plot_by_disorder_agegroup_id
        & closest_relative_group == input$cost_analyses_plot_by_disorder_closest_relative_id
        & population == input$cost_analyses_plot_by_disorder_population_id
        & additional_relative_req == input$cost_analyses_plot_by_disorder_additional_relative_req_id
        & cost_period == input$cost_analyses_plot_by_disorder_cost_period_id
      )

      plot_dat$cost_var <- plot_dat[[input$cost_analyses_plot_by_disorder_cost_type]]
      plot_dat <- plot_dat %>%
        select(var_name, cost_component, n_cases_included, cost_var)

    # Find maximum number of patients included in cost analysis for each brain disorder
    # And make label for y-axis variable
    n_included <- plot_dat %>%
      group_by(var_name) %>%
      summarize(n_cases_included = max(n_cases_included))

    plot_dat <- plot_dat %>%
      select(-n_cases_included) %>%
      left_join(n_included, by = "var_name") %>%
      mutate(
        var_name_label = paste0(
          var_name,
          " (n = ",
          formatC(n_cases_included, digits = 0, format = "d", big.mark = ","),
          ")"
        )
      ) %>%
      select(-n_cases_included)

    # Find total cost for each brain disorder to determine y-axis sorting in plot
    total_costs <- plot_dat %>%
      group_by(var_name_label) %>%
      summarize(total_cost = sum(cost_var, na.rm = TRUE), .groups = "keep") %>%
      ungroup() %>%
      arrange(total_cost)

    plot_dat <- plot_dat %>%
      mutate(
        var_name_label = factor(
          var_name_label,
          levels = total_costs$var_name_label,
          labels = total_costs$var_name_label
        )
      ) %>%
      select(-var_name)
    
    x_axis_label <- case_when(
      input$cost_analyses_plot_by_disorder_cost_type %in% c("act_cost_total", "att_cost_total") ~ "cost in million EUR",
      input$cost_analyses_plot_by_disorder_cost_type %in% c("act_cost_per_person", "att_cost_per_person") ~ "cost in EUR",
      .default = "UNKNOWN COST TYPE"
    )
    
    # Construct plot title
    title_cost_type <- case_when(
      input$cost_analyses_plot_by_disorder_cost_type == "act_cost_total" ~ "Total actual costs",
      input$cost_analyses_plot_by_disorder_cost_type == "act_cost_per_person" ~ "Per person actual costs",
      input$cost_analyses_plot_by_disorder_cost_type == "att_cost_total" ~ "Total attributable costs",
      input$cost_analyses_plot_by_disorder_cost_type == "att_cost_per_person" ~ "Per person attributable costs"
    )
    
    title_cost_period <- case_when(
      input$cost_analyses_plot_by_disorder_cost_period_id == "Year before index date" ~ "before",
      input$cost_analyses_plot_by_disorder_cost_period_id == "Year after index date" ~ "after",
      .default = "UNKNOWN COST PERIOD"
    )
    
    plot_title <- glue(
      "{title_cost_type} in the year {title_cost_period} the index date"
    )
    
      plot_subtitle <- input$cost_analyses_plot_by_disorder_population_id

    if (input$cost_analyses_plot_by_disorder_cost_type %in% c("att_cost_total", "att_cost_per_person")) {
      include_lost_production <- TRUE
    } else if (input$cost_analyses_plot_by_disorder_cost_type %in% c("act_cost_total", "act_cost_per_person")) {
      include_lost_production <- FALSE
    }

    # Split data
    
    plot_dat_lost_prod <- plot_dat %>%
      filter(cost_component == "Lost productivity (income loss)")
    
    plot_dat_other <- plot_dat %>%
      filter(cost_component != "Lost productivity (income loss)")
      

    # Make subplots
    
    # cvd-friendly qualitative color palette from "Fundamentals of Data
    # Visualization" figure 19.10
    cvd_color_palette <- c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442","#0072B2", "#D55E00"
    )

    plot_other <- plot_dat_other %>%
      ggplot(aes(x = cost_var, y = var_name_label, fill = cost_component)) +
      geom_bar(position = "stack", stat = "identity") +
      theme_bw() +
      scale_fill_manual(values = cvd_color_palette[3:6]) +
      scale_x_continuous(
        expand = expansion(mult = 0.01, add = 0)
      ) +
      labs(
        x = x_axis_label
      ) +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.line.x.bottom = element_line(colour = "black"),
        axis.text = element_text(size = 12, colour = "black")
      )
  
    if (include_lost_production) {
      plot_lost_prod <- plot_dat_lost_prod %>%
        ggplot(aes(x = -cost_var, y = var_name_label, fill = cost_component)) +
        geom_bar(position = "stack", stat = "identity") +
        theme_bw() +
        scale_fill_manual(values = cvd_color_palette[6]) +
        scale_x_continuous(
          expand = expansion(mult = 0.01, add = 0),
          labels = function(x) -x
        ) +
        labs(
          x = x_axis_label
        ) +
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.line.x.bottom = element_line(colour = "black"),
          axis.text = element_text(size = 12, colour = "black")
        )
    }
    
    if (include_lost_production) {
      combined_plot <- plot_lost_prod + plot_other
    } else {
      combined_plot <- plot_other
    }
      
    combined_plot +
      plot_annotation(title = plot_title)    

  })

  
  #### output$download_data_table ####
  
  # Reactive valaue for selected data
  datasetInput <- reactive({
    switch(
      input$download_data_select_data,
      "cost_results" = cost_results,
      "assess_relatives" = assess_relatives,
      "patient_characteristics_relatives" = patient_characteristics_relatives
    )
  })
  
  # Table of selected dataset
  output$download_data_table_preview <- render_gt({
    dataset_name <- input$download_data_select_data
    tbl_title <- glue("Preview of '{dataset_name}' data")
    datasetInput() %>%
      gt_preview(top_n = 10, bottom_n = 10) %>%
      tab_header(title = tbl_title) %>%
      tab_options(
        table.font.size = px(10),
        data_row.padding = px(0),
        column_labels.padding = px(0),
        heading.padding = px(0)
      )

      
  })
  
  # Downloadbale csv of selected dataset
  output$download_data_button <- downloadHandler(
    filename = function() {
      paste0(input$download_data_select_data, ".csv")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
 
  
}