library(bslib)
library(glue)
library(gt)
library(here)
library(patchwork)
library(scales)
library(shiny)
library(shinycssloaders)
library(tidyverse)

options(spinner.type = 6)

# Load helper functions
source("helpers.R")

# Load data
codelist <- readRDS(here("data", "codelist.rds"))

var_name_choices <- codelist %>%
  filter(group == "bd_def") %>%
  select(var_label) %>%
  distinct() %>%
  pull()
population_choices <- c("Prevalent cohort 2021", "Incident cohort 2016-2021")
additional_relative_req_choices <- c("Yes", "No")
pool_results_choices <- c("Yes", "No")
cost_period_choices <- c("Year after index date", "Year before index date")

page_navbar(
  title = "Brain disorders in Denmark",
  theme = bs_theme(bootswatch = "minty"),
  #### Main page ####
  nav_panel(title = "Main",
    card(htmlOutput("main_info"))
  ),
  #### Brain disease definitions ####
  nav_panel(title = "Definitions",
    navset_tab(
      nav_panel(title = "Brain disorders",
        withSpinner(gt_output("definitions_brain_disorders_table"))
      ),
      nav_panel(title = "Charlson Comorbidity Index",
        withSpinner(gt_output("definitions_cci_table"))
      ),
      nav_panel(title = "Education level",
        withSpinner(gt_output("definitions_education_table"))
      )
    )
  ),
  #### Studypopulation flowchart ####
  nav_panel(title = "Studypopulation flowchart",
    layout_columns(col_widths = c(8, 4),
      card(htmlOutput("flowchart_text")),
      card(htmlOutput("flowchart_closest_relative_def"))
    )
  ),
  #### Identification of "relative cohorts" ####
  nav_panel(title = 'Identification of relative cohorts',
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        selectInput(
          inputId = "identify_relatives_var_name_id",
          label = "Brain disorder",
          choices = var_name_choices
        ),
        selectInput(
          inputId = "identify_relatives_population_id",
          label = "Population",
          choices = population_choices
        ),
      ),
      withSpinner(gt_output("identify_relatives_table"))
    )
  ),
  #### Characteristics ####
  nav_panel(title = "Characteristics",
    navset_tab(
      nav_panel(title = "Closest relatives",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "patient_characteristics_relatives_var_name_id",
              label = "Brain disorder",
              choices = var_name_choices
            ),
            selectInput(
              inputId = "patient_characteristics_relatives_population_id",
              label = "Population",
              choices = population_choices
            ),
            selectInput(
              inputId = "patient_characteristics_relatives_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = pool_results_choices
            )
          ),
          withSpinner(gt_output("patient_characteristics_relatives_table"))
        )
      ),
      nav_panel(title = "Patients",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "patient_characteristics_cases_var_name_id",
              label = "Brain disorder",
              choices = var_name_choices
            )
          ),
          withSpinner(gt_output("patient_characteristics_cases_table"))
        )
      )
    )
  ),
  #### Cost analyses ####
  nav_panel(title = "Cost analyses",
    navset_tab(
      nav_panel(title = "Tables",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "cost_analyses_table_var_name_id",
              label = "Brain disorder",
              choices = var_name_choices
            ),
            selectInput(
              inputId = "cost_analyses_table_population_id",
              label = "Population",
              choices = population_choices
            ),
            selectInput(
              inputId = "cost_analyses_table_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = pool_results_choices
            ),
          ),
          withSpinner(gt_output("cost_analyses_table"))
        )
      ),
      nav_panel(title = "Plot - Cost by type of closest relative",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "cost_analyses_plot_by_type_var_name_id",
              label = "Brain disorder",
              choices = var_name_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_by_type_population_id",
              label = "Population",
              choices = population_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_by_type_cost_type",
              label = "Cost analysis",
              choices = list(
                "Attributable costs - Total" = "att_cost_total",
                "Attributable costs - Per person" = "att_cost_per_person",
                "Actual costs - Total" = "act_cost_total",
                "Actual costs - Per person" = "act_cost_per_person"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_by_type_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = pool_results_choices
            ),
          ),
          withSpinner(plotOutput("cost_analyses_plot_by_type", height = "600px", width = "50%"))
        )
      ),
      nav_panel(title = "Plot - Proportional income loss",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "cost_analyses_plot_relative_var_name_id",
              label = "Brain disorder",
              choices = var_name_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_relative_population_id",
              label = "Population",
              choices = population_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_relative_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = pool_results_choices
            ),
          ),
          withSpinner(plotOutput("cost_analyses_plot_relative", height = "600px", width = "50%"))
        )
      ),
      nav_panel(title = "Plot - Costs by brain disorder",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "cost_analyses_plot_by_disorder_agegroup_id",
              label = "Agegroup",
              choices = c(
                "Children and young people (0-24 years)",
                "Young adults and adults (25-64 years)",
                "Older people (65+ years)"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_by_disorder_closest_relative_id",
              label = "Closest relative group",
              choices = list(
                "Fathers",
                "Mothers"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_by_disorder_population_id",
              label = "Population",
              choices = population_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_by_disorder_cost_type",
              label = "Cost analysis",
              choices = list(
                "Attributable costs - Total" = "att_cost_total",
                "Attributable costs - Per person" = "att_cost_per_person",
                "Actual costs - Total" = "act_cost_total",
                "Actual costs - Per person" = "act_cost_per_person"
              )
            ),
          ),
          withSpinner(plotOutput("cost_analyses_plot_by_disorder", height = "600px", width = "50%"))
        )
      )
    )
  ),
  nav_spacer(),
  ### Download data ####
  nav_panel(title = "Download data",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "download_data_select_data",
          label = "Choose a dataset",
          choices = list(
            "Cost analyses" = "cost_results",
            "Identification of relative cohorts" = "assess_relatives",
            "Characteristics - Relatives" = "patient_characteristics_relatives"
          )
        ),
        downloadButton("download_data_button", "Download")
      ),
      withSpinner(gt_output("download_data_table_preview"))
    )
  ),
  #### Links ####
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a(shiny::icon("github"), "Source code", href = "https://github.com/thomas-rasmussen/brain_diseases_wp3_app", target = "_blank"))
  )
)
