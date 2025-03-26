library(bslib)
library(DT)
library(gt)
library(here)
library(patchwork)
library(shiny)
library(shinycssloaders)
library(tidyverse)

options(spinner.type = 6)

# Load helper functions
source("helpers.R")

# Load data
codelist <- readRDS(here("data", "codelist.rds"))

var_names_as_list <- codelist %>%
  filter(group == "bd_def") %>%
  select(var_name, var_label) %>%
  distinct()

var_names_as_list <- setNames(
  as.list(var_names_as_list$var_name),
  nm = var_names_as_list$var_label
)

page_navbar(
  title = "Version 0.0.2",
  theme = bs_theme(bootswatch = "lumen"),
  #### Main page ####
  nav_panel(title = "Main",
    card(htmlOutput("main_info"))
  ),
  #### Brain disease definitions ####
  nav_panel(title = "Definitions",
    navset_tab(
      nav_panel(title = "Brain diseases",
        htmlOutput("definitions_details_bd"),
        withSpinner(tableOutput("definitions_table_bd"))
      ),
      nav_panel(title = "Charlson Comorbidity Index",
        htmlOutput("definitions_details_cci"),
        withSpinner(tableOutput("definitions_table_cci"))
      ),
      nav_panel(title = "Education level",
        htmlOutput("definitions_details_education"),
        withSpinner(tableOutput("definitions_table_education"))
      )
    )
  ),
  #### Studypopulation flowchart ####
  nav_panel(title = "Studypopulation flowchart",
    layout_columns(col_widths = c(4, 4, 4),
      card(htmlOutput("flowchart_text")),
      card(withSpinner(imageOutput("flowchart_plot"))),
      card(htmlOutput("flowchart_closest_relative_def"))
    )
  ),
  #### Patient characteristics ####
  nav_panel(title = "Patient characteristics",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "patient_characteristics_var_name_id",
          label = "Brain disorder",
          choices = var_names_as_list
        ),
        selectInput(
          inputId = "patient_characteristics_population_id",
          label = "Population",
          choices = list(
            "Prevalent cohort 2021" = "prev_2021",
            "Incident cohort 2016-2021" = "inc_2016_2021"
          )
        ),
        selectInput(
          inputId = "patient_characteristics_same_education_req_id",
          label = "Comparison relatives are additionally required to have the same level of education as the closest relative of the associated brain disorder patient",
          choices = list(
            "Yes" = "yes",
            "No" = "no"
          )
        ),
        selectInput(
          inputId = "patient_characteristics_pool_relative_types_id",
          label = "Pool results for relative types for index patients aged 25 and older",
          choices = list(
            "Yes" = "yes",
            "No" = "no"
          )
        )
      ),
      withSpinner(gt_output("patient_characteristics_table"))
    )
  ),
  #### Cost analyses ####
  nav_panel(title = "Cost analyses",
    navset_tab(
      nav_panel(title = "Tables",
        layout_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "cost_analyses_table_var_name_id",
              label = "Brain disorder",
              choices = var_names_as_list
            ),
            selectInput(
              inputId = "cost_analyses_table_population_id",
              label = "Population",
              choices = list(
                "Prevalent cohort 2021" = "prev_2021",
                "Incident cohort 2016-2021" = "inc_2016_2021"
              )
            ),
            selectInput(
              inputId = "cost_analyses_table_same_education_req_id",
              label = "Comparison relatives are additionally required to have the same level of education as the closest relative of the associated brain disorder patient",
              choices = list(
                "Yes" = "yes",
                "No" = "no"
              )
            ),
            selectInput(
              inputId = "cost_analyses_table_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = list(
                "Yes" = "yes",
                "No" = "no"
              )
            ),
            selectInput(
              inputId = "cost_analyses_table_cost_period_id",
              label = "Cost period",
              choices = c(
                "Year after index date" = "after_index",
                "Year before index date" = "before_index"
              )
            )
          ),
          withSpinner(gt_output("cost_analyses_table"))
        )
      ),
      nav_panel(title = "Plots",
        layout_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "cost_analyses_plot_var_name_id",
              label = "Brain disorder",
              choices = var_names_as_list
            ),
            selectInput(
              inputId = "cost_analyses_plot_population_id",
              label = "Population",
              choices = list(
                "Prevalent cohort 2021" = "prev_2021",
                "Incident cohort 2016-2021" = "inc_2016_2021"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_cost_type",
              label = "Cost analysis",
              choices = list(
                "Attributable costs - Total" = "att_cost",
                "Attributable costs - Per person" = "att_cost_py",
                "Actual costs - Total" = "act_cost",
                "Actual costs - Per person" = "act_cost_py"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_same_education_req_id",
              label = "Comparison relatives are additionally required to have the same level of education as the closest relative of the associated brain disorder patient",
              choices = list(
                "Yes" = "yes",
                "No" = "no"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = list(
                "No" = "no",
                "Yes" = "yes"
              )
            ),
            selectInput(
              inputId = "cost_analyses_plot_cost_period_id",
              label = "Cost period",
              choices = c(
                "Year after index date" = "after_index",
                "Year before index date" = "before_index"
              )
            )
          ),
          withSpinner(plotOutput("cost_analyses_plot", height = "600px", width = "50%"))
        )
      )
    )
  ),
  #### Links ####
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(tags$a(shiny::icon("github"), "Source code", href = "https://github.com/thomas-rasmussen/brain_diseases_wp3_app", target = "_blank"))
  )
)
