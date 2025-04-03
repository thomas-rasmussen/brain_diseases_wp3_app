library(bslib)
library(glue)
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

var_name_choices <- codelist %>%
  filter(group == "bd_def") %>%
  select(var_label) %>%
  distinct()
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
    layout_columns(col_widths = c(5, 4, 3),
      card(htmlOutput("flowchart_text")),
      card(withSpinner(imageOutput("flowchart_plot"))),
      card(htmlOutput("flowchart_closest_relative_def"))
    )
  ),
  #### Identification of populations of relatives ####
  nav_panel(title = "Identification of populations of relatives",
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
        selectInput(
          inputId = "identify_relatives_additional_relative_req_id",
          label = "Add additional requirements for comparison relatives*",
          choices = additional_relative_req_choices
        ),
        html(glue(
          "<pre style='font-size:0.5vw'>*Comparison relatives are additionally required to fulfill
          the following with respect to the closest relative of the
          associated brain disorder patient:
          - Have the same level of education
          - Being born within five years of the person
          </pre>"
        ))
      ),
      withSpinner(gt_output("identify_relatives_table"))
    )
  ),
  #### Patient characteristics ####
  nav_panel(title = "Patient characteristics",
    layout_sidebar(
      sidebar = sidebar(
        width = 400,
        selectInput(
          inputId = "patient_characteristics_var_name_id",
          label = "Brain disorder",
          choices = var_name_choices
        ),
        selectInput(
          inputId = "patient_characteristics_population_id",
          label = "Population",
          choices = population_choices
        ),
        selectInput(
          inputId = "patient_characteristics_additional_relative_req_id",
          label = "Add additional requirements for comparison relatives*",
          choices = additional_relative_req_choices
        ),
        selectInput(
          inputId = "patient_characteristics_pool_relative_types_id",
          label = "Pool results for relative types for index patients aged 25 and older",
          choices = pool_results_choices
        ),
        html(glue(
          "<pre style='font-size:0.5vw'>*Comparison relatives are additionally required to fulfill
          the following with respect to the closest relative of the
          associated brain disorder patient:
          - Have the same level of education
          - Being born within five years of the person
          </pre>"
        ))
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
              inputId = "cost_analyses_table_additional_relative_req_id",
              label = "Add additional requirements for comparison relatives*",
              choices = additional_relative_req_choices
            ),
            selectInput(
              inputId = "cost_analyses_table_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = pool_results_choices
            ),
            selectInput(
              inputId = "cost_analyses_table_cost_period_id",
              label = "Cost period",
              choices = cost_period_choices
            ),
            html(glue(
              "<pre style='font-size:0.5vw'>*Comparison relatives are additionally required to fulfill
              the following with respect to the closest relative of the
              associated brain disorder patient:
              - Have the same level of education
              - Being born within five years of the person
              </pre>"
            ))
          ),
          withSpinner(gt_output("cost_analyses_table"))
        )
      ),
      nav_panel(title = "Plots",
        layout_sidebar(
          sidebar = sidebar(
            width = 400,
            selectInput(
              inputId = "cost_analyses_plot_var_name_id",
              label = "Brain disorder",
              choices = var_name_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_population_id",
              label = "Population",
              choices = population_choices
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
              inputId = "cost_analyses_plot_additional_relative_req_id",
              label = "Add additional requirements for comparison relatives*",
              choices = additional_relative_req_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_pool_relative_types_id",
              label = "Pool results for relative types for index patients aged 25 and older",
              choices = pool_results_choices
            ),
            selectInput(
              inputId = "cost_analyses_plot_cost_period_id",
              label = "Cost period",
              choices = cost_period_choices
            ),
            html(glue(
              "<pre style='font-size:0.5vw'>*Comparison relatives are additionally required to fulfill
              the following with respect to the closest relative of the
              associated brain disorder patient:
              - Have the same level of education
              - Being born within five years of the person
              </pre>"
            ))
          ),
          withSpinner(plotOutput("cost_analyses_plot", height = "600px", width = "50%"))
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
            "Identification of populations of relatives" = "assess_relatives",
            "Patient characteristics" = "patient_characteristics"
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
