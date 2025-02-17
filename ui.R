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
  title = "Brain diseases app (version vx.x.x)",
  theme = bs_theme(bootswatch = "lumen"),
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
      )
    )
  ),
  #### Patient characteristics ####
  nav_panel(title = "Patient characteristics",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          inputId = "patient_characteristics_var_name_id",
          label = "Brain disease",
          choices = var_names_as_list
        ),
        selectInput(
          inputId = "patient_characteristics_population_id",
          label = "Population",
          choices = list(
            "Incident cohort 2016-2021" = "inc_2016_2021",
            "Prevalent cohort 2021" = "prev_2021"
          )
        )
      ),
      withSpinner(tableOutput("patient_characteristics_table"))
    )
  ),
  #### Cost analyses ####
  nav_panel(title = "Cost analyses",
    navset_tab(
      # nav_panel(title = "Attributable costs - Total",
      #   layout_sidebar(
      #     sidebar = sidebar(
      #       selectInput(
      #         inputId = "cost_analyses_plot_att_total_population_id",
      #         label = "Population",
      #         choices = list(
      #           "Incident cohort 2016-2021" = "inc_2016_2021",
      #           "Prevalent cohort 2021" = "prev_2021"
      #         )
      #       )
      #     ),
      #     withSpinner(plotOutput("cost_analyses_plot_att_total", height = "600px", width = "1000px"))
      #   )
      # ),
      nav_panel(title = "Attributable costs - Per person"),
      nav_panel(title = "Data in table format",
        layout_sidebar(
          sidebar = sidebar(
            selectInput(
              inputId = "cost_analyses_table_var_name_id",
              label = "Brain disease",
              choices = c("All brain diseases" = "all", var_names_as_list)
            ),
            selectInput(
              inputId = "cost_analyses_table_population_id",
              label = "Population",
              choices = list(
                "All populations" = "all",
                "Incident cohort 2016-2021" = "inc_2016_2021",
                "Prevalent cohort 2021" = "prev_2021"
              )
            )
          ),
          withSpinner(tableOutput("cost_analyses_table"))
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
