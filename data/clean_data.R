# Import and clean data from project to be used in the app

library(dplyr)
library(here)
library(readr)
library(stringr)

version_id <- "v7_2025-03-26"


#### Import data ####

cost_results <- read_delim(
  here("data", version_id, paste0("all_cost_results_", version_id, ".csv")),
  delim = "|",
  col_types = list(
    .default = "d",
    "cost_component" = "c",
    "cost_period" = "c",
    "additional_relative_req" = "c",
    "var_pop" = "c",
    "closest_relative_group" = "c",
    "agegroup" = "c"
  )
)
  
codelist <-  read_delim(
  here("data", version_id, paste0("codelist_", version_id, ".csv")),
  delim = "|",
  col_types = list(.default = "c"),
  # Used readr::guess_encoding to guess what encoding was needed to correctly
  # read apostrophe in the parkinson disease label
  locale = readr::locale(encoding = "windows-1252")
)

patient_characteristics <-  read_delim(
  here("data", version_id, paste0("patient_characteristics_", version_id, ".csv")),
  delim = "|",
  col_types = list(.default = "c")
)
  
assess_relatives <- read_delim(
  here("data", version_id, paste0("assess_relatives_", version_id, ".csv")),
  delim = "|",
  col_types = list(
    .default = "d",
    "additional_relative_req" = "c",
    "var_pop" = "c",
    "agegroup" = "c",
    "closest_relative_type" = "c"
  )
)

#### Create additional datasets ####

var_name_labels <- codelist %>%
  filter(group %in% c("bd_def", "cci")) %>%
  select(var_name, var_label) %>%
  rename(var_name_label = var_label) %>%
  distinct()

cost_component_labels <- cost_results %>%
  select(cost_component) %>%
  distinct() %>%
  mutate(
    cost_component_label = case_match(
      cost_component,
      "secondary_sector" ~ "Hospital care",
      "filled_prescriptions" ~ "Filled prescriptions",
      "primary_sector" ~ "Primary sector",
      "lost_production_sickness" ~ "Lost productivity (income loss)",
      .default  = cost_component
    ),
    cost_component_f = factor(
      cost_component,
      levels = c("primary_sector",
                 "secondary_sector",
                 "filled_prescriptions",
                 "lost_production_sickness"
                ),
      labels = c("Primary sector",
                 "Hospital care",
                 "Filled prescriptions",
                 "Lost productivity (income loss)"
                )
                
      )
  )


population_labels <- codelist %>%
  filter(group == "population_def") %>%
  select(var_name) %>%
  mutate(
    population_label = case_match(
      var_name,
      "inc_2016_2021" ~ "Incident cohort 2016-2021",
      "prev_2021" ~ "Prevalent cohort 2021",
      .default = var_name
    )
  ) %>%
  rename(population = var_name)

agegroup_labels <- tibble(
  agegroup = c("0-24", "25-64", "65+"),
  agegroup_label = c("Children and young people (0-24 years)", "Young adults and adults (25-64 years)", "Older people (65+ years)")
)

cost_period_labels <- tibble(
  cost_period = c("before_index", "after_index"),
  cost_period_label = c("Before index date", "After index date")
)

additional_relative_req_labels <- tibble(
  additional_relative_req = c("no", "yes"),
  additional_relative_req_label = c("No", "Yes")
)

#### Add variables to data ####
cost_results <- cost_results %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    population = word(var_pop, 2, sep = "#"),
    closest_relative_group_label = case_match(
      closest_relative_group,
      "father" ~ "Fathers",
      "mother" ~ "Mothers",
      "child" ~ "Children",
      "parent" ~ "Parents",
      "partner" ~ "Partners",
      "pooled" ~ "Pooled",
      "sibling" ~ "Siblings"
    )
  ) %>%
  left_join(cost_component_labels, by = "cost_component") %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(population_labels, by = "population") %>%
  left_join(agegroup_labels, by = "agegroup") %>%
  left_join(additional_relative_req_labels, by = "additional_relative_req") %>%
  left_join(cost_period_labels, by = "cost_period")


dkk_to_eur <- codelist %>%
filter(var_name == "eur_to_dkk") %>%
select(code_include) %>%
pull() %>%
as.numeric()

patient_characteristics <- patient_characteristics %>%
  rename(
    stat_char = `__stat_char`,
    label = `__label`,
    var = `__var`,
    stat_num1 = `__stat_num1`,
    stat_num2 = `__stat_num2`,
    stat_num3 = `__stat_num3`
  ) %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(population_labels, by = "population") %>%
  left_join(agegroup_labels, by = "agegroup") %>%
  left_join(additional_relative_req_labels, by = "additional_relative_req") %>%
  mutate(
    closest_relative_group_label = case_match(
      closest_relative_group,
      "child" ~ "Children",
      "father" ~ "Fathers",
      "pooled" ~ "Pooled",
      "mother" ~ "Mothers",
      "parent" ~ "Parents",
      "partner" ~ "Partners",
      "sibling" ~ "Siblings"
    ),
    label_order = case_match(
      label,
      "__n" ~ 1,
      "male" ~ 2,
      "age_index" ~ 3,
      "cci_g: title" ~ 4,
      "cci_g: 0" ~ 5,
      "cci_g: 1-2" ~ 6,
      "cci_g: +3" ~ 7,
      "education_level: title" ~ 10,
      "education_level: no_education" ~ 11,
      "education_level: short_education" ~ 12,
      "education_level: medium_education" ~ 13,
      "education_level: long_education" ~ 14,
      "closest_relative_type: title" ~ 20,
      "closest_relative_type: mother" ~ 21,
      "closest_relative_type: father" ~ 22,
      "closest_relative_type: partner" ~ 23,
      "closest_relative_type: parent" ~ 24,
      "closest_relative_type: child" ~ 25,
      "closest_relative_type: sibling" ~ 26
    )
  )

assess_relatives <- assess_relatives %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(population_labels, by = "population") %>%
  left_join(agegroup_labels, by = "agegroup") %>%
  left_join(additional_relative_req_labels, by = "additional_relative_req") %>%
  mutate(
    closest_relative_type_label = case_match(
      closest_relative_type,
      "child" ~ "Children",
      "father" ~ "Fathers",
      "pooled" ~ "Pooled",
      "mother" ~ "Mothers",
      "parent" ~ "Parents",
      "partner" ~ "Partners",
      "sibling" ~ "Siblings",
      "no_relative" ~ "No available closest relative"
    ),
    closest_relative_type_order = case_match(
      closest_relative_type,
      "mother" ~ 1,
      "father" ~ 2,
      "partner" ~ 3,
      "parent" ~ 4,
      "child" ~ 5,
      "sibling" ~ 6,
      "no_relative" ~ 7
    )
  )


#### Reorder variables and sort data ####
cost_results <- cost_results %>%
  relocate(
    cost_period,
    cost_period_label,
    additional_relative_req,
    additional_relative_req_label,
    var_pop,
    population,
    population_label,
    var_name,
    var_name_label,
    agegroup,
    agegroup_label,
    closest_relative_group,
    closest_relative_group_label,
    cost_component,
    cost_component_label,
    act_cost,
    act_py,
    att_cost,
    att_py,
    act_cost_py,
    att_cost_py
  ) %>%
  mutate(
    cost_component_order = case_match(
      cost_component,
      "primary_sector" ~ 1,
      "secondary_sector" ~ 2,
      "filled_prescriptions" ~ 3,
      "lost_production_sickness" ~ 4
    )
  ) %>%
  arrange(
    cost_period, additional_relative_req, var_pop, var_name, agegroup,
    closest_relative_group, cost_component_order
  )

patient_characteristics <- patient_characteristics %>%
  relocate(
    additional_relative_req,
    additional_relative_req_label,
    var_pop,
    population,
    population_label,
    var_name,
    var_name_label,
    agegroup,
    agegroup_label,
    closest_relative_group,
    closest_relative_group_label,
    case_relative,
    var,
    label,
    stat_char,
    stat_num1,
    stat_num2,
    stat_num3
  ) %>%
  arrange(
    additional_relative_req, var_pop, var_name, agegroup,
    closest_relative_group, case_relative, label_order
  )

assess_relatives <- assess_relatives %>%
  relocate(
    additional_relative_req,
    additional_relative_req_label,
    var_pop,
    population,
    population_label,
    var_name,
    var_name_label,
    agegroup,
    agegroup_label,
    closest_relative_type,
    closest_relative_type_label
  ) %>%
  arrange(
    additional_relative_req, var_pop, var_name, agegroup,
    closest_relative_type_order
  )
  

#### Save cleaned data for app ####

# We choose not to compress the data to make it slightly faster to load
# the app
saveRDS(
  cost_results,
  here("data", "cost_results.rds"),
  compress = FALSE
)
saveRDS(
  codelist,
  here("data", "codelist.rds"),
  compress = FALSE
)
saveRDS(
  patient_characteristics,
  here("data", "patient_characteristics.rds"),
  compress = FALSE
)
saveRDS(
  assess_relatives,
  here("data", "assess_relatives.rds"),
  compress = FALSE
)


  