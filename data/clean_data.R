# Import and clean data from project to be used in the app

library(dplyr)
library(here)
library(readr)
library(stringr)

version_id <- "v2_2025-02-17"


#### Import data ####

cost_results <- read_delim(
  here("data", version_id, paste0("all_cost_results_", version_id, ".csv")),
  delim = "|",
  col_types = list(
    .default = "d",
    "cost_component" = "c",
    "var_pop" = "c",
    "closest_relative_type" = "c",
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
      "home_care" ~ "Home care",
      "filled_prescriptions" ~ "Filled prescriptions",
      "primary_sector" ~ "Primary sector",
      "lost_production_sickness" ~ "Lost productivity associated with illness",
      .default  = cost_component
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

closest_relative_type_labels <- tibble(
  closest_relative_type = c("mother", "father", "relative"),
  closest_relative_type_label = c("Mother", "Father", "Closest relative")
)

#### Add variables to data ####
cost_results <- cost_results %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    population = word(var_pop, 2, sep = "#")
  ) %>%
  left_join(cost_component_labels, by = "cost_component") %>%
  left_join(var_name_labels, by = "var_name") %>%
  left_join(population_labels, by = "population") %>%
  left_join(agegroup_labels, by = "agegroup") %>%
  left_join(closest_relative_type_labels, by = "closest_relative_type")


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
  left_join(closest_relative_type_labels, by = "closest_relative_type") %>%
  mutate(
    label_order = case_match(
      label,
      "__n" ~ 1,
      "male" ~ 2,
      "age_index" ~ 3,
      "cci_g: title" ~ 4,
      "cci_g: 0" ~ 5,
      "cci_g: 1-2" ~ 6,
      "cci_g: +3" ~ 7
    )
  ) 
  

#### Reorder variables and sort data ####
cost_results <- cost_results %>%
  relocate(
    var_pop,
    population,
    population_label,
    var_name,
    var_name_label,
    agegroup,
    agegroup_label,
    closest_relative_type,
    closest_relative_type_label,
    cost_component,
    cost_component_label,
    act_cost,
    act_py,
    att_cost,
    att_py,
    act_cost_py,
    att_cost_py
  ) %>%
  arrange(var_pop, var_name, agegroup, closest_relative_type, cost_component)

patient_characteristics <- patient_characteristics %>%
  relocate(
    var_pop,
    population,
    population_label,
    var_name,
    var_name_label,
    agegroup,
    agegroup_label,
    closest_relative_type,
    closest_relative_type_label,
    var,
    label,
    stat_char,
    stat_num1,
    stat_num2,
    stat_num3
  ) %>%
  arrange(var_pop, var_name, agegroup, closest_relative_type, label_order)



#### Save cleaned data for app ####

# We choose not to compress the data to make it slightly faster to load
# the app
saveRDS(cost_results, here("data", "cost_results.rds"), compress = FALSE)
saveRDS(codelist, here("data", "codelist.rds"), compress = FALSE)
saveRDS(patient_characteristics, here("data", "patient_characteristics.rds"), compress = FALSE)


  