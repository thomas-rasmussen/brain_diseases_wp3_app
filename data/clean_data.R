# Import and clean data from project to be used in the app

library(dplyr)
library(here)
library(readr)
library(stringr)

version_id <- "v9_2025-04-08"


#### Clean codelist ####

codelist <-  read_delim(
  here("data", version_id, paste0("codelist_", version_id, ".csv")),
  delim = "|",
  col_types = list(.default = "c"),
  # Used readr::guess_encoding to guess what encoding was needed to correctly
  # read apostrophe in the parkinson disease label
  locale = readr::locale(encoding = "windows-1252")
)

saveRDS(
  codelist,
  here("data", "codelist.rds"),
  compress = FALSE
)


#### Create utility data.frame's ####

# Find DKK to EUR exchange rate in codelist
eur_dkk_rate <- codelist %>%
  filter(str_starts(var_name, "eur_to_dkk")) %>%
  select(code_include) %>%
  pull() %>%
  as.numeric()

# data.frame's with variable values and corresponding labels
var_name_labels <- codelist %>%
  filter(group %in% c("bd_def")) %>%
  select(var_name, var_label) %>%
  rename(var_name_label = var_label) %>%
  distinct()

population_labels <- codelist %>%
  filter(group == "population_def") %>%
  select(var_name) %>%
  mutate(
    population_label = case_match(
      var_name,
      "prev_2021" ~ "Prevalent cohort 2021",
      "inc_2016_2021" ~ "Incident cohort 2016-2021",
      .default = var_name
    )
  ) %>%
  rename(population = var_name)

agegroup_labels <- tribble(
  ~agegroup, ~agegroup_label,
  "0-24",    "Children and young people (0-24 years)",
  "25-64",   "Young adults and adults (25-64 years)",
  "65+",     "Older people (65+ years)"
)

cost_period_labels <- tribble(
  ~cost_period,   ~cost_period_label,
  "before_index", "Year before index date",
  "after_index",  "Year after index date"
)

additional_relative_req_labels <- tribble(
  ~additional_relative_req, ~additional_relative_req_label,
  "no",                     "No",
  "yes",                    "Yes"
)

closest_relative_group_labels <- tribble(
  ~closest_relative_group, ~closest_relative_group_label,
  "father",                "Fathers",
  "mother",                "Mothers",
  "child",                 "Children",
  "parent",                "Parents",
  "partner",               "Partners",
  "pooled",                "Pooled",
  "sibling",               "Siblings",
  "no_relative",           "No available closest relative"
)

characteristic_level_labels <- tribble(
  ~characteristic_level,               ~characteristic_level_label,
  "__n",                               "Number of persons, n",
  "male",                              "Male, n (%)",
  "age_index",                         "Age, median (Q1;Q3)",
  "cci_g: title",                      "CCI, n (%)",
  "cci_g: 0",                          "\u2800\u28000",
  "cci_g: 1-2",                        "\u2800\u28001-2",
  "cci_g: +3",                         "\u2800\u2800+3",
  "education_level: title",            "Education level, n (%)",
  "education_level: no_education",     "\u2800\u2800No/Unknown education",
  "education_level: short_education",  "\u2800\u2800Primary",
  "education_level: medium_education", "\u2800\u2800Secondary",
  "education_level: long_education",   "\u2800\u2800Tertiary",
  "closest_relative_type: title",      "Relation to patient, n (%)",
  "closest_relative_type: mother",     "\u2800\u2800Mother",
  "closest_relative_type: father",     "\u2800\u2800Father",
  "closest_relative_type: partner",    "\u2800\u2800Partner",
  "closest_relative_type: parent",     "\u2800\u2800Parent",
  "closest_relative_type: child",      "\u2800\u2800Child",
  "closest_relative_type: sibling",    "\u2800\u2800Sibling"
)

cost_component_labels <- tribble(
  ~cost_component,            ~cost_component_label,
  "primary_sector",           "Primary sector",
  "secondary_sector",         "Hospital care",
  "filled_prescriptions",     "Filled prescriptions",
  "lost_production_sickness", "Lost productivity (income loss)"
)


#### Cost results ####

# Import
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

# Clean
cost_results <- cost_results %>%
  mutate(
    # Convert results from DKK to EUR/million EUR
    act_cost_total = act_cost_total / (eur_dkk_rate * 10**6),
    act_cost_per_person = act_cost_per_person / eur_dkk_rate,
    att_cost_total = att_cost_total / (eur_dkk_rate * 10**6),
    att_cost_per_person = att_cost_per_person / eur_dkk_rate,
    var_name = word(var_pop, 1, sep = "#"),
    var_name = factor(
      var_name,
      levels = var_name_labels$var_name,
      labels = var_name_labels$var_name_label
    ),
    population = word(var_pop, 2, sep = "#"),
    population = factor(
      population,
      levels = population_labels$population,
      labels = population_labels$population_label
    ),
    agegroup = factor(
      agegroup,
      levels = agegroup_labels$agegroup,
      labels = agegroup_labels$agegroup_label
    ),
    additional_relative_req = factor(
      additional_relative_req,
      levels = additional_relative_req_labels$additional_relative_req,
      labels = additional_relative_req_labels$additional_relative_req_label
    ),
    closest_relative_group = factor(
      closest_relative_group,
      levels = closest_relative_group_labels$closest_relative_group,
      labels = closest_relative_group_labels$closest_relative_group_label
    ),
    cost_component = factor(
      cost_component,
      levels = cost_component_labels$cost_component,
      labels = cost_component_labels$cost_component_label
    ),
    cost_period = factor(
      cost_period,
      levels = cost_period_labels$cost_period,
      labels = cost_period_labels$cost_period_label
    )
  )

# Reorder variables and sort
cost_results <- cost_results %>%
  relocate(
    cost_period,
    additional_relative_req,
    var_pop,
    population,
    var_name,
    agegroup,
    closest_relative_group,
    cost_component,
    n_cases_included,
    act_cost_total,
    act_py,
    att_cost_total,
    att_py,
    act_cost_per_person,
    att_cost_per_person,
    att_cost_prop
  ) %>%
  arrange(
    cost_period, additional_relative_req, var_pop, var_name, agegroup,
    closest_relative_group, cost_component
  ) %>%
  select(-var_pop)

# Save
saveRDS(
  cost_results,
  here("data", "cost_results.rds"),
  compress = FALSE
)


#### Patient characteristics - Relatives ####

# Import
patient_characteristics_relatives <-  read_delim(
  here(
    "data",
    version_id,
    paste0("patient_characteristics_relative_", version_id, ".csv")
  ),
  delim = "|",
  col_types = list(.default = "c")
)

# Clean
patient_characteristics_relatives <- patient_characteristics_relatives %>%
  rename(
    stat_char = `__stat_char`,
    characteristic_level = `__label`,
    characteristic_variable = `__var`,
    stat_num1 = `__stat_num1`,
    stat_num2 = `__stat_num2`,
    stat_num3 = `__stat_num3`
  ) %>%
  mutate(
    stat_char = ifelse(is.na(stat_char), "", stat_char),
    stat_char = case_match(
      characteristic_variable,
      "__n" ~ word(stat_char, 1),
      .default = stat_char
    ),
    var_name = word(var_pop, 1, sep = "#"),
    var_name = factor(
      var_name,
      levels = var_name_labels$var_name,
      labels = var_name_labels$var_name_label
    ),
    population = word(var_pop, 2, sep = "#"),
    population = factor(
      population,
      levels = population_labels$population,
      labels = population_labels$population_label
    ),
    characteristic_level = factor(
      characteristic_level,
      levels = characteristic_level_labels$characteristic_level,
      labels = characteristic_level_labels$characteristic_level_label
    ),
    agegroup = factor(
      agegroup,
      levels = agegroup_labels$agegroup,
      labels = agegroup_labels$agegroup_label
    ),
    additional_relative_req = factor(
      additional_relative_req,
      levels = additional_relative_req_labels$additional_relative_req,
      labels = additional_relative_req_labels$additional_relative_req_label
    ),
    closest_relative_group = factor(
      closest_relative_group,
      levels = closest_relative_group_labels$closest_relative_group,
      labels = closest_relative_group_labels$closest_relative_group_label
    )
  )
 
# Reorder variables and sort
patient_characteristics_relatives <- patient_characteristics_relatives %>%
  relocate(
    additional_relative_req,
    var_pop,
    population,
    var_name,
    agegroup,
    closest_relative_group,
    case_relative,
    characteristic_variable,
    characteristic_level,
    stat_char,
    stat_num1,
    stat_num2,
    stat_num3
  ) %>%
  arrange(
    additional_relative_req, var_pop, var_name, agegroup,
    closest_relative_group, case_relative, characteristic_level
  ) %>%
  select(-var_pop)

# Save
saveRDS(
  patient_characteristics_relatives,
  here("data", "patient_characteristics_relatives.rds"),
  compress = FALSE
)


#### Patient characteristics - Cases ####

# Import
patient_characteristics_cases <-  read_delim(
  here(
    "data",
    version_id,
    paste0("patient_characteristics_case_", version_id, ".csv")
  ),
  delim = "|",
  col_types = list(.default = "c")
)

# Clean
patient_characteristics_cases <- patient_characteristics_cases %>%
  rename(
    stat_char = `__stat_char`,
    characteristic_level = `__label`,
    characteristic_variable = `__var`,
    stat_num1 = `__stat_num1`,
    stat_num2 = `__stat_num2`,
    stat_num3 = `__stat_num3`
  ) %>%
  mutate(
    stat_char = ifelse(is.na(stat_char), "", stat_char),
    stat_char = case_match(
      characteristic_variable,
      "__n" ~ word(stat_char, 1),
      .default = stat_char
    ),
    var_name = word(var_pop, 1, sep = "#"),
    var_name = factor(
      var_name,
      levels = var_name_labels$var_name,
      labels = var_name_labels$var_name_label
    ),
    population = word(var_pop, 2, sep = "#"),
    population = factor(
      population,
      levels = population_labels$population,
      labels = population_labels$population_label
    ),
    characteristic_level = factor(
      characteristic_level,
      levels = characteristic_level_labels$characteristic_level,
      labels = characteristic_level_labels$characteristic_level_label
    ),
    agegroup = factor(
      agegroup,
      levels = agegroup_labels$agegroup,
      labels = agegroup_labels$agegroup_label
    ),
    additional_relative_req = factor(
      additional_relative_req,
      levels = additional_relative_req_labels$additional_relative_req,
      labels = additional_relative_req_labels$additional_relative_req_label
    )
  )
 
# Reorder variables and sort
patient_characteristics_cases <- patient_characteristics_cases %>%
  relocate(
    additional_relative_req,
    var_pop,
    population,
    var_name,
    agegroup,
    characteristic_variable,
    characteristic_level,
    stat_char,
    stat_num1,
    stat_num2,
    stat_num3
  ) %>%
  arrange(
    additional_relative_req, var_pop, var_name, agegroup,
    characteristic_level
  ) %>%
  select(-var_pop)

# Save
saveRDS(
  patient_characteristics_cases,
  here("data", "patient_characteristics_cases.rds"),
  compress = FALSE
)


#### Identification of populations of relatives ####

# Import
assess_relatives <- read_delim(
  here("data", version_id, paste0("assess_relatives_", version_id, ".csv")),
  delim = "|",
  col_types = list(
    .default = "d",
    "additional_relative_req" = "c",
    "var_pop" = "c",
    "agegroup" = "c",
    "closest_relative_group" = "c"
  )
)

# Clean
assess_relatives <- assess_relatives %>%
  mutate(
    var_name = word(var_pop, 1, sep = "#"),
    var_name = factor(
      var_name,
      levels = var_name_labels$var_name,
      labels = var_name_labels$var_name_label
    ),
    population = word(var_pop, 2, sep = "#"),
    population = factor(
      population,
      levels = population_labels$population,
      labels = population_labels$population_label
    ),
    agegroup = factor(
      agegroup,
      levels = agegroup_labels$agegroup,
      labels = agegroup_labels$agegroup_label
    ),
    additional_relative_req = factor(
      additional_relative_req,
      levels = additional_relative_req_labels$additional_relative_req,
      labels = additional_relative_req_labels$additional_relative_req_label
    ),
    closest_relative_group = factor(
      closest_relative_group,
      levels = closest_relative_group_labels$closest_relative_group,
      labels = closest_relative_group_labels$closest_relative_group_label
    )
  )

# Reorder variables and sort
assess_relatives <- assess_relatives %>%
  relocate(
    additional_relative_req,
    var_pop,
    population,
    var_name,
    agegroup,
    closest_relative_group,
  ) %>%
  arrange(
    additional_relative_req, var_pop, var_name,
    agegroup, closest_relative_group
  ) %>%
  select(-var_pop)
  
# Save
saveRDS(
  assess_relatives,
  here("data", "assess_relatives.rds"),
  compress = FALSE
)

