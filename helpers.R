# Version suffix used in filenames
version_id <- "v1_2025-01-15"

# Find label associated with population key value
population_label <- function(x) {
  case_when(
    x == "inc_2016_2021" ~ "Incident cohort 2016-2021",
    x == "prev_2021" ~ "Prevalent cohort 2021",
  )
}
