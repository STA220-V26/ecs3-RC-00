library(tidyverse)
library(data.table)
library(pointblank)
library(ggplot2)

# Loading the data set, Converting to a data.table, Sorting and indexing by ID
patients <-
readr::read_csv("data-fixed/patients.csv") |>
setDT() |>
setkey(id)

# Removing missing columns and rows
patients <- janitor::remove_empty(patients, quiet = FALSE)
# Removing column with only one constant value 
patients <- janitor::remove_constant(patients, quiet = FALSE)

checks <-
  patients |>
  create_agent(label = "Expectations of patient data") |>
  col_vals_between(
    where(is.Date),
    as.Date("1900-01-01"),
    as.Date(Sys.Date()),
    na_pass = TRUE,
    label = "Check if all dates are between 1900-01-01 and today's date"
  ) |>
  
  col_vals_gte(
    deathdate,
    vars(birthdate),
    na_pass = TRUE,
    label = "Checking that the chronological order of birth date and death date is correct"
  ) |>
  
  col_vals_regex(
    ssn, 
    "[0-9]{3}-[0-9]{2}-[0-9]{4}$",
    label = "Checking if the SSN is following the format: XXX-XX-XXXX"
  ) |>
  
  col_is_integer(
    id,
    label = "Checking if the ID column contain only integer values"
  ) |>
  
  # Checking marital status
  col_vals_in_set(
      marital,
      set = c("S", "M", "D", "W"),
      label = "Checking that marital status is one of the codes: S, M, D, or W."
    ) |>

  # Checking if gender only contains male and female
  col_vals_in_set(
      gender,
      set = c("M", "F"),
      label = "Checking if gender is recorded as 'M' or 'F'."
    ) |>

  # Checking if essential data is missing
  col_vals_not_null(
      columns = vars(id, birthdate),
      label = "Checking that essential identifiers (ID and Birthdate) are not missing."
    ) |>
  
  interrogate()

checks

# Exporting the report
export_report(checks, "patient_validation.html")

patients[, .N, marital]

# Convert marital status  labels
patients[,
marital := factor(
marital,
levels = c("S", "M", "D", "W"),
labels = c("Single", "Married", "Divorced", "Widowed")
)
]

# Identifying character columns that has fewer than 10 unique values
fctr_candidates <-
patients[, which(lapply(.SD, uniqueN) < 10), .SDcols = is.character] |>
names()

# Show the unique values
patients[,
lapply(.SD, \(x) paste(unique(x), collapse = ", ")),
.SDcols = fctr_candidates
] |>
glimpse()

# Converting categorical columns into factors
patients[,
names(.SD) := lapply(.SD, as.factor),
.SDcols = c("gender", "ethnicity")
]

# Count individuals by gender, race, and state, sorted by the smallest groups
patients[, .N, .(gender, race, state)][order(N)]

# Any race that makes up less than 5% (0.05) of the dataset will be labeled "Other"
patients[, race := forcats::fct_lump_prop(race, prop = 0.05)]

# Check the new distribution of the race variable
patients[, .N, race]

# Calculating todays' age
patients[, age := as.integer((as.IDate(Sys.Date()) - birthdate)) %/% 365.241]

# Visualizing the distribution
patients[, hist(age)]

# Only calculating the living and visualizing
patients[is.na(deathdate), hist(age)]

# Find a probable date for data collection 
lastdate <- 
  readr::read_csv("data-fixed/payer_transitions.csv") |>
  summarise(lastdate = max(start_date, na.rm = TRUE)) |>
  collect() |>
  pluck("lastdate") |>
  as.Date()

# Recalculating age
patients[, age_at_snapshot := as.integer((as.IDate(lastdate) - birthdate)) %/% 365.241]

# Alive patients
living_at_snapshot <- patients[is.na(deathdate)]

# Histogram of the living patients age distribution at the time of the assumed data extract
ggplot(living_at_snapshot, aes(x = age_at_snapshot)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Age Distribution of Living Patients",
       subtitle = paste("Reference Date:", lastdate),
       x = "Age (Years)", 
       y = "Count") +
  theme_minimal()

# Replace NA values in specific columns with empty strings
patients[, 
  names(.SD) := lapply(.SD, \(x) tidyr::replace_na(x, "")), 
  .SDcols = c("prefix", "middle")
]

# Combine name parts into a single string
patients[,
full_name := paste(
prefix,
first,
middle,
last,
fifelse(!suffix %in% c("", NA), paste0(", ", suffix), "")
)
]
patients[, full_name]

# Remove leading and trailing whitespace from all character columns
patients[, names(.SD) := lapply(.SD, trimws), .SDcols = is.character]

# Remove duplicated spaces between names
patients[, full_name := stringr::str_replace(full_name, " ", " ")]

# Remove unnecessary columns
patients[, c("prefix", "first", "middle", "last", "suffix", "maiden") := NULL]

# Create a column 'driver' and put whether the patients have a driver's license or not and the delete original 'drivers'column
patients[, driver := !is.na(drivers)][, drivers := NULL]

# Create an interactive map of patient locations
leaflet::leaflet(data = patients) |>
leaflet::addTiles() |>
leaflet::addMarkers(~lon, ~lat, label = ~full_name)
