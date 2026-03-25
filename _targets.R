# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 

# Set target options:
pkgs <- c(
  "janitor", # data cleaning
  "labelled", # labeling data
  "pointblank", # data validation and exploration
  "rvest", # get data from web pages
  "data.table", # fast data management
  "fs", # to work wit hthe file system
  "zip", # manipulate zip files
  "gt", #for nice tables
  "quarto",
  "tidyverse", # Data management
  "duckplyr"
)

invisible(lapply(pkgs, library, character.only = TRUE))

# Set target options:
tar_option_set(
  # Packages that your targets need for their tasks:
  packages = pkgs,
  format = "qs", # Default storage format. qs (which is actually qs2) is fast.
)

# Run the R scripts stored in the R/ folder where your have stored your custom functions:
tar_source("draft.R")


# We first download the data health care data of interest
if (!fs::file_exists("data.zip")) {
  curl::curl_download(
    "https://github.com/eribul/cs/raw/refs/heads/main/data.zip",
    "data.zip",
    quiet = FALSE
  )
}

zip::unzip("data.zip")