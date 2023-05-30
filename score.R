# sidebyside contest scoring
# Jack Stevenson, 2023-06
library(tidyverse)
library(readxl)
source("import.R")
# import score data from spreadsheet
input_filename <- "test.xlsx"
raw_data <- import_scores(input_filename)
scored_data <- raw_data |>
  dplyr::group_by(judge) |>
  mutate(z_score = as.vector(scale(score))) |>
  mutate(z_score = tidyr::replace_na(z_score, 0)) |>
  ungroup()
overall_scores <- scored_data |>
  dplyr::group_by(entry) |>
  dplyr::summarize(
    mean_z_score = mean(z_score),
    sd_z_score = stats::sd(z_score)
  )
