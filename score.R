# sidebyside contest scoring
# Jack Stevenson, 2023-06
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggprism)
library(ggdist)
source("import.R")
# import score data from spreadsheet
input_filename <- "test.xlsx"
raw_data <- import_scores(input_filename)
scores <- raw_data |>
  group_by(judge) |>
  mutate(z_score = as.vector(scale(score))) |>
  mutate(z_score = tidyr::replace_na(z_score, 0)) |>
  ungroup() |>
  mutate(entry = fct_reorder(entry, z_score, mean))

scores |>
  ggplot(aes(x = z_score, y = entry)) +
  theme_prism() +
  geom_boxplot(width = 0.3) +
  geom_point(stat = "summary", fun = "mean", size = 4) +
  labs(x = "Z score",
       y = "entry",
       title = "Standardized scores")
