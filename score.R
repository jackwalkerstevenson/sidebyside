# sidebyside contest scoring
# Jack Stevenson, 2023-06
# import libraries----------------------------------------------------
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggprism)
library(ggdist)
source("import.R")
# import score data from spreadsheet-------------------------------------
input_filename <- "2023-06-01 scores.xlsx"
key_filename <- "2023-06-01 beer key.xlsx"
raw_data <- import_scores(input_filename)
key <- readxl::read_excel(key_filename)
scores <- raw_data |>
  mutate(entry = as.numeric(entry)) |>
  left_join(key, by = "entry") |>
  mutate(entry = as.character(entry)) |>
  filter(type == "score") |>
  drop_na(score) |>
  mutate(score = as.numeric(score)) |>
  # group_by(entry) |>
  # mutate(z_score = as.vector(scale(score))) |>
  # mutate(z_score = tidyr::replace_na(z_score, 0)) |>
  # ungroup() |>
  mutate(entry = fct_reorder(entry, score, mean)) |>
  mutate(judge = fct_reorder(judge, score, mean))
# simple plot----------------------------------
mean_scores <- scores |>
  group_by(entry) |>
  summarize(mean_score = mean(score))
mean_scores |>
  ggplot(aes(x = mean_score, y = entry)) +
  theme_prism() +
  geom_point(size = 4) +
  geom_text(aes(label = as.character(round(mean_score, 1))), position = position_nudge(.5)) +
  geom_jitter(data = scores, aes(x = score), alpha = 0.4, width = 0.05) +
  labs(x = "enjoyment score",
     y = "beerb",
     title = "excellent and respectable beers")
# strict deviation---------------------------------------------------
scores_with_mean <- left_join(scores, mean_scores, by = "entry")
scores_with_mean |>
  mutate(dev = abs(score - mean_score)) |>
  mutate(judge = fct_reorder(judge, dev, mean)) |>
  ggplot(aes(x = dev, y = judge)) +
  geom_point(stat = "summary", fun = "mean", size = 4)
# ranking correlation------------------------------------
scores_with_mean <- left_join(scores, mean_scores, by = "entry")
# scores_with_mean |>



# plot person scores-------------------------------------------
scores |>
  ggplot(aes(x = score, y = judge)) +
  theme_prism() +
  # geom_boxplot(width = 0.3) +
  geom_violin() +
  # stat_halfeye() +
  geom_point(stat = "summary", fun = "mean", size = 4) +
  labs(x = "enjoyment score",
       y = "person",
       title = "excellent and respectable people")
# plot beer scores-------------------------------------------
scores |>
  ggplot(aes(x = score, y = entry)) +
  theme_prism() +
  # geom_boxplot(width = 0.3) +
  geom_violin() +
  # stat_halfeye() +
  geom_point(stat = "summary", fun = "mean", size = 4) +
  labs(x = "enjoyment score",
       y = "beerb",
       title = "excellent and respectable beers")
