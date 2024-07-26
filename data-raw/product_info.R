## code to prepare `complexity_colours` dataset goes here
library(stringr)
library(tibble)
library(readr)
library(dplyr)

complexity_colours <- tribble(
  ~sector,  ~colour,
  "Agriculture", "#f5cf23",
  "Minerals", "#bb968a",
  "Chemicals", "#c57bd9",
  "Textiles", "#7ddaa1",
  "Stone", "#dab47d",
  "Metals", "#d97b7b",
  "Machinery", "#7ba2d9",
  "Electronics", "#7ddada",
  "Vehicles", "#8d7bd8",
  "Other", "#2a607c",
  "Services", "#b23d6d"
) 

complexity_classification <- read_csv("data-raw/complexity_classifications.csv",
                                      show_col_types = FALSE) |>
  mutate(hs_product_code = str_pad(Code, "left", pad = "0", width = 4)) |>
  select(hs_product_code, sector = Sector) |>
  left_join(complexity_colours)

product_data <- read_tsv("data-raw/hs_product.tab")

product_data_section <- product_data |>
  filter(level == "section") |>
  select(product_id, section_name = hs_product_name_short_en)

product_data_two <- product_data |>
  filter(level == '2digit') |>
  select(product_id, parent_id, group_name = hs_product_name_short_en) |>
  left_join(product_data_section, by = c("parent_id" = "product_id")) |>
  select(-parent_id)

product_data_four <- product_data |>
  filter(level == "4digit") |>
  select(product_id, parent_id, hs_product_name_short_en,hs_product_code) |>
  left_join(product_data_two, by = c("parent_id" = "product_id"))

product_data_six <- product_data |>
  filter(level == "6digit") |>
  select(product_id, parent_id, six_name = hs_product_name_short_en)

product_data <- product_data_four

usethis::use_data(complexity_classification, product_data, internal = TRUE, overwrite = TRUE)

