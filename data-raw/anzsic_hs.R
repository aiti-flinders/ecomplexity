## code to prepare `anzsic_hs` dataset goes here
library(readxl)
library(dplyr)
library(stringr)

anzsic_hs <- read_excel("data-raw/5489_2024.xlsx",
                 sheet = "Appendix 6.1",
                 range = "A23:K12076") |> 
  mutate(hs_product_code = str_sub(AHECC, 0L, 4L)) |> 
  filter(is.na(`END DATE`)) |> 
  distinct(hs_product_code, `ANZSIC 2006`) |> 
  rename(anzsic = "ANZSIC 2006")

usethis::use_data(anzsic_hs, overwrite = TRUE)
