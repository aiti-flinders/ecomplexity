#' Add product names to a dataset
#'
#' @param data a data frame with a column of hs92 product codes
#' @param digits representation of hs92 product codes. either four or six digits. 
#'
#' @return tibble
#' @export
#'
#' @examples \dontrun{
#' library(complexitydata)
#' state_economic_complexity |> 
#' add_product_digits(digits = 4)
#' }
#' @importFrom rlang .data
add_product_names <- function(data, digits) {
  
  if (!digits %in% c("four", "4", 4, "six", "6", 6)) {
    stop()
  } else if (digits %in% c("four", "4", 4)) {
    digits <- "4digit"
  } else {
    digits <- "6digit"
  }
  
  prod_data <- product_data |> 
    dplyr::filter(.data$level == digits) |> 
    dplyr::select("hs_product_name_short_en", "hs_product_code")
  
  data |>  
    dplyr::left_join(prod_data)
}

#' Add Atlas of Economic Complexity section colours to products
#'
#' @param data a data frame with a column of hs92 product codes
#' @param digits representation of hs92 product codes. either four or six digits.
#'
#' @return tibble
#' @export
#'
#' @examples \dontrun{
#' read_complexitydata("state_economic_complexity") |>
#' add_product_colours(digits = 4)
#' }
add_product_colours <- function(data, digits) {

  if (!digits %in% c("four", "4", 4, "six", "6", 6)) {
    stop()
  } else if (digits %in% c("four", "4", 4)) {
    digits <- "4digit"
  } else {
    digits <- "6digit"
  }

  prod_data <- complexity_classification |>
    dplyr::select("hs_product_code", "colour")

  data |>
    dplyr::left_join(prod_data, by = 'hs_product_code')
}