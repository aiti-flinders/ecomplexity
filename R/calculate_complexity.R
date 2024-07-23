#' Calculate economic complexity indicators
#'
#' @param data data suitable for calculating complexity.
#' @param region  variable in `data` which corresponds to a region.
#' @param product variable in `data` which corresponds to a product.
#' @param value variable in `data` which corresponds to a value.
#' @param year year to calculate complexity indicators.
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#'
#' calculate_complexity(state_data,
#' region = "location_code",
#' product = "hs_product_code",
#' value = "export_value",
#' year = 1996)
#'
#' }
calculate_complexity <- function(data, region, product, value, year) {
  
  data <- data |>
    dplyr::filter(year == {{year}})
  
  matrix_to_df <- function(matrix, region, product, values_to) {
    
    as.matrix(matrix) |>
      dplyr::as_tibble(rownames = region) |>
      tidyr::pivot_longer(cols = -region,
                          names_to = dplyr::all_of(product),
                          values_to = values_to)
  }
  
  bi <- economiccomplexity::balassa_index(data, discrete = TRUE, country = {{region}}, product = {{product}},
                                          value = {{value}})
  
  complexity <- economiccomplexity::complexity_measures(bi, method = "eigenvalues")
  
  prox <- economiccomplexity::proximity(bi)
  
  outlook <- economiccomplexity::complexity_outlook(bi, prox$proximity_product, complexity$complexity_index_product)
  
  dens <- economiccomplexity::density(bi, prox$proximity_product)
  
  rca <- economiccomplexity::balassa_index(data, discrete = FALSE, country = {{region}}, product = {{product}}, value = {{value}}) |>
    matrix_to_df(region = region, product = product, values_to = "rca")
  
  pci <- tibble::enframe(complexity$complexity_index_product, name = product, value = "product_complexity_index")
  eci <- tibble::enframe(complexity$complexity_index_country, name = region, value = "country_complexity_index")
  coi <- tibble::enframe(outlook$complexity_outlook_index, name = region, value = "complexity_outlook_index")
  cog <- outlook$complexity_outlook_gain |> matrix_to_df(region = region, product = product, values_to = "cog")
  d <- dens |> matrix_to_df(region = region, product = product, values_to = "density")
  
  out <- dplyr::left_join(data, rca, by = c(region, product)) |>
    dplyr::left_join(pci, by = product) |>
    dplyr::left_join(eci, by = region) |>
    dplyr::left_join(coi, by = region) |>
    dplyr::left_join(cog, by = c(region, product)) |>
    dplyr::left_join(d, by = c(region, product))
  
  out
  
}

#' Calculate economic complexity indicators for multiple years
#'
#' @param data data suitable for calculating complexity.
#' @param years years to calculate complexity indicators.
#' @param region  variable in `data` which corresponds to a region.
#' @param product variable in `data` which corresponds to a product.
#' @param value variable in `data` which corresponds to a value.
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#'
#' calculate_complexity_time_series(state_data,
#' years = unique(state_data$year),
#' region = "location_code",
#' product = "hs_product_code",
#' value = "export_value")
#'
#' }
calculate_complexity_time_series <- function(data, years, region, product, value) {
  purrr::map(.x = years,
             .f = ~calculate_complexity(data, region = {{region}}, product = {{product}}, value = {{value}}, year = .x),
             .progress = TRUE) |>
    purrr::list_rbind()
}
