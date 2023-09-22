#' Calculate Complexity Measures
#'
#' @param data export data
#' @param year year to calculate complexity metrics
#'
#' @return dataframe
#' @export
#'
#' @examples
calculate_complexity <- function(data, year) {
  
  working_data <- data |> 
    dplyr::filter(year == {{year}})
  
  mcp <- working_data |> 
    economiccomplexity::balassa_index(discrete = TRUE,
                                      cutoff = 1,
                                      country = "location_code",
                                      product = "hs_product_code",
                                      value = "export_value")
  
  comp <- economiccomplexity::complexity_measures(mcp, "eigenvalues")
  
  pci <- tibble::enframe(comp$complexity_index_product, name = "hs_product_code", value = "product_complexity_index")
  eci <- tibble::enframe(comp$complexity_index_country, name = "location_code", value = "economic_complexity_index")
  m <- as.matrix(mcp) |> 
    as.data.frame() |> 
    tibble::rownames_to_column(var = "location_code") |> 
    tidyr::pivot_longer(cols = -location_code,
                        values_to = "m",
                        names_to = "hs_product_code")
  
  return_data <- working_data |> 
    dplyr::left_join(m, by = c("location_code", "hs_product_code")) |> 
    dplyr::left_join(pci, by = "hs_product_code") |> 
    dplyr::left_join(eci, by = "location_code")
  
  
}


#' Calculate complexity (multiple years)
#'
#' @param data export data
#' @param year years to calculate complexity metrics
#'
#' @return dataframe
#' @export
#'
#' @examples
calculate_complexity_time_series <- function(data, years) {
  purrr::map(.x = years, .f = ~calculate_complexity(data, year = .x),
             .progress = TRUE) |> 
    purrr::list_rbind()
  
  
}

#' Classify complexity products
#'
#' @param complexity_data data with complexity measures
#' @param base base year for comparison
#' @param current current year for comparison
#' @param country country for comparison
#'
#' @return dataframe
#' @export
#'
#' @examples
classify_products <- function(complexity_data, base, current, country = NULL) {
  
  complexity_data |> 
    dplyr::filter(year %in% c(base, current),
                  location_code == {{country}}) |> 
    tidyr::pivot_wider(names_from = year,
                       values_from = c(m, export_value),
                       id_cols = c(location_code, hs_product_code)) |> 
    dplyr::mutate(d_m = .data[[paste0("m_", current)]] - .data[[paste0("m_", base)]],
                  d_exp = .data[[paste0("export_value_", current)]] - .data[[paste0("export_value_", base)]],
                  status = dplyr::case_when(
                    d_m == 1 ~ "New Product",
                    d_m == -1 ~ "Lost Product",
                    d_m == 0 & d_exp > 0 ~ "Exports Growing",
                    d_m == 0 & d_exp < 0 ~ "Exports Declining")
                  )
  
  
}
