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
calculate_complexity <- function(data, region, product, value, year, method = "eigenvalues") {
  
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
  
  if (method == "eigenvalues") {
  complexity <- economiccomplexity::complexity_measures(bi, method = "eigenvalues")
  } else if (method == "fitness") {
    complexity <- economiccomplexity::complexity_measures(bi, method = "fitness")
  } else if (method == "shares") {
    complexity <- calculate_complexity_shares(data, region, product, value, verbose = TRUE)
    return(complexity)
  }
  
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



#' Calculate economic complexity indicators using Davies and Mare 2021 method of weighted correlations.
#'
#' @param data data suitable for calculating complexity.
#' @param region variable in `data` which corresponds to a region.
#' @param product variable in `data` which corresponds to a product. 
#' @param value variable in `data` which corresponds to a value.
#' @param method the method used to calculate economic complexity indicators. One of 'eigenvalue', 'fitness', or 'shares'.
#' @param verbose logical. FALSE (the default) provides no messages to the user. 
#'
#' @return the supplied `data` with calculated city and activity complexity. 
#' @export
#'
#' @examples \dontrun{
#' 
#' calculate_complexity_shares(data, region = "sa3", product = "indp", value = "count")
#' 
#' }
#' 
#' @importFrom stats cor cov.wt sd setNames
calculate_complexity_shares <- function(data, region, product, value, verbose = FALSE) {
  
  m <- data |> 
    tidyr::pivot_wider(id_cols = {{region}},
                       values_fill = 0,
                       names_from = {{product}},
                       values_from = {{value}}) |> 
    tibble::column_to_rownames(var = region) |> 
    as.matrix()
  
  activity_share <- m / rowSums(m)
  national_share_employment <- rowSums(m) / sum(m)
  
  # Relatedness of activities is the weighted covariance between the local share vectors for activities i and j
  # weighted by each regions share of total employment. This is equivalent to the weighted correlation. 
  # The relatedness between each activity is mapped to the interval [0,1]
  
  r_aa <- 0.5*(stats::cov.wt(x = activity_share, wt = national_share_employment, cor = TRUE)$cor + 1)
  
  # Complexity of activity a is the element a of the standardized second eigenvector of the row-standardized relatedness matrix R = r_aa/rowSums(r_aa)
  
  complexity <- list()
  complexity$complexity_index_product <- Re(eigen(r_aa/rowSums(r_aa))$vector[,2])
  #standardize
  complexity$complexity_index_product <- (complexity$complexity_index_product - mean(complexity$complexity_index_product))/sd(complexity$complexity_index_product)
  
  names(complexity$complexity_index_product) <- colnames(m)
  
  # Activity complexity is positively correlated with the weighted mean size of cities that contain activity a
  
  weighted_mean_city_size <- t(m/colSums(m)) %*% rowSums(m)
  
  if (cor(complexity$complexity_index_product, weighted_mean_city_size) < 0) {
    complexity$complexity_index_product <- -1*complexity$complexity_index_product
  }
  
  rpt_activity <-  glue::glue("most complex activity: {names(complexity$complexity_index_product[complexity$complexity_index_product == max(complexity$complexity_index_product)])}")
  
  # The relatedness of cities is symmetric to activities.
  city_share <- t(m / rowSums(m)) 
  national_share_activity <- colSums(m)/sum(m)
  
  r_cc <- 0.5*(cov.wt(x = city_share, wt = national_share_activity, cor = TRUE)$cor + 1)
  
  complexity$complexity_index_country <- Re(eigen(r_cc/rowSums(r_cc))$vector[,2])
  complexity$complexity_index_country <- (complexity$complexity_index_country - mean(complexity$complexity_index_country)) / sd(complexity$complexity_index_country)
  names(complexity$complexity_index_country) <- rownames(m)
  
  # City complexity is positively correlated with the local share weighted mean complexity of activities in city c. 
  # Having said that, I don't think this is working correctly in this context. Temporarily, it makes sense
  # to just make sure that the CBD have positive complexity. 
  

  local_share_mean_complexity <- activity_share %*% complexity$complexity_index_product

  if (cor(complexity$complexity_index_country, local_share_mean_complexity) < 0) {
    complexity$complexity_index_country <- -1 * complexity$complexity_index_country
  }
  
  rpt_city <- glue::glue("most complex city: {names(complexity$complexity_index_country[complexity$complexity_index_country == max(complexity$complexity_index_country)])}")
  
  if (verbose) {
    rpt_activity
    rpt_city
  }
  
  relatedness_density <- (m/rowSums(m))%*%r_aa |> 
    tibble::as_tibble(rownames = region) |> 
    tidyr::pivot_longer(cols = -region,
                        names_to = product,
                        values_to = "relatedness_density")
  
  mean_local_relatedness <- m/rowSums(m)
  
  mean_local_relatedness <- mean_local_relatedness |> 
    tibble::as_tibble(rownames = region) |> 
    tidyr::pivot_longer(cols =-region,
                        names_to = product,
                        values_to = "mean_local_relatedness")
  
  
  out <- dplyr::inner_join(data, tibble::enframe(complexity$complexity_index_country, name = region, value = "country_complexity_index")) |> 
    dplyr::inner_join(tibble::enframe(complexity$complexity_index_product, name = product, value = "product_complexity_index")) |> 
    dplyr::inner_join(relatedness_density) |> 
    dplyr::inner_join(mean_local_relatedness) |> 
    dplyr::mutate(mean_local_relatedness = mean_local_relatedness + relatedness_density)
  
  return(out)
  
  
  
  
}

calculate_proximity <- function(data, region, product, value) {
  
  bi <- economiccomplexity::balassa_index(data, discrete = TRUE, country = {{region}}, product = {{product}},
                                          value = {{value}})
  
  proximity <- economiccomplexity::proximity(bi)
  
  proximity
  
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
