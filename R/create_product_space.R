#' Title
#'
#' @param data trade data
#' @param country country 
#' @param year year
#' @param services whether to include services (NYI)
#'
#' @return ggraph object
#' @export
#'
#' @examples

create_product_space <- function(data, country, year, services = FALSE) {
  
  working_data <- data |> 
    dplyr::filter(year == {{year}},
                  !is.na(as.numeric(hs_product_code)))
  
  mcp <- working_data |> 
    economiccomplexity::balassa_index(discrete = TRUE,
                                      cutoff = 1,
                                      country = "location_code",
                                      product = "hs_product_code",
                                      value = "export_value")
  
  m <- as.matrix(mcp) |> 
    as.data.frame() |> 
    tibble::rownames_to_column(var = "location_code") |> 
    tidyr::pivot_longer(cols = -location_code,
                        values_to = "m",
                        names_to = "hs_product_code")
  
  prox <- economiccomplexity::proximity(mcp)
  
  product_space_colours <- readr::read_csv("data-raw/hs4_hex_colors_intl_atlas.txt",
                                           show_col_types = FALSE) %>%
    dplyr::select(hs4, colour = color)
  
  ps_data <- working_data |> 
    dplyr::filter(location_code == {{country}}) |> 
    dplyr::left_join(m, by = dplyr::join_by(location_code, hs_product_code)) |> 
    dplyr::left_join(product_space_colours, by = dplyr::join_by(hs_product_code == hs4)) |> 
    dplyr::select(hs_product_code, export_value, m, colour) 
  

  
  graph_size <- setNames(ps_data$export_value, ps_data$hs_product_code)
  graph_presence <- setNames(ps_data$m, ps_data$hs_product_code)
  graph_colour <- setNames(ps_data$colour, ps_data$hs_product_code)
  
  
  net <- economiccomplexity::projections(prox$proximity_country, prox$proximity_product, compute = "both")
  
  #Adjust dots for size (this is currently location size. Could update to do global size)
  
  igraph::V(net$network_product)$size <- graph_size[match(igraph::V(net$network_product)$name, names(graph_size))]
  
  # Set attribute for "presence"
  igraph::V(net$network_product)$presence <- graph_presence[match(igraph::V(net$network_product)$name, names(graph_presence))] 
  
  # Set attribute for colour
  igraph::V(net$network_product)$colour <- graph_colour[match(igraph::V(net$network_product)$name, names(graph_colour))]
  
  ggraph::ggraph(net$network_product, layout = "kk") +
    ggraph::geom_edge_link(edge_colour = "#a8a8a8") + 
    ggraph::geom_node_point(ggplot2::aes(size = size, alpha = factor(presence), colour = colour)) + 
    ggraph::geom_node_text(ggplot2::aes(label = name), size = 2, vjust = 2.2)
  
  
  
  
}
