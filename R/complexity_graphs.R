#' Economic complexity rank time series 
#'
#' @param data complexity dataset from `complexitydata` package
#'
#' @return ggraph2 object
#' @export
#'
#' @examples \dontrun{
#' library(complexitydata) 
#' graph_complexity_rank(data = state_economic_complexity)
#' }
graph_complexity_rank <- function(data) {
  
  data_final <- data |> 
    dplyr::slice_max(order_by = .data$year) |> 
    dplyr::distinct(.data$location_code, .data$eci_rank) |> 
    dplyr::mutate(location_code = strayr::clean_state(.data$location_code, to = "state_name"))
  
  data_first <- data |> 
    dplyr::slice_min(order_by = .data$year) |> 
    dplyr::distinct(.data$location_code, .data$eci_rank) |> 
    dplyr::mutate(location_code = strayr::clean_state(.data$location_code, to = "state_name"))
  
  data |> 
    dplyr::distinct(.data$year, .data$location_code, .data$eci_rank) |> 
    dplyr::mutate(year = as.Date(paste0(.data$year, "0101"), format = "%Y%d%m"),
                  location_code = strayr::clean_state(.data$location_code, to = "state_name")) |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$eci_rank, col = .data$location_code, fill = .data$location_code)) + 
    ggplot2::geom_line(linewidth = 0.75) +
    ggplot2::geom_point(shape = 21, colour = "white", size = 2) +
    ggplot2::scale_y_reverse(
      breaks = data_first$eci_rank,
      labels = paste0(data_first$location_code, " (", data_first$eci_rank, ")"),
      expand = c(0, 0),
      name = NULL, 
      sec.axis = ggplot2::dup_axis(
        breaks = data_final$eci_rank,
        labels = paste0(data_final$location_code, " (", data_final$eci_rank, ")"),
        name = NULL
      )
    ) +
    ggplot2::scale_x_date(limits = as.Date(c("19960101", "20210101"), format = "%Y%d%m"),
                          expand = ggplot2::expansion(mult = c(0.02, 0))) +
    ggplot2::scale_colour_manual(values = strayr::palette_state_name_2016,
                                 name = NULL) +
    ggplot2::scale_fill_manual(values = strayr::palette_state_name_2016,
                               name = NULL) +
    ggplot2::coord_cartesian(clip = "off") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "none",
                   axis.line.y.right = ggplot2::element_blank(),
                   axis.ticks.y.right = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0)))
  
  
  
}

#' Export Tree Map.
#' 
#' Colours adjusted to match Atlas of Economic Complexity. 
#'
#' @param year year
#' @param region which region to draw the map. Only Australian States supported. 
#'
#' @return ggraph2 object
#' @export
#'
#' @examples \dontrun{
#' graph_complexity_tree(2021, "SA")
#' }
graph_complexity_tree <- function(year, region) {
  
  atlas_pci <- read_complexitydata("atlas_pci")
  
  data <- read_complexitydata("state_economic_complexity") 
  
  data |> 
    dplyr::inner_join(atlas_pci, by = c("hs_product_code", "year")) |> 
    dplyr::filter(.data$year == {{year}},
                  .data$location_code == {{region}},
                  .data$hs_product_code != "unspecified") |>
    dplyr::mutate(export_value_share = scales::label_percent(accuracy = 0.1, scale = 100)(.data$export_value/sum(.data$export_value)),
                  pci_label = scales::label_number(accuracy = 0.01)(.data$pci)) |> 
    dplyr::left_join(product_data, by = "hs_product_code") |> 
    ggplot2::ggplot(ggplot2::aes(area = .data$export_value, fill = round(.data$pci, 3), subgroup = .data$section_name, 
                                 label = paste(.data$hs_product_name_short_en, .data$pci_label, sep = "\n"))) +
    treemapify::geom_treemap(colour = "white") + 
    treemapify::geom_treemap_text(colour = "white", place = "centre", size = 15) + 
    treemapify::geom_treemap_subgroup_border(colour = "white", size = 1.5) + 
    ggplot2::scale_fill_gradientn(colours = atlas_complexity_colours()$colour, 
                                  values = atlas_complexity_colours()$percent, 
                                  breaks = c(-3.35, 2.3),
                                  labels = c("Low Complexity", "High Complexity")) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(barwidth = 10, 
                                                   title = NULL,
                                                   direction = "horizontal",
                                                   draw.ulim = F,
                                                   draw.llim = F,
                                                   label.position = "top")) + 
    ggplot2::labs(caption = "Note: Australian product export data converted from AHECC to 4-digit Harmonised System (1992).\n
       Services export data aggregated to EBOPS - Communications, Insurance and Finance, Transportation, Travel and Other") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "bottom",
                   legend.justification = "center")
}

graph_complexity_product_space <- function(country, year, services = FALSE) {
  
  working_data <- read_complexitydata("combined_exports") |> 
    dplyr::filter(.data$year == {{year}},
                  !is.na(as.numeric(.data$hs_product_code)))
  
  mcp <- working_data |> 
    economiccomplexity::balassa_index(discrete = TRUE,
                                      cutoff = 1,
                                      country = "location_code",
                                      product = "hs_product_code",
                                      value = "export_value")
  
  m <- as.matrix(mcp) |> 
    as.data.frame() |> 
    tibble::rownames_to_column(var = "location_code") |> 
    tidyr::pivot_longer(cols = -.data$location_code,
                        values_to = "m",
                        names_to = "hs_product_code")
  
  prox <- economiccomplexity::proximity(mcp)
  
  product_space_colours <- complexity_classification
  
  ps_data <- working_data |> 
    dplyr::group_by(.data$hs_product_code) |> 
    dplyr::mutate(product_export_value = sum(.data$export_value)) |> 
    dplyr::filter(.data$location_code == {{country}}) |> 
    dplyr::left_join(m, by = dplyr::join_by("location_code", "hs_product_code")) |> 
    dplyr::left_join(product_space_colours, by = dplyr::join_by("hs_product_code" == "code"))
  dplyr::group_by(.data$colour) |> 
    dplyr::mutate(colour_id = factor(m * dplyr::cur_group_id()), 
                  colour = ifelse(.data$colour_id == 0, "grey", .data$colour)) |> 
    dplyr::ungroup()
  
  
  
  graph_size <- setNames(ps_data$product_export_value, ps_data$hs_product_code)
  graph_colour <- setNames(ps_data$colour_id, ps_data$hs_product_code)
  
  
  net <- economiccomplexity::projections(prox$proximity_country, prox$proximity_product, compute = "product")
  
  #Adjust dots for size (this is currently location size. Could update to do global size)
  
  igraph::V(net$network_product)$size <- graph_size[match(igraph::V(net$network_product)$name, names(graph_size))]
  
  # Set attribute for "presence"
  # igraph::V(net$network_product)$presence <- graph_presence[match(igraph::V(net$network_product)$name, names(graph_presence))] 
  
  # Set attribute for colour
  igraph::V(net$network_product)$colour <- graph_colour[match(igraph::V(net$network_product)$name, names(graph_colour))]
  
  ggraph::ggraph(net$network_product, layout = "kk") +
    ggraph::geom_edge_link(edge_colour = "#a8a8a8", alpha = 0.1) + 
    ggraph::geom_node_point(ggplot2::aes(size = .data$size, colour = .data$colour)) + 
    ggplot2::scale_colour_manual(values = setNames(ps_data$colour, ps_data$colour_id))
  
  
  
  
}

#' Data Coverage
#'
#' @param data 
#' @param region 
#' @param activity 
#' @param flip 
#'
#' @return ggplot2 object
#' @export
#'
#' @examples \dontrun{
#' graph_complexity_coverage(data, "sa3", "indp")
#' }
graph_complexity_coverage <- function(data, region, activity, flip = FALSE) {
  
  region_size = paste0(region, "_size")
  activity_size = paste0(activity, "_size")
  
  data |> 
    dplyr::group_by(.data[[region]])  |> 
    dplyr::mutate({{region_size}} := sum(count)) |> 
    dplyr::group_by(.data[[activity]]) |> 
    dplyr::mutate({{activity_size}} := sum(count)) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(presence = count > 0,
                  {{region}} := forcats::fct_infreq(.data[[region]], w = .data[[region_size]]),
                  {{activity}} := forcats::fct_infreq(.data[[activity]], w = .data[[activity_size]])) |> 
    ggplot2::ggplot(ggplot2::aes(x = if(!flip) .data[[region]] else .data[[activity]],
                                 y = if(!flip) .data[[activity]] else .data[[region]],
                                 fill = presence)) + 
    ggplot2::geom_raster() + 
    ggplot2::scale_fill_manual(breaks = c(TRUE, FALSE),
                               values = c("#3182db", "white")) + 
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(fill = NA),
                   legend.position = "none") + 
    ggplot2::coord_equal()
}

#' @importFrom grDevices rgb
atlas_complexity_colours <- function() {
  
  rgbm <- function(r, g, b) {
    rgb(r, g, b, maxColorValue = 255)
  }
  
  tibble::tribble(
    ~"colour", ~"percent",
    rgbm(227, 159, 96), 0,
    rgbm(230, 168, 112), 0.2465319,
    rgbm(240, 202, 168), 0.4982955,
    rgbm(244, 218, 193), 0.56296481,
    rgbm(248, 230, 213), 0.6080569,
    rgbm(204, 233, 231), 0.6331994,
    rgbm(33, 159, 149), 0.889297,
    rgbm(2, 146, 135), 1
  )
}


