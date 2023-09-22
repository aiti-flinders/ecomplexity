#' Add product names to a dataset
#'
#' @param data 
#' @param digits 
#'
#' @return dataframe
#' @export
#'
#' @examples
#' @importFrom dplyr left_join filter select %>%
#' 
add_product_names <- function(data, digits) {
  
  if (!digits %in% c("four", "4", 4, "six", "6", 6)) {
    stop()
  } else if (digits %in% c("four", "4", 4)) {
    digits <- "4digit"
  } else {
    digits <- "6digit"
  }
  
  prod_data <- product_data %>%
    filter(level == digits) %>%
    select(hs_product_name_short_en, hs_product_code)
  
  data %>% 
    left_join(prod_data)
}