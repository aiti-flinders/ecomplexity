#' Harmonised System (1992) product data. 
#' 
#' A dataset containing the names, level, and product code of products categorised under the 
#' 1992 version of the Harmonised System
#' 
#' @format A dataframe with 6406 rows and 5 columns:
#' \describe{
#' \item{product_id}{id}
#' \item{hs_product_code}{character representation of the product code}
#' \item{hs_product_name_short_en}{english name of the product}
#' \item{level}{level of detail the product is categorised under (can be section 2 digits, 4 digits, 6 digits)}
#' \item{parent_id}{which overarching level the product fits under}
#' }
#' @source \url{https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/3BAL1O/WQZPYX&version=4.0}
"product_data"