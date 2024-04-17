#' Harmonised System (1992) product data. 
#' 
#' A dataset containing the names, level, and product code of products categorised under the 
#' 1992 version of the Harmonised System
#' 
#' @format A tibble with 6,406 rows and 5 columns:
#' \describe{
#' \item{product_id}{id}
#' \item{hs_product_code}{character representation of the product code - HS92 4 digits}
#' \item{hs_product_name_short_en}{english name of the product}
#' \item{level}{level of detail the product is categorised under (can be section 2 digits, 4 digits, 6 digits)}
#' \item{parent_id}{which overarching level the product fits under}
#' }
#' @source \url{https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/3BAL1O/WQZPYX&version=4.0}
"product_data"

#' Harmonised System (1992) product data.
#' 
#' A dataset containing the names, product code, group name and section name of products
#' categorised under the 1992 version of the Harmonised System. 
#' 
#' @format A tibble with 1,248 rows and 6 columns:
#' \describe{
#' \item{product_id}{id}
#' \item{parent_id}{link to products parent group}
#' \item{hs_product_name_short_en}{english short name of the product}
#' \item{hs_product_code}{character representation of the product code - HS92 4 digits}
#' \item{group_name}{english short name of the product group}
#' \item{section_name}{english short name of the product section}
#' }
#' @source \url{https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/3BAL1O/WQZPYX&version=4.0}
"product_data_all"


#' Atlas of Economic Complexity Countries.
#' 
#' The name and ISO-3 representation of countries included in the Atlas of Economic Complexity
#' 
#' @format A tibble with 133 rows and 2 columns:
#' \describe{
#' \item{Country}{Country}
#' \item{location_code}{3-digit ISO country code}
#' }
"atlas_countries"

#' ANZSIC to Harmonised System concordance
#' 
#' The relationship between product codes and Australian industry classifications.
#' 
#' @format A tibble with 1,521 rows and 2 columns:
#' \describe{
#' \item{hs_product_code}{character representation of the product code - HS92 4 digits}
#' \item{anzsic}{character representation of the ANZSIC class}
#' }
"anzsic_hs"

