#' Read economic complexity data from the complexitydata GitHub repository
#'
#' @param name
#' @param refresh
#' @param export_dir
#' @param .validate_name
#'
#' @return
#' @export
#'
#' @examples
read_complexitydata <- function(name = NULL,
                                refresh = FALSE,
                                export_dir = tempdir(),
                                .validate_name = TRUE) {
  
  base_url <- "https://github.com/aiti-flinders/complexitydata/raw/master/data/"
  url <- paste0(base_url, name, ".rda")
  
  out_path <- file.path(export_dir, paste0(name, ".rda"))
  
  if (!file.exists(out_path) | refresh) {
    
    tryCatch(
      utils::download.file(url,
                           destfile = out_path,
                           mode = "wb"),
      error = "Download failed."
    )
    
    
    
  } else {
    message("Reading ", name, " file found in ", export_dir)
  }
  
  load(out_path)
  
  d <- get(name)
  
  return(d)
}
