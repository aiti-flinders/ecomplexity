#' Read and load into the environment, economic complexity data from the complexitydata GitHub repository
#'
#' @param name the name of the dataset available at `aiti-flinders/complexitydata`
#' @param refresh TRUE to force re download the file, even if it already exists on disk. 
#' @param export_dir directory to save the downloaded file. The default saves to a temporary directory. 
#'
#' @return the downloaded data is loaded into the current R environment. 
#' @export
#'
#' @examples \dontrun{
#' read_complexitydata("sa3_indp1_occp1")
#' }
read_complexitydata <- function(name = NULL,
                                refresh = FALSE,
                                export_dir = tempdir()) {
  
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
