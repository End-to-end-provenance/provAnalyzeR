#' Addin for RStudio 
#'
#' Runs the analyzer on the active script. This function is added as an item in
#' the Addins menu.
#' 
#' @export
prov.analyze.run.addin <- function() {
  # code here from lintr
  filename <- rstudioapi::getSourceEditorContext()
  if (filename$path == "") {
    return("Current source has no path. Please save before continue")
  }
  
  provAnalyzeR::prov.analyze.run(filename$path)
}