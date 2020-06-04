# Copyright (C) President and Fellows of Harvard College and 
# Trustees of Mount Holyoke College, 2018.

# This program is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public
#   License along with this program.  If not, see
#   <http://www.gnu.org/licenses/>.

###############################################################################

#' Provenance analysis functions
#' 
#' prov.analyze uses the provenance from the last execution of prov.run and outputs
#' a text analysis to the R console based on dynamic analysis.
#' 
#' These functions use provenance collected using the rdtLite or rdt packages.
#' 
#' For provenance collected from executing a script file, the analysis identifies:
#' \itemize{
#'   \item The name of the script file executed
#'   \item The names of any variables used but not set in the current session
#'   \item Any variables assigned to reserved values c, t, T, or F
#'   \item Any variable type changes that occurred
#'   \item Any functions that were defined multiple times
#' }
#' 
#' Creating a zip file depends on a zip executable being on the search path.
#' By default, it looks for a program named zip.  To use a program with 
#' a different name, set the value of the R_ZIPCMD environment variable.  This
#' code has been tested with Unix zip and with 7-zip on Windows.  
#'
#' @param save if true saves the analysis to the file prov-analyze.txt in the 
#' provenance directory
#' @param create.zip if true all of the provenance data will be packaged up
#'   into a zip file stored in the current working directory.
#' 
#' @export
#' @examples 
#' \dontrun{prov.analyze ()}
#' @rdname analyze
prov.analyze <- function (save=FALSE, create.zip=FALSE) 
{
  # clear environment first
  .clear()
  
  # Determine which provenance collector to use
  tool <- get.tool()
  if (tool == "rdtLite") {
    prov.json <- rdtLite::prov.json
  } else {
    prov.json <- rdt::prov.json
  }
  
  # initialise environment
  .analyze.init(prov.json(), is.file = FALSE)
  
  # create the analysis summary
  analyze.prov.summary (save, create.zip)
}

#' prov.analyze.file
#' 
#' prov.analyze.file reads a JSON file that contains provenance and outputs
#' a text analysis to the R console based on dynamic analysis.
#' 
#' @param prov.file the path to the file containing provenance
#'
#' @export
#' @examples 
#' \dontrun{
#' testdata <- system.file("testdata", "prov.json", package = "provAnalyzeR")
#' prov.analyze.file(testdata)}
#' @rdname analyze
prov.analyze.file <- function(prov.file, save=FALSE, create.zip=FALSE)
{
  # clear environment first
  .clear()
  
  # # initialise environment
  .analyze.init(prov.file, is.file = TRUE)
  
  # create the analysis summary
  analyze.prov.summary (save, create.zip)
}

#' prov.analyze.run
#'
#' prov.analyze.run executes a script, collects provenance, and outputs a
#' text summary of any coding anomalies to the console.
#'
#' @param r.script the name of a file containing an R script
#' @param ... extra parameters are passed to the provenance collector.  See rdt's prov.run function
#'    or rdtLites's prov.run function for details.
#'    
#' @export 
#' @examples 
#' \dontrun{
#' testdata <- system.file("testscripts", "console.R", package = "provAnalzeR")
#' prov.analyze.run (testdata)}
#' @rdname analyze
prov.analyze.run <- function(r.script, save=FALSE, create.zip=FALSE, ...) {
  # clear environment first
  .clear()
  
  # Determine which provenance collector to use
  tool <- get.tool()
  if (tool == "rdtLite") {
    prov.run <- rdtLite::prov.run
    prov.json <- rdtLite::prov.json
  } else {
    prov.run <- rdt::prov.run
    prov.json <- rdt:: prov.json
  }
  
  # run the script
  tryCatch (prov.run(r.script, ...), error = function(x) {print (x)})
  
  # initialise environment
  .analyze.init(prov.json(), is.file = FALSE)
  
  # Create the provenance summary
  analyze.prov.summary (save, create.zip)
}

#' analyze.prov.summary summarizes course-grained coding anomolies found in the provenance.
#' 
#' @param prov the json in a string
#' @param save if true saves the summary to the file prov-summary.txt in the 
#' provenance directory
#' @param create.zip if true all of the provenance data will be packaged up
#'   into a zip file stored in the current working directory.
#' @noRd
analyze.prov.summary <- function (save, create.zip) {
  environment <- provParseR::get.environment(.analyze.env$prov)
  
  if (save) {
    save.to.text.file(environment)
  }
  else {
    generate.summaries(environment)
  }
  
  if (create.zip) {
    save.to.zip.file (environment)
  }
}

#' generate.summaries creates the text summary, writing it to the
#' current output sink(s)
#' 
#' @param environment the environemnt data frame extracted from the provenance
#' @noRd
generate.summaries <- function(environment) {
  # get file name
  script.path <- environment[environment$label == "script", ]$value
  script.file <- sub(".*/", "", script.path)
  
  if (script.file != "") {
    cat (paste ("POTENTIAL ANOMALIES DETECTED for", script.file, "\n\n"))
  } else {
    # NOT CURRENTLY IMPLEMENTED
    cat (paste ("POTENTIAL ANOMALIES DETECTED for Console Session\n\n"))
  }
  
  # dynamic anlaysis summaries
  generate.preexisting.summary(provParseR::get.preexisting(.analyze.env$prov)) 
  generate.invalid.names.summary()
  generate.type.changes.summary()
  generate.function.reassignments.summary()
}

#' generate.preexisting.summary lists variables in the global environment that are 
#' used but not set by a script or a console session.
#' 
#' @param vars a data frame of preexisting variables
#' 
#' @noRd
generate.preexisting.summary <- function(vars) {
  cat (paste ("PRE-EXISTING:\n"))
  
  if (is.null(vars) || nrow(vars) == 0) {
    cat("None\n")
  } else {
    for (i in 1:nrow(vars)) {
      cat(vars[i, "name"], "\n")
    }
  }
  cat("\n")
}

#' generate.invalid.names.summary lists variables that use the 
#' name of a predefined entity but will not cause an R error.
#' Currently checked for: c, t, T, F
#' 
#' @noRd
generate.invalid.names.summary <- function() {
  cat (paste ("INVALID NAMES:\n"))
  
  # get all invalid names
  invalid.names <- analyze.invalid.names()
  
  if (is.double(invalid.names) && invalid.names == 0) 
    cat("None\n")
  else if(!is.null(invalid.names))
    print(invalid.names)
  
  cat("\n")
}

#' generate.type.changes.summary lists variables in the global environment that 
#' undergo one or more type changes
#' 
#' @param var Optional. Variable name(s) to be queried. If not NA, the results will
#'            be filtered to show only those with the given variable name.
#' 
#' @noRd
generate.type.changes.summary <- function(var = NA) {
  cat (paste ("TYPE CHANGES:\n"))
  
  # get all variables with type changes
  type.changes <- analyze.type.changes(var)
  
  if (is.double(type.changes) && type.changes == 0) 
    cat("None\n")
  else if(!is.null(type.changes))
    print(type.changes)
  
  cat("\n")
  
}

#' generate.function.reassignments.summary lists function variables in the global
#' environment that are bound to multiple functions throughout the script.
#' 
#' @param var Optional. Variable name(s) to be queried. If not NA, the results will
#'            be filtered to show only those with the given variable name.
#' 
#' @noRd
generate.function.reassignments.summary <- function(var = NA) {
  cat (paste ("FUNCTION REASSIGNMENTS:\n"))
  
  # get all function variables that are reassigned
  function.reassignments <- analyze.function.reassignments(var)
  
  if (is.double(function.reassignments) && function.reassignments == 0) 
    cat("None\n")
  else if(!is.null(function.reassignments))
    print(function.reassignments)
  
  cat("\n")
}
