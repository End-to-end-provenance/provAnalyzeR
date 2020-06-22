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

library(lintr)

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
prov.analyze <- function(save=FALSE, create.zip=FALSE) {
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
  analyze.prov.summary(save, create.zip)
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
  tryCatch(prov.run(r.script, ...), error = function(x) {
    print(x)
  })

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
analyze.prov.summary <- function(save, create.zip) {
  environment <- provParseR::get.environment(.analyze.env$prov)
  
  if (save) {
    save.to.text.file(environment)
  }
  else {
    generate.summaries(environment)
  }
  
  if (create.zip) {
    save.to.zip.file(environment)
  }
}

#' generate.summaries creates the text summary, writing it to the
#' current output sink(s)
#' 
#' @import lintr
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
  generate.elapsed.time.summary()
  
  # run lintr static analysis
  generate.lintr.analysis()
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
  else if (!is.null(invalid.names))
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
generate.type.changes.summary <- function(vars = NA) {
  # get all variables with type changes
  type.changes <- analyze.type.changes(vars)
  
  cat ("TYPE CHANGES:\n")
  
  if (is.double(type.changes) && type.changes == 0) {
    cat("None\n") # FIX THIS PORTION 
  }
  else if(!is.null(type.changes)) {
    # generate markers
    create.markers(type.changes, "info")

    # loop through each element, printing relevant information
    lapply(c(1:length(type.changes)), function(i) {
      var <- type.changes[[i]]

      cat(paste("The type of variable ", names(type.changes[i]), " has changed. ",
                names(type.changes[i]), " was declared on line ", var$startLine[1],
                " in script ", var$scriptNum[1], ".\n", sep = ""))

      lapply(c(2:nrow(var)), function(j) {
        cat(paste("\t", j-1, ": Script ", var$scriptNum[j], ", line ", 
                  var$startLine[j], "\n", sep = ""))
        
        
        # if there were container changes, print
        if (!identical(grep("c", var$changes), integer(0))) {
          cat(paste("\t\tcontainer changed to: ", var$container[j],
          "\n\t\tfrom: ", var$container[j-1], "\n",
          sep = ""))
        }
    
        # if there were dimension changes, print
        if (!identical(grep("d", var$changes), integer(0))) {
          cat(paste("\t\tdimension changed to: ", var$dimension[j],
          "\n\t\tfrom: ", var$dimension[j-1], "\n",
          sep = ""))
        }

        # if there were type changes, print
        if (!identical(grep("t", var$changes), integer(0))) {
          cat(paste("\t\ttype changed to: ", var$type[j],
          "\n\t\tfrom: ", var$type[j-1], "\n",
          sep = ""))
        }
        
        # print(var$code)
        # print(is.character(var$code))
        # print(nchar(var$code))
        # if (nchar(var$code) > 50) 
        #   cat(paste("code snippet: ", substring(var$code, 1, 47), "..."))
        # else
        #   cat(paste("code snippet: ", var$code))
      })

    })
  }
  
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

#' Generates markers for type changes in the markers pane of RStudio.
#' 
#' @param changes.list The list of all type changes that occurred in the script.
#' @param type ID for the markers pane specifying the type of change.
#' Available: "error", "warning", "info", "style", or "usage"
#' 
#' @noRd
generate.elapsed.time.summary <- function() {
  analyze.elapsed.time()
}

#' Runs lintr with specified linters. Prints output as markers, which appear
#' in the markers pane on RStudio.
#' 
#' @noRd
generate.lintr.analysis <- function() {
  environment <- provParseR::get.environment(.analyze.env$prov)
  script <- environment$value[environment$label == "script"]
  
  # set linters to use, or NULL if not desired
  linters <- with_defaults(object_usage_linter = NULL,
                          absolute_path_linter = NULL,
                          nonportable_path_linter = NULL,
                          pipe_continuation_linter = NULL,
                          assignment_linter,  # check that <- is always used for assignment
                          camel_case_linter = NULL,
                          closed_curly_linter = NULL,
                          commas_linter = NULL,
                          commented_code_linter = NULL,
                          cyclocomp_linter,  # check for overly complicated expressions
                          equals_na_linter = NULL,  # possibly include?
                          extraction_operator_linter = NULL, # possibly include?
                          function_left_parentheses_linter = NULL,
                          implicit_integer_linter = NULL,
                          infix_spaces_linter = NULL,
                          line_length_linter = NULL,
                          no_tab_linter = NULL,
                          object_length_linter = NULL,
                          object_name_linter = NULL,
                          object_name_linter = NULL,
                          paren_brace_linter = NULL,
                          semicolon_terminator_linter = NULL,
                          seq_linter, # check for 1:nrow(...) type expressions
                          single_quotes_linter = NULL,
                          spaces_inside_linter = NULL,
                          spaces_left_parentheses_linter = NULL,
                          todo_comment_linter = NULL,
                          trailing_blank_lines_linter = NULL,
                          trailing_whitespace_linter = NULL,
                          T_and_F_symbol_linter,  # avoid using T or F for TRUE and FALSE
                          undesirable_function_linter,  # report use of functions like options or sapply
                          undesirable_operator_linter,  # report use of undesirable operators
                          unneeded_concatenation_linter)  # report if c() used unnecessarily)
  
    print(lint(script, linters=linters))
}

#' Generates markers for type changes in the markers pane of RStudio.
#' 
#' @param changes.list The list of all type changes that occurred in the script.
#' @param type ID for the markers pane specifying the type of change.
#' Available: "error", "warning", "info", "style", or "usage"
#' 
#' 
#' @noRd
create.markers <- function(changes.list, type) {
  environment <- provParseR::get.environment(.analyze.env$prov)
  script <- environment$value[environment$label == "script"]
  
  # markers example
  markers <- lapply(c(1:length(changes.list)), function(i) {
    var <- changes.list[[i]]
    
    markers <- lapply(c(2:nrow(var)), function(j) {
      marker <- list()
      marker$type <- type
      marker$file <- script
      marker$line <- var$startLine[j]
      marker$column <- 1
      marker$message <- paste("The type of variable", names(changes.list[i]), "has changed.")
      
      return(marker)
    })
  })
  # combine lists into one list
  markers <- unlist(markers, recursive = FALSE)

  if (rstudioapi::isAvailable()) {
    rstudioapi::callFun("sourceMarkers",
                        name = "type changes",
                        markers = markers)
  }
}
